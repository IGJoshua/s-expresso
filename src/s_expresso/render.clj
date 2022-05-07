(ns s-expresso.render
  "Utilities for rendering scenes."
  (:require
   [clojure.set :as set]
   [clojure.spec.alpha :as s]
   [clojure.tools.logging :as log]
   [s-expresso.memory :as mem]
   [s-expresso.resource :as res]
   [s-expresso.util :as util]))

(defprotocol RenderOp
  "An operation with dependencies.

  Implementors must ensure that `apply-op!` may be run regardless of whether or
  not it has been initialized, and it should render as much as is possible with
  current resources."
  :extend-via-metadata true
  (op-deps [op]
    "Returns the resources which are used by this operation.

    This should not do any heavy computation.

    The returned value must be a map from resource ids to resolvers.

    A resolver must be a function of no arguments returning either to the final
    resource, or a future. If it derefs to a future, then the future will return
    either the final resource, or another delay which will be run on the render
    thread, and its derefed value used as the final resource.")
  (apply-op! [op render-state]
    "Renders as much as possible with currently-initialized resources."))

(s/def ::game-state (s/keys :req [::systems]
                            :opt [::interpolator]))
(s/def ::render-state (s/keys :req [::resolvers ::resources]))

(s/def ::system (s/fspec :args (s/cat :state ::game-state)
                         :ret (s/coll-of (partial satisfies? RenderOp))))
(s/def ::coll-of-systems (s/coll-of ::system))
(s/def ::systems (s/or :const ::coll-of-systems
                       :var (s/and var? #(s/valid? ::coll-of-systems (deref %)))))
(s/def ::interpolator (s/fspec :args (s/cat :new-state ::game-state
                                            :old-state ::game-state
                                            :factor float?)
                               :ret ::game-state))

(s/def ::resource-id any?)
(s/def ::resolvers (s/map-of ::resource-id future?))
(s/def ::resources (s/map-of ::resource-id any?))
(s/def ::active-resources (s/coll-of ::resource-id :kind set?))

(defmacro resolver
  "Constructs a resolver for loading resources off the render thread.

  All the bindings are constructed off the render thread in-order, with a heap
  allocator to ensure they can be used correctly from the render thread.

  The body itself will be run from the render thread and is where all operations
  that require access to the OpenGL context should be run from. The resulting
  value is what is placed in the resource map.

  Once the body is completed but before the render thread returns all of the
  bindings will be [[res/free]]'d in reverse order."
  {:style/indent 1}
  [bindings & body]
  (let [binding-syms (map second (partition 2 bindings))]
    `(fn []
       (mem/with-heap-allocator
         (future
           (let [~@bindings]
             (delay
               (try ~@body
                    (finally ~@(map #(-> `(res/free ~%))
                                    (reverse binding-syms)))))))))))
(s/fdef resolver
  :args (s/cat :bindings (s/spec (s/* (s/cat :binding simple-symbol?
                                             :value any?)))
               :body (s/* any?)))

(defn prepare-ops
  "Collects all the [[RenderOps]] from the `game-state`.

  When `last-state` is passed, the state used is interpolated between it and the
  current state, based on `factor`. Returns a seq of [[RenderOps]]."
  ([game-state]
   (prepare-ops game-state nil nil))
  ([game-state last-state factor]
   (let [state (cond-> game-state
                 (and last-state
                      (::interpolator game-state)
                      factor)
                 ((util/when-pred (::interpolator game-state) var? deref) last-state factor))
         systems (::systems state)
         systems (cond-> systems
                   (var? systems) deref)]
     (mapcat #(% state) systems))))
(s/fdef prepare-ops
  :args (s/cat :game-state ::game-state
               :optional-args
               (s/? (s/cat :last-state ::game-state
                           :factor float?)))
  :ret (s/coll-of (partial satisfies? RenderOp)))

(defn step-resolvers
  "Updates the `render-state` by putting any resolved resources into the correct place."
  [render-state]
  (let [resolvers (::resolvers render-state)
        realized-keys (into #{}
                            (comp (filter (comp realized? val))
                                  (map key))
                            resolvers)
        new-resolvers (into {}
                            (filter (comp (complement realized-keys) key))
                            resolvers)
        resources (into (::resources render-state)
                        (comp (filter (comp realized-keys key))
                              ;; TODO(Joshua): Maybe limit the number of things
                              ;; that get loaded per frame to prevent stuttering
                              ;; while loading assets? This may not be a
                              ;; priority, but would cause issues in
                              ;; streaming-based games
                              (keep #(try [(key %)
                                           (-> %
                                               val
                                               deref
                                               (util/when-pred delay? deref))]
                                          (catch Exception e
                                            (log/error e "Exception while resolving a resource")))))
                        resolvers)]
    (assoc render-state
           ::resolvers new-resolvers
           ::resources resources)))
(s/fdef step-resolvers
  :args (s/cat :render-state ::render-state)
  :ret ::render-state)

(defn render-scene!
  "Runs all the render operations in sequence."
  [ops render-state]
  (run! #(try (apply-op! % render-state)
              (catch Exception e
                (log/error e (str "Exception while applying an operation of type: "
                                  (.getName (class %))))))
        ops))
(s/fdef render-scene!
  :args (s/cat :ops (partial satisfies? RenderOp)
               :render-state ::render-state)
  :ret nil?)

(defn collect-deps
  "Collects a map of resolvers for a sequence of `ops`.

  This assumes that each resource will have a unique key, or that if both keys
  are the same, the resolvers will load the same resource."
  [ops]
  (reduce merge
          (keep #(try (op-deps %)
                      (catch Exception e
                        (log/error e (str "Exception while collecting dependencies for an operation of type: "
                                          (.getName (class %))))
                        ;; NOTE(Joshua): return nil to remove this dependency
                        ;; from the list
                        nil))
                ops)))
(s/fdef collect-deps
  :args (s/cat :ops (partial satisfies? RenderOp))
  :ret ::resolvers)

(defn step-renderer!
  "Renders the current scene and returns an updated render state.

  This will render the scene immediately, but afterwards will take some time to
  ensure that renderer dependencies are loaded."
  ([render-state game-state]
   (step-renderer! render-state game-state nil nil))
  ([render-state game-state last-state factor]
   (let [ops (prepare-ops game-state last-state factor)]
     (render-scene! ops render-state)
     (let [active-resources (::active-resources render-state #{})
           new-deps (sequence
                     (comp (filter (comp (complement active-resources) key))
                           (keep #(try [(key %)
                                        ((val %))]
                                       (catch Exception e
                                         (log/error e "Exception while attempting to construct a resolver")
                                         ;; NOTE(Joshua): return nil to remove this resource
                                         ;; from the list
                                         nil))))
                     (collect-deps ops))
           new-resources (into {}
                               (filter (comp (complement future?) second))
                               new-deps)
           new-resolvers (into {}
                               (filter (comp future? second))
                               new-deps)]
       (update (update (update (step-resolvers render-state)
                               ::resolvers merge new-resolvers)
                       ::resources merge new-resources)
               ::active-resources set/union (into #{} (map first) new-deps))))))
(s/fdef step-renderer!
  :args (s/cat :render-state ::render-state
               :game-state ::game-state
               :optional-args
               (s/? (s/cat :last-state ::game-state
                           :factor float?)))
  :ret ::render-state)

(defn shutdown-state
  "Closes all the open resources in the `render-state`.

  If there are any resources currently being loaded, this will block until they
  are complete, before unloading them."
  [render-state]
  (run! (comp res/free val) (::resources render-state))
  (run! (comp res/free (util/when-pred delay? deref) val) (::resolvers render-state))
  nil)
(s/fdef shutdown-state
  :args (s/cat :render-state ::render-state)
  :ret nil?)
