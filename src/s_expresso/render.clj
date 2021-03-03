(ns s-expresso.render
  "Utilities for rendering scenes."
  (:require
   [clojure.spec.alpha :as s]))

(defprotocol RenderOp
  "An operation with dependencies.
  Implementors must ensure that `apply-op!` may be run regardless of whether or
  not it has been initialized, and it should render as much as is possible with
  current resources."
  :extend-via-metadata true
  (op-deps [op]
    "Returns the resources which are used by this operation.
    This should not do any heavy computation.")
  (apply-op! [op render-state]
    "Renders as much as possible with currently-initialized resources."))

(defprotocol Resolver
  "Manages the resolution of a resource for rendering."
  :extend-via-metadata true
  (step-resolver [resolver]
    "Steps the resolution of the resource.
    Returns a vector of the next resolver, and if it's completed.")
  (resolved-resource [resolver]
    "Returns the resource after complete resolution."))

(s/def ::game-state (s/keys :req [::systems]
                            :opt [::interpolator]))
(s/def ::render-state (s/keys :req [::resolvers ::resources]))

(s/def ::system (s/fspec :args (s/cat :state ::game-state)
                         :ret (s/coll-of (partial satisfies? RenderOp))))
(s/def ::systems (s/coll-of ::system))
(s/def ::interpolator (s/fspec :args (s/cat :new-state ::game-state
                                            :old-state ::game-state
                                            :factor float?)
                               :ret ::game-state))

(s/def ::resource-id any?)
(s/def ::resolvers (s/map-of ::resource-id (partial satisfies? Resolver)))
(s/def ::resources (s/map-of ::resource-id any?))

(defn prepare-ops
  "Collects all the [[RenderOps]] from the `game-state`.
  When `last-state` is passed, the state used is interpolated between it and the
  current state, based on `factor`. Returns a seq of [[RenderOps]]."
  ([game-state]
   (prepare-ops game-state nil nil))
  ([game-state last-state factor]
   (let [state (cond-> game-state
                 (and last-state
                      (::interpolator game-state))
                 ((::interpolator game-state) last-state factor))
         systems (::systems state)]
     (apply concat (map #(% state) systems)))))
(s/fdef prepare-ops
  :args (s/cat :game-state ::game-state
               :optional-args
               (s/? (s/cat :last-state ::game-state
                           :factor float?)))
  :ret (s/coll-of (partial satisfies? RenderOp)))

(defn step-resolvers
  "Steps each [[Resolver]] in `render-state`.
  If a resolver is complete, it will be added to the resolved resources in the
  state."
  [render-state]
  (let [resolvers (::resolvers render-state)
        [resources resolvers]
        (reduce (fn [[resources resolvers] [key resolver]]
                  (let [[new-resolver done?] (step-resolver resolver)]
                    (if done?
                      [(assoc resources key (resolved-resource new-resolver)) resolvers]
                      [resources (assoc resolvers key new-resolver)])))
                [(::resources render-state) {}]
                resolvers)]
    (assoc render-state
           ::resources resources
           ::resolvers resolvers)))
(s/fdef step-resolvers
  :args (s/cat :render-state ::render-state)
  :ret ::render-state)

(defn render-scene!
  "Runs all the render operations in sequence."
  [ops render-state]
  (run! #(apply-op! % render-state) ops))
(s/fdef render-scene!
  :args (s/cat :ops (partial satisfies? RenderOp)
               :render-state ::render-state)
  :ret nil?)

(defn collect-deps
  "Collects a map of [[Resolver]]s for a sequence of `ops`.
  This assumes that each resource will have a unique key, or that if both keys
  are the same, the resolvers will load the same resource."
  [ops]
  (apply merge (map op-deps ops)))
(s/fdef collect-deps
  :args (s/cat :ops (partial satisfies? RenderOp))
  :ret ::resolvers)

(defn step-renderer
  "Renders the current scene and returns an updated render state."
  ([render-state game-state]
   (step-renderer render-state game-state nil nil))
  ([render-state game-state last-state factor]
   (let [ops (prepare-ops game-state last-state factor)]
     (render-scene! ops render-state)
     (let [new-deps (apply dissoc (collect-deps ops) (keys (::resources render-state)))
           render-state (update render-state ::resolvers #(merge new-deps %))]
       (step-resolvers render-state)))))
(s/fdef step-renderer
  :args (s/cat :render-state ::render-state
               :game-state ::game-state
               :optional-args
               (s/? (s/cat :last-state ::game-state
                           :factor float?)))
  :ret ::render-state)
