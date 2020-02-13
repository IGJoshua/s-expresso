(ns s-expresso.resource
  "Defines a protocol for unmanaged system resources.
  Such resources must be handled manually, like GPU memory, OS windows, etc.,
  and require special code to be released.")

(defprotocol Resource
  "Resources handle the process of freeing unmanaged resources."
  (free [live-resource] "Frees a live resource"))
