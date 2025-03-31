(ns app.plugins.core
  (:require
   [app.main.store :as st]
   [potok.v2.core :as ptk]
   [app.util.globals :refer [global]]
   [app.util.object :as obj]))

(defn load-plugin!
  "Calls the underlying JavaScript function to load a plugin.
   This function wraps the native call to .ɵloadPlugin."
  [{:keys [plugin-id name description host code icon permissions]}]
  (try
    (st/emit! (ptk/event :save-current-plugin plugin-id))
    (.ɵloadPlugin
     ^js global
     #js {:pluginId plugin-id
          :name name
          :description description
          :host host
          :code code
          :icon icon
          :permissions (apply array permissions)}
     (fn []
       (st/emit! (ptk/event :remove-current-plugin plugin-id))))
    (catch :default e
      (st/emit! (ptk/event :remove-current-plugin plugin-id))
      (.error js/console "Error in load-plugin!:" e))))

(defn open-plugin!
  [manifest]
  (load-plugin! manifest))
