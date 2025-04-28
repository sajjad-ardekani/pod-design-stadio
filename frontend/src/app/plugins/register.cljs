;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.plugins.register
  (:require
   [app.main.data.event :as ev]
   [app.main.data.notifications :as ntf]
   [app.util.i18n :as i18n :refer [tr]]
   [app.util.globals :as ug]
   [app.main.refs :as refs]
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.schema :as sm]
   [app.common.types.plugins :as ctp]
   [app.common.uuid :as uuid]
   [app.main.repo :as rp]
   [app.main.store :as st]
   [app.plugins.core :as pc]
   [app.util.object :as obj]
   [potok.v2.core :as ptk]
   [app.util.globals :refer [global]]
   [beicon.v2.core :as rx]))

;; Stores the installed plugins information
(defonce ^:private registry (atom {}))

(defn plugins-list
  "Retrieves the plugin data as an ordered list of plugin elements"
  []
  (->> (:ids @registry)
       (mapv #(dm/get-in @registry [:data %]))))

(defn get-plugin
  [id]
  (dm/get-in @registry [:data id]))

(defn parse-manifest
  "Read the manifest.json defined by the plugins definition and transforms it into an
  object that will be stored in the register."
  [plugin-url ^js manifest]
  (let [name (obj/get manifest "name")
        desc (obj/get manifest "description")
        code (obj/get manifest "code")
        icon (obj/get manifest "icon")

        permissions (into #{} (obj/get manifest "permissions" []))
        permissions
        (cond-> permissions
          (contains? permissions "content:write")
          (conj "content:read")

          (contains? permissions "library:write")
          (conj "library:read")

          (contains? permissions "comment:write")
          (conj "comment:read"))

        origin (obj/get (js/URL. plugin-url) "origin")

        prev-plugin
        (->> (:data @registry)
             (vals)
             (d/seek (fn [plugin]
                       (and (= name (:name plugin))
                            (= origin (:host plugin))))))

        plugin-id (d/nilv (:plugin-id prev-plugin) (str (uuid/next)))

        manifest
        (d/without-nils
         {:plugin-id plugin-id
          :url plugin-url
          :name name
          :description desc
          :host origin
          :code code
          :icon icon
          :permissions (into #{} (map str) permissions)})]
    (when (sm/validate ::ctp/registry-entry manifest)
      manifest)))

(defn save-to-store
  []
  ;; TODO: need this for the transition to the new schema. We can remove eventually
  (let [registry (update @registry :data d/update-vals d/without-nils)]
    (->> (rp/cmd! :update-profile-props {:props {:plugins registry}})
         (rx/subs! identity))))

(defn load-from-store
  []
  (reset! registry (get-in @st/state [:profile :props :plugins] {})))

(defn init
  []
  (load-from-store))

(defn install-plugin!
  [plugin]
  (letfn [(update-ids [ids]
            (conj
             (->> ids (remove #(= % (:plugin-id plugin))))
             (:plugin-id plugin)))]
    (swap! registry #(-> %
                         (update :ids update-ids)
                         (update :data assoc (:plugin-id plugin) plugin)))
    (save-to-store)))

(defn remove-plugin!
  [{:keys [plugin-id]}]
  (letfn [(update-ids [ids]
            (->> ids
                 (remove #(= % plugin-id))))]
    (swap! registry #(-> %
                         (update :ids update-ids)
                         (update :data dissoc plugin-id)))
    (save-to-store)))

(defn check-permission
  [plugin-id permission]
  (or (= plugin-id "TEST")
      (let [{:keys [permissions]} (dm/get-in @registry [:data plugin-id])]
        (contains? permissions permission))))

;; Define a predicate that returns true when the workspace is loaded.
(defn app-loaded? []
  ;; Adjust the selector as needed (for example, if the workspace element has id "workspace")
  (boolean (js/document.getElementById "left-sidebar-aside")))

;; Define a polling function that waits until workspace-loaded? returns true.
(defn wait-for-app [callback]
  (if (app-loaded?)
    (callback)
    (js/requestAnimationFrame #(wait-for-app callback))))

(def default-plugin-manifest-url
  "https://design.sweelux.com/react-plugin/manifest.json")

(defn auto-install-and-open-default-plugin []
  "Fetches the default plugin manifest, installs it, and triggers opening the plugin once the workspace is loaded."
  (-> (js/fetch default-plugin-manifest-url)
      (.then (fn [response]
               (.json response)))
      (.then (fn [manifest]
               (let [plugin (parse-manifest default-plugin-manifest-url manifest)]
                 (when plugin
                   (install-plugin! plugin)
                   ;; Emit event to signal plugin start (optional)
                   (st/emit! (ptk/event :app.main.data.event/event
                                        {:app.main.data.event/name "start-plugin"
                                         :name (:name plugin)
                                         :host (:host plugin)}))
                   ;; Instead of a fixed timeout, wait until the workspace is loaded
                   (wait-for-app
                    (fn []
                      (let [user-can-edit? (:can-edit (deref refs/permissions))]
                        (pc/open-plugin! plugin))))))))
      (.catch (fn [err]
                (js/console.error "Failed to install default plugin:" err)))))

(defn init
  "Loads stored plugins and auto-installs & opens the default plugin."
  []
  (load-from-store)
  (auto-install-and-open-default-plugin))

