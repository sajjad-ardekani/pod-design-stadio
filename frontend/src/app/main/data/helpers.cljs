;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.data.helpers
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.files.helpers :as cfh]
   [app.common.geom.point :as gpt]
   [app.common.geom.shapes :as gsh]
   [clojure.string :as str]
   [app.common.types.path :as path]))

(defn lookup-profile
  ([state]
   (:profile state))
  ([state profile-id]
   (dm/get-in state [:profiles profile-id])))

(defn lookup-libraries
  "Retrieve all libraries, including the local file."
  [state]
  (:files state))

(defn lookup-file
  ([state]
   (lookup-file state (:current-file-id state)))
  ([state file-id]
   (dm/get-in state [:files file-id])))

(defn lookup-file-data
  ([state]
   (lookup-file-data state (:current-file-id state)))
  ([state file-id]
   (dm/get-in state [:files file-id :data])))

(defn get-page
  [fdata page-id]
  (dm/get-in fdata [:pages-index page-id]))

(defn lookup-page
  ([state]
   (let [file-id (:current-file-id state)
         page-id (:current-page-id state)]
     (lookup-page state file-id page-id)))
  ([state page-id]
   (let [file-id (:current-file-id state)]
     (lookup-page state file-id page-id)))
  ([state file-id page-id]
   (dm/get-in state [:files file-id :data :pages-index page-id])))

(defn lookup-page-objects
  ([state]
   (lookup-page-objects state
                        (:current-file-id state)
                        (:current-page-id state)))
  ([state page-id]
   (lookup-page-objects state
                        (:current-file-id state)
                        page-id))
  ([state file-id page-id]
   (-> (lookup-page state file-id page-id)
       (get :objects))))

(defn process-selected
  ([objects selected]
   (process-selected objects selected nil))

  ([objects selected {:keys [omit-blocked?] :or {omit-blocked? false}}]
   (letfn [(selectable? [id]
             (and (contains? objects id)
                  (or (not omit-blocked?)
                      (not (dm/get-in objects [id :blocked] false)))))]
     (let [selected (->> selected (cfh/clean-loops objects))]
       (into (d/ordered-set)
             (filter selectable?)
             selected)))))

(defn split-text-shapes
  "Split text shapes from non-text shapes"
  [objects ids]
  (loop [ids (seq ids)
         text-ids []
         shape-ids []]
    (if-let [id (first ids)]
      (let [shape (get objects id)]
        (if (cfh/text-shape? shape)
          (recur (rest ids)
                 (conj text-ids id)
                 shape-ids)
          (recur (rest ids)
                 text-ids
                 (conj shape-ids id))))
      [text-ids shape-ids])))

;; DEPRECATED
(defn lookup-selected-raw
  [state]
  (dm/get-in state [:workspace-local :selected]))

(defn get-selected-ids
  [state]
  (dm/get-in state [:workspace-local :selected]))

(defn lookup-selected
  ([state]
   (lookup-selected state (:current-page-id state) nil))
  ([state options]
   (lookup-selected state (:current-page-id state) options))
  ([state page-id options]
   (let [objects  (lookup-page-objects state page-id)
         selected (dm/get-in state [:workspace-local :selected])]
     (process-selected objects selected options))))

(defn lookup-shape
  ([state id]
   (lookup-shape state (:current-page-id state) id))

  ([state page-id id]
   (let [objects (lookup-page-objects state page-id)]
     (get objects id))))

(defn lookup-shapes
  ([state ids]
   (lookup-shapes state (:current-page-id state) ids))
  ([state page-id ids]
   (let [objects (lookup-page-objects state page-id)]
     (into [] (keep (d/getf objects)) ids))))

(defn update-file
  ([state f]
   (update-file state (:current-file-id state) f))
  ([state file-id f]
   (d/update-in-when state [:files file-id] f)))

(defn update-page
  ([state f]
   (update-page state
                (:current-file-id state)
                (:current-page-id state)
                f))
  ([state page-id f]
   (update-page state
                (:current-file-id state)
                page-id
                f))
  ([state file-id page-id f]
   (d/update-in-when state [:files file-id :data :pages-index page-id] f)))

(defn filter-shapes
  ([state filter-fn]
   (filter-shapes state (:current-page-id state) filter-fn))
  ([state page-id filter-fn]
   (let [objects (lookup-page-objects state page-id)]
     (into [] (filter filter-fn) (vals objects)))))

(defn select-bool-children
  [state parent-id]
  (let [objects (lookup-page-objects state)

        shape-modifiers
        (:workspace-modifiers state)

        content-modifiers
        (dm/get-in state [:workspace-local :edit-path])]

    (reduce (fn [result id]
              (if-let [shape (get objects id)]
                (let [modifiers (dm/get-in shape-modifiers [id :modifiers])
                      shape     (if (some? modifiers)
                                  (gsh/transform-shape shape modifiers)
                                  shape)
                      modifiers (dm/get-in content-modifiers [id :content-modifiers])
                      shape     (if (some? modifiers)
                                  (update shape :content path/apply-content-modifiers modifiers)
                                  shape)]
                  (assoc result id shape))
                result))
            {}
            (cfh/get-children-ids objects parent-id))))

(defn get-viewport-center
  [state]
  (when-let [{:keys [x y width height]} (get-in state [:workspace-local :vbox])]
    (gpt/point (+ x (/ width 2)) (+ y (/ height 2)))))

(defn lookup-team-files
  ([state]
   (lookup-team-files state (:current-team-id state)))
  ([state team-id]
   (->> state
        :files
        (filter #(= team-id (:team-id (val %))))
        (into {}))))

(defn lookup-team-projects
  ([state]
   (lookup-team-projects (:current-team-id state)))
  ([state team-id]
   (->> state
        :projects
        (filter #(= team-id (:team-id (val %))))
        (into {}))))

(defn- ->clj-if-js
  "Convert JS object to CLJ map only when necessary. Keep string keys (no keywordize)."
  [x]
  (cond
    (map? x) x
    (nil? x) nil
    :else
    (try
      (js->clj x :keywordize-keys false)
      (catch :default _
        x))))

(defn- get-from-js-or-map
  "Fetch key `kstr` from `m` that can be either a CLJ map (with string or keyword keys)
   or a JS object. Returns nil if not found. Tries multiple variants:
   string, keyword, underscore/kebab variants."
  [m kstr]
  (when (some? m)
    (let [alt1 (str/replace kstr "_" "-")
          alt2 (str/replace kstr "-" "_")
          try-keys [kstr (keyword kstr) alt1 alt2 (keyword alt1) (keyword alt2)]]
      (cond
        (map? m)
        (some (fn [k] (when (contains? m k) (get m k))) try-keys)

        :else
        ;; JS object: try aget with string keys (js objects don't respond to contains?)
        (some (fn [k]
                (let [ks (if (keyword? k) (name k) (str k))
                      v  (try (aget m ks) (catch :default _ nil))]
                  (when (some? v) v)))
              try-keys)))))

(defn- lookup-plugin-namespace
  "Given plugin-data `pd` (map or JS object) and namespace `ns-str`, return the ns map/object."
  [pd ns-str]
  (when (some? pd)
    (let [pd-clj (->clj-if-js pd)
          ns-val (when (map? pd-clj) (get-from-js-or-map pd-clj ns-str))]
      (if (some? ns-val)
        (->clj-if-js ns-val)
        ;; fallback: try reading directly from original pd (JS object case)
        (let [v (get-from-js-or-map pd ns-str)]
          (when (some? v) (->clj-if-js v)))))))

(defn get-shape-plugin-data
  "Return the plugin-data value for `key` on `shape`."
  ([shape key] (get-shape-plugin-data shape nil key))
  ([shape ns key]
   (let [kstr (if (keyword? key) (name key) (str key))

         ;; 1) Try direct JS path first (works when shape and plugin-data are native JS objects)
         pd-js    (or (aget shape "plugin-data"))
         ns-js    (when (and ns pd-js) (aget pd-js ns))
         val-js   (cond
                    (and ns-js (some? ns-js)) (or (aget ns-js kstr) (aget ns-js (name (keyword kstr))))
                    (some? pd-js)            (or (aget pd-js kstr) (aget pd-js (name (keyword kstr))))
                    :else                   nil)]

     (if (some? val-js)
       val-js
       ;; 2) Fallback: robust CLJ/js->clj handling
       (let [pd-raw (or (get shape :plugin-data)
                        (get shape "plugin-data"))
             pd     (->clj-if-js pd-raw)]
         (if ns
           (let [nsmap (lookup-plugin-namespace pd ns)]
             (when (some? nsmap)
               (or (get-from-js-or-map nsmap kstr)
                   (get-from-js-or-map nsmap (str/replace kstr "_" "-"))
                   (get-from-js-or-map nsmap (str/replace kstr "-" "_")))))
           (get-from-js-or-map pd kstr)))))))

(defn- truthy-plugin-value?
  "Return true for common truthy plugin-data values."
  [v]
  (when (some? v)
    (let [s (-> (str v) str/trim str/lower-case)]
      (or (= s "1")
          (= s "true")
          (= s "yes")
          (= s "on")
          (= s "t")
          (= v 1)))))

(defn shape-is-print-area?
  "Return true if shape plugin-data marks it as a print area."
  [shape]
  (let [val (or (get-shape-plugin-data shape "shared/podconverge" "isPrintArea")
                (get-shape-plugin-data shape "shared/podconverge" "isPrintAreaBackground")
                (get-shape-plugin-data shape "shared/podconverge" "isBoardPrintArea"))]
    (boolean (truthy-plugin-value? val))))

(defn- remove-print-area-ids
  "Filter out print-area ids from `ids` using `objects` map."
  [ids objects]
  (->> ids
       (remove (fn [id]
                 (let [shape (get objects id)]
                   (and shape (shape-is-print-area? shape)))))
       (into [])))
