;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.ds.product.loader
  (:require-macros
    [app.common.data.macros :as dm]
    [app.main.style :as stl])
  (:require
    [app.common.data :as d]
    [app.common.math :as mth]
    [app.util.i18n :as i18n :refer [tr]]
    [beicon.v2.core :as rx]
    [rumext.v2 :as mf]))

(defn- get-tips []
  [{:title (tr "loader.tips.01.title") :message (tr "loader.tips.01.message")}
   {:title (tr "loader.tips.02.title") :message (tr "loader.tips.02.message")}
   {:title (tr "loader.tips.03.title") :message (tr "loader.tips.03.message")}
   {:title (tr "loader.tips.04.title") :message (tr "loader.tips.04.message")}
   {:title (tr "loader.tips.05.title") :message (tr "loader.tips.05.message")}
   {:title (tr "loader.tips.06.title") :message (tr "loader.tips.06.message")}
   {:title (tr "loader.tips.07.title") :message (tr "loader.tips.07.message")}
   {:title (tr "loader.tips.08.title") :message (tr "loader.tips.08.message")}
   {:title (tr "loader.tips.09.title") :message (tr "loader.tips.09.message")}
   {:title (tr "loader.tips.10.title") :message (tr "loader.tips.10.message")}])

(mf/defc loader-icon* {::mf/private true}
  [{:keys [width height title] :as _opts :rest _}]
  (let [class     (stl/css :loader)
        ;; Full SVG as a Clojure string (quotes escaped)
        svg-html  (str
          "<svg width=\"133\" height=\"19\" viewBox=\"0 0 133 19\" fill=\"none\" xmlns=\"http://www.w3.org/2000/svg\">"
          "<g id=\"Group 69\">"
            "<g id=\"PodConverge\">"
              "<path d=\"M29.4219 0.828358C31.0801 0.828358 32.29 1.18673 33.0518 1.90346C33.8135 2.61432 34.1943 3.59543 34.1943 4.84678C34.1943 5.41077 34.1094 5.95126 33.9395 6.46825C33.7695 6.97936 33.4912 7.43466 33.1045 7.83416C32.7236 8.23365 32.2139 8.55089 31.5752 8.78589C30.9365 9.01501 30.1484 9.12957 29.2109 9.12957H28.042V13.712H25.3174V0.828358H29.4219ZM29.2812 3.06669H28.042V6.89124H28.9385C29.4482 6.89124 29.8906 6.82368 30.2656 6.68855C30.6406 6.55343 30.9307 6.34194 31.1357 6.05407C31.3408 5.7662 31.4434 5.39608 31.4434 4.94371C31.4434 4.30923 31.2676 3.83924 30.916 3.53374C30.5645 3.22237 30.0195 3.06669 29.2812 3.06669Z\" fill=\"url(#paint0_linear_10859_41715)\"/>"
              "<path d=\"M44.0789 8.76826C44.0789 9.59074 43.9675 10.3192 43.7449 10.9537C43.5281 11.5882 43.2087 12.1258 42.7869 12.5664C42.3709 13.0011 41.8669 13.3301 41.2752 13.5534C40.6892 13.7766 40.0271 13.8882 39.2888 13.8882C38.5974 13.8882 37.9617 13.7766 37.3816 13.5534C36.8074 13.3301 36.3064 13.0011 35.8787 12.5664C35.4568 12.1258 35.1287 11.5882 34.8943 10.9537C34.6658 10.3192 34.5515 9.59074 34.5515 8.76826C34.5515 7.67553 34.7449 6.75024 35.1316 5.99238C35.5183 5.23452 36.0691 4.65878 36.7839 4.26516C37.4988 3.87155 38.3513 3.67474 39.3416 3.67474C40.2615 3.67474 41.0759 3.87155 41.7849 4.26516C42.4998 4.65878 43.0593 5.23452 43.4636 5.99238C43.8738 6.75024 44.0789 7.67553 44.0789 8.76826ZM37.2849 8.76826C37.2849 9.4145 37.3552 9.95792 37.4959 10.3985C37.6365 10.8392 37.8562 11.1711 38.155 11.3943C38.4539 11.6176 38.8435 11.7292 39.324 11.7292C39.7986 11.7292 40.1824 11.6176 40.4753 11.3943C40.7742 11.1711 40.991 10.8392 41.1257 10.3985C41.2664 9.95792 41.3367 9.4145 41.3367 8.76826C41.3367 8.11615 41.2664 7.57566 41.1257 7.14679C40.991 6.71205 40.7742 6.386 40.4753 6.16863C40.1765 5.95126 39.7869 5.84257 39.3064 5.84257C38.5974 5.84257 38.0818 6.08638 37.7595 6.57399C37.4431 7.06161 37.2849 7.79303 37.2849 8.76826Z\" fill=\"url(#paint1_linear_10859_41715)\"/>"
              "<path d=\"M48.145 13.8882C47.0493 13.8882 46.1558 13.4594 45.4644 12.6016C44.7788 11.738 44.436 10.472 44.436 8.80351C44.436 7.11742 44.7847 5.84257 45.4819 4.97896C46.1792 4.10948 47.0903 3.67474 48.2153 3.67474C48.6841 3.67474 49.0972 3.73936 49.4546 3.86861C49.812 3.99786 50.1196 4.17117 50.3775 4.38854C50.6411 4.60591 50.8638 4.84972 51.0454 5.11996H51.1333C51.0982 4.93196 51.0542 4.65584 51.0015 4.2916C50.9546 3.92148 50.9312 3.54255 50.9312 3.15481V0H53.6206V13
              ;; … continue pasting every <path>, <rect>, and <defs> …
              "<defs>"
                "<linearGradient id=\"paint0_linear_10859_41715\" x1=\"165.39\" y1=\"-3.2586\" x2=\"2.6737\" y2=\"66.2665\" gradientUnits=\"userSpaceOnUse\">"
                  "<stop offset=\"0.247671\" stop-color=\"#4E1587\"/>"
                  "<stop offset=\"0.485\" stop-color=\"#A62DC3\"/>"
                  "<stop offset=\"0.770098\" stop-color=\"#660579\"/>"
                "</linearGradient>"
                ;; … and so on for paint1 through paint10 …
              "</defs>"
          "</svg>")

        wrapper   {:dangerouslySetInnerHTML #js {:__html svg-html}
                   :role             "status"
                   :className        class
                   :style            #js {:opacity    0
                                          :transition "opacity 0.8s ease-in-out"}}

        ;; When the icon mounts, bump opacity to 1
        on-mount (fn [this]
                   (.setAttribute this "style" "opacity:1; transition:opacity 0.8s ease-in-out"))]
    (js/React.useEffect
      (fn []
        (let [el (js/document.querySelector (str "." class))]
          (when el (on-mount el))))
      #js [])
    [:> :div wrapper]))

(def ^:private schema:loader
  [:map
   [:class        {:optional true} :string]
   [:width        {:optional true} :int]
   [:height       {:optional true} :int]
   [:title        {:optional true} :string]
   [:overlay      {:optional true} :boolean]
   [:file-loading {:optional true} :boolean]])

(mf/defc loader* {::mf/schema schema:loader}
  [{:keys [class width height title overlay children file-loading] :rest props}]
  (let [w      (or width 133)
        h      (or height 19)
        cn     (dm/str (d/nilv class "") " "
                       (stl/css-case :wrapper true
                                     :wrapper-overlay overlay
                                     :file-loading file-loading))
        title' (or title (tr "labels.loading"))
        tips   (mf/use-memo get-tips)
        tip*   (mf/use-state nil)
        tip    @tip*]
    (mf/with-effect [file-loading tips]
      (when file-loading
        (let [sub (->> (rx/timer 1000 4000)
                       (rx/subs! #(reset! tip* (rand-nth tips))))]
          (partial rx/dispose! sub))))
    [:> :div {:className cn}
     [:> loader-icon* {:width w :height h :title title'}]
     (when (and file-loading tip)
       [:div {:className (stl/css :tips-container)}
        [:div {:className (stl/css :tip-title)}   (:title   tip)]
        [:div {:className (stl/css :tip-message)} (:message tip)]])
     children]))
