;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as
;; defined by the Mozilla Public License, v. 2.0.
;;
;; Copyright (c) 2020-2021 UXBOX Labs SL

(ns app.main.ui.shapes.svg-defs
  (:require
   #_[app.common.geom.shapes :as gsh]
   [app.common.geom.matrix :as gmt]
   [rumext.alpha :as mf]
   [app.util.svg :as usvg]
   [app.util.object :as obj]
   ))

(defn add-matrix [attrs transform-key transform-matrix]
  (update attrs
          transform-key
          (fn [val]
            (if val
              (str transform-matrix " " val)
              (str transform-matrix)))))


(mf/defc svg-node [{:keys [node prefix-id transform]}]
  (cond
    (string? node) node

    :else
    (let [{:keys [tag attrs content]} node

          transform-gradient? (and (#{:linearGradient :radialGradient} tag)
                                   (= "userSpaceOnUse" (get attrs :gradientUnits "userSpaceOnUse")))

          transform-pattern?  (and false
                                   (= :pattern tag)
                                   (= "userSpaceOnUse" (get attrs :patternContentUnits "userSpaceOnUse")))
          
          attrs (-> attrs
                    (usvg/update-attr-ids prefix-id)
                    (usvg/clean-attrs)

                    (cond->
                        transform-gradient? (add-matrix :gradientTransform transform)
                        transform-pattern?  (add-matrix :patternTransform transform)))

          [wrapper wrapper-props] (if (= tag :mask)
                                    ["g" #js {:transform (str transform)}]
                                    [mf/Fragment (obj/new)])]

      [:> (name tag) (clj->js attrs)
       [:> wrapper wrapper-props
        (for [node content] [:& svg-node {:node node
                                          :prefix-id prefix-id}])]])))

(mf/defc svg-defs [{:keys [shape render-id]}]
  (let [svg-defs (:svg-defs shape)
        transform (mf/use-memo (mf/deps shape) #(if (= :svg-raw (:type shape))
                                                  (gmt/matrix)
                                                  (usvg/svg-transform-matrix shape)))
        prefix-id
        (fn [id]
          (cond->> id
            (contains? svg-defs id) (str render-id "-")))]

    (when (and svg-defs (not (empty? svg-defs)))
      (for [svg-def (vals svg-defs)]
        [:& svg-node {:node svg-def
                      :prefix-id prefix-id
                      :transform transform}]))))

