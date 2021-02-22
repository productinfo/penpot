;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as
;; defined by the Mozilla Public License, v. 2.0.
;;
;; Copyright (c) 2016-2020 UXBOX Labs SL

(ns app.main.ui.shapes.attrs
  (:require
   [rumext.alpha :as mf]
   [cuerdas.core :as str]
   [app.util.object :as obj]
   [app.main.ui.context :as muc]
   [app.util.svg :as usvg]))

(defn- stroke-type->dasharray
  [style]
  (case style
    :mixed "5,5,1,5"
    :dotted "5,5"
    :dashed "10,10"
    nil))

(defn add-border-radius [attrs shape]
  (if (or (:rx shape) (:ry shape))
    (obj/merge! attrs #js {:rx (:rx shape)
                           :ry (:ry shape)})
    attrs))

(defn add-fill [attrs shape render-id]
  (let [fill-attrs (cond
                     (contains? shape :fill-color-gradient)
                     (let [fill-color-gradient-id (str "fill-color-gradient_" render-id)]
                       {:fill (str/format "url(#%s)" fill-color-gradient-id)})

                     (contains? shape :fill-color)
                     {:fill (:fill-color shape)}

                     ;; If contains svg-attrs the origin is svg. If it's not svg origin
                     ;; we setup the default fill as transparent (instead of black)
                     (not (contains? shape :svg-attrs))
                     {:fill "transparent"}

                     :else
                     {})

        fill-attrs (cond-> fill-attrs
                     (contains? shape :fill-opacity)
                     (assoc :fillOpacity (:fill-opacity shape)))]

    (obj/merge! attrs (clj->js fill-attrs))))

(defn add-stroke [attrs shape render-id]
  (let [stroke-style (:stroke-style shape :none)
        stroke-color-gradient-id (str "stroke-color-gradient_" render-id)]
    (if (not= stroke-style :none)
      (let [stroke-attrs
            (cond-> {:strokeWidth (:stroke-width shape 1)}
              (:stroke-color-gradient shape)
              (assoc :stroke (str/format "url(#%s)" stroke-color-gradient-id))

              (not (:stroke-color-gradient shape))
              (assoc :stroke (:stroke-color shape nil)
                     :strokeOpacity (:stroke-opacity shape nil))

              (not= stroke-style :svg)
              (assoc :strokeDasharray (stroke-type->dasharray stroke-style)))]
        (obj/merge! attrs (clj->js stroke-attrs)))
      attrs)))


(defn extract-svg-attrs
  [render-id svg-defs svg-attrs]
  (let [replace-id (fn [id]
                     (if (contains? svg-defs id)
                       (str render-id "-" id)
                       id))
        svg-attrs (-> svg-attrs
                      (usvg/update-attr-ids replace-id)
                      (usvg/clean-attrs))

        attrs  (-> svg-attrs (dissoc :style) (clj->js))
        styles (-> svg-attrs (:style {}) (clj->js))]
    [attrs styles]))

(defn extract-style-attrs
  ([shape]
   (let [render-id (mf/use-ctx muc/render-ctx)
         svg-defs  (:svg-defs shape {})
         svg-attrs (:svg-attrs shape {})

         [svg-attrs svg-styles] (mf/use-memo
                                 (mf/deps render-id svg-defs svg-attrs)
                                 #(extract-svg-attrs render-id svg-defs svg-attrs))

         styles (-> (obj/new)
                    (obj/merge! svg-styles)
                    (add-fill shape render-id)
                    (add-stroke shape render-id))]
     (-> (obj/new)
         (obj/merge! svg-attrs)
         (add-border-radius shape)
         (obj/set! "style" styles)))))
