;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.data.workspace.drawing.common
  (:require
   [app.common.data.macros :as dm]
   [app.common.geom.shapes :as gsh]
   [app.common.math :as mth]
   [app.common.pages.helpers :as cph]
   [app.common.types.modifiers :as ctm]
   [app.common.types.shape :as cts]
   [app.main.data.workspace.shapes :as dwsh]
   [app.main.data.workspace.state-helpers :as wsh]
   [app.main.data.workspace.undo :as dwu]
   [app.main.worker :as uw]
   [beicon.core :as rx]
   [potok.core :as ptk]))

(defn clear-drawing
  []
  (ptk/reify ::clear-drawing
    ptk/UpdateEvent
    (update [_ state]
      (update state :workspace-drawing dissoc :tool :object))))

(defn handle-finish-drawing
  []
  (ptk/reify ::handle-finish-drawing
    ptk/WatchEvent
    (watch [_ state _]
      (let [tool    (dm/get-in state [:workspace-drawing :tool])
            shape   (dm/get-in state [:workspace-drawing :object])
            objects (wsh/lookup-page-objects state)
            page-id (:current-page-id state)]

        (prn "handle-finish-drawing" shape)

        (rx/concat
         (when (:initialized? shape)
           (let [click-draw? (:click-draw? shape)
                 text?       (cph/text-shape? shape)
                 vbox        (dm/get-in state [:workspace-local :vbox])

                 min-side    (min 100 mth/floor (:width vbox) (:height vbox))

                 shape
                 (cond-> shape
                   (not click-draw?)
                   (assoc :grow-type :fixed)

                   (and click-draw? (not text?))
                   (-> (assoc :width min-side :height min-side)
                       (cts/setup-rect)
                       (gsh/transform-shape (ctm/move-modifiers (- (/ min-side 2)) (- (/ min-side 2)))))

                   (and click-draw? text?)
                   (-> (assoc :height 17 :width 4 :grow-type :auto-width)
                       (cts/setup-rect))

                   :always
                   (dissoc :initialized? :click-draw?))]

             ;; Add & select the created shape to the workspace
             (rx/concat
              (if (= :text (:type shape))
                (rx/of (dwu/start-undo-transaction (:id shape)))
                (rx/empty))

              (rx/of (dwsh/add-shape shape {:no-select? (= tool :curve)}))

              (if (= :frame (:type shape))
                (->> (uw/ask! {:cmd :selection/query
                               :page-id page-id
                               :rect (:selrect shape)
                               :include-frames? true
                               :full-frame? true})
                     (rx/map #(cph/clean-loops objects %))
                     (rx/map #(dwsh/move-shapes-into-frame (:id shape) %)))
                (rx/empty)))))

         ;; Delay so the mouse event can read the drawing state
         (->> (rx/of (clear-drawing))
              (rx/delay 0)))))))

