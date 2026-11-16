;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.main.data.workspace.move-to-page
  (:require
   [app.common.files.changes-builder :as pcb]
   [app.common.files.helpers :as cfh]
   [app.common.logic.shapes :as cls]
   [app.common.types.page :as ctp]
   [app.main.data.changes :as dch]
   [app.main.data.helpers :as dsh]
   [app.main.data.notifications :as notifications]
   [app.main.data.workspace.selection :as dws]
   [app.main.data.workspace.undo :as dwu]
   [app.util.i18n :refer [tr]]
   [beicon.v2.core :as rx]
   [potok.v2.core :as ptk]))

(defn- get-tree-shapes
  "Traverses shapes top-down starting from root-ids, ensuring parents appear before their children."
  [objects root-ids]
  (letfn [(traverse [id]
            (let [shape (get objects id)
                  children (:shapes shape)]
              (cons shape (mapcat traverse children))))]
    (mapcat traverse root-ids)))

(defn move-shapes-to-page
  "Moves shapes identified by `ids` from their source page to `target-page-id`."
  ([ids target-page-id]
   (move-shapes-to-page nil ids target-page-id))
  ([src-page-id ids target-page-id]
   (ptk/reify ::move-shapes-to-page
     ptk/WatchEvent
     (watch [it state _]
       (let [file-id         (:current-file-id state)
             src-page-id     (or src-page-id (:current-page-id state))
             fdata           (dsh/lookup-file-data state file-id)
             src-page        (dsh/get-page fdata src-page-id)
             dst-page        (dsh/get-page fdata target-page-id)
             src-objects     (:objects src-page)
             dst-objects     (:objects dst-page)

             ;; Clean loops and filter valid ids
             ids             (->> (cfh/clean-loops src-objects ids)
                                  (filterv #(contains? src-objects %)))
             top-level-set   (set ids)]

         (if (or (empty? ids) (nil? dst-page) (= src-page-id target-page-id))
           (rx/empty)

           (let [;; Collect all shapes to move in top-down hierarchy order
                 shapes-to-move (get-tree-shapes src-objects ids)
                 root           ctp/root
                 undo-id        (js/Symbol)

                 ;; 1. Generate delete changes on source page (handles flows, masks, touched, undo)
                 [_all-parents delete-changes]
                 (-> (pcb/empty-changes it (:id src-page))
                     (cls/generate-delete-shapes fdata src-page src-objects ids
                                                 {:undo-id undo-id}))

                 ;; Build destination additions separately. Reusing the source
                 ;; builder would replay source deletes against destination
                 ;; objects when with-objects resets the local apply count.
                 add-changes
                 (-> (pcb/empty-changes it target-page-id)
                     (pcb/with-page dst-page)
                     (pcb/with-objects dst-objects)
                     (as-> chgs
                           (reduce
                            (fn [c shape]
                              (let [top-level? (contains? top-level-set (:id shape))
                                    shape'     (if top-level?
                                                 (assoc shape :parent-id root :frame-id root)
                                                 shape)]
                                (pcb/add-object c shape' {:index nil})))
                            chgs
                            shapes-to-move)))

                 changes (pcb/concat-changes delete-changes add-changes)]

             (rx/of (dwu/start-undo-transaction undo-id)
                    (dch/commit-changes changes)
                    (dws/clear-selection)
                    (ptk/data-event :layout/update {:ids [root]})
                    (notifications/success
                     (str (tr "Moved to ") (:name dst-page)))))))))))
