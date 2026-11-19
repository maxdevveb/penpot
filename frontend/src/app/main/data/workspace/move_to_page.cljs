;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.main.data.workspace.move-to-page
  (:require
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

(defn move-shapes-to-page
  "Moves shapes identified by `ids` from their source page to `target-page-id`."
  ([ids target-page-id]
   (move-shapes-to-page nil ids target-page-id))
  ([src-page-id ids target-page-id]
   (ptk/reify ::move-shapes-to-page
     ptk/WatchEvent
     (watch [it state _]
       (let [file-id     (:current-file-id state)
             src-page-id (or src-page-id (:current-page-id state))
             fdata       (dsh/lookup-file-data state file-id)
             dst-page    (dsh/get-page fdata target-page-id)
             undo-id     (js/Symbol)
             changes     (cls/generate-move-shapes-to-page it fdata src-page-id target-page-id ids
                                                           {:undo-id undo-id})]
         (if (nil? changes)
           (rx/empty)
           (rx/of (dwu/start-undo-transaction undo-id)
                  (dch/commit-changes changes)
                  (dws/deselect-all)
                  (ptk/data-event :layout/update {:ids [ctp/root]})
                  (notifications/success
                   (str (tr "Moved to ") (:name dst-page))))))))))
