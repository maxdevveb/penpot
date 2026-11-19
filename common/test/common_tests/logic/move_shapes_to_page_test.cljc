;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns common-tests.logic.move-shapes-to-page-test
  (:require
   [app.common.files.changes-builder :as pcb]
   [app.common.logic.libraries :as cll]
   [app.common.logic.shapes :as cls]
   [app.common.test-helpers.components :as thc]
   [app.common.test-helpers.compositions :as tho]
   [app.common.test-helpers.files :as thf]
   [app.common.test-helpers.ids-map :as thi]
   [app.common.test-helpers.shapes :as ths]
   [app.common.test-helpers.variants :as thv]
   [app.common.types.components-list :as ctkl]
   [app.common.types.file :as ctf]
   [app.common.types.pages-list :as ctpl]
   [app.common.uuid :as uuid]
   [clojure.test :as t]))

(t/use-fixtures :each thi/test-fixture)

(t/deftest test-move-regular-board-to-page-and-undo
  (let [file      (-> (thf/sample-file :file1 :page-label :page1)
                      (tho/add-frame :board-1)
                      (ths/add-sample-shape :rect-1 :parent-label :board-1)
                      (thf/add-sample-page :page2))

        src-id    (thi/id :page1)
        dst-id    (thi/id :page2)
        board-id  (thi/id :board-1)
        rect-id   (thi/id :rect-1)

        changes   (cls/generate-move-shapes-to-page nil (:data file) src-id dst-id #{board-id})
        file'     (thf/apply-changes file changes)
        src-page' (ctpl/get-page (:data file') src-id)
        dst-page' (ctpl/get-page (:data file') dst-id)
        board'    (get-in dst-page' [:objects board-id])
        rect'     (get-in dst-page' [:objects rect-id])

        undone    (thf/apply-undo-changes file' changes)
        src-undo  (ctpl/get-page (:data undone) src-id)
        dst-undo  (ctpl/get-page (:data undone) dst-id)]

    ;; Moved cleanly with IDs and hierarchy intact
    (t/is (nil? (get-in src-page' [:objects board-id])))
    (t/is (nil? (get-in src-page' [:objects rect-id])))
    (t/is (some? board'))
    (t/is (= uuid/zero (:parent-id board')))
    (t/is (= uuid/zero (:frame-id board')))
    (t/is (= board-id (:parent-id rect')))

    ;; Undo restores source page and removes from target page
    (t/is (some? (get-in src-undo [:objects board-id])))
    (t/is (some? (get-in src-undo [:objects rect-id])))
    (t/is (nil? (get-in dst-undo [:objects board-id])))))

(t/deftest test-move-main-component-to-page-preserves-library-sync-and-undo
  (let [file       (-> (thf/sample-file :file1 :page-label :page1)
                       (tho/add-simple-component :comp1 :comp1-root :comp1-child
                                                 :child-params {:fills (ths/sample-fills-color :fill-color "#111111")})
                       (thf/add-sample-page :page2)
                       (thf/add-sample-page :page3)
                       (thc/instantiate-component :comp1 :copy-root :children-labels [:copy-child]))

        src-id     (thi/id :page1)
        dst-id     (thi/id :page2)
        copy-pg-id (thi/id :page3)
        comp-id    (thi/id :comp1)
        root-id    (thi/id :comp1-root)
        child-id   (thi/id :comp1-child)
        before-cnt (count (ctkl/components-seq (:data file)))

        ;; Move main component from page1 to page2
        changes    (cls/generate-move-shapes-to-page nil (:data file) src-id dst-id #{root-id})
        file'      (thf/apply-changes file changes)
        fdata'     (:data file')
        comp'      (ctkl/get-component fdata' comp-id)
        dst-page'  (ctpl/get-page fdata' dst-id)
        moved-root (get-in dst-page' [:objects root-id])

        ;; Edit moved main component on page2 and sync to copy on page3
        edit-chgs  (cls/generate-update-shapes (pcb/empty-changes nil dst-id)
                                               #{child-id}
                                               #(assoc % :fills (ths/sample-fills-color :fill-color "#fabada"))
                                               (:objects dst-page')
                                               {})
        edited     (thf/apply-changes file' edit-chgs)
        sync-chgs  (cll/generate-sync-file-changes (pcb/empty-changes)
                                                   nil
                                                   :components
                                                   (:id edited)
                                                   comp-id
                                                   (:id edited)
                                                   {(:id edited) edited}
                                                   (:id edited))
        synced     (thf/apply-changes edited sync-chgs)
        copy-page  (ctpl/get-page (:data synced) copy-pg-id)
        copy-child (get-in copy-page [:objects (thi/id :copy-child)])

        ;; Undo move
        undone     (thf/apply-undo-changes file' changes)
        comp-undo  (ctkl/get-component (:data undone) comp-id)]

    ;; Local component stays alive and points to destination page
    (t/is (= before-cnt (count (ctkl/components-seq fdata'))))
    (t/is (some? comp'))
    (t/is (not (:deleted comp')))
    (t/is (= dst-id (:main-instance-page comp')))
    (t/is (= root-id (:main-instance-id comp')))
    (t/is (= root-id (:id (ctf/get-component-root fdata' comp'))))
    (t/is (true? (:main-instance moved-root)))
    (t/is (= comp-id (:component-id moved-root)))

    ;; Existing copy on page3 receives edits made to the moved main component
    (t/is (= "#fabada" (:fill-color (first (:fills copy-child)))))

    ;; Undo restores both board page and :main-instance-page
    (t/is (= src-id (:main-instance-page comp-undo)))
    (t/is (= root-id (:id (ctf/get-component-root (:data undone) comp-undo))))
    (t/is (some? (get-in (ctpl/get-page (:data undone) src-id) [:objects root-id])))
    (t/is (nil? (get-in (ctpl/get-page (:data undone) dst-id) [:objects root-id])))))

(t/deftest test-move-variant-container-to-page-preserves-variants-and-undo
  (let [file       (-> (thf/sample-file :file1 :page-label :page1)
                       (thv/add-variant-with-child :variant-container
                                                   :comp1 :root1
                                                   :comp2 :root2
                                                   :child1 :child2)
                       (thf/add-sample-page :page2))

        src-id     (thi/id :page1)
        dst-id     (thi/id :page2)
        cont-id    (thi/id :variant-container)
        comp1-id   (thi/id :comp1)
        comp2-id   (thi/id :comp2)
        root1-id   (thi/id :root1)
        root2-id   (thi/id :root2)
        before-cnt (count (ctkl/components-seq (:data file)))

        ;; Move variant container from page1 to page2
        changes    (cls/generate-move-shapes-to-page nil (:data file) src-id dst-id #{cont-id})
        file'      (thf/apply-changes file changes)
        fdata'     (:data file')
        dst-page'  (ctpl/get-page fdata' dst-id)
        comp1'     (ctkl/get-component fdata' comp1-id)
        comp2'     (ctkl/get-component fdata' comp2-id)
        root1'     (get-in dst-page' [:objects root1-id])
        root2'     (get-in dst-page' [:objects root2-id])

        ;; Undo move
        undone     (thf/apply-undo-changes file' changes)
        comp1-undo (ctkl/get-component (:data undone) comp1-id)
        comp2-undo (ctkl/get-component (:data undone) comp2-id)]

    ;; Local component count unchanged and both variants remain alive
    (t/is (= before-cnt (count (ctkl/components-seq fdata'))))
    (t/is (some? (get-in dst-page' [:objects cont-id])))
    (t/is (nil? (get-in (ctpl/get-page fdata' src-id) [:objects cont-id])))

    ;; Variant 1 preserved and relocated
    (t/is (some? comp1'))
    (t/is (not (:deleted comp1')))
    (t/is (= dst-id (:main-instance-page comp1')))
    (t/is (= root1-id (:main-instance-id comp1')))
    (t/is (= cont-id (:variant-id comp1')))
    (t/is (= root1-id (:id (ctf/get-component-root fdata' comp1'))))
    (t/is (true? (:main-instance root1')))
    (t/is (= comp1-id (:component-id root1')))

    ;; Variant 2 preserved and relocated
    (t/is (some? comp2'))
    (t/is (not (:deleted comp2')))
    (t/is (= dst-id (:main-instance-page comp2')))
    (t/is (= root2-id (:main-instance-id comp2')))
    (t/is (= cont-id (:variant-id comp2')))
    (t/is (= root2-id (:id (ctf/get-component-root fdata' comp2'))))
    (t/is (true? (:main-instance root2')))
    (t/is (= comp2-id (:component-id root2')))

    ;; Undo restores container page and :main-instance-page for all variants
    (t/is (= src-id (:main-instance-page comp1-undo)))
    (t/is (= src-id (:main-instance-page comp2-undo)))
    (t/is (some? (get-in (ctpl/get-page (:data undone) src-id) [:objects cont-id])))
    (t/is (nil? (get-in (ctpl/get-page (:data undone) dst-id) [:objects cont-id])))))
