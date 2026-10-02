;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Undo and redo for text components and documents.

             (def u (undo-manager! editor))   ; menu Z / menu shift Z bound
             (undo! u) (redo! u)
             (without-undo u (text! editor \"loaded\"))"}
  seesaw.undo
  (:require [seesaw.keymap :refer [map-key]]
            [seesaw.meta :refer [get-meta put-meta!]])
  (:import (javax.swing.undo UndoManager CompoundEdit)
           (javax.swing.event UndoableEditListener)
           (javax.swing.text Document JTextComponent)))

(defn- to-document ^Document [target]
  (if (instance? Document target)
    target
    (.getDocument ^JTextComponent target)))

(defn undo-manager!
  "Track undoable edits of a text component (or a Document) and return the
  javax.swing.undo.UndoManager. Options:

    :limit  Maximum number of edits kept (default 100)
    :keys?  Bind menu Z to undo and menu shift Z / menu Y to redo on the
            text component (default true)

  Note that edits are recorded per document: if the component's document is
  replaced, call this again for the new one."
  [target & {:keys [limit keys?] :or {limit 100 keys? true}}]
  (let [doc (to-document target)
        um  (doto (UndoManager.) (.setLimit limit))
        l   (reify UndoableEditListener
              (undoableEditHappened [_ e]
                (let [edit (.getEdit e)]
                  (if-let [^CompoundEdit group (get-meta um ::group)]
                    (.addEdit group edit)
                    (when-not (get-meta um ::paused?)
                      (.addEdit um edit))))))]
    (.addUndoableEditListener doc l)
    (put-meta! um ::stop #(.removeUndoableEditListener doc l))
    (when (and keys? (instance? JTextComponent target))
      (map-key target "menu Z" (fn [_] (when (.canUndo um) (.undo um))) :scope :self)
      (map-key target "menu shift Z" (fn [_] (when (.canRedo um) (.redo um))) :scope :self)
      (map-key target "menu Y" (fn [_] (when (.canRedo um) (.redo um))) :scope :self))
    um))

(defn stop!
  "Stop recording edits with an undo manager made by (undo-manager!)."
  [^UndoManager um]
  (when-let [f (get-meta um ::stop)] (f))
  um)

(defn undo!
  "Undo the last edit, if any. Returns true if something was undone."
  [^UndoManager um]
  (if (.canUndo um) (do (.undo um) true) false))

(defn redo!
  "Redo the last undone edit, if any. Returns true if something was redone."
  [^UndoManager um]
  (if (.canRedo um) (do (.redo um) true) false))

(defn can-undo? [^UndoManager um] (.canUndo um))
(defn can-redo? [^UndoManager um] (.canRedo um))

(defn clear!
  "Forget all edits, e.g. after loading a new document."
  [^UndoManager um]
  (.discardAllEdits um)
  um)

(defn undo-label
  "The platform's label for the undo menu item, e.g. \"Undo Typing\"."
  [^UndoManager um]
  (.getUndoPresentationName um))

(defn redo-label
  "The platform's label for the redo menu item."
  [^UndoManager um]
  (.getRedoPresentationName um))

(defn without-undo*
  [^UndoManager um f]
  (let [was (get-meta um ::paused?)]
    (put-meta! um ::paused? true)
    (try (f) (finally (put-meta! um ::paused? was)))))

(defmacro without-undo
  "Run body without recording its edits, e.g. when loading text or applying a
  programmatic change the user shouldn't undo."
  [um & body]
  `(without-undo* ~um (fn [] ~@body)))

(defn as-one-edit*
  [^UndoManager um f]
  (if (get-meta um ::group)
    (f)
    (let [group (CompoundEdit.)]
      (put-meta! um ::group group)
      (try
        (f)
        (finally
          (put-meta! um ::group nil)
          (.end group)
          (when (.isSignificant group)
            (.addEdit um group)))))))

(defmacro as-one-edit
  "Run body so all its edits undo and redo as a single step, e.g. a
  find-and-replace-all or a formatting change that touches several ranges."
  [um & body]
  `(as-one-edit* ~um (fn [] ~@body)))
