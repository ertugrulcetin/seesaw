;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns seesaw.test.undo
  (:require
   [lazytest.core :refer [defdescribe expect expect-it it]]
   [seesaw.core :as core]
   [seesaw.keymap :refer [trigger!]]
   [seesaw.undo :refer :all]))

(defdescribe undo-manager-test
  (it "undoes and redoes typing"
    (let [t (core/text)
          u (undo-manager! t)]
      (.insertString (.getDocument t) 0 "abc" nil)
      (expect (can-undo? u))
      (expect (undo! u))
      (expect (= "" (core/text t)))
      (expect (redo! u))
      (expect (= "abc" (core/text t)))))
  (it "binds menu Z and menu shift Z"
    (let [t (core/text)
          u (undo-manager! t)]
      (.insertString (.getDocument t) 0 "abc" nil)
      (trigger! t "menu Z")
      (expect (= "" (core/text t)))
      (trigger! t "menu shift Z")
      (expect (= "abc" (core/text t)))))
  (it "skips edits made in without-undo"
    (let [t (core/text)
          u (undo-manager! t)]
      (without-undo u (core/text! t "loaded"))
      (expect (not (can-undo? u)))
      (expect (= "loaded" (core/text t)))))
  (it "groups edits with as-one-edit"
    (let [t (core/text)
          u (undo-manager! t)
          d (.getDocument t)]
      (as-one-edit u
        (.insertString d 0 "a" nil)
        (.insertString d 1 "b" nil)
        (.insertString d 2 "c" nil))
      (undo! u)
      (expect (= "" (core/text t)))
      (expect (not (can-undo? u)))))
  (it "can be cleared and stopped"
    (let [t (core/text)
          u (undo-manager! t)]
      (.insertString (.getDocument t) 0 "abc" nil)
      (clear! u)
      (expect (not (can-undo? u)))
      (stop! u)
      (.insertString (.getDocument t) 0 "x" nil)
      (expect (not (can-undo? u)))))
  (expect-it "returns false when there's nothing to undo"
    (false? (undo! (undo-manager! (core/text))))))
