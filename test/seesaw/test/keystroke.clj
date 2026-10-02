;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns seesaw.test.keystroke
  (:require
   [lazytest.core :refer [defdescribe expect expect-it it]]
   [seesaw.keystroke :refer [keystroke label]])
  (:import
   (java.awt Toolkit)
   (javax.swing KeyStroke)))

(defdescribe keystroke-test
  (it "creates a keystroke from a descriptor string"
    (let [ks (keystroke "ctrl S")]
      (expect (= KeyStroke (class ks)))
      (expect (= java.awt.event.KeyEvent/VK_S (.getKeyCode ks)))))
  (expect-it "returns nil for nil input"
    (nil? (keystroke nil)))
  (it "returns input if it's a KeyStroke"
    (let [ks (KeyStroke/getKeyStroke "alt X")]
      (expect (= ks (keystroke ks)))))
  (it "returns a keystroke for a string"
    (let [ks (keystroke "alt X")]
      (expect (= java.awt.event.KeyEvent/VK_X (.getKeyCode ks)))))
  (it "substitute platform-specific menu modifier for \"menu\" modifier"
    (let [ks (keystroke "menu X")]
      (expect (= java.awt.event.KeyEvent/VK_X (.getKeyCode ks)))
      (expect (= (.. (Toolkit/getDefaultToolkit) getMenuShortcutKeyMask) (bit-and 7 (.getModifiers ks))))))
  (it "returns a keystroke for a char"
    (let [ks (keystroke \A)]
      (expect (= \A (.getKeyChar ks))))))


(defdescribe keystroke-menu-test
  (it "accepts cmd as an alias for menu"
    (expect (= (keystroke "menu S") (keystroke "cmd S"))))
  (it "only replaces menu as a whole word"
    (let [ks (keystroke "CONTEXT_MENU")]
      (expect (= java.awt.event.KeyEvent/VK_CONTEXT_MENU (.getKeyCode ks)))
      (expect (= 0 (.getModifiers ks))))))

(defdescribe label-test
  (it "uses symbols on macOS"
    ; Apple's order: control, option, shift, command
    (expect (= "\u21E7\u2318G" (label "meta shift G" :mac? true)))
    (expect (= "\u2303\u2325K" (label "ctrl alt K" :mac? true)))
    (expect (= "\u2318\u21A9" (label "meta ENTER" :mac? true))))
  (it "uses names elsewhere"
    (expect (= "Ctrl+Shift+G" (label "ctrl shift G" :mac? false)))
    (expect (= "Enter" (label "ENTER" :mac? false)))
    (expect (= "Ctrl+PgDn" (label "ctrl PAGE_DOWN" :mac? false)))
    (expect (= "Alt+F4" (label "alt F4" :mac? false)))
    (expect (= "Ctrl+/" (label "ctrl SLASH" :mac? false)))))
