;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns seesaw.test.keymap
  (:require
   [lazytest.core :refer [defdescribe describe expect it]]
   [seesaw.core :refer [action button frame menubar menu menu-item text vertical-panel]]
   [seesaw.keymap :refer [map-key trigger!]]
   [seesaw.keystroke :refer [keystroke]])
  (:import (javax.swing JComponent)))

(defdescribe map-key-test
  (describe "a keystroke and action"
    (it "maps the key to the action in :descendants scope by default"
      (let [b (button)
            k (keystroke "A")
            a (action)
            _ (map-key b k a)
            id (.. b (getInputMap JComponent/WHEN_ANCESTOR_OF_FOCUSED_COMPONENT) (get k))]
        (expect (= a (.. b (getActionMap) (get id))))))
    (it "maps the key to the action in the given scope"
      (let [b (button)
            k (keystroke "A")
            a (action)
            _ (map-key b k a :scope :self)
            id (.. b (getInputMap JComponent/WHEN_FOCUSED) (get k))]
        (expect (= a (.. b (getActionMap) (get id)))))))

  (describe "a keystroke and a function"
    (it "maps the key to an action that calls the function"
      (let [b (button)
            k (keystroke "A")
            called (atom nil)
            a (fn [e] (reset! called true))
            _ (map-key b k a)
            id (.. b (getInputMap JComponent/WHEN_ANCESTOR_OF_FOCUSED_COMPONENT) (get k))]
        (.. b (getActionMap) (get id) (actionPerformed nil))
        (expect @called))))

  (it "returns a function that undoes its effect"
        (let [b (button)
              k (keystroke "A")
              called (atom 0)
              a (fn [e] (swap! called inc))
              remove-fn (map-key b k a)
              id (.. b (getInputMap JComponent/WHEN_ANCESTOR_OF_FOCUSED_COMPONENT) (get k))]
          (expect (.. b (getActionMap) (get id)))
          (remove-fn)
          (expect (nil? (.. b (getActionMap) (get id))))))

  (describe "a keystroke and a button"
    (it "maps the key to .doClick on the button"
      (let [k (keystroke "A")
            called (atom nil)
            b (button :listen [:action (fn [_] (reset! called true))])
            _ (map-key b k b)
            id (.. b (getInputMap JComponent/WHEN_ANCESTOR_OF_FOCUSED_COMPONENT) (get k))]
        (.. b (getActionMap) (get id) (actionPerformed nil))
        (expect @called))))
  (it "can assign an :id to a mapping"
    (let [k (keystroke "A")
          b (button)
          _ (map-key b k b :id :foo :scope :global)
          id (.. b (getInputMap JComponent/WHEN_IN_FOCUSED_WINDOW) (get k))]
      (expect (= id :foo)))))

(defdescribe trigger!-test
  (it "performs a :self mapping"
    (let [called (atom nil)
          t (text)]
      (map-key t "menu B" (fn [_] (reset! called :bold)) :scope :self)
      (expect (trigger! t "menu B"))
      (expect (= :bold @called))))
  (it "finds :descendants mappings on ancestors"
    (let [called (atom 0)
          t (text)
          p (vertical-panel :items [t])]
      (map-key p "F2" (fn [_] (swap! called inc)))
      (expect (trigger! t "F2"))
      (expect (= 1 @called))))
  (it "finds :global mappings and menu accelerators in the window"
    (let [called (atom [])
          t  (text)
          mi (menu-item :text "Find" :key "menu F" :listen [:action (fn [_] (swap! called conj :menu))])
          f  (frame :content (vertical-panel :items [t]) :menubar (menubar :items [(menu :text "Edit" :items [mi])]))]
      (map-key (.getContentPane f) "F3" (fn [_] (swap! called conj :global)) :scope :global)
      (expect (trigger! t "F3"))
      (expect (trigger! t "menu F"))
      (expect (= [:global :menu] @called))
      (.dispose f)))
  (it "returns false when nothing is bound"
    (expect (false? (trigger! (text) "F11"))))
  (it ":none blocks a look and feel binding so a global mapping wins"
    (let [called (atom 0)
          t (text :text "abc")
          f (frame :content (vertical-panel :items [t]))
          select-all "menu A"]
      ; the text field's own select-all binding comes from the look and feel
      (expect (trigger! t select-all))
      (map-key (.getContentPane f) select-all (fn [_] (swap! called inc)) :scope :global)
      (expect (trigger! t select-all))
      (expect (= 0 @called))
      (map-key t select-all :none :scope :self)
      (expect (trigger! t select-all))
      (expect (= 1 @called))
      (.dispose f))))
