;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns seesaw.test.coverage
  "Generic bean options and listeners, and the widgets and helpers that
  cover the rest of Swing."
  (:require
   [lazytest.core :refer [defdescribe expect expect-it it]]
   [seesaw.color :as color]
   [seesaw.core :refer :all]
   [seesaw.graphics :as g]
   [seesaw.laf :as laf]
   [seesaw.pref :as pref]
   [seesaw.swingx :as sx]
   [seesaw.table]
   [seesaw.timer :as timer]
   [seesaw.tree])
  (:import (java.awt Color)
           (java.awt.event MouseEvent FocusEvent)
           (java.util EventObject)
           (javax.swing DefaultListModel JLayeredPane)))

(defdescribe bean-option-fallback-test
  (it "sets and gets any bean property without an explicit option"
    (let [b (button :text "x" :default-capable? false :icon-text-gap 9 :rollover-enabled? true)]
      (expect (= 9 (config b :icon-text-gap)))
      (expect (false? (config b :default-capable?)))
      (expect (true? (config b :rollover-enabled?)))))
  (it "converts values by property type"
    (let [t (text :caret-color :red :disabled-text-color "#00ff00" :selected-text-color :blue)]
      (expect (= Color/RED (config t :caret-color)))
      (expect (= Color/GREEN (config t :disabled-text-color)))))
  (it "works with config! later and with ratom bindings"
    (let [a (seesaw.ratom/ratom 3)
          b (button :icon-text-gap a)]
      (expect (= 3 (config b :icon-text-gap)))
      (reset! a 12)
      (invoke-now nil)
      (expect (= 12 (config b :icon-text-gap)))))
  (it "still rejects unknown options"
    (expect (try (button :no-such-thing 1) false (catch IllegalArgumentException _ true)))))

(defdescribe generic-listener-test
  (it "listens to any listener type the target supports"
    (let [m (DefaultListModel.)
          seen (atom [])]
      (listen m :interval-added (fn [e] (swap! seen conj (:index0 (event-info e)))))
      (.addElement m "a")
      (.addElement m "b")
      (expect (= [0 1] @seen))))
  (it "supports table model events"
    (let [t (table :model [:columns [:a] :rows [[1]]])
          seen (atom 0)]
      (listen (config t :model) :table-changed (fn [_] (swap! seen inc)))
      (seesaw.table/insert-at! (config t :model) 0 [2])
      (expect (pos? @seen))))
  (it "returns a function that removes the listener"
    (let [m (DefaultListModel.)
          seen (atom 0)
          remove-fn (listen m :interval-added (fn [_] (swap! seen inc)))]
      (.addElement m "a")
      (remove-fn)
      (.addElement m "b")
      (expect (= 1 @seen)))))

(defdescribe layer!-test
  (it "puts a widget on a window's layered pane, optionally filling it"
    (let [f (frame :size [300 :by 200] :content (label "content"))
          overlay (label "overlay")]
      (pack! f)
      (layer! f overlay :bounds :fill)
      (expect (= JLayeredPane/PALETTE_LAYER
                 (.getLayer (.getLayeredPane f) overlay)))
      ; the window may still be resized by the platform (e.g. a minimum width
      ; on macOS); :fill follows it once the resize reaches the UI thread
      (loop [i 0]
        (invoke-now nil)
        (when (and (< i 40) (not= (width (.getLayeredPane f)) (width overlay)))
          (Thread/sleep 50)
          (recur (inc i))))
      (expect (= (width (.getLayeredPane f)) (width overlay)))
      (unlayer! overlay)
      (expect (nil? (parent overlay)))
      (dispose! f)))
  (it "builds a layered pane from [widget layer] pairs"
    (let [a (label "a") b (label "b")
          p (layered-pane :items [[a :default] [b :popup]])]
      (expect (= JLayeredPane/POPUP_LAYER (.getLayer p b))))))

(defdescribe new-widgets-test
  (it "internal frames in a desktop pane"
    (let [i (internal-frame :title "Doc" :content (label "hi") :size [200 :by 100])
          d (desktop-pane :items [i])]
      (expect (= "Doc" (config i :title)))
      (expect (= [i] (config d :items)))
      (expect (.isClosable i))))
  (it "formatted text with a value"
    (let [f (formatted-text :format :integer :value 42)]
      (expect (= 42 (config f :value)))))
  (it "scroll bar and color chooser selections"
    (let [sb (scroll-bar :min 0 :max 100 :value 30)
          cc (color-chooser :color :red)]
      (expect (= 30 (selection sb)))
      (selection! sb 50)
      (expect (= 50 (config sb :value)))
      (expect (= Color/RED (selection cc)))
      (let [seen (atom nil)]
        (listen cc :selection (fn [_] (reset! seen (selection cc))))
        (selection! cc :blue)
        (expect (= Color/BLUE @seen)))))
  (it "jlayer paints over its view"
    (let [l (jlayer (label "x") :paint (fn [c g] (g/draw g (g/rect 0 0 4 4) (g/style :background :red))))]
      (.setSize l 10 10)
      (.doLayout l)
      (expect (= (.getRGB Color/RED) (.getRGB (g/snapshot l) 1 1))))))

(defdescribe helpers-test
  (it "click! fires action listeners"
    (let [n (atom 0) b (button :listen [:action (fn [_] (swap! n inc))])]
      (click! [b b])
      (expect (= 2 @n))))
  (it "children and parent"
    (let [a (label "a") p (vertical-panel :items [a])]
      (expect (= [a] (children p)))
      (expect (= p (parent a)))))
  (it "expand-all!, expand! and collapse! trees"
    (let [t (tree :model (seesaw.tree/simple-tree-model :kids :kids {:kids [{:kids [{:kids []}]}]}))]
      (expand-all! t)
      (expect (= 3 (.getRowCount t)))
      (collapse! t 0)
      (expect (= 1 (.getRowCount t)))
      (expand! t 0)
      ; JTree remembers the expanded child
      (expect (= 3 (.getRowCount t)))))
  (it "event-info turns an event into data"
    (let [e (MouseEvent. (label) MouseEvent/MOUSE_CLICKED 0 0 3 4 2 false)]
      (expect (= {:x 3 :y 4 :click-count 2} (select-keys (event-info e) [:x :y :click-count])))))
  (it "event-kind names AWT events"
    (expect (= :mouse-clicked (event-kind (MouseEvent. (label) MouseEvent/MOUSE_CLICKED 0 0 1 1 1 false))))
    (expect (= :focus-gained (event-kind (FocusEvent. (label) FocusEvent/FOCUS_GAINED))))
    (expect (nil? (event-kind (EventObject. "x")))))
  (it "color helpers"
    (expect (= "#3b82f6" (color/hex "#3b82f6")))
    (expect (< (.getRed (color/darker "#808080")) 128 (.getRed (color/brighter "#808080")))))
  (it "timer start!/stop!"
    (let [t (timer/timer identity :start? false)]
      (timer/start! t)
      (expect (timer/running? t))
      (timer/stop! t)
      (expect (not (timer/running? t)))))
  (it "preferences round trip EDN values"
    (let [n (pref/preferences-node* "seesaw-test-coverage")]
      (pref/put-pref! n :volume {:level 7})
      (expect (= {:level 7} (pref/get-pref n :volume)))
      (expect (= :none (pref/get-pref n :missing :none)))
      (.removeNode n)))
  (it ":current paints with the graphics' color"
    (let [img (g/buffered-image 4 4)
          gr  (.createGraphics img)]
      (.setColor gr Color/BLUE)
      (g/draw gr (g/rect 0 0 4 4) (g/style :background :current))
      (expect (= (.getRGB Color/BLUE) (.getRGB img 1 1)))))
  (expect-it "laf-name"
    (string? (laf/laf-name))))

(defdescribe add!-null-layout-test
  (it "adds to containers without a layout manager"
    (let [d (desktop-pane)
          f (internal-frame :title "x")]
      (add! d f)
      (expect (= [f] (children d))))))
