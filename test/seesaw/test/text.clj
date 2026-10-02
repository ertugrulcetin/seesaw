;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns seesaw.test.text
  (:require
   [lazytest.core :refer [defdescribe expect expect-it it]]
   [seesaw.core :as core]
   [seesaw.graphics :as g]
   [seesaw.text :refer :all])
  (:import (java.awt Color Rectangle)))

(defdescribe highlight!-test
  (it "adds highlights in independent layers"
    (let [t (core/text :multi-line? true :text "hello world, hello")]
      (highlight! t :find [[0 5] [13 18]] :color :yellow)
      (highlight! t :diff [[6 11]] :color :green)
      (expect (= [[0 5] [13 18]] (highlights t :find)))
      (expect (= [[6 11]] (highlights t :diff)))
      (expect (= 3 (count (.getHighlights (.getHighlighter t)))))
      (highlight! t :find [[1 2]])
      (expect (= [[1 2]] (highlights t :find)))
      (expect (= 2 (count (.getHighlights (.getHighlighter t)))))
      (clear-highlights! t :diff)
      (expect (= [] (highlights t :diff)))
      (clear-highlights! t)
      (expect (= 0 (count (.getHighlights (.getHighlighter t)))))))
  (it "clamps and skips empty ranges"
    (let [t (core/text :text "abc")]
      (highlight! t :x [[-5 2] [2 2] [1 100]])
      (expect (= [[0 2] [1 3]] (highlights t :x)))))
  (it "paints rounded highlights behind the text"
    (let [t (doto (core/text :text "WWWWWWWW" :foreground :black :background :white)
              (.setSize 200 30))
          _ (highlight! t :x [[0 4]] :color :red :arc 2)
          img (g/snapshot t)
          r (rect-at t 1)
          red? (fn [x y] (let [c (Color. (.getRGB img x y))]
                           (and (> (.getRed c) 200) (< (.getGreen c) 80))))]
      (expect (some (fn [y] (red? (inc (.x r)) y)) (range (.y r) (+ (.y r) (.height r))))))))

(defdescribe geometry-test
  (it "maps between positions and points"
    (let [t (doto (core/text :text "hello") (.setSize 200 30))
          r (rect-at t 3)]
      (expect (instance? Rectangle r))
      (expect (= 3 (position-at t [(inc (.x r)) (+ (.y r) (quot (.height r) 2))])))))
  (it "scrolls to a position without moving the caret"
    (let [t (core/text :multi-line? true :text (apply str (repeat 200 "line\n")))
          sp (core/scrollable t :preferred-size [100 :by 100])
          f (core/pack! (core/frame :content sp))]
      (core/config! t :caret-position 0)
      (scroll-to-position! t 900 :padding 10)
      (expect (pos? (.y (.getViewPosition (.getViewport sp)))))
      (expect (= 0 (.getCaretPosition t)))
      (.dispose f))))

(defdescribe editing-test
  (it "inserts, appends, deletes and replaces"
    (let [t (core/text :text "world")]
      (insert-text! t 0 "hello ")
      (append-text! t "!")
      (expect (= "hello world!" (core/text t)))
      (expect (= 12 (length t)))
      (delete-text! t 5 11)
      (expect (= "hello!" (core/text t)))
      (core/selection! t [0 5])
      (replace-selection! t "bye")
      (expect (= "bye!" (core/text t))))))
