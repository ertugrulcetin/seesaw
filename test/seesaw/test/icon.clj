;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns seesaw.test.icon
  (:require
   [clojure.java.io :as jio]
   [lazytest.core :refer [defdescribe expect expect-it it]]
   [seesaw.graphics :as g]
   [seesaw.icon :refer [icon paint-icon]])
  (:import (java.awt Color)
           (java.io File)
           (javax.swing ImageIcon JLabel)))

(defdescribe icon-test
  (expect-it "returns nil given nil"
    (nil? (icon nil)))
  (it "returns its input given an Icon"
    (let [i (ImageIcon.)]
      (expect (= i (icon i)))))
  (it "returns an icon given an image"
    (let [image (g/buffered-image 16 16)
          i (icon image)]
      (expect (instance? ImageIcon i))
      (expect (= image (.getImage i)))))
  (it "returns an icon given a URL"
    (let [i (icon (jio/resource "seesaw/test/examples/rss.gif"))]
      (expect (instance? ImageIcon i))))
  (it "returns an icon given a path to an icon on the classpath"
    (let [i (icon "seesaw/test/examples/rss.gif")]
      (expect (instance? ImageIcon i))))
  (it "returns an icon given a File"
    (let [i (icon (File. "test/seesaw/test/examples/rss.gif"))]
      (expect (instance? ImageIcon i))))
  (it "returns an icon given a i18n keyword"
    (let [i (icon ::test-icon)]
      (expect (instance? ImageIcon i)))))


(defdescribe paint-icon-test
  (it "has a size and paints with the component's foreground"
    (let [seen (atom nil)
          i (paint-icon 12 10 (fn [c g] (reset! seen [c (.getColor g)]) (.fillRect g 0 0 12 10)))
          l (doto (JLabel.) (.setForeground Color/RED))
          img (g/buffered-image 20 20)
          gr (.createGraphics img)]
      (expect (= 12 (.getIconWidth i)))
      (expect (= 10 (.getIconHeight i)))
      (.paintIcon i l gr 3 4)
      (.dispose gr)
      (expect (= [l Color/RED] @seen))
      (expect (= (.getRGB Color/RED) (.getRGB img 3 4)))
      (expect (= 0 (.getRGB img 2 4)))))
  (it "uses :color instead when given"
    (let [seen (atom nil)
          i (paint-icon 4 4 (fn [c g] (reset! seen (.getColor g))) :color :blue)]
      (.paintIcon i (JLabel.) (.createGraphics (g/buffered-image 4 4)) 0 0)
      (expect (= Color/BLUE @seen)))))

(defdescribe paint-icon-arity-test
  (it "accepts options after a square size"
    (let [i (paint-icon 14 (fn [_ _]) :color :red)]
      (expect (= [14 14] [(.getIconWidth i) (.getIconHeight i)])))))
