;  Copyright (c) Dave Ray, 2012. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns seesaw.test.widgets.rounded-label
  (:require [seesaw.widgets.rounded-label :refer :all]
            [lazytest.core :refer [defdescribe expect-it it expect]])
  (:import (java.awt Color)
           (java.awt.image BufferedImage)
           (javax.swing JLabel)))

(defdescribe rounded-label-test
  (expect-it "creates a sub-class of label"
    (instance? JLabel (rounded-label)))
  (it "honors label options"
    (let [rl (rounded-label :text "hi" :background :blue)]
      (expect (= "hi" (.getText rl)))
      (expect (= Color/BLUE (.getBackground rl)))))
  (it "can be painted. Issue #136"
    (let [rl (doto (rounded-label :text "hi") (.setSize 50 20))
          img (BufferedImage. 50 20 BufferedImage/TYPE_INT_ARGB)
          g (.createGraphics img)]
      (.paint rl g)
      (.dispose g)
      (expect true))))
