;  Copyright (c) Dave Ray, 2012. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns seesaw.test.javafx
  (:require
   [lazytest.core :refer [defdescribe expect expect-it it]]
   [seesaw.core :as core]
   [seesaw.javafx :refer :all])
  (:import (java.awt Color)
           (javafx.embed.swing JFXPanel)))

(defdescribe jfxpanel-test
  (expect-it "creates a JFXPanel"
    (instance? JFXPanel (jfxpanel)))
  (it "supports the default widget options"
    (let [p (jfxpanel :id :fx :background :blue)]
      (expect (= :fx (core/id-of p)))
      (expect (= Color/BLUE (.getBackground p)))))
  (expect-it "is reachable through seesaw.core/jfxpanel"
    (instance? JFXPanel (core/jfxpanel :id :fx))))
