;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns seesaw.test.laf
  (:require
   [lazytest.core :refer [defdescribe expect expect-it it]]
   [seesaw.core :as core]
   [seesaw.laf :refer :all]))

(defdescribe laf-test
  (it "switches look and feel and updates open windows"
    (let [original (laf)
          changes (atom [])
          stop (on-change #(swap! changes conj (class %)))
          l (core/label "hi")
          f (core/frame :content l)]
      (try
        (set-laf! :flat-dark :defaults {"Component.arc" 7})
        (expect (instance? com.formdev.flatlaf.FlatDarkLaf (laf)))
        (expect (dark?))
        (expect (= 7 (javax.swing.UIManager/get "Component.arc")))
        (expect (instance? com.formdev.flatlaf.ui.FlatLabelUI (core/invoke-now (.getUI l))))
        (set-laf! :flat-light)
        (expect (not (dark?)))
        (core/invoke-now nil)
        (expect (= [com.formdev.flatlaf.FlatDarkLaf com.formdev.flatlaf.FlatLightLaf] @changes))
        (finally
          (stop)
          (.dispose f)
          (set-laf! original)))))
  (it "converts color defaults"
    (put-defaults! {"Seesaw.testColor" "#ff0000"})
    (expect (= java.awt.Color/RED (javax.swing.UIManager/get "Seesaw.testColor"))))
  (it "rejects unknown look and feels"
    (expect (try (set-laf! :nope) false (catch IllegalArgumentException _ true)))))
