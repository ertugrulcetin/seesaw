;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns seesaw.test.forms
  (:require
   [lazytest.core :refer [defdescribe expect it]]
   [seesaw.core :refer [action button text]]
   [seesaw.forms :refer [forms-panel next-column next-line separator span title]])
  (:import (com.jgoodies.forms.layout Sizes)
           (javax.swing JPanel)))

(defdescribe forms-panel-test
  (it "Creates a JPanel using a JGoodies form builder"
    (let [p (forms-panel
              "pref,4dlu,80dlu,8dlu,pref,4dlu,80dlu"
              :column-groups [[1 5]]
              :leading-column-offset 0
              :line-gap-size (Sizes/pixel 5)
              :items [(title "JGoodies forms test")
                      (separator "General")
                      "Company" (span (text) 5)
                      "Contact" (span (text) 5)
                      "Click here" (next-column 2) (span (action :name "A button") 3)
                      (next-line)
                      (separator "Propeller")
                      "PTI/kW"  (text :columns 10) "Power/kW" (text :columns 10)
                      "R/mm"    (text :columns 10) "D/mm"     (text :columns 10)
                      (separator)]
              :default-dialog-border? true)]
      (expect (instance? JPanel p)))))

(defdescribe next-column-test
  (it "moves to a later column on the same line"
    (let [b (button :text "B")
          p (forms-panel "pref,4dlu,pref,4dlu,pref"
                         :items ["L" (next-column 2) b])
          cc (.getConstraints (.getLayout p) b)]
      (expect (= 5 (.gridX cc)))
      (expect (= 1 (.gridY cc))))))
