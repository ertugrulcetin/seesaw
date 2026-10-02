;  Copyright (c) Dave Ray, 2012. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns seesaw.test.mouse
  (:require
   [lazytest.core :refer [defdescribe describe expect it]]
   [seesaw.mouse :as mouse]))

(defn- fake-event [[x y] modex btn]
  (java.awt.event.MouseEvent.
    (javax.swing.JLabel.) ,
    0, 0 , modex,
    x, y, 1, false,
    btn))

(defdescribe location-test
  (describe "with no arguments"
    (it "returns the [x y] mouse location on the whole screen"
      (let [[x y] (mouse/location)
            p     (.getLocation (java.awt.MouseInfo/getPointerInfo))]
        (expect (= (.x p) x))
        (expect (= (.y p) y)))))
  (describe "with a MouseEvent argument"
    (it "returns the [x y] of the event"
      (let [e (fake-event [123 456] 0 0)]
        (expect (= [123 456] (mouse/location e)))))))

(defdescribe button-down?-test
  (describe "with a MouseEvent"
    (it "returns true if the button is down"
      (let [e (fake-event [0 0] java.awt.event.InputEvent/BUTTON2_DOWN_MASK 0)]
        (expect (mouse/button-down? e :center))))))

(defdescribe button-test
  (describe "with a MouseEvent"
    (it "returns the button whose state changed"
      (let [e (fake-event [0 0] 0 java.awt.event.MouseEvent/BUTTON3)]
        (expect (= :right (mouse/button e)))))))
