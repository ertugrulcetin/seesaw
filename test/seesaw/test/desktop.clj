;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns seesaw.test.desktop
  (:require
   [lazytest.core :refer [defdescribe expect expect-it it]]
   [seesaw.desktop :refer :all]))

; Only side-effect free bits: actually browsing or opening would launch apps.
(defdescribe desktop-test
  (expect-it "supported? returns a boolean"
    (boolean? (supported? :browse)))
  (it "rejects unknown actions"
    (expect (try (supported? :nope) false (catch IllegalArgumentException _ true))))
  (it "app-handlers! reports what it installed"
    (let [r (app-handlers! :about nil)]
      (expect (= #{:about} (set (keys r))))
      (expect (boolean? (:about r)))))
  (it "platform-properties! sets the macOS properties"
    (let [old (System/getProperty "apple.awt.application.appearance")]
      (platform-properties! :appearance :dark)
      (expect (= "NSAppearanceNameDarkAqua" (System/getProperty "apple.awt.application.appearance")))
      (if old
        (System/setProperty "apple.awt.application.appearance" old)
        (System/clearProperty "apple.awt.application.appearance")))))
