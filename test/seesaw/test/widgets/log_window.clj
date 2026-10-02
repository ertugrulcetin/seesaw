;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns seesaw.test.widgets.log-window
  (:require
   [lazytest.core :refer [defdescribe expect-it]]
   [seesaw.core :refer :all]
   [seesaw.widgets.log-window :refer :all]))

(defdescribe log-window-test
  (expect-it "creates a JTextArea"
    (instance? javax.swing.JTextArea (log-window)))
  (expect-it "has :limit option"
    (= 55 (config (log-window :limit 55) :limit)))
  (expect-it "has :auto-scroll? option"
    (not (config (log-window :auto-scroll? false) :auto-scroll?)))
  (expect-it "satisfies LogWindow protocol"
    (satisfies? LogWindow (log-window))))

