;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns seesaw.make-widget
  (:require [seesaw.icon :refer [icon]])
  (:import (clojure.lang IPersistentVector Keyword)
           (java.awt Component Dimension)
           (java.net URL)
           (javax.swing Action Box JButton JLabel JSeparator)))

(defprotocol MakeWidget
  (make-widget* [v]))

(defmacro ^{:private true} def-make-widget [t b & forms]
  `(extend-type
     ~t
     MakeWidget
     (~'make-widget* ~b ~@forms)))

(def-make-widget Component [c] c)

(def-make-widget Dimension [v] (Box/createRigidArea v))

(def-make-widget Action [v] (JButton. v))

(def-make-widget Keyword
                 [v]
                 (condp = v
                   :separator (JSeparator.)
                   :fill-h (Box/createHorizontalGlue)
                   :fill-v (Box/createVerticalGlue)))

(def-make-widget IPersistentVector
                 [[v0 v1 v2]]
                 (cond
                   (= :fill-h v0) (Box/createHorizontalStrut v1)
                   (= :fill-v v0) (Box/createVerticalStrut v1)
                   (= :by v1) (Box/createRigidArea (Dimension. v0 v2))))

(def-make-widget String
                 [v]
                 (JLabel. v))

(def-make-widget URL
                 [v]
                 (JLabel. (icon v)))

