;  Copyright (c) Dave Ray, 2012. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Support for embedding JavaFX in Swing via javafx.embed.swing.JFXPanel.
           Requires OpenJFX (org.openjfx/javafx-swing) on the classpath."}
  seesaw.javafx
  (:require [seesaw.core :as core]
            [seesaw.options :as options]
            [seesaw.widget-options :refer [widget-option-provider]]))

(widget-option-provider javafx.embed.swing.JFXPanel core/default-options)

(defn jfxpanel
  "Create a javafx.embed.swing.JFXPanel. Supports the default widget options."
  [& {:keys [] :as opts}]
  (let [p (core/construct javafx.embed.swing.JFXPanel)]
    (options/apply-options p opts)))
