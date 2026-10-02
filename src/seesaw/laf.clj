;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Look and feel: install one (including FlatLaf themes), set UI
           defaults, switch themes at runtime and react to it.

           FlatLaf isn't a dependency of seesaw. To use the :flat-* themes add
           com.formdev/flatlaf to your project."}
  seesaw.laf
  (:require [seesaw.color :as color]
            [seesaw.invoke :refer [invoke-now*]]
            [seesaw.util :refer [illegal-argument]])
  (:import (javax.swing LookAndFeel SwingUtilities UIManager)
           (java.beans PropertyChangeListener)))

(def ^{:private true} laf-classes
  {:system         (UIManager/getSystemLookAndFeelClassName)
   :cross-platform (UIManager/getCrossPlatformLookAndFeelClassName)
   :metal          "javax.swing.plaf.metal.MetalLookAndFeel"
   :nimbus         "javax.swing.plaf.nimbus.NimbusLookAndFeel"
   :flat-light     "com.formdev.flatlaf.FlatLightLaf"
   :flat-dark      "com.formdev.flatlaf.FlatDarkLaf"
   :flat-intellij  "com.formdev.flatlaf.FlatIntelliJLaf"
   :flat-darcula   "com.formdev.flatlaf.FlatDarculaLaf"
   :flat-mac-light "com.formdev.flatlaf.themes.FlatMacLightLaf"
   :flat-mac-dark  "com.formdev.flatlaf.themes.FlatMacDarkLaf"})

(defn- flatlaf-class []
  (try (Class/forName "com.formdev.flatlaf.FlatLaf") (catch ClassNotFoundException _ nil)))

(defn- flatlaf? [laf]
  (when-let [c (flatlaf-class)] (instance? c laf)))

(defn- to-laf ^LookAndFeel [v]
  (cond
    (instance? LookAndFeel v) v
    (class? v)   (.newInstance (.getConstructor ^Class v (make-array Class 0)) (object-array 0))
    (keyword? v) (if-let [c (laf-classes v)]
                   (try
                     (to-laf (Class/forName c))
                     (catch ClassNotFoundException _
                       (illegal-argument "Look and feel %s needs %s on the classpath" v c)))
                   (illegal-argument "Unknown look and feel %s. Must be one of %s" v (keys laf-classes)))
    (string? v)  (to-laf (Class/forName v))
    :else (illegal-argument "Don't know how to make a look and feel from %s" v)))

(defn laf
  "Returns the current javax.swing.LookAndFeel."
  []
  (UIManager/getLookAndFeel))

(defn put-defaults!
  "Set UIManager defaults from a map, e.g.

    (put-defaults! {\"Component.arc\" 8 \"ScrollBar.width\" 10})

  These survive look and feel changes. Colors may be anything accepted by
  (seesaw.color/to-color) when the key ends in \"color\", \"Background\" or
  \"Foreground\". Call (update-ui!) afterwards to apply them to existing
  widgets."
  [defaults]
  (doseq [[k v] defaults]
    (let [k (name k)
          color-key? (re-find #"(?i)(color|background|foreground)$" k)]
      (UIManager/put k (if (and color-key? (or (keyword? v) (string? v)))
                         (color/to-color v)
                         v))))
  defaults)

(defn update-ui!
  "Update all open windows to the current look and feel and UI defaults."
  []
  (invoke-now*
    (fn []
      (if (flatlaf? (laf))
        (clojure.lang.Reflector/invokeStaticMethod ^Class (flatlaf-class) "updateUI" (object-array 0))
        (doseq [w (java.awt.Window/getWindows)]
          (SwingUtilities/updateComponentTreeUI w))))))

(defn set-laf!
  "Install a look and feel, optionally with UI defaults, and update open
  windows. laf may be a LookAndFeel, its class or class name, or one of:

    :system :cross-platform :metal :nimbus
    :flat-light :flat-dark :flat-intellij :flat-darcula
    :flat-mac-light :flat-mac-dark      (these need FlatLaf on the classpath)

  Options:

    :defaults  A map of UIManager defaults, see (put-defaults!)

  Can be called again at any time to switch themes.

  Examples:

    (set-laf! :flat-mac-dark :defaults {\"Component.arc\" 8})
    (set-laf! (if dark? :flat-dark :flat-light))
  "
  [laf & {:keys [defaults]}]
  (let [l (to-laf laf)]
    (invoke-now*
      (fn []
        (when defaults (put-defaults! defaults))
        (UIManager/setLookAndFeel l)))
    (update-ui!)
    l))

(defn dark?
  "True if the current look and feel is dark. Uses FlatLaf's own flag when
  FlatLaf is installed, otherwise the brightness of Panel.background."
  []
  (let [l (laf)]
    (if (flatlaf? l)
      (boolean (clojure.lang.Reflector/invokeStaticMethod ^Class (flatlaf-class) "isLafDark" (object-array 0)))
      (if-let [^java.awt.Color c (UIManager/getColor "Panel.background")]
        (< (+ (* 0.299 (.getRed c)) (* 0.587 (.getGreen c)) (* 0.114 (.getBlue c))) 128)
        false))))

(defn on-change
  "Call (f laf) on the Swing thread whenever the look and feel changes, e.g. to
  recompute colors for custom painting. Returns a function that stops
  listening."
  [f]
  (let [l (reify PropertyChangeListener
            (propertyChange [_ e]
              (when (= "lookAndFeel" (.getPropertyName e))
                (let [v (.getNewValue e)]
                  (if (SwingUtilities/isEventDispatchThread)
                    (f v)
                    (SwingUtilities/invokeLater #(f v)))))))]
    (UIManager/addPropertyChangeListener l)
    #(UIManager/removePropertyChangeListener l)))
