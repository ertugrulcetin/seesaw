;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Functions for loading and creating icons."
      :author "Dave Ray"}
  seesaw.icon
  (:require [clojure.java.io :as jio]
            [seesaw.util :refer [resource resource-key? to-url]])
  (:import (javax.swing ImageIcon)
           (java.awt Component Graphics2D RenderingHints)))

;*******************************************************************************
; Icons

(defn icon 
  "Loads an icon. The parameter p can be any of the following:
  
    nil              - returns nil
    javax.swing.Icon - returns the icon
    java.awt.Image   - returns an ImageIcon around the image
    java.net.URL     - Load the icon from the given URL
    an i18n keyword  - Load the icon from the resource bundle
    classpath path string  - Load the icon from the classpath
    URL string       - Load the icon from the given URL
    java.io.File     - Load the icon from the File

  This is the function used to process the :icon property on most widgets
  and windows. Thus, any of these values may be used for the :icon property.
  "
  ^javax.swing.ImageIcon [p]
  (cond
    (nil? p) nil 
    (instance? javax.swing.Icon p) p
    (instance? java.awt.Image p)   (ImageIcon. ^java.awt.Image p)
    (instance? java.net.URL p)     (ImageIcon. ^java.net.URL p)
    (instance? java.io.File p)     (ImageIcon. (.getAbsolutePath ^java.io.File p))
    (resource-key? p)              (icon (resource p))
    :else
      (if-let [url (jio/resource (str p))]
        (icon url)
        (if-let [url (to-url p)] 
          (ImageIcon. url)))))


(defn paint-icon
  "Create an icon painted by a function, e.g. a small vector glyph that
  follows the theme.

    (paint-icon 16 (fn [c g] ...))          ; square
    (paint-icon 20 16 (fn [c g] ...) :color :red)

  (paint c g) is called with the component the icon is painted on and a
  java.awt.Graphics2D translated to the icon's top-left corner, anti-aliased,
  with its color already set to the component's foreground (or :color if
  given). Draw with the functions in seesaw.graphics, e.g. (draw g ...).

  Options:

    :color  Paint color instead of the component's foreground. Anything
            accepted by (seesaw.color/to-color)
    :disabled-color  Color used when the component is disabled (default:
            the component's foreground with reduced alpha)
  "
  [width & args]
  ; (paint-icon size paint & opts) or (paint-icon width height paint & opts)
  (let [[height paint & {:keys [color disabled-color]}] (if (fn? (first args))
                                                          (cons width args)
                                                          args)]
   (let [color (some-> color ((requiring-resolve 'seesaw.color/to-color)))
         disabled-color (some-> disabled-color ((requiring-resolve 'seesaw.color/to-color)))]
     (reify javax.swing.Icon
       (getIconWidth [_] width)
       (getIconHeight [_] height)
       (paintIcon [_ c g x y]
         (let [^Graphics2D g2 (.create g)
               ^Component c c
               fg (or (when c (.getForeground c)) java.awt.Color/BLACK)
               ^java.awt.Color fg (if (and c (not (.isEnabled c)))
                                    (or disabled-color
                                        (java.awt.Color. (.getRed fg) (.getGreen fg) (.getBlue fg)
                                                         (int (/ (.getAlpha fg) 2.5))))
                                    (or color fg))]
           (try
             (.translate g2 (int x) (int y))
             (.setRenderingHint g2 RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
             (.setRenderingHint g2 RenderingHints/KEY_STROKE_CONTROL RenderingHints/VALUE_STROKE_PURE)
             (.setColor g2 fg)
             (paint c g2)
             (finally (.dispose g2)))))))))
