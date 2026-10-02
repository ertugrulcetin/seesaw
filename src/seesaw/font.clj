;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Functions for handling fonts. Note that most core widget functions
            use these implicitly through the :font option."
      :author "Dave Ray"}
  seesaw.font
  (:require [seesaw.util :refer [constant-map resource resource-key?]])
  (:import (java.awt Font GraphicsEnvironment)
           (java.awt.font TextAttribute)
           (java.util Map)
           (javax.swing JLabel UIManager)))

(defn font-families
  "Returns a seq of strings naming the font families on the system. These
  are the names that are valid in :name option (seesaw.font/font) as well
  as in font descriptor strings like \"Arial-BOLD-20\"
  
  See:
    (seesaw.core/font)
  "
  ([] (font-families nil))
  ([locale]
   (-> (GraphicsEnvironment/getLocalGraphicsEnvironment)
       (.getAvailableFontFamilyNames locale)
       seq)))

(def ^{:private true} style-table (constant-map Font :bold :plain :italic))
(defn- get-style-mask [v]
  (if (keyword? v)
    (get-style-mask [v])
    (reduce bit-or 0 (map style-table v))))

(def ^{:private true} name-table (constant-map Font :monospaced :serif :sans-serif))

(declare to-font)

(def ^{:private true} weight-table
  {:extra-light TextAttribute/WEIGHT_EXTRA_LIGHT
   :light TextAttribute/WEIGHT_LIGHT
   :regular TextAttribute/WEIGHT_REGULAR
   :medium TextAttribute/WEIGHT_MEDIUM
   :semibold TextAttribute/WEIGHT_SEMIBOLD
   :bold TextAttribute/WEIGHT_BOLD
   :heavy TextAttribute/WEIGHT_HEAVY
   :extra-bold TextAttribute/WEIGHT_EXTRABOLD
   :ultra-bold TextAttribute/WEIGHT_ULTRABOLD})

(defn- text-attributes
  "TextAttribute map for the extra font options"
  [{:keys [weight tracking underline? strikethrough? kerning? ligatures?]}]
  (cond-> {}
          weight (assoc TextAttribute/WEIGHT (float (get weight-table weight weight)))
          tracking (assoc TextAttribute/TRACKING (float tracking))
          (some? underline?) (assoc TextAttribute/UNDERLINE (if underline? TextAttribute/UNDERLINE_ON (int -1)))
          (some? strikethrough?) (assoc TextAttribute/STRIKETHROUGH (boolean strikethrough?))
          (some? kerning?) (assoc TextAttribute/KERNING (int (if kerning? TextAttribute/KERNING_ON 0)))
          (some? ligatures?) (assoc TextAttribute/LIGATURES (int (if ligatures? TextAttribute/LIGATURES_ON 0)))))

(defn font
  "Create and return a Font.

      (font name)
      (font ... options ...)

  Options are:

    :name   The name of the font. Besides string values, also possible are 
            any of :monospaced, :serif, :sans-serif. See (seesaw.font/font-families)
            to get a system-specific list of all valid values.
    :style  The style. One of :bold, :plain, :italic, or a set of those values
            to combine them. Default: :plain.
    :size   The size of the font. Default: 12.
    :from   A Font from which to derive the new Font.
    :weight One of :extra-light :light :regular :medium :semibold :bold
            :heavy :extra-bold :ultra-bold, or a number. Finer grained than
            :style, if the font has those weights.
    :tracking  Letter spacing as a fraction of the size, e.g. 0.05
    :underline?, :strikethrough?, :kerning?, :ligatures?  booleans

   Returns a java.awt.Font instance.

  Examples:

    ; Create a font from a font-spec (see JavaDocs)
    (font \"ARIAL-ITALIC-20\")

    ; Create a 12 pt bold and italic monospace
    (font :style #{:bold :italic} :name :monospaced)

  See:
    (seesaw.font/font-families)
    http://download.oracle.com/javase/6/docs/api/java/awt/Font.html
  "
  [& args]
  (if (= 1 (count args))
    (let [v (first args)]
      (if (resource-key? v)
        (Font/decode (resource v))
        (Font/decode (name v))))
    (let [{:keys [style size from] :as opts} args
          font-name (:name opts)
          font-style (get-style-mask (or style :plain))
          font-size (or size 12)
          ^Font from (to-font from)
          ^Font f (if from
                    (let [^Integer derived-style (if style font-style (.getStyle from))
                          derived-size (if size font-size (.getSize from))]
                      (.deriveFont from derived-style (float derived-size)))
                    (Font. (get name-table font-name font-name) font-style font-size))
          attrs (text-attributes opts)]
      (if (seq attrs)
        (.deriveFont f ^Map attrs)
        f))))

(defn can-display?
  "True if font has glyphs for every character in string s."
  [f s]
  (= -1 (.canDisplayUpTo ^Font (to-font f) (str s))))

(defn first-available
  "The first of the given font family names that's installed, or nil. Handy
  for font stacks: (first-available \"Inter\" \"SF Pro Text\" \"Segoe UI\")"
  [& names]
  (let [installed (set (font-families))]
    (first (filter installed names))))

(defn line-height
  "The line height (ascent + descent + leading) of a font in pixels."
  [f]
  (.getHeight (.getFontMetrics (JLabel.) ^Font (to-font f))))

(defn default-font
  "Look up a default font from the UIManager.

  Example:

    (default-font \"Label.font\")

  Returns an instane of java.awt.Font

  See:
    http://download.oracle.com/javase/6/docs/api/javax/swing/UIManager.html#getFont%28java.lang.Object%29
  "
  [name]
  (.getFont (UIManager/getDefaults) name))

(defn to-font
  [f]
  (cond
    (nil? f) nil
    (instance? Font f) f
    (map? f) (apply font (flatten (seq f)))
    true (font f))) 
    
