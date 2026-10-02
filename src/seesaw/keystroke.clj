;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns seesaw.keystroke
  (:import (javax.swing KeyStroke)
           (java.awt Toolkit)
           (java.awt.event InputEvent))
  (:require [clojure.string :only [join split]]
            [seesaw.util :refer [illegal-argument resource resource-key?]]))

(def ^{:private true} modifier-masks {
  InputEvent/CTRL_DOWN_MASK "ctrl"
  InputEvent/META_DOWN_MASK "meta"
  InputEvent/ALT_DOWN_MASK  "alt"
})

(defn- menu-modifier []
  (modifier-masks (.getMenuShortcutKeyMaskEx (Toolkit/getDefaultToolkit)) "ctrl"))

(defn- preprocess-descriptor [s]
  ; "menu" and its alias "cmd" are whole words, so key names like CONTEXT_MENU
  ; are left alone
  (clojure.string/replace s #"\b(menu|cmd)\b" (menu-modifier)))

(defn keystroke
  "Convert an argument to a KeyStroke. When the argument is a string, follows 
  the keystroke descriptor syntax for KeyStroke/getKeyStroke (see link below).

  For example,

    (keystroke \"ctrl S\")

  Note that there is one additional modifier supported, \"menu\" which will
  replace the modifier with the appropriate platform-specific modifier key for
  menus. For example, on Windows it will be \"ctrl\", while on OSX, it will be
  the \"command\" key. Yay! \"cmd\" is accepted as an alias for \"menu\".

  arg can also be an i18n resource keyword.

  See http://download.oracle.com/javase/6/docs/api/javax/swing/KeyStroke.html#getKeyStroke(java.lang.String)"
  ^KeyStroke [arg]
  (cond 
    (nil? arg)                nil
    (instance? KeyStroke arg) arg
    (char? arg)               (KeyStroke/getKeyStroke ^Character arg)
    (resource-key? arg)       (keystroke (resource arg))
    :else (if-let [ks (KeyStroke/getKeyStroke ^String (preprocess-descriptor (str arg)))]
            ks
            (illegal-argument "Invalid keystroke descriptor: %s" arg))))


(def ^{:private true} mac?
  (.startsWith (.toLowerCase (System/getProperty "os.name" "")) "mac"))

(def ^{:private true} mac-modifier-symbols
  [[InputEvent/CTRL_DOWN_MASK  "\u2303"]
   [InputEvent/ALT_DOWN_MASK   "\u2325"]
   [InputEvent/SHIFT_DOWN_MASK "\u21E7"]
   [InputEvent/META_DOWN_MASK  "\u2318"]])

(def ^{:private true} modifier-names
  [[InputEvent/CTRL_DOWN_MASK  "Ctrl"]
   [InputEvent/META_DOWN_MASK  "Meta"]
   [InputEvent/ALT_DOWN_MASK   "Alt"]
   [InputEvent/SHIFT_DOWN_MASK "Shift"]])

(def ^{:private true} vk-names
  ; KeyEvent/VK_ENTER -> "ENTER", ...
  (delay
    (into {}
          (for [^java.lang.reflect.Field f (.getFields java.awt.event.KeyEvent)
                :let [n (.getName f)]
                :when (and (.startsWith n "VK_")
                           (= Integer/TYPE (.getType f)))]
            [(.getInt f nil) (subs n 3)]))))

(def ^{:private true} mac-key-symbols
  {"ENTER" "\u21A9" "BACK_SPACE" "\u232B" "DELETE" "\u2326" "ESCAPE" "\u238B"
   "TAB" "\u21E5" "LEFT" "\u2190" "RIGHT" "\u2192" "UP" "\u2191" "DOWN" "\u2193"
   "PAGE_UP" "\u21DE" "PAGE_DOWN" "\u21DF" "HOME" "\u2196" "END" "\u2198"
   "SPACE" "Space"})

(def ^{:private true} key-words
  {"BACK_SPACE" "Backspace" "PAGE_UP" "PgUp" "PAGE_DOWN" "PgDn" "ESCAPE" "Esc"
   "DELETE" "Del" "BACK_SLASH" "\\" "SLASH" "/" "COMMA" "," "PERIOD" "."
   "SEMICOLON" ";" "EQUALS" "=" "MINUS" "-" "OPEN_BRACKET" "[" "CLOSE_BRACKET" "]"
   "QUOTE" "'" "BACK_QUOTE" "`" "PLUS" "+"})

(defn- capitalize-words [s]
  (clojure.string/join " " (for [w (clojure.string/split s #"_")]
                             (str (subs w 0 1) (.toLowerCase (subs w 1))))))

(defn- key-name [^KeyStroke ks mac?]
  (if (= java.awt.event.KeyEvent/VK_UNDEFINED (.getKeyCode ks))
    (str (.getKeyChar ks))
    (let [n (get @vk-names (.getKeyCode ks) (str (.getKeyCode ks)))]
      (or (when mac? (mac-key-symbols n))
          (key-words n)
          (if (= 1 (count n)) n (capitalize-words n))))))

(defn label
  "Returns a human readable label for a keystroke (anything accepted by
  (keystroke)), suitable for tooltips and menus. Uses the platform's
  conventions:

    (label \"menu shift G\") ;=> \"\u21E7\u2318G\" on macOS, \"Ctrl+Shift+G\" elsewhere
    (label \"ENTER\")        ;=> \"\u21A9\" on macOS, \"Enter\" elsewhere

  Pass :mac? true or false to choose the convention explicitly."
  [ks & {:keys [mac?] :or {mac? mac?}}]
  (let [; "menu" means the menu key of the convention being shown, not of
        ; the machine this runs on
        ks (if (string? ks)
             (clojure.string/replace ks #"\b(menu|cmd)\b" (if mac? "meta" "ctrl"))
             ks)
        ^KeyStroke ks (keystroke ks)
        mods (.getModifiers ks)
        held (fn [table] (for [[mask s] table :when (pos? (bit-and mods mask))] s))]
    (if mac?
      (apply str (concat (held mac-modifier-symbols) [(key-name ks true)]))
      (clojure.string/join "+" (concat (held modifier-names) [(key-name ks false)])))))
