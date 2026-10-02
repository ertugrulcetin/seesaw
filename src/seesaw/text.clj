;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Text component helpers: highlight layers, geometry, editing and
           scrolling. Works with any JTextComponent (text, text area,
           styled-text, editor-pane)."}
  seesaw.text
  (:require [seesaw.color :refer [to-color]]
            [seesaw.meta :refer [get-meta put-meta!]]
            [seesaw.to-widget :refer [to-widget*]])
  (:import (javax.swing.text JTextComponent Position$Bias View
            LayeredHighlighter$LayerPainter)
           (java.awt Graphics Graphics2D Rectangle RenderingHints Shape)
           (java.awt.geom RoundRectangle2D$Double)))

(defn- ^JTextComponent to-text [target]
  (let [t (to-widget* target)]
    (if (instance? JTextComponent t)
      t
      (throw (IllegalArgumentException. (str "Expected a text component, got " t))))))

;*******************************************************************************
; Highlights

(defn- range-shape ^Rectangle [^JTextComponent c ^View view ^Shape bounds offs0 offs1]
  (if (and (= offs0 (.getStartOffset view)) (= offs1 (.getEndOffset view)))
    (.getBounds bounds)
    (.getBounds (.modelToView view (int offs0) Position$Bias/Forward
                              (int offs1) Position$Bias/Backward bounds))))

(defn painter
  "A highlight painter that fills a rounded rectangle behind each run of
  highlighted text. Options:

    :color    Fill color (default: the component's selection color)
    :border   Outline color, or nil for none
    :arc      Corner radius in pixels (default 4)
    :padding  Extra pixels around the text, n or [horizontal vertical]
              (default [1 0])

  Use it with (highlight!) or (.addHighlight (.getHighlighter t) ...)."
  [& {:keys [color border arc padding] :or {arc 4 padding [1 0]}}]
  (let [[px py] (if (sequential? padding) padding [padding padding])
        fill    (some-> color to-color)
        stroke  (some-> border to-color)]
    (proxy [LayeredHighlighter$LayerPainter] []
      (paint [g p0 p1 bounds c])
      (paintLayer [^Graphics g offs0 offs1 ^Shape bounds ^JTextComponent c ^View view]
        (let [^Rectangle r (range-shape c view bounds offs0 offs1)
              g2 ^Graphics2D (.create g)
              rr (RoundRectangle2D$Double. (- (.x r) px) (- (.y r) py)
                                           (+ (.width r) (* 2 px)) (+ (.height r) (* 2 py))
                                           (* 2 arc) (* 2 arc))]
          (try
            (.setRenderingHint g2 RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
            (.setColor g2 (or fill (.getSelectionColor c)))
            (.fill g2 rr)
            (when stroke
              (.setColor g2 stroke)
              (.draw g2 rr))
            (finally (.dispose g2)))
          r)))))

(defn- layer-key [layer] [::highlights layer])

(defn clear-highlights!
  "Remove the highlights of one layer, or all layers added with (highlight!).
  Returns the text component."
  ([target]
   (let [t (to-text target)]
     (doseq [layer (get-meta t ::layers)]
       (clear-highlights! t layer))
     t))
  ([target layer]
   (let [t (to-text target)
         h (.getHighlighter t)]
     (doseq [tag (get-meta t (layer-key layer))]
       (.removeHighlight h tag))
     (put-meta! t (layer-key layer) nil)
     t)))

(defn highlight!
  "Replace the highlights in a named layer of a text component. Layers are
  independent, e.g. :diff and :find highlights can coexist and be cleared
  separately. ranges is a seq of [start end] character offsets.

  The remaining options go to (seesaw.text/painter): :color, :border, :arc
  and :padding. Pass :painter to use your own
  javax.swing.text.Highlighter$HighlightPainter instead.

  Returns the text component.

  Examples:

    (highlight! editor :find [[10 15] [42 47]] :color \"#ffe08a\" :arc 3)
    (highlight! editor :find [])        ; same as (clear-highlights! editor :find)
  "
  [target layer ranges & {:as opts}]
  (let [t (to-text target)
        h (.getHighlighter t)
        p (or (:painter opts) (apply painter (mapcat identity (dissoc opts :painter))))
        len (.getLength (.getDocument t))]
    (clear-highlights! t layer)
    (put-meta! t ::layers (conj (or (get-meta t ::layers) #{}) layer))
    (put-meta! t (layer-key layer)
               (doall (for [[start end] ranges
                            :let [start (max 0 (min start len))
                                  end   (max start (min end len))]
                            :when (< start end)]
                        (.addHighlight h start end p))))
    t))

(defn highlights
  "The [start end] ranges currently highlighted in a layer."
  [target layer]
  (let [t (to-text target)]
    (vec (for [^javax.swing.text.Highlighter$Highlight tag (get-meta t (layer-key layer))]
           [(.getStartOffset tag) (.getEndOffset tag)]))))

;*******************************************************************************
; Geometry and scrolling

(defn position-at
  "The character offset at a point in the text component (a java.awt.Point or
  [x y]), e.g. under the mouse in a :mouse-moved handler."
  [target point]
  (let [t (to-text target)
        p (if (instance? java.awt.geom.Point2D point)
            point
            (let [[x y] point] (java.awt.Point. (int x) (int y))))]
    (.viewToModel2D t p)))

(defn rect-at
  "The java.awt.Rectangle (in the component's coordinates) of the character
  at offset pos, or nil if it isn't laid out yet."
  [target pos]
  (some-> (.modelToView2D (to-text target) (int pos)) .getBounds))

(defn scroll-to-position!
  "Scroll a text component so the character at pos is visible, with :padding
  pixels around it (default 0), without moving the caret. Returns the text
  component."
  [target pos & {:keys [padding] :or {padding 0}}]
  (let [t (to-text target)]
    (when-let [^Rectangle r (rect-at t pos)]
      (.grow r (int padding) (int padding))
      (.scrollRectToVisible t r))
    t))

;*******************************************************************************
; Editing

(defn length
  "The length of the text in a text component."
  [target]
  (.getLength (.getDocument (to-text target))))

(defn insert-text!
  "Insert string s at offset pos. Returns the text component."
  [target pos s]
  (let [t (to-text target)]
    (.insertString (.getDocument t) (int pos) (str s) nil)
    t))

(defn append-text!
  "Append string s to the end of the text. Returns the text component."
  [target s]
  (insert-text! target (length target) s))

(defn delete-text!
  "Delete the text between offsets start and end. Returns the text component."
  [target start end]
  (let [t (to-text target)]
    (.remove (.getDocument t) (int start) (int (- end start)))
    t))

(defn replace-selection!
  "Replace the selected text with s (or insert it at the caret), like typing.
  Returns the text component."
  [target s]
  (let [t (to-text target)]
    (.replaceSelection t (str s))
    t))

(defn cut!   "Cut the selection to the clipboard."   [target] (doto (to-text target) .cut))
(defn copy!  "Copy the selection to the clipboard."  [target] (doto (to-text target) .copy))
(defn paste! "Paste the clipboard at the selection." [target] (doto (to-text target) .paste))
