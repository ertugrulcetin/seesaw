;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "SwingX integration. Unfortunately, SwingX is hosted on java.net which means
           it looks abandoned most of the time. Downloads are here
           http://java.net/downloads/swingx/releases/1.6/

           This is an incomplete wrapper. If something's missing that you want, just ask."
      :author "Dave Ray"}
  seesaw.swingx
  (:require [seesaw.color]
            [seesaw.core :refer [ConfigIcon abstract-panel button-options config
                                 config! construct default-options
                                 get-icon* label-options listbox-options
                                 make-widget set-icon* table-options to-widget tree-options]]
            [seesaw.event :refer [listen-for-named-event listen-to-property]]
            [seesaw.icon :refer [icon]]
            [seesaw.layout :refer [box-layout default-items-option grid-layout]]
            [seesaw.options :refer [apply-options bean-option
                                    default-option option-map resource-option]]
            [seesaw.selection :refer [Selection ViewModelIndexConversion]]
            [seesaw.util :refer [constant-map illegal-argument resource to-uri]]
            [seesaw.widget-options :refer [widget-option-provider]])
  (:import (java.awt BorderLayout CardLayout Color FlowLayout)
           (java.util Collection)
           (java.util.regex Pattern)
           (javax.swing Action SortOrder)
           (org.jdesktop.swingx JXBusyLabel JXButton JXColorSelectionButton JXHeader
                                JXHyperlink JXLabel JXList JXPanel JXTable
                                JXTaskPane JXTaskPaneContainer JXTitledPanel JXTree)
           (org.jdesktop.swingx.decorator ColorHighlighter ComponentAdapter
                                          Highlighter HighlighterFactory
                                          HighlightPredicate
                                          HighlightPredicate$AndHighlightPredicate
                                          HighlightPredicate$ColumnHighlightPredicate
                                          HighlightPredicate$DepthHighlightPredicate
                                          HighlightPredicate$EqualsHighlightPredicate
                                          HighlightPredicate$IdentifierHighlightPredicate
                                          HighlightPredicate$NotHighlightPredicate
                                          HighlightPredicate$OrHighlightPredicate HighlightPredicate$RowGroupHighlightPredicate
                                          HighlightPredicate$TypeHighlightPredicate IconHighlighter PatternPredicate
                                          ShadingColorHighlighter)))

;*******************************************************************************
; Highlighter Predicates

(def p-built-in
  (constant-map HighlightPredicate
                :always
                :never
                :even
                :odd
                :integer-negative
                :editable
                :read-only
                :has-focus
                :is-folder
                :is-leaf
                :rollover-row))

(declare p-pattern)
(declare p-fn)

(defn- to-p ^HighlightPredicate [v]
  (cond
    (instance? HighlightPredicate v) v
    (instance? Pattern v) (p-pattern v)
    (keyword? v) (p-built-in v)
    (fn? v) (p-fn v)
    :else (illegal-argument "Don't know how to make predicate from %s" v)))

(defn p-fn [f]
  (reify
    HighlightPredicate
    (isHighlighted [this renderer adapter]
      (boolean (f renderer adapter)))))

(defn p-value
  "A predicate that highlights rows whose value in column (a model index) passes
  (pred value)."
  [column pred]
  (p-fn (fn [_ ^ComponentAdapter adapter]
          (pred (.getValue adapter (.convertColumnIndexToView adapter (int column)))))))

(defn p-and [& args]
  (HighlightPredicate$AndHighlightPredicate.
    ^Collection (doall (map to-p args))))

(defn p-or [& args]
  (HighlightPredicate$OrHighlightPredicate.
    ^Collection (doall (map to-p args))))

(defn p-not [p]
  (HighlightPredicate$NotHighlightPredicate. (to-p p)))

(defn p-type [class]
  (HighlightPredicate$TypeHighlightPredicate. class))

(defn p-eq [value]
  (HighlightPredicate$EqualsHighlightPredicate. value))

(defn p-column-names [& names]
  (HighlightPredicate$IdentifierHighlightPredicate. (to-array names)))

(defn p-column-indexes [& indexes]
  (HighlightPredicate$ColumnHighlightPredicate. (int-array indexes)))

(defn p-row-group [lines-per-group]
  (HighlightPredicate$RowGroupHighlightPredicate. lines-per-group))

(defn p-depths [& depths]
  (HighlightPredicate$DepthHighlightPredicate. (int-array depths)))

(defn p-pattern [pattern & {:keys [test-column highlight-column]}]
  (PatternPredicate.
    ^Pattern (re-pattern pattern)
    (int (or test-column -1))
    (int (or highlight-column -1))))

;*******************************************************************************
; Highlighters

(defn hl-color
  [& {:keys [foreground background
             selected-background selected-foreground]}]
  (fn self
    ([] (self :always))
    ([p]
     (ColorHighlighter.
       (to-p p)
       (seesaw.color/to-color background)
       (seesaw.color/to-color foreground)
       (seesaw.color/to-color selected-background)
       (seesaw.color/to-color selected-foreground)))))

(defn hl-icon
  [i]
  (fn self
    ([] (self :always))
    ([p]
     (IconHighlighter.
       (to-p p)
       (icon i)))))

(defn hl-shade
  []
  (fn self
    ([] (self :always))
    ([p]
     (ShadingColorHighlighter.
       (to-p p)))))

(defn hl-simple-striping
  [& {:keys [background lines-per-stripe]}]
  (cond
    (and background lines-per-stripe)
    (HighlighterFactory/createSimpleStriping
      (seesaw.color/to-color background) lines-per-stripe)
    background
    (HighlighterFactory/createSimpleStriping ^Color (seesaw.color/to-color background))
    lines-per-stripe
    (HighlighterFactory/createSimpleStriping (int lines-per-stripe))
    :else
    (HighlighterFactory/createSimpleStriping)))

(defn to-highlighter ^Highlighter [v]
  (cond
    (instance? Highlighter v) v
    (= :shade v) (hl-shade)
    (= :alternate-striping v) (HighlighterFactory/createAlternateStriping)
    (= :simple-striping v) (hl-simple-striping)
    :else (illegal-argument "Don't know how to make highlighter from %s" v)))

(defprotocol HighlighterHost
  (get-highlighters* [this])
  (set-highlighters* [this hs])
  (add-highlighter* [this h])
  (remove-highlighter* [this h]))

(defmacro default-highlighter-host
  [class]
  `(extend-protocol HighlighterHost
     ~class
     (~'get-highlighters* [this#]
       (. this# ~'getHighlighters))
     (~'set-highlighters* [this# hs#]
       (. this# ~'setHighlighters hs#))
     (~'add-highlighter* [this# h#]
       (. this# ~'addHighlighter h#))
     (~'remove-highlighter* [this# h#]
       (. this# ~'removeHighlighter h#))))

(defn get-highlighters [target]
  (seq (get-highlighters* (to-widget target))))

(defn set-highlighters [target hs]
  (set-highlighters* (to-widget target)
                     (into-array Highlighter (map to-highlighter hs)))
  target)

(defn add-highlighter [target hl]
  (add-highlighter* (to-widget target) (to-highlighter hl))
  target)

(defn remove-highlighter [target hl]
  (remove-highlighter* (to-widget target) hl)
  target)

(def highlighter-host-options
  (option-map
    (default-option :highlighters set-highlighters get-highlighters)))

;*******************************************************************************
; XButton

(def button-x-options
  (merge
    button-options
    (option-map
      (bean-option :background-painter JXButton)
      (bean-option :foreground-painter JXButton)
      (bean-option :paint-border-insets? JXButton boolean))))

(widget-option-provider JXButton button-x-options)

(defn button-x
  "Creates a org.jdesktop.swingx.JXButton which is an improved (button) that
  supports painters. Supports these additional options:


    :foreground-painter The foreground painter
    :background-painter The background painter
    :paint-border-insets? Default to true. If false painter paints entire
        background.

  Examples:

  See:
    (seesaw.core/button)
    (seesaw.core/button-options)
    (seesaw.swingx/button-x-options)
  "
  [& args]
  (apply-options (construct JXButton) args))

;*******************************************************************************
; XLabel

(def label-x-options
  (merge
    label-options
    (option-map
      ; TODO label-x text-alignment, painter, etc
      (bean-option [:wrap-lines? :line-wrap?] JXLabel boolean)
      (bean-option :text-rotation JXLabel)
      (bean-option :background-painter JXLabel)
      (bean-option :foreground-painter JXLabel))))

(widget-option-provider JXLabel label-x-options)

(defn label-x
  "Creates a org.jdesktop.swingx.JXLabel which is an improved (label) that
  supports wrapped text, rotation, etc. Additional options:

    :wrap-lines? When true, text is wrapped to fit
    :text-rotation Rotation of text in radians

  Examples:

    (label-x :text        \"This is really a very very very very very very long label\"
            :wrap-lines? true
            :rotation    (Math/toRadians 90.0))

  See:
    (seesaw.core/label)
    (seesaw.core/label-options)
    (seesaw.swingx/label-x-options)
  "
  [& args]
  (apply-options (construct JXLabel) args))

;*******************************************************************************
; BusyLabel

(def busy-label-options
  (merge
    label-options
    (option-map
      ; TODO busy-label text-alignment, painter, etc
      (bean-option :busy? JXBusyLabel boolean))))

(widget-option-provider JXBusyLabel busy-label-options)

(defn busy-label
  "Creates a org.jdesktop.swingx.JXBusyLabel which is a label that shows
  'busy' status with a spinner, kind of like an indeterminate progress bar.
  Additional options:

    :busy? Whether busy status should be shown or not. Defaults to false.

  Examples:

    (busy-label :text \"Processing ...\"
                :busy? true)

  See:
    (seesaw.core/label)
    (seesaw.core/label-options)
    (seesaw.swingx/busy-label-options)
  "
  [& args]
  (apply-options (construct JXBusyLabel) args))

;*******************************************************************************
; Hyperlink
(def hyperlink-options
  (merge
    button-options
    (option-map
      ; JXHyperlink has setURI but no getURI
      (default-option :uri #(.setURI ^JXHyperlink %1 (to-uri %2))))))

(widget-option-provider JXHyperlink hyperlink-options)

(defn hyperlink
  "Constuct an org.jdesktop.swingx.JXHyperlink which is a button that looks like
  a link and opens its URI in the system browser. In addition to all the options of
  a button, supports:

    :uri A string, java.net.URL, or java.net.URI with the URI to open

  Examples:

    (hyperlink :text \"Click Me\" :uri \"http://google.com\")

  See:
    (seesaw.core/button)
    (seesaw.core/button-options)
  "
  [& args]
  (apply-options (construct JXHyperlink) args))

;*******************************************************************************
; TaskPane

(extend-protocol ConfigIcon
  JXTaskPane
  (get-icon* [this] (.getIcon this))
  (set-icon* [this v]
    (.setIcon this (icon v))))

(def task-pane-options
  (merge
    default-options
    (option-map
      default-items-option
      ; TODO I have to add this manually because relying on the impl from default-options
      ; fails with "No implementation of method: :set-icon* :(
      (default-option :icon set-icon* get-icon*)
      (resource-option :resource [:title :icon])
      (bean-option :title JXTaskPane resource)
      (bean-option :animated? JXTaskPane boolean)
      (bean-option :collapsed? JXTaskPane boolean)
      (bean-option :scroll-on-expand? JXTaskPane boolean)
      (bean-option :special? JXTaskPane boolean)
      (default-option :actions
                      (fn [^JXTaskPane c actions]
                        (doseq [^Action a actions]
                          (.add c a)))))))

(widget-option-provider
  JXTaskPane
  task-pane-options)

(defn task-pane
  "Create a org.jdesktop.swingx.JXTaskPane which is a collapsable component with a title
  and icon. It is generally used as an item inside a task-pane-container.  Supports the
  following additional options

    :resource Get icon and title from a resource
    :icon The icon
    :title The pane's title
    :animated? True if collapse is animated
    :collapsed? True if the pane should be collapsed
    :scroll-on-expand? If true, when expanded, it's container will scroll the pane into
                       view
    :special? If true, the pane will be displayed in a 'special' way depending on
              look and feel

  The pane can be populated with the standard :items option, which just takes a
  sequence of widgets. Additionally, the :actions option takes a sequence of
  action objects and makes hyper-links out of them.

  See:
    (seesaw.swingx/task-pane-options)
    (seesaw.swingx/task-pane-container)
  "
  [& args]
  (apply-options
    (construct JXTaskPane)
    args))

(def task-pane-container-options
  (merge
    default-options
    (option-map
      (default-option
        :items
        #(doseq [^JXTaskPane p %2]
           (.add ^JXTaskPaneContainer %1 p))))))

(widget-option-provider
  JXTaskPaneContainer
  task-pane-container-options)

(defn task-pane-container
  "Creates a container for task panes. Supports the following additional
  options:

    :items Sequence of task-panes to display

  Examples:

    (task-pane-container
      :items [(task-pane :title \"First\"
                :actions [(action :name \"HI\")
                          (action :name \"BYE\")])
              (task-pane :title \"Second\"
                :actions [(action :name \"HI\")
                          (action :name \"BYE\")])
              (task-pane :title \"Third\" :special? true :collapsed? true
                :items [(button :text \"YEP\")])])
  See:
    (seesaw.swingx/task-pane-container-options)
    (seesaw.swingx/task-pane)
  "
  [& args]
  (apply-options
    (construct JXTaskPaneContainer)
    args))

;*******************************************************************************
; Color Selection Button

(def color-selection-button-options
  (merge
    button-options
    {:selection (:background button-options)}))

(widget-option-provider
  JXColorSelectionButton
  color-selection-button-options)

(defn color-selection-button
  "Creates a color selection button. In addition to normal button options,
  supports:

    :selection A color value. See (seesaw.color/to-color)

  The currently selected color canbe retrieved with (seesaw.core/selection).

  Examples:

    (def b (color-selection-button :selection :aliceblue))

    (selection! b java.awt.Color/RED)

    (listen b :selection
      (fn [e]
        (println \"Selected color changed to \")))

  See:
    (seesaw.swingx/color-selection-button-options)
    (seesaw.color/color)
  "
  [& args]
  (apply-options
    (construct JXColorSelectionButton)
    args))

; Extend selection and selection event stuff for color button.

(extend-protocol Selection
  JXColorSelectionButton
  (get-selection [this] [(config this :selection)])
  (set-selection [this [v]] (config! this :selection v)))

(defmethod listen-for-named-event
  [JXColorSelectionButton :selection]
  [this event-name event-fn]
  (listen-to-property this "background" event-fn))

;*******************************************************************************
; Header

(extend-protocol ConfigIcon
  JXHeader
  (get-icon* [this] (.getIcon this))
  (set-icon* [this v] (.setIcon this (icon v))))

(def header-options
  (merge
    default-options
    (option-map
      (bean-option :title JXHeader resource)
      (default-option :icon set-icon* get-icon*)
      (bean-option :description JXHeader resource))))

(widget-option-provider JXHeader header-options)

(defn header
  "Creates a header which consists of a title, description (supports basic HTML)
  and an icon. Additional options:

    :title The title. May be a resource.
    :description The description. Supports basic HTML (3.2). May be a resource.
    :icon The icon. May be a resource.

  Examples:

    (header :title \"This is a title\"
            :description \"<html>A <b>description</b> with some
                          <i>italics</i></html>\"
            :icon \"http://url/to/icon.png\")

  See:
    (seesaw.swingx/header-options)
  "
  [& args]
  (apply-options
    (construct JXHeader)
    args))

;*******************************************************************************
; JXList

(def ^:private sort-order-table
  {:ascending SortOrder/ASCENDING
   :descending SortOrder/DESCENDING})

; Override view/model index conversion so that the default selection
; handler from JList will work.
(extend-protocol ViewModelIndexConversion
  JXList
  (index-to-model [this index] (.convertIndexToModel this index))
  (index-to-view [this index] (.convertIndexToView this index)))

(default-highlighter-host JXList)

(def listbox-x-options
  (merge
    listbox-options
    highlighter-host-options
    (option-map
      ; When the model is changed, make sure the sort order is preserved
      ; Otherwise, it doesn't look like :sort-with is working.
      (default-option :model
                      (fn [^JXList c v]
                        (let [old (.getSortOrder c)]
                          ((:setter (:model listbox-options)) c v)
                          (.setSortOrder c old)))
                      (:getter (:model listbox-options)))

      (bean-option :sort-order JXList sort-order-table)

      (default-option :sort-with
                      (fn [^JXList c v]
                        (doto c
                          (.setComparator v)
                          (.setSortOrder SortOrder/ASCENDING)))
                      (fn [^JXList c]
                        (.getComparator c))))))

(widget-option-provider JXList listbox-x-options)

(defn listbox-x
  "Create a JXList which is basically an improved (seesaw.core/listbox).
  Additional capabilities include sorting, searching, and highlighting.
  Beyond listbox, has the following additional options:

    :sort-with    A comparator (like <, >, etc) used to sort the items in the
                  model.
    :sort-order   :ascending or descending
    :highlighters A list of highlighters

  By default, ctrl/cmd-F is bound to the search function.

  Examples:

  See:
    (seesaw.core/listbox)
  "
  ^JXList [& args]
  (apply-options
    (doto (construct JXList)
      (.setAutoCreateRowSorter true)
      (.setRolloverEnabled true))
    args))

;*******************************************************************************
; JXTitledPanel

(def titled-panel-options
  (merge
    default-options
    (option-map
      (resource-option :resource [:title :title-color])
      ; the title bar painter (there's no setPainter)
      (bean-option [:painter :title-painter] JXTitledPanel)
      (bean-option :title JXTitledPanel resource)
      (bean-option [:title-color :title-foreground] JXTitledPanel seesaw.color/to-color)
      (bean-option [:content :content-container] JXTitledPanel make-widget)
      (bean-option :right-decoration JXTitledPanel make-widget)
      (bean-option :left-decoration JXTitledPanel make-widget))))

(widget-option-provider
  JXTitledPanel
  titled-panel-options)

(defn titled-panel
  "Creates a panel with a title and content. Has the following properties:

    :content The content widget. Passed through (seesaw.core/to-widget)
    :title   The text of the title. May be a resource.
    :title-color Text color. Passed through (seesaw.color/to-color). May
             be resource.
    :left-decoration Decoration widget on left of title.
    :right-decoration Decoration widget on right of title.
    :resource Set :title and :title-color from a resource bundle
    :painter Painter used on the title

  Examples:

    (titled-panel :title \"Error\"
                  :title-color :red
                  :content (label-x :wrap-lines? true
                                   :text \"An error occurred!\"))

  See:
    (seesaw.core/listbox)
  "
  ^JXTitledPanel [& args]
  (apply-options
    (construct JXTitledPanel)
    args))

;*******************************************************************************
; JXTree

(default-highlighter-host JXTree)

(def tree-x-options
  (merge
    tree-options
    highlighter-host-options
    (option-map)))

(widget-option-provider JXTree tree-x-options)

(defn tree-x
  "Create a JXTree which is basically an improved (seesaw.core/tree).
  Additional capabilities include searching, and highlighting.
  Beyond tree, has the following additional options:

    :highlighters A list of highlighters

  By default, ctrl/cmd-F is bound to the search function.

  Examples:

  See:
    (seesaw.core/tree-options)
    (seesaw.core/tree)
  "
  ^JXTree [& args]
  (apply-options
    (doto (construct JXTree)
      (.setRolloverEnabled true))
    args))

;*******************************************************************************
; JXTable

(default-highlighter-host JXTable)

(def table-x-options
  (merge
    table-options
    highlighter-host-options
    (option-map
      (bean-option :column-control-visible? JXTable boolean)
      (bean-option :horizontal-scroll-enabled? JXTable boolean)
      (bean-option :column-margin JXTable))))

(widget-option-provider JXTable table-x-options)

(defn table-x
  "Create a JXTable which is basically an improved (seesaw.core/table).
  Additional capabilities include searching, sorting and highlighting.
  Beyond table, has the following additional options:

    :column-control-visible? Show column visibility control in upper right corner.
                             Defaults to true.
    :column-margin           Set margin between cells in pixels
    :highlighters            A list of highlighters
    :horizontal-scroll-enabled? Allow horizontal scrollbars. Defaults to false.

  By default, ctrl/cmd-F is bound to the search function.

  Examples:

  See:
    (seesaw.core/table-options)
    (seesaw.core/table)
  "
  ^JXTable [& args]
  (apply-options
    (doto (construct JXTable)
      (.setRolloverEnabled true)
      (.setColumnControlVisible true))
    args))


;*******************************************************************************
; JXPanel

(def panel-x-options
  (merge
    default-options
    (option-map
      (bean-option :alpha JXPanel))))

(widget-option-provider
  JXPanel
  panel-x-options)

(defn- abstract-panel-x [layout opts]
  (abstract-panel (construct JXPanel) layout opts))

(defn xyz-panel-x ^JXPanel [& opts]
  (abstract-panel-x nil opts))

(defn border-panel-x ^JXPanel [& opts]
  (abstract-panel-x (BorderLayout.) opts))

(defn flow-panel-x ^JXPanel [& opts]
  (abstract-panel-x (FlowLayout.) opts))

(defn horizontal-panel-x ^JXPanel [& opts]
  (abstract-panel-x (box-layout :horizontal) opts))

(defn vertical-panel-x ^JXPanel [& opts]
  (abstract-panel-x (box-layout :vertical) opts))

(defn grid-panel-x
  ^JXPanel [& {:keys [rows columns] :as opts}]
  (abstract-panel-x (grid-layout rows columns) opts))

(defn card-panel-x ^JXPanel [& opts]
  (abstract-panel-x (CardLayout.) opts))

