;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Functions for implementing custom cell renderers. Note that on
            many core functions (listbox, tree, combobox, etc) a render function
            can be given directly to the :renderer option."
      :author "Dave Ray"}
  seesaw.cells
  (:require [seesaw.util :refer [illegal-argument]])
  (:import (javax.swing DefaultListCellRenderer JComboBox JList JTree
                        ListCellRenderer)
           (javax.swing.tree DefaultTreeCellRenderer TreeCellRenderer)))

(def ^{:private true} nil-fn (constantly nil))

(defn default-list-cell-renderer
  [render-fn]
  (if (instance? ListCellRenderer render-fn)
    render-fn
    (proxy [DefaultListCellRenderer] []
      (getListCellRendererComponent [component value index selected? focus?]
        (let [^DefaultListCellRenderer this this]
          (proxy-super getListCellRendererComponent component value index selected? focus?)
          (render-fn this {:this this
                           :component component
                           :value value
                           :index index
                           :selected? selected?
                           :focus? focus?})
          this)))))

(defn default-tree-cell-renderer
  [render-fn]
  (if (instance? TreeCellRenderer render-fn)
    render-fn
    (proxy [DefaultTreeCellRenderer] []
      (getTreeCellRendererComponent [component value selected? expanded? leaf? row focus?]
        (let [^DefaultTreeCellRenderer this this]
          (proxy-super getTreeCellRendererComponent component value selected? expanded? leaf? row focus?)
          (render-fn this {:this this
                           :component component
                           :value value
                           :selected? selected?
                           :expaned? expanded?
                           :leaf? leaf?
                           :row row
                           :focus? focus?})
          this)))))

(defn to-cell-renderer
  [target arg]
  (cond
    (or (instance? JList target)
        (instance? JComboBox target)) (default-list-cell-renderer arg)
    (instance? JTree target) (default-tree-cell-renderer arg)
    :else (illegal-argument "Don't know how to make cell renderer for %s" (class arg))))
      

