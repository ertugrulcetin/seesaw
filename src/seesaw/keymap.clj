;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Functions for mapping key strokes to actions."
      :author "Dave Ray"}
  seesaw.keymap
  (:require [seesaw.util :refer [illegal-argument]]
            [seesaw.keystroke :refer [keystroke]]
            [seesaw.action :refer [action]]
            [seesaw.to-widget :refer [to-widget*]]))

(defn- ^javax.swing.Action to-action [act]
  (cond
    (nil? act) nil

    (instance? javax.swing.Action act) act

    (instance? javax.swing.AbstractButton act)
      (let [^javax.swing.AbstractButton b act]
        (action :handler (fn [_] (.doClick b))))

    (fn? act) 
      (action :handler act)
    :else (illegal-argument "Don't know how to make key-map action from '%s'" act)))

(defn- ^javax.swing.JComponent to-target [target]
  (cond
    (instance? javax.swing.JComponent target) target
    (instance? javax.swing.JFrame target) (.getRootPane target)
    :else (illegal-argument "Don't know how to map keys on '%s'" target)))

(def ^{:private true} scope-table
  { :descendants javax.swing.JComponent/WHEN_ANCESTOR_OF_FOCUSED_COMPONENT
    :self        javax.swing.JComponent/WHEN_FOCUSED
    :global      javax.swing.JComponent/WHEN_IN_FOCUSED_WINDOW })

(def ^{:private true} default-scope (:descendants scope-table))

(defn map-key
  "Install a key mapping on a widget. 
  
  Key mappings are hopelessly entwined with keyboard focus and the widget 
  hierarchy. When a key is pressed in a widget with focus, each widget up
  the hierarchy gets a chance to handle it. There three 'scopes' with
  which a mapping may be registered:
  
    :self 

      The mapping only handles key presses when the widget itself has
      the keyboard focus. Use this, for example, to install custom
      key mappings in a text box.
  
    :descendants
  
      The mapping handles key presses when the widget itself or any
      of its descendants has keyboard focus. 
  
    :global
  
      The mapping handles key presses as long as the top-level window
      containing the widget is active. This is what's used for menu
      shortcuts and should be used for other app-wide mappings.
  
  Given this, each mapping is installed on a particular widget along
  with the desired keystroke and action to perform. The keystroke can
  be any valid argument to (seesaw.keystroke/keystroke). The action
  can be one of the following:
  
    * A javax.swing.Action. See (seesaw.core/action)
    * A single-argument function. An action will automatically be
      created around it.
    * A button, menu, menuitem, or other button-y thing. An action
      that programmatically clicks the button will be created.
    * nil to disable or remove a mapping 
    * :none to block the key in this widget's own input map, including the
      bindings it inherits from the look and feel. As usual in Swing, the
      key then goes on to ancestors and :global mappings, so this lets e.g.
      a menu accelerator win over a text field's built-in binding.

  target may be a widget, frame, or something convertible through to-widget.

  Returns a function that removes the key mapping.

  Examples:

    ; In frame f, key \"K\" clicks button b
    (map-key f \"K\" b)

    ; In text box t, map ctrl+enter to a function
    (map-key t \"control ENTER\"
      (fn [e] (alert e \"You pressed ctrl+enter!\")))

  See:
    (seesaw.keystroke/keystroke)
    http://download.oracle.com/javase/tutorial/uiswing/misc/keybinding.html
  "
  [target key act & {:keys [scope id] :as opts}]
  (let [target (to-target (to-widget* target))
        scope  (scope-table scope default-scope)
        im     (.getInputMap target scope)
        am     (.getActionMap target)
        ks     (keystroke key)]
    (if (= :none act)
      (do
        (.put im ks "none")
        (fn [] (.remove im ks)))
      (let [act (to-action act)
            id  (or id act)]
        (.put im ks id)
        (.put am id act)
        (fn []
          (.remove im ks)
          (.remove am id))))))

(defn- binding-action
  "The enabled action bound to ks in c's input map for the given condition"
  [^javax.swing.JComponent c condition ks]
  (when-let [id (.get (.getInputMap c condition) ks)]
    (when-not (= "none" id)
      (when-let [^javax.swing.Action a (.get (.getActionMap c) id)]
        (when (.isEnabled a) a)))))

(defn- window-components
  "All JComponents in w, including menu items that live in closed menus"
  [^java.awt.Container w]
  (letfn [(walk [^java.awt.Component c]
            (cons c (mapcat walk
                            (cond
                              (instance? javax.swing.JMenu c)
                                (.getMenuComponents ^javax.swing.JMenu c)
                              (instance? java.awt.Container c)
                                (.getComponents ^java.awt.Container c)))))]
    (filter #(instance? javax.swing.JComponent %) (walk w))))

(defn trigger!
  "Perform the action that pressing key would trigger in target, following
  Swing's precedence: target's own :self mapping, then :descendants mappings
  of target and its ancestors, then :global mappings (including menu
  accelerators) in target's window. Doesn't need keyboard focus, so it's
  handy in tests and for things like command palettes.

  Returns true if an action was performed, false otherwise.

  Examples:

    (trigger! text-field \"menu B\")
    (trigger! frame \"menu shift G\")
  "
  [target key]
  (let [target (to-target (to-widget* target))
        ks     (keystroke key)
        ancestors (take-while some? (iterate #(.getParent ^java.awt.Component %) target))
        window (javax.swing.SwingUtilities/getWindowAncestor target)
        found  (or (binding-action target javax.swing.JComponent/WHEN_FOCUSED ks)
                   (some (fn [c]
                           (when (instance? javax.swing.JComponent c)
                             (when-let [a (binding-action c javax.swing.JComponent/WHEN_ANCESTOR_OF_FOCUSED_COMPONENT ks)]
                               [c a])))
                         ancestors)
                   (some (fn [c]
                           (when-let [a (binding-action c javax.swing.JComponent/WHEN_IN_FOCUSED_WINDOW ks)]
                             [c a]))
                         (window-components (or window (last ancestors)))))
        [source ^javax.swing.Action a] (if (vector? found) found [target found])]
    (if a
      (do
        (.actionPerformed a (java.awt.event.ActionEvent.
                              source java.awt.event.ActionEvent/ACTION_PERFORMED
                              (str (.getValue a javax.swing.Action/ACTION_COMMAND_KEY))))
        true)
      false)))

