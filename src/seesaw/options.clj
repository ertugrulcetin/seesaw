;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc    "Functions for dealing with options."
      :author "Dave Ray"}
seesaw.options
  (:require [seesaw.meta :refer [get-meta put-meta!]]
            [seesaw.ratom :as ratom]
            [seesaw.util :refer [camelize illegal-argument check-args resource
                                 resource-key?]]))

(defprotocol OptionProvider
  (get-option-maps* [this]))

(defn get-option-map [this]
  (apply merge (get-option-maps* this)))

(defmacro option-provider [class options]
  `(extend-protocol OptionProvider
     ~class
     (~'get-option-maps* [this#] [~options])))

(defrecord Option [name setter getter examples])

(declare apply-options)

(defn- strip-question-mark
  [^String s]
  (if (.endsWith s "?")
    (.substring s 0 (dec (count s)))
    s))

(defn- setter-name [property]
  (->> property
       name
       strip-question-mark
       (str "set-")
       camelize
       symbol))

(defn- getter-name [property]
  (let [property (name property)
        prefix   (if (.endsWith property "?") "is-" "get-")]
    (->> property
         name
         strip-question-mark
         (str prefix)
         camelize
         symbol)))

(defn- split-bean-option-name [v]
  (cond
    (vector? v) v
    :else [v v]))

(defmacro bean-option
  [name-arg target-type & [set-conv get-conv examples]]
  (let [[option-name bean-property-name] (split-bean-option-name name-arg)
        target (gensym "target")]
    `(Option. ~option-name
              (fn [~(with-meta target {:tag target-type}) value#]
                (. ~target ~(setter-name bean-property-name) (~(or set-conv `identity) value#)))
              (fn [~(with-meta target {:tag target-type})]
                (~(or get-conv `identity) (. ~target ~(getter-name bean-property-name))))
              ~examples)))

(defn default-option
  ([name] (default-option name (fn [_ _] (illegal-argument "No setter defined for option %s" name))))
  ([name setter] (default-option name setter (fn [_] (illegal-argument "No getter defined for option %s" name))))
  ([name setter getter] (default-option name setter getter nil))
  ([name setter getter examples] (Option. name setter getter examples)))

(defn ignore-option
  "Might be used to explicitly ignore the default behaviour of options."
  ([name examples] (default-option name (fn [_ _]) (fn [_ _]) "Internal use."))
  ([name] (ignore-option name nil)))

(defn resource-option
  "Defines an option that takes a j18n namespace-qualified keyword as a
  value. The keyword is used as a prefix for the set of properties in
  the given key list. This allows subsets of widget options to be configured
  from a resource bundle.
  
  Example:
    ; The :resource property looks in a resource bundle for 
    ; prefix.text, prefix.foreground, etc.
    (resource-option :resource [:text :foreground :background])
  "
  [option-name keys]
  (default-option
    option-name
    (fn [target value]
      {:pre [(resource-key? value)]}
      (let [nspace (namespace value)
            prefix (name value)]
        (apply-options
          target (mapcat (fn [k]
                           (let [prop (keyword nspace (str prefix "." k))]
                             (when-let [v (resource prop)]
                               [(keyword k) v])))
                         (map name keys)))))
    nil
    [(str "A i18n prefix for a resource with keys")
     (pr-str keys)]))

(defn- apply-option
  [target ^Option opt v]
  (if-let [setter (:setter opt)]
    (let [binding-key [::binding (:name opt)]]
      ; a new value replaces any reactive binding previously set for this option
      (when-let [unbind (get-meta target binding-key)]
        (unbind)
        (put-meta! target binding-key nil))
      (if (ratom/reactive? v)
        (put-meta! target binding-key (ratom/bind! target setter v (:getter opt)))
        (setter target v)))
    (illegal-argument "No setter found for option %s" (:name opt))))

;; Any JavaBean property of the target works as an option, even without an
;; explicit option definition: :continuous-layout? -> continuousLayout. Values
;; are converted by the property's type, e.g. colors, fonts, borders, icons.

(defn- property-descriptors [^Class c]
  (into {}
        (for [^java.beans.PropertyDescriptor pd
              (.getPropertyDescriptors (java.beans.Introspector/getBeanInfo c))]
          [(.getName pd) pd])))

(def ^{:private true} class-properties (memoize property-descriptors))

(defn- property-name [name]
  (let [n (clojure.core/name name)
        n (if (.endsWith n "?") (subs n 0 (dec (count n))) n)]
    (camelize n)))

(defn- converter [^Class t]
  (let [conv (fn [sym] (let [f (requiring-resolve sym)] #(f %)))]
    (cond
      (= t Boolean/TYPE)                     boolean
      (= t Integer/TYPE)                     int
      (= t Long/TYPE)                        long
      (= t Float/TYPE)                       float
      (= t Double/TYPE)                      double
      (= t Character/TYPE)                   char
      (.isAssignableFrom java.awt.Color t)   (conv 'seesaw.color/to-color)
      (.isAssignableFrom java.awt.Font t)    (conv 'seesaw.font/to-font)
      (.isAssignableFrom javax.swing.border.Border t) (conv 'seesaw.border/to-border)
      (.isAssignableFrom javax.swing.Icon t) (conv 'seesaw.icon/icon)
      (.isAssignableFrom java.awt.Dimension t) (conv 'seesaw.util/to-dimension)
      (.isAssignableFrom java.awt.Insets t)  (conv 'seesaw.util/to-insets)
      (.isAssignableFrom java.awt.Cursor t)  (conv 'seesaw.cursor/cursor)
      (= String t)                           #(some-> % str)
      :else                                  identity)))

(defn- bean-property-option [target name]
  (when-let [^java.beans.PropertyDescriptor pd (get (class-properties (class target)) (property-name name))]
    (let [setter (.getWriteMethod pd)
          getter (.getReadMethod pd)]
      (when (or setter getter)
        (let [convert (if setter (converter (first (.getParameterTypes setter))) identity)]
          (default-option
            name
            (if setter
              (fn [t v] (.invoke setter t (object-array [(convert v)])))
              (fn [_ _] (illegal-argument "Property %s of %s is read-only" name (class target))))
            (if getter
              (fn [t] (.invoke getter t (object-array 0)))
              (fn [_] (illegal-argument "Property %s of %s is write-only" name (class target))))
            [(str "The " (.getName pd) " bean property")]))))))

(defn- ^Option lookup-option [target handler-maps name]
  (if-let [opt (or (some #(if % (% name)) handler-maps)
                   (bean-property-option target name))]
    opt
    (illegal-argument "%s does not support the %s option" (class target) name)))

(defn- apply-options*
  [target opts handler-maps]
  (let [pairs (if (map? opts) opts (partition 2 opts))]
    (doseq [[k v] pairs]
      (let [opt (lookup-option target handler-maps k)]
        (apply-option target opt v))))
  target)

(defn apply-options
  [target opts]
  (check-args (or (map? opts) (even? (count opts)))
              "opts must be a map or have an even number of entries")
  (apply-options* target opts (get-option-maps* target)))

(defn ignore-options
  "Create a ignore-map for options, which should be ignored. Ready to
  be merged into default option maps."
  [source-options]
  (into {} (for [k (keys source-options)] [k (ignore-option k)])))

(defn around-option
  ([parent-option set-conv get-conv examples]
   (default-option (:name parent-option)
                   (fn [target value]
                     ((:setter parent-option) target ((or set-conv identity) value)))
                   (fn [target]
                     ((or get-conv identity) ((:getter parent-option) target)))
                   examples))
  ([parent-option set-conv get-conv]
   (around-option parent-option set-conv get-conv nil)))

(defn option-map
  "Construct an option map from a list of options."
  [& opts]
  (into {} (map (juxt :name identity) opts)))

(defn get-option-value
  ([target name] (get-option-value target name (get-option-maps* target)))
  ([target name handlers]
   (let [^Option option (lookup-option target handlers name)
         getter         (:getter option)]
     (if getter
       (getter target)
       (illegal-argument "Option %s cannot be read from %s" name (class target))))))

(defn set-option-value
  ([target name value] (set-option-value target name (get-option-maps* target)))
  ([target name value handlers]
   (let [^Option option (lookup-option target handlers name)
         setter         (:setter option)]
     (if setter
       (setter target value)
       (illegal-argument "Option %s cannot be set on %s" name (class target))))))

