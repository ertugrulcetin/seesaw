;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns seesaw.timer
  (:require [seesaw.action :refer [action]]
            [seesaw.options :refer [bean-option apply-options option-map
                                    option-provider]]))

(def ^{:private true} timer-opts
  (option-map
    (bean-option :initial-delay javax.swing.Timer)
    (bean-option :delay javax.swing.Timer)
    (bean-option :repeats? javax.swing.Timer boolean)))

(option-provider javax.swing.Timer timer-opts)

(defn- timer-handler [f initial-value]
  (let [value (atom initial-value)]
    (fn [event]
      (swap! value f))))

(defn timer
  "Creates a new Swing timer that periodically executes the single-argument
  function f. The argument is a \"state\" of the timer. Each time the function
  is called its previous return value is passed to it. Kind of like (reduce)
  but spread out over time :) The following options are supported:

    :initial-value The first value passed to the handler function. Defaults to nil.
    :initial-delay Delay, in milliseconds, of first call. Defaults to 0.
    :delay         Delay, in milliseconds, between calls. Defaults to 1000.
    :repeats?      If true, the timer runs forever, otherwise, it's a
                  \"one-shot\" timer. Defaults to true.
    :start?        Whether to start the timer immediately. Defaults to true.

  See http://download.oracle.com/javase/6/docs/api/javax/swing/Timer.html
  "
  ^javax.swing.Timer [f & {:keys [start? initial-value] :or {start? true} :as opts}]
  (let [a (action :handler (timer-handler f initial-value))
        t (javax.swing.Timer. 0 a)]
    (.setDelay t 1000)
    (apply-options t (dissoc opts :start? :initial-value))
    (when start? (.start t))
    t))


(defn restart!
  "Restart a timer: cancel any pending call and wait its initial delay again."
  [^javax.swing.Timer t]
  (.restart t)
  t)

(defn debounce
  "Returns a function that calls f only once calls stop for ms milliseconds,
  with the arguments of the last call. f runs on the Swing thread. Typical
  for autosave or search-as-you-type:

    (def save-soon (debounce 700 save!))
    (listen editor :document (fn [_] (save-soon)))

  Use (cancel! d) to drop a pending call and (flush! d) to run it now."
  [ms f]
  (let [args (atom nil)
        t    (doto (javax.swing.Timer. (int ms) nil)
               (.setRepeats false))]
    (.addActionListener t (reify java.awt.event.ActionListener
                            (actionPerformed [_ _]
                              (let [[a] (reset-vals! args nil)]
                                (when a (apply f (second a)))))))
    (with-meta
      (fn [& xs]
        (reset! args [:pending xs])
        (.restart t)
        nil)
      {::timer t ::args args ::f f})))

(defn cancel!
  "Drop the pending call of a (debounce) function. Returns d."
  [d]
  (let [{t ::timer args ::args} (meta d)]
    (.stop ^javax.swing.Timer t)
    (reset! args nil)
    d))

(defn flush!
  "Run the pending call of a (debounce) function now, on the calling thread.
  Returns d."
  [d]
  (let [{t ::timer args ::args f ::f} (meta d)
        [a] (reset-vals! args nil)]
    (.stop ^javax.swing.Timer t)
    (when a (apply f (second a)))
    d))

(defn start!
  "Start a timer. Returns it."
  [^javax.swing.Timer t]
  (.start t)
  t)

(defn stop!
  "Stop a timer. Returns it."
  [^javax.swing.Timer t]
  (.stop t)
  t)

(defn running?
  "True if the timer is running."
  [^javax.swing.Timer t]
  (.isRunning t))
