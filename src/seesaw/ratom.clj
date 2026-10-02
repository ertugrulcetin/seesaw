;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Reactive atoms and re-frame style subscriptions. Pass a ratom,
           reaction or subscription as a widget option value and the widget
           is updated whenever the value changes:

             (def app-db (ratom {:title \"my-title\"}))

             (frame :title (subscribe app-db [:title]))

             (swap! app-db assoc :title \"new title\") ; frame title updates"}
  seesaw.ratom
  (:require [seesaw.invoke :refer [invoke-soon*]])
  (:import (clojure.lang Atom IAtom IAtom2 IDeref IMeta IRef)
           (java.io Writer)
           (java.lang.ref WeakReference)
           (java.util Collections Set WeakHashMap)
           (java.util.concurrent ConcurrentHashMap)
           (java.util.function BiFunction)))

(defprotocol Reactive
  "A value that widget options can be bound to.")

(defn reactive?
  "Returns true if x is a ratom, reaction or subscription."
  [x]
  (instance? seesaw.ratom.Reactive x))

;*******************************************************************************
; ratom

; subs caches this ratom's subscriptions, see (subscribe)
(deftype RAtom [^Atom state ^ConcurrentHashMap subs]
  Reactive

  IAtom
  (swap [_ f] (.swap state f))
  (swap [_ f x] (.swap state f x))
  (swap [_ f x y] (.swap state f x y))
  (swap [_ f x y args] (.swap state f x y args))
  (compareAndSet [_ old new] (.compareAndSet state old new))
  (reset [_ new] (.reset state new))

  IAtom2
  (swapVals [_ f] (.swapVals state f))
  (swapVals [_ f x] (.swapVals state f x))
  (swapVals [_ f x y] (.swapVals state f x y))
  (swapVals [_ f x y args] (.swapVals state f x y args))
  (resetVals [_ new] (.resetVals state new))

  IDeref
  (deref [_] (.deref state))

  IRef
  (setValidator [_ f] (.setValidator state f))
  (getValidator [_] (.getValidator state))
  (getWatches [_] (.getWatches state))
  (addWatch [this k f] (.addWatch state k (fn [k _ o n] (f k this o n))) this)
  (removeWatch [this k] (.removeWatch state k) this)

  IMeta
  (meta [_] (.meta state)))

(defn ratom
  "Create a reactive atom. It behaves like a regular atom (swap!, reset!,
  add-watch, ...) and can be passed as a widget option value, or used as the
  source of (reaction) and (subscribe).

  Takes the same :meta and :validator options as clojure.core/atom."
  [x & options]
  (RAtom. (apply atom x options) (ConcurrentHashMap.)))

;*******************************************************************************
; reaction

(def ^{:private true} no-value (Object.))

(defn- memo-value
  "(f src), reusing the last result while src is unchanged"
  [cache f src]
  (let [[s v] @cache]
    (if (identical? s src)
      v
      (let [v (f src)]
        (reset! cache [src v])
        v))))

(defprotocol ^{:private true} Refreshable
  (-refresh! [this] "Recompute the value, e.g. after its function changed, and
                     notify watchers if it differs."))

(defn- notify-change!
  "Recompute and tell watchers if the value differs from what they last saw.
  Comparing against the last notified value (rather than recomputing f on the
  old source value) also works when f reads other reactive state, e.g. a
  subscription handler that derefs another subscription."
  [this cache f source last watches]
  (let [new (memo-value cache f @source)
        [old] (reset-vals! last new)]
    (when-not (= old new)
      (doseq [[k watch] @watches]
        (watch k this old new)))))

; last holds the value watchers were last told about
(deftype Reaction [source f cache watches last]
  Reactive

  Refreshable
  (-refresh! [this]
    (reset! cache [no-value nil])
    (when (seq @watches)
      (notify-change! this cache f source last watches))
    this)

  IDeref
  (deref [_] (memo-value cache f @source))

  IRef
  (getWatches [_] @watches)
  (addWatch [this k watch]
    (let [[before] (swap-vals! watches assoc k watch)]
      ; watch the source only while this reaction has watchers of its own, so
      ; an unused reaction can be garbage collected
      (when (empty? before)
        (reset! last (memo-value cache f @source))
        (add-watch source this
                   (fn [_ _ _ _]
                     ; watchers only hear about changes to the derived value
                     (notify-change! this cache f source last watches)))))
    this)
  (removeWatch [this k]
    (let [[before after] (swap-vals! watches dissoc k)]
      (when (and (seq before) (empty? after))
        (remove-watch source this)))
    this))

(defmethod print-method Reaction [r ^Writer w]
  (.write w (str "#<Reaction " (pr-str @r) ">")))

(defn reaction
  "Create a read-only reactive value computed as (f @source), where source is a
  ratom or another reaction. Bound widgets are updated only when the derived
  value changes.

    (reaction app-db #(count (:todos %)))"
  [source f]
  (Reaction. source f (atom [no-value nil]) (atom {}) (atom nil)))

;*******************************************************************************
; re-frame style subscriptions

(defonce ^{:doc "The default app state used by (subscribe query-v)."}
         app-db (ratom {}))

(defonce ^{:private true} subscriptions (atom {}))

; query-id -> weak set of the live subscription reactions for it, so (reg-sub)
; can refresh them when a handler is re-registered, e.g. from the REPL
(defonce ^{:private true} live-subscriptions (atom {}))

(defn- weak-set []
  (Collections/synchronizedSet
    (Collections/newSetFromMap (WeakHashMap.))))

(defn- track-subscription! [query-id r]
  (let [subs (or (get @live-subscriptions query-id)
                 (get (swap! live-subscriptions update query-id #(or % (weak-set))) query-id))]
    (.add ^Set subs r)
    r))

(defn- live-subscriptions-for [query-id]
  (when-let [^Set subs (get @live-subscriptions query-id)]
    (locking subs (vec subs))))

(defn reg-sub
  "Register a subscription handler. handler is called as (handler db query-v),
  where db is the current value of the subscribed ratom.

    (reg-sub :todo-count (fn [db _] (count (:todos db))))
    (reg-sub :todo (fn [db [_ id]] (get-in db [:todos id])))

  Re-registering a handler (e.g. re-evaluating the form at the REPL) applies
  to existing subscriptions too: they're recomputed and bound widgets update."
  [query-id handler]
  (swap! subscriptions assoc query-id handler)
  (doseq [r (live-subscriptions-for query-id)]
    (-refresh! r))
  query-id)

(defn- cached-subscription
  "Get or create the reaction for key in db's subscription cache. Entries are
  weak, so a reaction lives only while something (e.g. a bound widget) uses it."
  [^RAtom db key make]
  (let [^ConcurrentHashMap cache (.-subs db)
        result (volatile! nil)
        created (volatile! false)]
    (.compute cache key
              (reify BiFunction
                (apply [_ _ ref]
                  (if-let [r (some-> ^WeakReference ref .get)]
                    (do (vreset! result r) ref)
                    (let [r (make)]
                      (vreset! result r)
                      (vreset! created true)
                      (WeakReference. r))))))
    (when @created
      ; drop entries whose reactions were collected
      (.removeIf (.values cache) #(nil? (.get ^WeakReference %))))
    @result))

(defn subscribe
  "Subscribe to a value derived from a ratom (app-db by default), re-frame
  style. Returns a reaction: pass it straight to a widget option to bind it,
  or deref it for the current value.

  Subscriptions on a ratom are cached: the same db and query-v give the same
  reaction, and its value is computed once per change to db, however many
  widgets are bound to it.

  If a handler was registered for (first query-v) with (reg-sub), it computes
  the value. Otherwise query-v is a path into the db, as with get-in.
  Re-registering the handler updates existing subscriptions.

    (frame :title (subscribe [:title]))            ; (:title @app-db)
    (label :text  (subscribe app-db [:user :name]))
    (label :text  (subscribe [:todo-count]))       ; registered handler"
  ([query-v] (subscribe app-db query-v))
  ([db query-v]
   {:pre [(vector? query-v) (seq query-v)]}
   (let [query-id (first query-v)
         ; look the handler up on every computation so a re-registered one
         ; takes effect
         compute (fn [v]
                   (if-let [handler (get @subscriptions query-id)]
                     (handler v query-v)
                     (get-in v query-v)))
         make #(track-subscription! query-id (reaction db compute))]
     (if (instance? RAtom db)
       (cached-subscription db query-v make)
       (make)))))

;*******************************************************************************
; Binding to widget options

(defn bind!
  "Call (setter target @source) now, then again on the Swing thread whenever
  source changes. The binding only holds a weak reference to target so it
  doesn't keep discarded widgets alive. Returns a function that removes the
  binding.

  If getter is given, the setter is skipped when (getter target) already
  equals the new value. That makes two-way bindings safe: a text field that
  writes its text to the ratom from a document listener isn't set again
  (Swing forbids changing a document while notifying its listeners)."
  ([target setter source] (bind! target setter source nil))
  ([target setter source getter]
   (let [k (gensym "seesaw-ratom-binding")
         target (WeakReference. target)
         unbind #(remove-watch source k)
         current (fn [t]
                   (if getter
                     (try (getter t) (catch Exception _ ::unknown))
                     ::unknown))
         set! (fn [t v]
                (when-not (= v (current t))
                  (setter t v)))]
     (add-watch source k
                (fn [_ _ o n]
                  (if-let [t (.get target)]
                    (when-not (= o n)
                      (invoke-soon* set! t n))
                    (unbind))))
     (setter (.get target) @source)
     unbind)))
