;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns seesaw.test.ratom
  (:require
   [lazytest.core :refer [defdescribe describe expect expect-it it]]
   [seesaw.core :as core]
   [seesaw.invoke :refer [invoke-now]]
   [seesaw.ratom :refer :all])
  (:import (clojure.lang IRef)
           (java.lang.ref WeakReference)
           (java.util Map)
           (javax.swing SwingUtilities)))

(defdescribe ratom-test
  (it "behaves like an atom"
    (let [a (ratom 1)]
      (expect (= 1 @a))
      (expect (= 2 (swap! a inc)))
      (expect (= 5 (reset! a 5)))
      (expect (= [5 6] (swap-vals! a inc)))
      (expect (compare-and-set! a 6 7))
      (expect (= 7 @a))))
  (it "supports :meta and :validator"
    (let [a (ratom 1 :meta {:a 1} :validator pos?)]
      (expect (= {:a 1} (meta a)))
      (expect (try (reset! a -1) false (catch IllegalStateException _ true)))))
  (it "passes itself to watches"
    (let [a (ratom 1) seen (atom nil)]
      (add-watch a :k (fn [k r o n] (reset! seen [k (identical? r a) o n])))
      (swap! a inc)
      (expect (= [:k true 1 2] @seen))))
  (expect-it "is reactive"
    (reactive? (ratom 1)))
  (expect-it "plain atoms are not reactive"
    (not (reactive? (atom 1)))))

(defdescribe reaction-test
  (it "derefs to (f @source)"
    (let [a (ratom {:n 1})]
      (expect (= 2 @(reaction a #(inc (:n %)))))))
  (it "notifies watches only when the derived value changes"
    (let [a (ratom {:n 1 :other 1})
          r (reaction a :n)
          seen (atom [])]
      (add-watch r :k (fn [_ _ o n] (swap! seen conj [o n])))
      (swap! a update :other inc)
      (swap! a update :n inc)
      (remove-watch r :k)
      (swap! a update :n inc)
      (expect (= [[1 2]] @seen))))
  (expect-it "can be derived from another reaction"
    (= 4 @(reaction (reaction (ratom 1) inc) #(* 2 %)))))

(defdescribe subscribe-test
  (it "treats an unregistered query as a path"
    (let [db (ratom {:user {:name "bob"}})]
      (expect (= "bob" @(subscribe db [:user :name])))))
  (it "uses a handler registered with reg-sub"
    (reg-sub ::count (fn [db [_ k]] (count (get db k))))
    (let [db (ratom {:todos [1 2 3]})
          s  (subscribe db [::count :todos])]
      (expect (= 3 @s))
      (swap! db update :todos conj 4)
      (expect (= 4 @s))))
  (it "defaults to app-db"
    (reset! app-db {:title "hi"})
    (expect (= "hi" @(subscribe [:title])))))

(defn- gc-until [pred]
  (loop [i 0]
    (when (and (not (pred)) (< i 50))
      (System/gc) (Thread/sleep 20) (recur (inc i)))))

(defdescribe reaction-memo-test
  (it "computes its value once per source change"
    (let [calls (atom 0)
          a (ratom 1)
          r (reaction a #(do (swap! calls inc) (* 10 %)))]
      (add-watch r :w1 (fn [& _]))
      (add-watch r :w2 (fn [& _]))
      (expect (= 10 @r))
      (expect (= 10 @r))
      (expect (= 1 @calls))
      (swap! a inc)
      (expect (= 20 @r))
      (expect (= 2 @calls))))
  (it "only watches its source while it has watchers"
    (let [a (ratom 1)
          r (reaction a inc)]
      (add-watch r :w1 (fn [& _]))
      (add-watch r :w2 (fn [& _]))
      (expect (= 1 (count (.getWatches ^IRef a))))
      (remove-watch r :w1)
      (expect (= 1 (count (.getWatches ^IRef a))))
      (remove-watch r :w2)
      (expect (empty? (.getWatches ^IRef a))))))

(defdescribe subscription-cache-test
  (it "returns the same reaction for the same db and query"
    (let [db (ratom {:a 1 :b 2})
          s  (subscribe db [:a])]
      (expect (identical? s (subscribe db [:a])))
      (expect (not (identical? s (subscribe db [:b]))))
      (expect (not (identical? s (subscribe (ratom {:a 1}) [:a]))))))
  (it "runs the handler once per change for many bound widgets"
    (let [calls (atom 0)
          db    (ratom {:n 1})
          _     (reg-sub ::counted (fn [db _] (swap! calls inc) (:n db)))
          ls    (doall (for [_ (range 5)] (core/label :text (subscribe db [::counted]))))]
      (reset! calls 0)
      (swap! db update :n inc)
      (invoke-now nil)
      (expect (= 1 @calls))
      (expect (every? #(= "2" (core/text %)) ls))))
  (it "applies a re-registered handler to existing subscriptions"
    (let [db (ratom {:n 1})
          _  (reg-sub ::rereg (fn [db _] (:n db)))
          s1 (subscribe db [::rereg])
          _  (expect (= 1 @s1))
          _  (reg-sub ::rereg (fn [db _] (* 100 (:n db))))
          s2 (subscribe db [::rereg])]
      (expect (= 100 @s1))
      (expect (identical? s1 s2))))
  (it "updates bound widgets when a handler is re-registered, like a REPL reload"
    (let [db    (ratom {:todos [1 2]})
          calls (atom [])
          _     (reg-sub ::reload (fn [db _] (swap! calls conj :v1) (count (:todos db))))
          l     (core/label :text (subscribe db [::reload]))]
      (expect (= "2" (core/text l)))
      (reg-sub ::reload (fn [db _] (swap! calls conj :v2) (* 10 (count (:todos db)))))
      (invoke-now nil)
      (expect (= "20" (core/text l)))
      (swap! db update :todos conj 3)
      (invoke-now nil)
      (expect (= "30" (core/text l)))
      (expect (= :v2 (last @calls)))
      (expect (not-any? #{:v1} (drop-while #{:v1} @calls)))))
  (it "refreshes path subscriptions when a handler is registered for their id"
    (let [db (ratom {::late 5})
          s  (subscribe db [::late])]
      (expect (= 5 @s))
      (reg-sub ::late (fn [db _] (inc (::late db))))
      (expect (= 6 @s))))
  (it "keeps a subscription cached while a widget is bound to it"
    (let [db (ratom {:a 1})
          l  (core/label :text (subscribe db [:a]))
          h  (System/identityHashCode (subscribe db [:a]))]
      (gc-until (constantly false))
      (expect (= h (System/identityHashCode (subscribe db [:a]))))
      (expect (some? l))))
  (it "lets unused subscriptions be garbage collected"
    (let [db (ratom {:a 1})
          w  (WeakReference. (subscribe db [:a]))]
      (gc-until #(nil? (.get w)))
      (expect (nil? (.get w)))
      (subscribe db [:b])
      (expect (= 1 (.size ^Map (.-subs ^seesaw.ratom.RAtom db)))))))

(defdescribe widget-binding-test
  (it "sets the option from a subscription and updates it on change"
    (let [db (ratom {:title "a" :size [100 :by 50]})
          f  (core/frame :title (subscribe db [:title]) :size (subscribe db [:size]))]
      (expect (= "a" (.getTitle f)))
      (swap! db assoc :title "b" :size [200 :by 80])
      (invoke-now nil)
      (expect (= "b" (.getTitle f)))
      (expect (= 200 (.getWidth f)))))
  (it "accepts a ratom directly"
    (let [a (ratom "x")
          l (core/label :text a)]
      (reset! a "y")
      (invoke-now nil)
      (expect (= "y" (core/text l)))))
  (it "replaces an existing binding when the option is set again"
    (let [a (ratom "x")
          b (ratom "q")
          l (core/label :text a)]
      (core/config! l :text b)
      (reset! a "ignored")
      (invoke-now nil)
      (expect (= "q" (core/text l)))
      (core/config! l :text "plain")
      (reset! b "ignored")
      (invoke-now nil)
      (expect (= "plain" (core/text l)))
      (expect (empty? (.getWatches ^IRef a)))
      (expect (empty? (.getWatches ^IRef b)))))
  (it "doesn't keep a discarded widget alive"
    (let [a (ratom "x")
          w (WeakReference. (core/label :text a))]
      (loop [i 0]
        (when (and (some? (.get w)) (< i 50)) (System/gc) (Thread/sleep 20) (recur (inc i))))
      (reset! a "y")
      (expect (nil? (.get w)))
      (expect (empty? (.getWatches ^IRef a)))))
  (it "updates widgets on the Swing thread"
    (let [a (ratom "x")
          on-edt (promise)
          l (core/label :text a)]
      (core/listen l :property-change
                   (fn [e] (when (= "text" (.getPropertyName e))
                             (deliver on-edt (SwingUtilities/isEventDispatchThread)))))
      @(future (reset! a "y"))
      (expect (deref on-edt 2000 false)))))

(defdescribe two-way-binding-test
  (it "lets a text field write its text to the ratom it's bound to"
    (let [db (ratom {:q ""})
          errors (atom [])
          t (core/text :text (subscribe db [:q]))]
      (core/listen t :document
                   (fn [_] (try (swap! db assoc :q (core/text t))
                                (catch Throwable e (swap! errors conj e)))))
      (invoke-now (.insertString (.getDocument t) 0 "abc" nil))
      (invoke-now nil)
      (expect (= "abc" (:q @db)))
      (expect (= "abc" (core/text t)))
      (expect (empty? @errors))
      (swap! db assoc :q "xyz")
      (invoke-now nil)
      (expect (= "xyz" (core/text t))))))

(defdescribe nested-subscription-test
  (it "notifies when a handler derefs another subscription"
    (let [db (ratom {:items [1 2 3] :min 0})
          _  (reg-sub ::big (fn [db _] (filterv #(> % (:min db)) (:items db))))
          _  (reg-sub ::count-big (fn [_ _] (count @(subscribe db [::big]))))
          seen (atom [])
          s  (subscribe db [::count-big])]
      (add-watch s :w (fn [_ _ o n] (swap! seen conj [o n])))
      (swap! db assoc :min 1)
      (swap! db assoc :min 2)
      (swap! db assoc :other true)
      (expect (= [[3 2] [2 1]] @seen)))))
