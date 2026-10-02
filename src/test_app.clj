(ns test-app
  (:require [seesaw.core :as s]
            [seesaw.ratom :refer [ratom reg-sub subscribe]]))

(def app-db (ratom {:title "my-title"
                    :size [200 :by 325]
                    :todos ["write docs"]}))

(reg-sub :todo-count
         (fn [db _]
           (println "ertu22")
           (count (:todos db))))

(defn- view []
  (s/frame :title (subscribe app-db [:title])
           :size (subscribe app-db [:size])
           :content (s/label :text (subscribe app-db [:todo-count]))
           :visible? true))

(defn -main [& _args]
  (view))

(comment
  (-main)
  (swap! app-db assoc :title "helloo")
  (swap! app-db update :todos conj "ses")
  )
