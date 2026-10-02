(ns test-app
  "Seesaw Studio: one app that exercises as much of seesaw as possible.

  All state lives in a single, deeply nested ratom (app-db). Views read it
  through re-frame style subscriptions (reg-sub / subscribe), and change it
  only by dispatching events (reg-event / dispatch!), which are pure
  functions of the db. Side effects (dialogs, files, theme) live in event
  handlers' callers or in watches.

  Run: lein run -m test-app     REPL: (-main), then evaluate forms below."
  (:require [clojure.string :as str]
            [seesaw.behave :as behave]
            [seesaw.bind :as b]
            [seesaw.border :as border]
            [seesaw.cells :as cells]
            [seesaw.chooser :as chooser]
            [seesaw.clipboard :as clipboard]
            [seesaw.color :as color]
            [seesaw.core :as s]
            [seesaw.cursor :as cursor]
            [seesaw.desktop :as desktop]
            [seesaw.dnd :as dnd]
            [seesaw.font :as font]
            [seesaw.forms :as forms]
            [seesaw.graphics :as g]
            [seesaw.icon :as icon]
            [seesaw.keymap :as keymap]
            [seesaw.keystroke :as keystroke]
            [seesaw.laf :as laf]
            [seesaw.mig :as mig]
            [seesaw.mouse :as mouse]
            [seesaw.pref :as pref]
            [seesaw.ratom :refer [ratom reaction reg-sub subscribe]]
            [seesaw.rsyntax :as rsyntax]
            [seesaw.style :as style]
            [seesaw.swingx :as sx]
            [seesaw.text :as text]
            [seesaw.timer :as timer]
            [seesaw.tree :as tree]
            [seesaw.undo :as undo]
            [seesaw.widgets.log-window :as log-window]
            [seesaw.widgets.rounded-label :refer [rounded-label]])
  (:import (java.awt Color)))

;*******************************************************************************
; State: one ratom for the whole app

(def sample-text
  "Seesaw Studio\n\nThis editor is a styled-text widget. Try the find bar (menu F), undo (menu Z), bold (menu B) and the paragraph styles in the toolbar.\n\nSwing is old, but with a nice wrapper it is still a pleasant way to build desktop apps. Find the word swing to see highlights.")

(def sample-code
  "(ns hello\n  (:require [seesaw.core :as s]))\n\n(defn -main [& args]\n  (-> (s/frame :title \"Hello\" :content \"Hello, Seesaw\")\n      s/pack!\n      s/show!))\n")

(def initial-db
  {:ui {:section :dashboard
        :theme :flat-light
        :sidebar? true
        :status "Ready"
        :zoom 13
        :density :comfortable
        :toasts []}
   :session {:started (System/currentTimeMillis)
             :uptime 0
             :clicks 0
             :events []}
   :user {:profile {:name "Ada Lovelace" :email "ada@example.com" :role :admin
                    :country "United Kingdom" :birth-year 1815
                    :bio "First programmer."}
          :prefs {:notify? true
                  :volume 40
                  :channels #{:email}
                  :accent "#3b82f6"
                  :rating 3}}
   :todos {:items {1 {:id 1 :title "Write the docs" :done? false :priority :high :tags #{:work} :estimate 3}
                   2 {:id 2 :title "Fix the reflection warnings" :done? true :priority :medium :tags #{:work :code} :estimate 2}
                   3 {:id 3 :title "Buy milk" :done? false :priority :low :tags #{:home} :estimate 1}
                   4 {:id 4 :title "Refactor ratom caching" :done? false :priority :high :tags #{:code} :estimate 5}}
           :next-id 5
           :filter :all
           :query ""
           :sort :priority}
   :editor {:text sample-text
            :code sample-code
            :find {:query "" :case? false}
            :font {:family "SansSerif" :size 15 :line-spacing 0.25}}
   :canvas {:tool :rect
            :color "#ef4444"
            :stroke 2
            :fill? true
            :shapes [{:type :rect :x 30 :y 30 :w 120 :h 80 :color "#3b82f6" :fill? true :stroke 2}
                     {:type :ellipse :x 190 :y 50 :w 90 :h 90 :color "#22c55e" :fill? false :stroke 4}
                     {:type :text :x 40 :y 170 :text "Click to add shapes" :color "#111827"}]}
   :layout {:grid-cols 3
            :sidebar-width 180
            :card :first
            :split 0.5}
   :lab {:items ["Alpha" "Beta" "Gamma" "Delta" "Epsilon"]
         :box [40 40]
         :dropped []
         :bound 30
         :form {:first "Grace" :last "Hopper" :lang "COBOL" :admin? true}
         :strength ""}
   :tree {:root {:name "Workspace"
                 :children [{:name "seesaw" :children [{:name "core.clj" :size 4120}
                                                       {:name "ratom.clj" :size 260}
                                                       {:name "text.clj" :size 180}]}
                            {:name "clj-gram" :children [{:name "ui.clj" :size 790}
                                                         {:name "swing" :children [{:name "layout.clj" :size 179}
                                                                                   {:name "window.clj" :size 182}]}]}
                            {:name "notes.md" :size 12}]}
          :selected nil}})

(defonce app-db (ratom initial-db))

;*******************************************************************************
; Events: (reg-event id (fn [db event] new-db)), (dispatch! [id & args])

(defonce ^:private event-handlers (atom {}))

(defn reg-event [id handler]
  (swap! event-handlers assoc id handler)
  id)

(defn dispatch!
  "Apply the event's handler to app-db. Returns the new db."
  [[id :as event]]
  (if-let [handler (get @event-handlers id)]
    (swap! app-db (fn [db]
                    (-> (handler db event)
                        (update-in [:session :events] #(take 50 (cons id %))))))
    (throw (ex-info (str "No event handler for " id) {:event event}))))

(reg-event :set (fn [db [_ path v]] (assoc-in db path v)))
(reg-event :update (fn [db [_ path f & args]] (apply update-in db path f args)))
(reg-event :status (fn [db [_ msg]] (assoc-in db [:ui :status] msg)))
(reg-event :reset (fn [_ _] (assoc initial-db :session (assoc (:session initial-db) :started (System/currentTimeMillis)))))
(reg-event :tick (fn [db _] (update-in db [:session :uptime] inc)))
(reg-event :click (fn [db _] (update-in db [:session :clicks] inc)))

(reg-event :todo/add
           (fn [db [_ todo]]
             (let [id (get-in db [:todos :next-id])]
               (-> db
                   (assoc-in [:todos :items id] (merge {:done? false :priority :medium :tags #{} :estimate 1}
                                                       todo {:id id}))
                   (update-in [:todos :next-id] inc)
                   (assoc-in [:ui :status] (str "Added \"" (:title todo) "\""))))))
(reg-event :todo/toggle (fn [db [_ id]] (update-in db [:todos :items id :done?] not)))
(reg-event :todo/remove
           (fn [db [_ ids]]
             (-> db
                 (update-in [:todos :items] #(apply dissoc % ids))
                 (assoc-in [:ui :status] (str "Removed " (count ids) " todo(s)")))))
(reg-event :todo/clear-done
           (fn [db _]
             (update-in db [:todos :items] #(into {} (remove (comp :done? val)) %))))

(reg-event :canvas/add-shape
           (fn [db [_ x y]]
             (let [{:keys [tool color stroke fill?]} (:canvas db)
                   shape (case tool
                           :rect {:type :rect :x x :y y :w 80 :h 50}
                           :ellipse {:type :ellipse :x x :y y :w 60 :h 60}
                           :rounded {:type :rounded :x x :y y :w 90 :h 50}
                           :star {:type :star :x x :y y :w 60 :h 60}
                           :text {:type :text :x x :y y :text "Seesaw!"})]
               (update-in db [:canvas :shapes] conj
                          (assoc shape :color color :stroke stroke :fill? fill?)))))
(reg-event :canvas/clear (fn [db _] (assoc-in db [:canvas :shapes] [])))
(reg-event :canvas/undo (fn [db _] (update-in db [:canvas :shapes] #(if (seq %) (pop %) %))))

(defn- tree-path->db-path
  "[0 1] (child indexes) -> [:tree :root :children 0 :children 1]"
  [indexes]
  (into [:tree :root] (mapcat (fn [i] [:children i]) indexes)))

(reg-event :tree/add-child
           (fn [db [_ indexes name]]
             (update-in db (conj (tree-path->db-path indexes) :children)
                        (fnil conj []) {:name name :size 0})))

;*******************************************************************************
; Subscriptions

(reg-sub :ui/title
         (fn [db _]
           (let [section (get-in db [:ui :section])
                 open (count (remove :done? (vals (get-in db [:todos :items]))))]
             (str "Seesaw Studio - " (str/capitalize (name section))
                  (when (pos? open) (str " (" open " open)"))))))

(reg-sub :todos/visible
         (fn [db _]
           (let [{:keys [items query] show :filter order :sort} (:todos db)
                 q (str/lower-case (or query ""))]
             (->> (vals items)
                  (filter (case show
                            :all identity
                            :open (complement :done?)
                            :done :done?))
                  (filter #(str/includes? (str/lower-case (:title %)) q))
                  (sort-by (case order
                             :priority #(get {:high 0 :medium 1 :low 2} (:priority %))
                             :title :title
                             :estimate (comp - :estimate)))
                  vec))))

(reg-sub :todos/table
         (fn [db _]
           (let [todos @(subscribe app-db [:todos/visible])]
             [:columns [{:key :done? :text "Done" :class Boolean}
                        {:key :title :text "Title"}
                        {:key :priority :text "Priority"}
                        {:key :tags :text "Tags"}
                        {:key :estimate :text "Est. (h)" :class Integer}]
              :rows (for [t todos]
                      (-> t
                          (update :priority name)
                          (update :tags #(str/join ", " (map name (sort %))))
                          (update :estimate int)))])))

(reg-sub :todos/stats
         (fn [db _]
           (let [items (vals (get-in db [:todos :items]))
                 done (count (filter :done? items))
                 total (count items)]
             {:total total
              :done done
              :open (- total done)
              :hours (reduce + (map :estimate (remove :done? items)))
              :pct (if (pos? total) (int (* 100 (/ done total))) 0)})))

(reg-sub :editor/word-count
         (fn [db _]
           (let [t (get-in db [:editor :text])]
             (count (re-seq #"\S+" t)))))

(reg-sub :editor/find-matches
         (fn [db _]
           (let [{:keys [text find]} (:editor db)
                 {:keys [query case?]} find]
             (if (str/blank? query)
               []
               (let [m (re-matcher (re-pattern (str (when-not case? "(?i)")
                                                    (java.util.regex.Pattern/quote query)))
                                   text)]
                 (loop [acc []]
                   (if (.find m) (recur (conj acc [(.start m) (.end m)])) acc)))))))

(reg-sub :session/uptime-label
         (fn [db _]
           (let [secs (get-in db [:session :uptime])]
             (format "Up %02d:%02d" (quot secs 60) (mod secs 60)))))

(reg-sub :canvas/summary
         (fn [db _]
           (let [shapes (get-in db [:canvas :shapes])]
             (str (count shapes) " shapes: "
                  (str/join ", " (for [[t n] (frequencies (map :type shapes))] (str n " " (name t))))))))

; Reactions derived from subscriptions
(def completion-text (reaction (subscribe app-db [:todos/stats]) #(str (:pct %) "% complete")))
(def hours-text (reaction (subscribe app-db [:todos/stats]) #(str (:hours %) "h left")))
(def volume-text (reaction (subscribe app-db [:user :prefs :volume]) #(str "Volume: " % "%")))
(def find-text (reaction (subscribe app-db [:editor/find-matches])
                         #(case (count %) 0 "No matches" 1 "1 match" (str (count %) " matches"))))

(defn sub
  "Subscribe to a path or a registered query on app-db."
  [query-v]
  (subscribe app-db query-v))

;*******************************************************************************
; Helpers

(defn sync!
  "Write a widget's value to path whenever it changes (the other direction of
  a subscription bound to one of its options)."
  [w path & {:keys [event read] :or {event :selection read s/selection}}]
  (s/listen w event (fn [_] (dispatch! [:set path (read w)])))
  w)

(defn- glyph
  "A 16px icon drawn in the component's foreground color"
  [kind]
  (icon/paint-icon
    16
    (fn [_ ^java.awt.Graphics2D g]
      (.setStroke g (java.awt.BasicStroke. 1.6))
      (case kind
        :dashboard (do (.drawRoundRect g 2 2 5 5 2 2) (.drawRoundRect g 9 2 5 5 2 2)
                       (.drawRoundRect g 2 9 5 5 2 2) (.drawRoundRect g 9 9 5 5 2 2))
        :todos (do (.drawRoundRect g 2 2 12 12 3 3) (.drawPolyline g (int-array [5 7 11]) (int-array [8 11 5]) 3))
        :editor (do (.drawLine g 3 4 13 4) (.drawLine g 3 8 13 8) (.drawLine g 3 12 9 12))
        :canvas (do (.drawOval g 2 2 12 12) (.fillOval g 6 6 4 4))
        :layouts (do (.drawRect g 2 2 12 12) (.drawLine g 7 2 7 14) (.drawLine g 7 8 14 8))
        :tree (do (.drawLine g 4 3 4 13) (.drawLine g 4 7 10 7) (.drawLine g 4 12 10 12)
                  (.fillOval g 10 5 4 4) (.fillOval g 10 10 4 4))
        :swingx (do (.drawLine g 3 3 13 13) (.drawLine g 13 3 3 13))
        :settings (do (.drawOval g 4 4 8 8) (.drawLine g 8 1 8 4) (.drawLine g 8 12 8 15)
                      (.drawLine g 1 8 4 8) (.drawLine g 12 8 15 8))
        :lab (do (.drawLine g 6 2 10 2) (.drawLine g 7 2 7 7) (.drawLine g 9 2 9 7)
                 (.drawPolygon g (int-array [7 3 13 9]) (int-array [7 14 14 7]) 4))
        :plus (do (.drawLine g 8 3 8 13) (.drawLine g 3 8 13 8))
        :trash (do (.drawRect g 4 5 8 9) (.drawLine g 2 4 14 4) (.drawLine g 6 2 10 2))
        :search (do (.drawOval g 2 2 9 9) (.drawLine g 10 10 14 14))
        (.fillRect g 4 4 8 8)))))

(defn- card
  "A dashboard card: rounded border, FlatLaf style, title + big value"
  [title value-ref & {:keys [icon-kind footer]}]
  (s/border-panel
    :border (border/compound-border (border/empty-border :thickness 12)
                                    (border/rounded-border :radius 10 :color "#d1d5db"))
    :style {:background "lighten(@background,2%)"}
    :north (s/label :text title :icon (glyph icon-kind) :style-class "small"
                    :font (font/font :name :sans-serif :size 12 :weight :semibold :tracking 0.04))
    :center (s/label :text value-ref :style {:font "+12 bold"} :halign :left)
    :south (when footer (s/label :text footer :foreground :gray))))

(def sections
  [{:id :dashboard :label "Dashboard"}
   {:id :todos :label "Todos"}
   {:id :editor :label "Editor"}
   {:id :canvas :label "Canvas"}
   {:id :layouts :label "Layouts"}
   {:id :tree :label "Tree"}
   {:id :swingx :label "SwingX"}
   {:id :lab :label "Lab"}
   {:id :settings :label "Settings"}])

(defonce ^:private log (atom nil))

(defn log! [fmt & args]
  (when-let [w @log]
    (log-window/log w (str (java.time.LocalTime/now) "  " (apply format fmt args) "\n"))))

;*******************************************************************************
; Actions (shared by menus, toolbar and key bindings)

(declare root frame-ref new-todo-dialog export-canvas! about! editor-widgets)

(def actions
  {:new-todo (s/action :name "New Todo..." :key "menu N" :icon (glyph :plus)
                       :tip (str "Add a todo (" (keystroke/label "menu N") ")")
                       :handler (fn [_] (new-todo-dialog)))
   :export (s/action :name "Export Canvas..." :key "menu E"
                     :handler (fn [_] (export-canvas!)))
   :quit (s/action :name "Quit" :key "menu Q"
                   :handler (fn [_] (some-> @frame-ref s/close!)))
   :find (s/action :name "Find" :key "menu F" :icon (glyph :search)
                   :handler (fn [_]
                              (dispatch! [:set [:ui :section] :editor])
                              (some-> (:find @editor-widgets) s/request-focus!)))
   :next-section (s/action :name "Next Section" :key "menu CLOSE_BRACKET"
                           :handler (fn [_]
                                      (let [ids (mapv :id sections)
                                            i (.indexOf ^java.util.List ids (get-in @app-db [:ui :section]))]
                                        (dispatch! [:set [:ui :section] (ids (mod (inc i) (count ids)))]))))
   :reset (s/action :name "Reset State" :handler (fn [_] (when (s/confirm @frame-ref "Reset all state?" :option-type :ok-cancel)
                                                           (dispatch! [:reset]))))
   :about (s/action :name "About Seesaw Studio" :handler (fn [_] (about!)))})

;*******************************************************************************
; Dashboard: cards, buttons, toggles, sliders, spinners, progress

(defn dashboard []
  (let [group (s/button-group)
        roles [:admin :editor :viewer]]
    (s/scrollable
      (mig/mig-panel
        :constraints ["wrap 4, insets 16, gap 12" "[grow,fill][grow,fill][grow,fill][grow,fill]" ""]
        :items
        [[(card "OPEN TODOS" (reaction (sub [:todos/stats]) :open) :icon-kind :todos :footer "not done yet")]
         [(card "COMPLETED" completion-text :icon-kind :dashboard)]
         [(card "HOURS LEFT" hours-text :icon-kind :editor)]
         [(card "UPTIME" (sub [:session/uptime-label]) :icon-kind :settings)]

         [(s/label :text "Progress" :style-class "h3") "span, gaptop 8"]
         [(s/progress-bar :min 0 :max 100 :value (reaction (sub [:todos/stats]) :pct)
                          :paint-string? true) "span"]

         [(s/label :text "Buttons" :style-class "h3") "span, gaptop 8"]
         [(s/horizontal-panel
            :items [(s/button :text "Click me" :listen [:action (fn [_] (dispatch! [:click]))]
                              :mnemonic \C)
                    [:fill-h 8]
                    (s/label :text (reaction (sub [:session :clicks]) #(str "Clicked " % " times")))
                    [:fill-h 16]
                    (s/button :text "Toolbar" :button-type :toolbar)
                    (s/button :text "Round" :button-type :round-rect)
                    (s/button :text "Borderless" :button-type :borderless)
                    (s/button :text "Help" :button-type :help)
                    [:fill-h 16]
                    (s/toggle :text "Notify" :selected? (sub [:user :prefs :notify?])
                              :listen [:action #(dispatch! [:set [:user :prefs :notify?] (s/selection %)])])
                    (rounded-label :text "rounded" :background "#fde68a" :foreground "#1f2937" :border 6)])
          "span"]

         [(s/label :text "Inputs" :style-class "h3") "span, gaptop 8"]
         [(s/label :text volume-text)]
         [(-> (s/slider :min 0 :max 100 :value (sub [:user :prefs :volume])
                        :major-tick-spacing 25 :minor-tick-spacing 5
                        :paint-ticks? true :paint-labels? true :snap-to-ticks? false)
              (sync! [:user :prefs :volume]))
          "span 2"]
         [(-> (s/spinner :model (s/spinner-model 40 :from 0 :to 100 :by 5)
                         :selection (sub [:user :prefs :volume]))
              (sync! [:user :prefs :volume] :read #(int (s/selection %))))]

         [(s/label "Role")]
         [(s/horizontal-panel
            :items (for [r roles]
                     (s/radio :text (str/capitalize (name r)) :group group
                              :selected? (reaction (sub [:user :profile :role]) #(= r %))
                              :listen [:action (fn [_] (dispatch! [:set [:user :profile :role] r]))])))
          "span 3"]

         [(s/label "Channels")]
         [(s/horizontal-panel
            :items (for [c [:email :sms :push :slack]]
                     (s/checkbox :text (name c)
                                 :selected? (reaction (sub [:user :prefs :channels]) #(contains? % c))
                                 :listen [:action (fn [e]
                                                    (dispatch! [:update [:user :prefs :channels]
                                                                (if (s/selection e) conj disj) c]))])))
          "span 3"]

         [(s/label "Country")]
         [(-> (s/combobox :model ["United Kingdom" "Finland" "Turkey" "Japan" "Brazil"]
                          :selection (sub [:user :profile :country]))
              (sync! [:user :profile :country]))
          "span 3"]

         [(s/label "Recent events")]
         [(s/scrollable (s/listbox :model (reaction (sub [:session :events]) #(map name %))
                                   :visible-row-count 6)
                        :preferred-size [200 :by 110])
          "span 3"]]))))

;*******************************************************************************
; Todos: table bound to a subscription, filters, search with debounce, dialogs

(defn new-todo-dialog []
  (let [title (s/text :columns 24 :placeholder "What needs doing?")
        priority (s/combobox :model [:high :medium :low] :selected-item :medium
                             :renderer (fn [r {:keys [value]}] (s/config! r :text (str/capitalize (name value)))))
        estimate (s/spinner :model (s/spinner-model 1 :from 1 :to 40 :by 1))
        tags (s/listbox :model [:work :home :code :errands] :selection-mode :multi-interval
                        :visible-row-count 4)
        ok (s/button :text "Add")
        cancel (s/button :text "Cancel")
        content (mig/mig-panel
                  :constraints ["wrap 2, insets 12" "[right][grow,fill]" ""]
                  :items [["Title"] [title]
                          ["Priority"] [priority]
                          ["Estimate (h)"] [estimate]
                          ["Tags" "top"] [(s/scrollable tags)]
                          [(s/flow-panel :align :right :items [cancel ok]) "span, growx"]])
        dlg (s/custom-dialog :title "New Todo" :modal? true :content content
                             :parent @frame-ref :resizable? false)]
    (behave/when-focused-select-all title)
    (s/listen ok :action (fn [e]
                           (if (str/blank? (s/text title))
                             (s/config! title :outline :error)
                             (s/return-from-dialog e {:title (s/text title)
                                                      :priority (s/selection priority)
                                                      :estimate (s/selection estimate)
                                                      :tags (set (s/selection tags {:multi? true}))}))))
    (s/listen cancel :action (fn [e] (s/return-from-dialog e nil)))
    (s/listen title :action (fn [_] (.doClick ^javax.swing.AbstractButton ok)))
    (-> dlg s/pack! (s/center! @frame-ref))
    (when-let [todo (s/show! dlg)]
      (dispatch! [:todo/add todo]))))

(defn todos []
  (let [search (s/text :placeholder "Search todos" :clear-button? true
                       :leading-icon (glyph :search) :columns 18)
        set-query (timer/debounce 250 (fn [q] (dispatch! [:set [:todos :query] q])))
        highlighters (fn []
                       ; built from the current theme's colors, see laf/on-change below
                       (let [dark (laf/dark?)
                             band (if dark
                                    (.brighter (color/default-color "Table.background"))
                                    (color/color "#eef2ff"))]
                         [(sx/hl-simple-striping :background band)
                          ((sx/hl-color :foreground (if dark "#6b7280" "#9ca3af"))
                           (sx/p-fn (fn [_ ^org.jdesktop.swingx.decorator.ComponentAdapter adapter]
                                     (true? (.getValue adapter 0)))))]))
        t (sx/table-x :model (sub [:todos/table])
                      :highlighters (highlighters)
                      :column-control-visible? true
                      :show-grid? false
                      :selection-mode :multi-interval)
        selected-ids (fn []
                       (let [rows (s/selection t {:multi? true})]
                         (map #(:id (nth @(sub [:todos/visible]) %)) rows)))
        remove! (fn [_] (when-let [ids (seq (selected-ids))]
                          (dispatch! [:todo/remove ids])))]
    (s/listen search :document (fn [_] (set-query (s/text search))))
    (laf/on-change (fn [_] (s/config! t :highlighters (highlighters))))
    (s/listen t :mouse-clicked (fn [^java.awt.event.MouseEvent e] (when (= 2 (.getClickCount e))
                                         (doseq [id (selected-ids)] (dispatch! [:todo/toggle id])))))
    (keymap/map-key t "DELETE" remove! :scope :self)
    (keymap/map-key t "BACK_SPACE" remove! :scope :self)
    (s/config! t :popup (fn [_] [(s/action :name "Toggle done" :handler (fn [_] (doseq [id (selected-ids)] (dispatch! [:todo/toggle id]))))
                                 (s/action :name "Remove" :icon (glyph :trash) :handler remove!)
                                 :separator
                                 (s/action :name "Copy titles" :handler
                                           (fn [_] (clipboard/contents!
                                                     (str/join "\n" (map #(get-in @app-db [:todos :items % :title]) (selected-ids))))))]))
    (s/border-panel
      :border 12 :vgap 8
      :north (s/toolbar
               :floatable? false
               :items [(:new-todo actions)
                       (s/action :name "Remove" :icon (glyph :trash) :handler remove!)
                       (s/action :name "Clear done" :handler (fn [_] (dispatch! [:todo/clear-done])))
                       :separator
                       search
                       [:fill-h 8]
                       (-> (s/combobox :model [:all :open :done]
                                       :selection (sub [:todos :filter])
                                       :renderer (fn [r {:keys [value]}] (s/config! r :text (str "Show: " (name value)))))
                           (sync! [:todos :filter]))
                       (-> (s/combobox :model [:priority :title :estimate]
                                       :selection (sub [:todos :sort])
                                       :renderer (fn [r {:keys [value]}] (s/config! r :text (str "Sort: " (name value)))))
                           (sync! [:todos :sort]))])
      :center (s/scrollable t)
      :south (s/label :text (reaction (sub [:todos/stats])
                                      #(format "%d todos, %d open, %d done - double-click to toggle, Delete removes"
                                               (:total %) (:open %) (:done %)))
                      :foreground :gray))))

;*******************************************************************************
; Editor: styled text, find with highlights, undo, styles, syntax, log

(defonce editor-widgets (atom {}))

(defn editor []
  (let [{:keys [family size line-spacing]} (get-in @app-db [:editor :font])
        pane (s/styled-text
               :text (get-in @app-db [:editor :text])
               :wrap-lines? true
               :margin 16
               :default-style [:font family :size size :line-spacing line-spacing]
               :styles [[:bold :bold true]
                        [:italic :italic true]
                        [:strike :strikethrough true :color "#6b7280"]
                        [:mark :background "#fde68a"]
                        [:heading :size 22 :bold true :space-below 6]
                        [:quote :left-indent 24 :italic true :color "#4b5563"]
                        [:center :alignment :center]])
        um (undo/undo-manager! pane)
        find (s/text :placeholder "Find" :columns 16 :clear-button? true
                     :text (sub [:editor :find :query]))
        case? (s/checkbox :text "Aa" :tip "Match case" :selected? (sub [:editor :find :case?]))
        style-btn (fn [label style & {:keys [paragraph?]}]
                    (s/button :text label :button-type :toolbar :focusable? false
                              :listen [:action (fn [_]
                                                 (let [[a b] (s/selection pane)
                                                       a (or a (.getCaretPosition ^javax.swing.JTextPane pane))
                                                       len (max 1 (- (or b a) a))]
                                                   (undo/as-one-edit um
                                                                     (if paragraph?
                                                                       (s/style-paragraph! pane style a len)
                                                                       (s/style-text! pane style a len)))))]))
        code (rsyntax/text-area :text (get-in @app-db [:editor :code]) :syntax :clojure)
        logw (log-window/log-window :limit 20000)]
    (reset! log logw)
    (reset! editor-widgets {:pane pane :find find})
    (sync! find [:editor :find :query] :event :document :read s/text)
    (sync! case? [:editor :find :case?])
    ; keep the db's copy of the text current, for word count and find
    (s/listen pane :document (fn [_] (dispatch! [:set [:editor :text] (s/text pane)])))
    (sync! code [:editor :code] :event :document :read s/text)
    ; highlight matches whenever they change
    (add-watch (sub [:editor/find-matches]) ::find
               (fn [_ _ _ ranges]
                 (s/invoke-later
                   (text/highlight! pane :find ranges :color "#fde047" :arc 3)
                   (when-let [[a] (first ranges)] (text/scroll-to-position! pane a :padding 40)))))
    (keymap/map-key pane "menu B" (fn [_] (.doClick ^javax.swing.AbstractButton (style-btn "B" :bold))) :scope :self)
    (keymap/map-key find "ESCAPE" (fn [_] (dispatch! [:set [:editor :find :query] ""]) (s/request-focus! pane)) :scope :self)
    (keymap/map-key find "ENTER"
                    (fn [_]
                      (let [caret (.getCaretPosition ^javax.swing.JTextPane pane)
                            matches @(sub [:editor/find-matches])]
                        (when-let [[a b] (or (first (filter #(> (first %) caret) matches)) (first matches))]
                          (s/selection! pane [a b])
                          (text/scroll-to-position! pane a :padding 40))))
                    :scope :self)
    (s/tabbed-panel
      :placement :top
      :tabs [{:title "Document"
              :icon (glyph :editor)
              :content (s/border-panel
                         :north (s/toolbar
                                  :floatable? false
                                  :items [(style-btn "B" :bold) (style-btn "I" :italic) (style-btn "S" :strike)
                                          (style-btn "Mark" :mark) :separator
                                          (style-btn "H1" :heading) (style-btn "Quote" :quote :paragraph? true)
                                          (style-btn "Center" :center :paragraph? true) :separator
                                          (s/button :text "Undo" :button-type :toolbar :listen [:action (fn [_] (undo/undo! um))])
                                          (s/button :text "Redo" :button-type :toolbar :listen [:action (fn [_] (undo/redo! um))])
                                          :separator find case?
                                          (s/label :text find-text :border [0 8 0 0])])
                         :center (s/scrollable pane :border nil :unit-increment 16)
                         :south (s/label :text (reaction (sub [:editor/word-count]) #(str % " words"))
                                         :border 6 :foreground :gray))}
             {:title "Code" :tip "RSyntaxTextArea with Clojure highlighting"
              :content (s/scrollable code)}
             {:title "Log" :tip "seesaw.widgets.log-window"
              :content (s/scrollable logw)}])))

;*******************************************************************************
; Canvas: custom painting from data, mouse input, export

(defn- star-path [x y w h]
  (let [cx (+ x (/ w 2)) cy (+ y (/ h 2)) r (/ w 2) r2 (/ w 5)
        pts (for [i (range 10)]
              (let [a (- (* i (/ Math/PI 5)) (/ Math/PI 2))
                    rr (if (even? i) r r2)]
                [(+ cx (* rr (Math/cos a))) (+ cy (* rr (Math/sin a)))]))]
    (apply g/polygon pts)))

(defn- paint-shapes [^java.awt.Component c g2]
  (let [w (.getWidth c) h (.getHeight c)]
    (g/draw g2 (g/rect 0 0 w h)
            (g/style :background (g/linear-gradient :start [0 0] :end [0 h]
                                                    :colors ["#ffffff" "#eef2ff"])))
    (doseq [x (range 0 w 20)]
      (g/draw g2 (g/line x 0 x h) (g/style :foreground "#e5e7eb")))
    (doseq [{:keys [type x y w h color stroke fill? text]} (get-in @app-db [:canvas :shapes])]
      (let [st (g/style :foreground color :stroke (g/stroke :width (or stroke 1) :cap :round)
                        :background (when fill? (color/color color 160))
                        :font (font/font :name :sans-serif :size 18 :weight :bold))]
        (case type
          :rect (g/draw g2 (g/rect x y w h) st)
          :ellipse (g/draw g2 (g/ellipse x y w h) st)
          :rounded (g/draw g2 (g/rounded-rect x y w h 16 16) st)
          :star (g/draw g2 (star-path x y w h) st)
          :text (g/draw g2 (g/string-shape x y text) st))))
    (g/draw g2 (g/path [] (move-to 20 (- h 20)) (curve-to 80 (- h 80) 160 (- h 0) 220 (- h 50))
                       (quad-to 260 (- h 90) 300 (- h 30)))
            (g/style :foreground "#a855f7" :stroke (g/stroke :width 3 :dashes [8 6])))))

(defonce canvas-ref (atom nil))

(defn export-canvas! []
  (when-let [c @canvas-ref]
    (when-let [f (chooser/choose-native-file @frame-ref :type :save :file "canvas.png"
                                             :title "Export canvas")]
      (g/write-png! (g/snapshot c :scale 2) f)
      (dispatch! [:status (str "Exported " (.getName ^java.io.File f))])
      (when (s/confirm @frame-ref (str "Saved " f ". Show it?") :option-type :yes-no)
        (desktop/reveal! f)))))

(defn canvas []
  (let [c (s/canvas :paint paint-shapes :background :white
                    :cursor :crosshair :preferred-size [600 :by 400])
        tools [:rect :ellipse :rounded :star :text]
        group (s/button-group)]
    (reset! canvas-ref c)
    (s/listen c :mouse-clicked (fn [e]
                                 (let [[x y] (mouse/location e)]
                                   (dispatch! [:canvas/add-shape x y]))))
    (add-watch (sub [:canvas :shapes]) ::repaint (fn [& _] (s/repaint! c)))
    (s/border-panel
      :north (s/toolbar
               :floatable? false
               :items (concat
                        (for [t tools]
                          (s/toggle :text (str/capitalize (name t)) :group group
                                    :selected? (reaction (sub [:canvas :tool]) #(= t %))
                                    :listen [:action (fn [_] (dispatch! [:set [:canvas :tool] t]))]))
                        [:separator
                         (s/checkbox :text "Fill" :selected? (sub [:canvas :fill?])
                                     :listen [:action #(dispatch! [:set [:canvas :fill?] (s/selection %)])])
                         (s/label " Stroke ")
                         (-> (s/spinner :model (s/spinner-model 2 :from 1 :to 12 :by 1)
                                        :selection (sub [:canvas :stroke]) :maximum-size [60 :by 30])
                             (sync! [:canvas :stroke] :read #(int (s/selection %))))
                         (s/button :text "Color..."
                                   :icon (reaction (sub [:canvas :color])
                                                   (fn [hex] (icon/paint-icon 14 (fn [_ ^java.awt.Graphics2D g] (.fillOval g 1 1 12 12)) :color hex)))
                                   :listen [:action (fn [_]
                                                      (when-let [^Color col (chooser/choose-color @frame-ref :color (get-in @app-db [:canvas :color]))]
                                                        (dispatch! [:set [:canvas :color]
                                                                    (format "#%02x%02x%02x" (.getRed col) (.getGreen col) (.getBlue col))])))])
                         :separator
                         (s/button :text "Undo" :listen [:action (fn [_] (dispatch! [:canvas/undo]))])
                         (s/button :text "Clear" :listen [:action (fn [_] (dispatch! [:canvas/clear]))])
                         (:export actions)]))
      :center c
      :south (s/label :text (sub [:canvas/summary]) :border 6 :foreground :gray))))

;*******************************************************************************
; Layouts: every layout manager, runtime changes

(defn layouts []
  (let [grid (s/grid-panel :columns (get-in @app-db [:layout :grid-cols]) :hgap 6 :vgap 6
                           :items (for [i (range 12)]
                                    (s/label :text (str "Cell " i) :halign :center :border 1
                                             :background (color/color (+ 120 (* 10 i)) 180 230))))
        cards (s/card-panel :items [[(s/label :text "First card" :halign :center :font "SansSerif-BOLD-24") :first]
                                    [(s/label :text "Second card" :halign :center :font "Serif-ITALIC-24") :second]
                                    [(s/label :text "Third card" :halign :center :font "Monospaced-PLAIN-24") :third]])
        flowing (s/flow-panel :align :left :hgap 8 :vgap 8
                              :items (for [w (str/split "Swing layouts reflow when the scroll pane follows the viewport width so this panel wraps instead of scrolling" #" ")]
                                       (s/label :text w :border (border/rounded-border :radius 8 :padding [2 6] :color "#93c5fd"))))]
    (add-watch (sub [:layout :grid-cols]) ::grid (fn [_ _ _ n] (s/invoke-later (s/config! grid :columns n))))
    (add-watch (sub [:layout :card]) ::cards (fn [_ _ _ k] (s/invoke-later (s/show-card! cards k))))
    (s/tabbed-panel
      :placement :left
      :overflow :scroll
      :tabs [{:title "Grid"
              :content (s/border-panel
                         :north (s/horizontal-panel
                                  :border 8
                                  :items ["Columns: "
                                          (-> (s/slider :min 1 :max 6 :value (sub [:layout :grid-cols])
                                                        :snap-to-ticks? true :major-tick-spacing 1 :paint-ticks? true)
                                              (sync! [:layout :grid-cols]))])
                         :center grid)}
             {:title "Cards"
              :content (s/border-panel
                         :north (s/flow-panel :items (for [k [:first :second :third]]
                                                       (s/button :text (name k) :listen [:action (fn [_] (dispatch! [:set [:layout :card] k]))])))
                         :center cards)}
             {:title "Border"
              :content (s/border-panel :hgap 4 :vgap 4
                                       :north (s/label :text "North" :halign :center :background "#fecaca")
                                       :south (s/label :text "South" :halign :center :background "#bbf7d0")
                                       :east (s/label :text "East" :background "#bfdbfe")
                                       :west (s/label :text "West" :background "#fde68a")
                                       :center (s/label :text "Center" :halign :center :background "#e9d5ff"))}
             {:title "Box"
              :content (s/vertical-panel
                         :items [(s/horizontal-panel :items ["left" :fill-h "right"])
                                 [:fill-v 20]
                                 (s/horizontal-panel :items ["a" [:fill-h 30] "b" [:fill-h 30] "c"])
                                 :fill-v])}
             {:title "Mig"
              :content (s/scrollable
                         (mig/mig-panel
                           :constraints ["wrap 2, insets 16" "[right][grow,fill]" ""]
                           :items [["Name"] [(s/text :text (sub [:user :profile :name]))]
                                   ["Email"] [(-> (s/text :text (sub [:user :profile :email]) :placeholder "you@example.com")
                                                  (sync! [:user :profile :email] :event :document :read s/text))]
                                   ["Password"] [(s/password :placeholder "secret" :echo-char (char 0x2022))]
                                   ["Bio" "top"] [(s/scrollable (s/text :multi-line? true :wrap-lines? true :rows 4
                                                                        :text (sub [:user :profile :bio])))]
                                   ["Birth year"] [(s/spinner :model (s/spinner-model 1815 :from 1800 :to 2026 :by 1)
                                                              :selection (sub [:user :profile :birth-year]))]
                                   [(s/separator) "span, growx, gaptop 10"]
                                   ["" "skip"] [(s/button :text "Save" :listen [:action (fn [_] (dispatch! [:status "Saved profile"]))])
                                                "split 2, right"]]))}
             {:title "Forms"
              :content (forms/forms-panel
                         "right:pref, 4dlu, 120dlu, 8dlu, right:pref, 4dlu, 80dlu"
                         :default-dialog-border? true
                         :items [(forms/title "JGoodies Forms")
                                 (forms/separator "General")
                                 "Company" (forms/span (s/text) 5)
                                 "Contact" (s/text) "Phone" (s/text)
                                 (forms/next-line)
                                 (forms/separator "Details")
                                 "Notes" (forms/span (s/text) 5)])}
             {:title "XYZ"
              :content (s/xyz-panel
                         :items [(s/label :text "absolute (20,20)" :bounds [20 20 150 24] :background "#fef3c7")
                                 (s/button :text "at 200,60" :bounds [200 60 120 30])
                                 (s/label :text "(60,120)" :bounds [60 120 100 24] :border 1)])}
             {:title "Split"
              :content (s/top-bottom-split
                         (s/left-right-split (s/label :text "top-left" :halign :center)
                                             (s/label :text "top-right" :halign :center)
                                             :divider-location 0.3 :continuous-layout? true :one-touch-expandable? true)
                         (s/label :text "bottom" :halign :center)
                         :divider-location 0.6 :resize-weight 0.5)}
             {:title "Reflow"
              :content (s/scrollable flowing :fit-width? true :hscroll :never)}])))

;*******************************************************************************
; Tree: a nested part of app-db as a tree model

(defn tree-view []
  (let [model (fn [root] (tree/simple-tree-model :children #(seq (:children %)) root))
        t (s/tree :model (reaction (sub [:tree :root]) model)
                  :root-visible? true :shows-root-handles? true
                  :renderer (fn [r {:keys [value]}]
                              (s/config! r :text (str (:name value) (when (:size value) (str "  (" (:size value) " lines)")))
                                         :icon (glyph (if (:children value) :layouts :editor)))))
        details (s/label :text (reaction (sub [:tree :selected])
                                         #(if % (str "Selected: " (str/join " / " (map :name %))) "Select a node"))
                         :border 8)]
    (s/listen t :selection (fn [_]
                             (dispatch! [:set [:tree :selected]
                                         (some-> (s/selection t) vec)])))
    (s/border-panel
      :north (s/toolbar
               :floatable? false
               :items [(s/button :text "Add child"
                                 :listen [:action
                                          (fn [_]
                                            (when-let [path (s/selection t)]
                                              (when (:children (last path))
                                                (when-let [name (s/input @frame-ref "Name of the new node:" :value "new.clj")]
                                                  ; child indexes along the selected path
                                                  (let [idx (map (fn [parent child] (.indexOf ^java.util.List (:children parent) child))
                                                                 path (rest path))]
                                                    (dispatch! [:tree/add-child (vec idx) name]))))))])
                       (s/button :text "Expand all"
                                 :listen [:action (fn [_] (doseq [i (range 50)] (.expandRow ^javax.swing.JTree t i)))])])
      :center (s/scrollable t)
      :south details)))

;*******************************************************************************
; SwingX

(defn swingx []
  (sx/border-panel-x
    :north (sx/header :title "SwingX components" :description "Headers, task panes, busy labels, hyperlinks and more."
                      :icon (glyph :swingx))
    :west (sx/task-pane-container
            :items [(sx/task-pane :title "Actions" :icon (glyph :plus)
                                  :items [(:new-todo actions) (:find actions) (:about actions)])
                    (sx/task-pane :title "Links" :collapsed? false
                                  :items [(sx/hyperlink :text "seesaw on GitHub"
                                                        :listen [:action (fn [_] (desktop/browse! "https://github.com/clj-commons/seesaw"))])
                                          (sx/hyperlink :text "FlatLaf"
                                                        :listen [:action (fn [_] (desktop/browse! "https://www.formdev.com/flatlaf/"))])])])
    :center (sx/titled-panel
              :title "Busy, colors and lists"
              :content (s/vertical-panel
                         :border 12
                         :items [(sx/busy-label :text "Working..." :busy? true)
                                 [:fill-v 10]
                                 (sx/label-x :text "label-x wraps long text and can rotate it. It's handy for descriptions that should wrap to the available width."
                                             :wrap-lines? true)
                                 [:fill-v 10]
                                 (sx/color-selection-button :selection (Color. 0x3b82f6))
                                 [:fill-v 10]
                                 (s/scrollable
                                   (sx/listbox-x :model (reaction (sub [:todos :items]) #(map :title (vals %)))
                                                 :sort-order :ascending
                                                 :highlighters [(sx/hl-simple-striping)]))]))))

;*******************************************************************************
; Settings: theme, fonts, keys, dialogs, windows, desktop

(defn- font-sample [label & opts]
  (s/label :text label :font (apply font/font :name :sans-serif :size 16 opts)))

(defn settings []
  (let [themes [:flat-light :flat-dark :flat-mac-light :flat-mac-dark :flat-intellij :flat-darcula :system]]
    (s/scrollable
      (mig/mig-panel
        :constraints ["wrap 2, insets 16, gap 10" "[right][grow,fill]" ""]
        :items
        [[(s/label :text "Appearance" :style-class "h2") "span, growx"]
         ["Theme"]
         [(-> (s/combobox :model themes :selection (sub [:ui :theme])
                          :renderer (fn [r {:keys [value]}] (s/config! r :text (name value))))
              (sync! [:ui :theme]))]
         ["Accent"]
         [(s/label :text (sub [:user :prefs :accent])
                   :border (border/rounded-border :radius 6 :thickness 2 :padding 4 :color (get-in @app-db [:user :prefs :accent])))]
         ["Sidebar"]
         [(s/checkbox :text "Show sidebar" :selected? (sub [:ui :sidebar?])
                      :listen [:action #(dispatch! [:set [:ui :sidebar?] (s/selection %)])])]

         [(s/label :text "Fonts" :style-class "h2") "span, growx, gaptop 12"]
         ["Weights"] [(s/horizontal-panel :items [(font-sample "Light " :weight :light)
                                                  (font-sample "Regular " :weight :regular)
                                                  (font-sample "Semibold " :weight :semibold)
                                                  (font-sample "Bold" :weight :bold)])]
         ["Decorations"] [(s/horizontal-panel :items [(font-sample "T R A C K E D  " :tracking 0.2)
                                                      (font-sample "underlined  " :underline? true)
                                                      (font-sample "struck" :strikethrough? true)])]
         ["Installed"] [(s/label (str "First available of Inter / Helvetica / Arial: "
                                      (font/first-available "Inter" "Helvetica Neue" "Helvetica" "Arial")))]

         [(s/label :text "Shortcuts" :style-class "h2") "span, growx, gaptop 12"]
         [(s/label :text (str/join "    " (for [[k a] [["menu N" "New todo"] ["menu F" "Find"] ["menu E" "Export"]
                                                       ["menu CLOSE_BRACKET" "Next section"] ["menu Q" "Quit"]]]
                                            (str (keystroke/label k) " " a))))
          "span, growx"]
         [(s/label :text (str "Elsewhere these read: "
                              (str/join ", " (map #(keystroke/label % :mac? false) ["menu N" "menu shift F" "alt F4"]))))
          "span, growx"]

         [(s/label :text "Dialogs" :style-class "h2") "span, growx, gaptop 12"]
         [(s/flow-panel
            :align :left
            :items [(s/button :text "Alert" :listen [:action (fn [_] (s/alert @frame-ref "Hello from seesaw!"))])
                    (s/button :text "Confirm" :listen [:action (fn [_] (dispatch! [:status (str "Confirm returned " (pr-str (s/confirm @frame-ref "Are you sure?" :type :question)))]))])
                    (s/button :text "Input" :listen [:action (fn [_] (when-let [v (s/input @frame-ref "Your name?" :value (get-in @app-db [:user :profile :name]))]
                                                                       (dispatch! [:set [:user :profile :name] v])))])
                    (s/button :text "Choose" :listen [:action (fn [_] (when-let [v (s/input @frame-ref "Pick a role" :choices [:admin :editor :viewer] :to-string name)]
                                                                        (dispatch! [:set [:user :profile :role] v])))])
                    (s/button :text "Option dialog"
                              :listen [:action (fn [_] (-> (s/dialog :content "Pick one" :options [(s/button :text "Left" :listen [:action #(s/return-from-dialog % :left)])
                                                                                                   (s/button :text "Right" :listen [:action #(s/return-from-dialog % :right)])])
                                                           s/pack! s/show!
                                                           (as-> r (dispatch! [:status (str "Dialog returned " r)]))))])
                    (s/button :text "Open file..." :listen [:action (fn [_] (when-let [f (chooser/choose-file @frame-ref :filters [["Clojure" ["clj" "edn"]]])]
                                                                              (dispatch! [:status (str "Picked " f)])))])
                    (s/button :text "Native open..." :listen [:action (fn [_] (when-let [fs (chooser/choose-native-file @frame-ref :multi? true :title "Pick files")]
                                                                                (dispatch! [:status (str "Picked " (count fs) " file(s)")])))])])
          "span, growx"]

         [(s/label :text "Window" :style-class "h2") "span, growx, gaptop 12"]
         [(s/flow-panel
            :align :left
            :items [(s/button :text "Center" :listen [:action (fn [_] (s/center! @frame-ref))])
                    (s/button :text "Maximize" :listen [:action (fn [_] (s/maximize! @frame-ref))])
                    (s/button :text "Restore" :listen [:action (fn [_] (s/restore! @frame-ref))])
                    (s/button :text "Minimize" :listen [:action (fn [_] (s/minimize! @frame-ref))])
                    (s/button :text "Badge" :listen [:action (fn [_] (desktop/badge! (str (:open @(sub [:todos/stats])))))])
                    (s/button :text "Clear badge" :listen [:action (fn [_] (desktop/badge! nil))])
                    (s/button :text "Stylesheet" :tip "seesaw.style/apply-stylesheet on the whole window"
                              :listen [:action (fn [_] (style/apply-stylesheet @frame-ref {[:.h2] {:foreground "#3b82f6"}}))])
                    (s/button :text "Reset state" :action (:reset actions))])
          "span, growx"]]))))

;*******************************************************************************
; Lab: bind, drag and drop, dragging, value/select/with-widgets, dynamic
; children, editor pane, windows, password, full screen, JavaFX

(reg-event :lab/move-item
           (fn [db [_ item index]]
             (update-in db [:lab :items]
                        (fn [items]
                          (let [without (vec (remove #{item} items))
                                index   (min (max 0 index) (count without))]
                            (vec (concat (subvec without 0 index) [item] (subvec without index))))))))

(reg-event :lab/dropped-files
           (fn [db [_ files]]
             (-> db
                 (update-in [:lab :dropped] into (map str files))
                 (assoc-in [:ui :status] (str "Dropped " (count files) " file(s)")))))

(defn- strength [^chars pw]
  (let [s (String. pw)]
    (cond
      (empty? s) ""
      (< (count s) 6) "weak"
      (and (re-find #"\d" s) (re-find #"[A-Z]" s) (>= (count s) 10)) "strong"
      :else "ok")))

(defn toast!
  "A short-lived undecorated window (s/window) in the frame's corner"
  [msg]
  (let [f @frame-ref
        w (s/window :content (s/label :text msg :border 12 :style {:background "#111827" :foreground "#f9fafb"}
                                      :background "#111827" :foreground "#f9fafb"))]
    (s/pack! w)
    (when f
      (let [p (.getLocationOnScreen ^java.awt.Component f)]
        (s/move! w :to [(+ (.x p) (- (.getWidth ^java.awt.Component f) (.getWidth ^java.awt.Component w) 24))
                        (+ (.y p) (- (.getHeight ^java.awt.Component f) (.getHeight ^java.awt.Component w) 48))])))
    (s/show! w)
    (timer/timer (fn [_] (s/dispose! w)) :initial-delay 1800 :repeats? false)
    w))

(defn lab []
  (let [; seesaw.bind: slider -> transform -> label, independent of app-db
        bslider (s/slider :min 0 :max 100 :value 30)
        blabel  (s/label)
        ; drag-and-drop reordering of a list stored in app-db
        dnd-list (s/listbox :model (sub [:lab :items])
                            :drag-enabled? true
                            :drop-mode :insert
                            :renderer (cells/default-list-cell-renderer
                                        (fn [r {:keys [value index]}]
                                          (s/config! r :text (str (inc index) ". " value)))))
        drop-zone (s/label :text "Drop files here" :halign :center
                           :border (border/rounded-border :radius 12 :thickness 2 :padding 24 :color "#9ca3af"))
        ; behave/when-mouse-dragged on an absolutely positioned box
        box  (s/label :text "drag me" :halign :center :background "#c7d2fe" :foreground "#1e1b4b"
                      :cursor :move :bounds [40 40 90 40])
        arena (s/xyz-panel :items [box] :background "#f8fafc" :preferred-size [400 :by 160]
                           :border (border/line-border :color "#e5e7eb"))
        ; dynamic children: add!/remove!/replace!
        chips (s/flow-panel :align :left)
        ; a form read and written as a map with value/value!
        form (s/with-widgets [(s/text :id :first)
                              (s/text :id :last)
                              (s/combobox :id :lang :model ["COBOL" "Fortran" "Lisp" "Clojure"])
                              (s/checkbox :id :admin? :text "Admin")]
               (mig/mig-panel :constraints ["wrap 2" "[right][grow,fill]" ""]
                              :items [["First"] [first] ["Last"] [last] ["Language"] [lang] [""] [admin?]]))
        pw (s/password :placeholder "Type a password" :columns 18)
        html (s/editor-pane :content-type "text/html" :editable? false
                            :text "<html><body style='font-family:sans-serif'><h3>editor-pane</h3><p>HTML with <a href='https://github.com/clj-commons/seesaw'>a link</a>. Click it.</p></body></html>")]
    (b/bind (b/selection bslider) (b/transform #(str "seesaw.bind says: " % " px")) (b/property blabel :text))
    (b/bind (b/selection bslider) (b/b-swap! app-db (fn [db v] (assoc-in db [:lab :bound] v))))
    (s/config! dnd-list :transfer-handler
               (dnd/default-transfer-handler
                 :import [dnd/string-flavor (fn [{:keys [data drop-location]}]
                                              (dispatch! [:lab/move-item data (:index drop-location 0)])
                                              true)]
                 :export {:actions (constantly :move)
                          :start   (fn [w] [dnd/string-flavor (s/selection w)])}))
    (s/config! drop-zone :transfer-handler
               (dnd/default-transfer-handler
                 :import [dnd/file-list-flavor (fn [{:keys [data]}] (dispatch! [:lab/dropped-files data]) true)
                          dnd/string-flavor (fn [{:keys [data]}] (dispatch! [:status (str "Dropped text: " data)]) true)]))
    (behave/when-mouse-dragged box
                               :drag (fn [_ [dx dy]]
                                       (dispatch! [:update [:lab :box] (fn [[x y]] [(+ x dx) (+ y dy)])])))
    (add-watch (sub [:lab :box]) ::box
               (fn [_ _ _ [x y]] (s/invoke-later (s/config! box :bounds [x y :* :*]))))
    (s/value! form (get-in @app-db [:lab :form]))
    (s/listen pw :document (fn [_] (s/with-password* pw #(dispatch! [:set [:lab :strength] (strength %)]))))
    (s/listen html :hyperlink (fn [^javax.swing.event.HyperlinkEvent e]
                                (when (= javax.swing.event.HyperlinkEvent$EventType/ACTIVATED (.getEventType e))
                                  (desktop/browse! (.getURL e)))))
    (s/tabbed-panel
      :tabs [{:title "Bind & DnD"
              :content (mig/mig-panel
                         :constraints ["wrap 2, insets 16, gap 12" "[grow,fill][grow,fill]" ""]
                         :items [[(s/label :text "seesaw.bind" :style-class "h3") "span"]
                                 [bslider] [blabel]
                                 [(s/label :text (reaction (sub [:lab :bound]) #(str "...and b-swap! put " % " in app-db"))) "span"]
                                 [(s/grid-panel :columns 2 :hgap 12 :vgap 6
                                                :items [(s/label :text "Drag to reorder (seesaw.dnd)" :style-class "h3")
                                                        (s/label :text "Drop files from Finder" :style-class "h3")])
                                  "span, gaptop 12"]
                                 [(s/grid-panel :columns 2 :hgap 12
                                                :items [(s/scrollable dnd-list) drop-zone])
                                  "span, h 140!"]
                                 [(s/label :text (reaction (sub [:lab :dropped]) #(str (count %) " dropped: " (str/join ", " (take 3 %))))) "span"]])}
             {:title "Drag & dynamic"
              :content (s/border-panel
                         :border 16 :vgap 12
                         :north (s/label :text (reaction (sub [:lab :box]) #(str "behave/when-mouse-dragged - box at " (pr-str %))))
                         :center arena
                         :south (s/vertical-panel
                                  :items [(s/flow-panel
                                            :align :left
                                            :items [(s/button :text "add!" :listen [:action (fn [_] (s/add! chips (s/label :text (str "chip " (inc (count (.getComponents ^java.awt.Container chips))))
                                                                                                                                  :class :chip :border (border/rounded-border :radius 8 :padding [2 8]))))])
                                                    (s/button :text "remove! last" :listen [:action (fn [_] (when-let [l (last (.getComponents ^java.awt.Container chips))] (s/remove! chips l)))])
                                                    (s/button :text "replace! first" :listen [:action (fn [_] (when-let [f (first (.getComponents ^java.awt.Container chips))]
                                                                                                                 (s/replace! chips f (s/label :text "replaced" :class :chip :foreground "#dc2626"))))])
                                                    (s/button :text "select .chip" :listen [:action (fn [_] (dispatch! [:status (str (count (s/select chips [:.chip])) " widgets with class chip")]))])])
                                          chips]))}
             {:title "value / with-widgets"
              :content (s/border-panel
                         :border 16
                         :center form
                         :south (s/flow-panel
                                  :align :left
                                  :items [(s/button :text "Read (value)" :listen [:action (fn [_] (dispatch! [:set [:lab :form] (s/value form)])
                                                                                            (dispatch! [:status (pr-str (s/value form))]))])
                                          (s/button :text "Reset (value!)" :listen [:action (fn [_] (s/value! form (get-in initial-db [:lab :form])))])
                                          (s/button :text "Focus #last" :listen [:action (fn [_] (s/request-focus! (s/select form [:#last])))])
                                          pw
                                          (s/label :text (reaction (sub [:lab :strength]) #(if (str/blank? %) "" (str "strength: " %))))]))}
             {:title "HTML"
              :content (s/scrollable html)}
             {:title "Windows"
              :content (s/flow-panel
                         :align :left :border 16
                         :items [(s/button :text "Toast (s/window)" :listen [:action (fn [_] (toast! "Saved! This is a JWindow."))])
                                 (s/button :text "Toggle full screen" :listen [:action (fn [_] (s/toggle-full-screen! @frame-ref))])
                                 (s/button :text "Scroll log to bottom" :listen [:action (fn [_] (some-> @log (s/scroll! :to :bottom)))])
                                 (s/button :text "Copy app-db" :listen [:action (fn [_] (clipboard/contents! (pr-str @app-db))
                                                                                  (dispatch! [:status "app-db copied to clipboard"]))])
                                 (s/button :text "Custom cursor" :listen [:action (fn [e] (s/config! (s/to-root e) :cursor (cursor/cursor :wait))
                                                                                    (timer/timer (fn [_] (s/config! @frame-ref :cursor :default)) :initial-delay 1000 :repeats? false))])
                                 (s/button :text "JavaFX panel"
                                           :listen [:action (fn [_]
                                                              (try
                                                                (-> (s/frame :title "JFXPanel" :size [300 :by 200]
                                                                             :content (s/jfxpanel :background :white))
                                                                    s/show!)
                                                                (catch Throwable t
                                                                  (s/alert (str "JavaFX isn't available: " (.getMessage t))))))])])}])))

;*******************************************************************************
; Shell: frame, menus, sidebar, cards, status bar

(defonce frame-ref (atom nil))

(defn about! []
  (s/alert @frame-ref (str "Seesaw Studio\n\nLook and feel: " (.getName ^javax.swing.LookAndFeel (laf/laf))
                           "\nDark: " (laf/dark?)
                           "\nEvents dispatched: " (count (get-in @app-db [:session :events])))))

(defn- menubar []
  (let [themes [[:flat-light "Light"] [:flat-dark "Dark"] [:flat-mac-light "macOS Light"] [:flat-mac-dark "macOS Dark"]]
        group (s/button-group)]
    (s/menubar
      :items [(s/menu :text "File" :mnemonic \F
                      :items [(:new-todo actions) (:export actions) :separator (:reset actions) :separator (:quit actions)])
              (s/menu :text "Edit"
                      :items [(s/menu-item :text "Undo" :key "menu Z"
                                           :listen [:action (fn [_] (some-> (:pane @editor-widgets) (keymap/trigger! "menu Z")))])
                              (s/menu-item :text "Redo" :key "menu shift Z"
                                           :listen [:action (fn [_] (some-> (:pane @editor-widgets) (keymap/trigger! "menu shift Z")))])
                              :separator
                              (:find actions)])
              (s/menu :text "View"
                      :items (concat
                               [(s/checkbox-menu-item :text "Sidebar" :key "menu shift S"
                                                      :selected? (sub [:ui :sidebar?])
                                                      :listen [:action #(dispatch! [:set [:ui :sidebar?] (s/selection %)])])
                                (:next-section actions)
                                :separator]
                               (for [[k label] themes]
                                 (s/radio-menu-item :text label :group group
                                                    :selected? (reaction (sub [:ui :theme]) #(= k %))
                                                    :listen [:action (fn [_] (dispatch! [:set [:ui :theme] k]))]))
                               [:separator
                                (s/menu :text "Go to"
                                        :items (for [{:keys [id label]} sections]
                                                 (s/menu-item :text label :icon (glyph id)
                                                              :listen [:action (fn [_] (dispatch! [:set [:ui :section] id]))])))]))
              (s/menu :text "Help" :items [(:about actions)])])))

(defn- sidebar []
  (let [lb (s/listbox :model sections
                      :selection-mode :single
                      :renderer (fn [r {:keys [value]}]
                                  (s/config! r :text (:label value) :icon (glyph (:id value)) :border 6))
                      :fixed-cell-height 32)]
    ; db -> list selection, list selection -> db
    (add-watch (sub [:ui :section]) ::sidebar
               (fn [_ _ _ id] (s/invoke-later (s/selection! lb (first (filter #(= id (:id %)) sections))))))
    (s/selection! lb (first (filter #(= (get-in @app-db [:ui :section]) (:id %)) sections)))
    (s/listen lb :selection (fn [_] (when-let [v (s/selection lb)]
                                      (dispatch! [:set [:ui :section] (:id v)]))))
    (s/scrollable lb :border nil :preferred-size [180 :by 400]
                  :style {:background "darken(@background,3%)"})))

(defn root []
  (let [views {:dashboard dashboard :todos todos :editor editor :canvas canvas
               :layouts layouts :tree tree-view :swingx swingx :lab lab :settings settings}
        cards (s/card-panel :items (for [{:keys [id]} sections] [((views id)) id]))
        side (sidebar)
        split (s/left-right-split side cards :divider-location 180 :divider-size 1 :continuous-layout? true)]
    (s/show-card! cards (get-in @app-db [:ui :section]))
    (add-watch (sub [:ui :section]) ::cards (fn [_ _ _ id] (s/invoke-later (s/show-card! cards id))))
    (add-watch (sub [:ui :sidebar?]) ::sidebar
               (fn [_ _ _ show?]
                 (s/invoke-later
                   (s/config! side :visible? show?)
                   (s/config! split :divider-location (if show? (get-in @app-db [:layout :sidebar-width]) 0)))))
    ; remember the width the user drags the sidebar to
    (s/listen split :property-change
              (fn [^java.beans.PropertyChangeEvent e]
                (when (and (= "dividerLocation" (.getPropertyName e)) (.isVisible ^java.awt.Component side))
                  (let [w (.getDividerLocation ^javax.swing.JSplitPane split)]
                    (when (> w 40) (dispatch! [:set [:layout :sidebar-width] w]))))))
    (s/config! side :visible? (get-in @app-db [:ui :sidebar?]))
    (s/border-panel
      :center split
      :south (s/border-panel
               :border (border/compound-border (border/empty-border :top 4 :bottom 4 :left 10 :right 10)
                                               (border/line-border :top 1 :color "#e5e7eb"))
               :west (s/label :text (sub [:ui :status]))
               :east (s/label :text (sub [:session/uptime-label]) :foreground :gray)))))

(defonce ^:private uptime-timer (atom nil))

(defn- install-watches! []
  ; theme changes are a side effect of the db
  (add-watch (sub [:ui :theme]) ::theme
             (fn [_ _ _ theme]
               (s/invoke-later
                 (laf/set-laf! theme :defaults {"Component.arc" 8 "ScrollBar.width" 10 "TextComponent.arc" 6})
                 (log! "theme %s, dark? %s" theme (laf/dark?)))))
  ; persist a few prefs with java.util.prefs
  (let [node (pref/preferences-node "seesaw-studio")]
    (when-let [saved (.get node "volume" nil)]
      (dispatch! [:set [:user :prefs :volume] (Long/parseLong saved)]))
    (add-watch (sub [:user :prefs :volume]) ::persist (fn [_ _ _ v] (.put node "volume" (str v)))))
  (add-watch app-db ::log (fn [_ _ o n] (when-not (= (:todos o) (:todos n)) (log! "todos changed: %s" (count (get-in n [:todos :items])))))))

(defn -main [& _args]
  (desktop/platform-properties! :app-name "Seesaw Studio" :screen-menu-bar? true :appearance :system)
  (s/invoke-now
    (laf/set-laf! (get-in @app-db [:ui :theme]) :defaults {"Component.arc" 8 "ScrollBar.width" 10 "TextComponent.arc" 6})
    (some-> @frame-ref s/dispose!)
    (let [f (s/frame :title (sub [:ui/title])
                     :size [1100 :by 760]
                     :minimum-size [800 :by 560]
                     :menubar (menubar)
                     :content (root)
                     :unified-title? false
                     :on-close :dispose)]
      (reset! frame-ref f)
      (install-watches!)
      (some-> ^javax.swing.Timer @uptime-timer .stop)
      (reset! uptime-timer (timer/timer (fn [_] (dispatch! [:tick])) :delay 1000 :initial-delay 1000))
      (s/listen f :window-closed (fn [_] (some-> ^javax.swing.Timer @uptime-timer .stop)))
      (desktop/app-handlers! :about (fn [_] (about!))
                             :quit (fn [_] (s/confirm f "Quit Seesaw Studio?" :option-type :ok-cancel)))
      (keymap/map-key f "F1" (fn [_] (about!)) :scope :global)
      (log! "started, %d sections" (count sections))
      (-> f s/center! s/show!))))

(comment
  (-main)
  (dispatch! [:set [:ui :section] :todos])
  (dispatch! [:todo/add {:title "From the REPL" :priority :high}])
  (dispatch! [:set [:ui :theme] :flat-dark])
  (dispatch! [:set [:user :prefs :volume] 80])
  (dispatch! [:canvas/add-shape 300 200])
  (swap! app-db assoc-in [:editor :find :query] "swing")
  @(sub [:todos/stats])
  ; re-evaluate a reg-sub with new code and the open window updates:
  (reg-sub :session/uptime-label (fn [db _] (str "Uptime: " (get-in db [:session :uptime]) "s")))
  (reset! app-db initial-db))
