;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Desktop integration: open URLs and files with the system,
           application menu handlers (About, Settings, Quit), dock/taskbar
           icon and badge, and macOS startup properties.

           Wraps java.awt.Desktop and java.awt.Taskbar. Functions for things
           the platform doesn't support do nothing and return false."}
  seesaw.desktop
  (:require [clojure.java.io :as io]
            [seesaw.util :refer [illegal-argument]])
  (:import (java.awt Desktop Desktop$Action Taskbar Taskbar$Feature)
           (java.awt.desktop AboutHandler PreferencesHandler QuitHandler
                             QuitStrategy OpenFilesHandler AppReopenedListener)
           (java.net URI)))

(def ^{:private true} actions
  {:browse            Desktop$Action/BROWSE
   :open              Desktop$Action/OPEN
   :edit              Desktop$Action/EDIT
   :mail              Desktop$Action/MAIL
   :print             Desktop$Action/PRINT
   :reveal            Desktop$Action/BROWSE_FILE_DIR
   :about             Desktop$Action/APP_ABOUT
   :preferences       Desktop$Action/APP_PREFERENCES
   :quit              Desktop$Action/APP_QUIT_HANDLER
   :quit-strategy     Desktop$Action/APP_QUIT_STRATEGY
   :open-files        Desktop$Action/APP_OPEN_FILE
   :app-events        Desktop$Action/APP_EVENT_REOPENED})

(defn- desktop ^Desktop []
  (when (and (not (java.awt.GraphicsEnvironment/isHeadless)) (Desktop/isDesktopSupported))
    (Desktop/getDesktop)))

(defn supported?
  "True if the platform supports a desktop action, one of:
  :browse :open :edit :mail :print :reveal :about :preferences :quit
  :quit-strategy :open-files :app-events"
  [action]
  (let [a (or (actions action) (illegal-argument "Unknown desktop action %s" action))]
    (boolean (some-> (desktop) (.isSupported a)))))

(defmacro ^{:private true} when-supported [action & body]
  `(if (supported? ~action)
     (do ~@body true)
     false))

(defn- to-uri ^URI [v]
  (if (instance? URI v) v (URI. (str v))))

(defn browse!
  "Open a URL (string, java.net.URI or java.net.URL) in the default browser."
  [url]
  (when-supported :browse (.browse (desktop) (to-uri url))))

(defn open!
  "Open a file or folder with its default application."
  [file]
  (when-supported :open (.open (desktop) (io/file file))))

(defn edit!
  "Open a file in its default editor."
  [file]
  (when-supported :edit (.edit (desktop) (io/file file))))

(defn mail!
  "Open the mail client, optionally with a mailto: URI."
  ([] (when-supported :mail (.mail (desktop))))
  ([mailto] (when-supported :mail (.mail (desktop) (to-uri mailto)))))

(defn reveal!
  "Show a file in the platform's file manager (e.g. Finder), selected.
  Falls back to opening its folder where selecting isn't supported."
  [file]
  (let [f (io/file file)]
    (if (supported? :reveal)
      (do (.browseFileDirectory (desktop) f) true)
      (open! (if (.isDirectory f) f (.getParentFile f))))))

(defn app-handlers!
  "Install handlers for the application menu and app events (the macOS app
  menu, mostly). Each is optional:

    :about        (fn [e])  About <app> menu item
    :preferences  (fn [e])  Settings... menu item
    :quit         (fn [e])  Quit request (Cmd-Q, dock menu, logout). Return
                            truthy to quit, falsey to cancel.
    :reopen       (fn [e])  App re-activated, e.g. its dock icon was clicked
                            while it had no windows
    :open-files   (fn [files])  Files opened with the app (seq of java.io.File)

  Pass nil for a handler to remove it. Returns a map of which handlers were
  installed, e.g. {:about true :quit false}."
  [& {:as handlers}]
  (into {}
        (for [[k f] handlers]
          [k (case k
               :about       (when-supported :about
                              (.setAboutHandler (desktop)
                                (when f (reify AboutHandler (handleAbout [_ e] (f e))))))
               :preferences (when-supported :preferences
                              (.setPreferencesHandler (desktop)
                                (when f (reify PreferencesHandler (handlePreferences [_ e] (f e))))))
               :quit        (when-supported :quit
                              (.setQuitHandler (desktop)
                                (when f
                                  (reify QuitHandler
                                    (handleQuitRequestWith [_ e response]
                                      (if (f e) (.performQuit response) (.cancelQuit response)))))))
               :open-files  (when-supported :open-files
                              (.setOpenFileHandler (desktop)
                                (when f (reify OpenFilesHandler
                                          (openFiles [_ e] (f (seq (.getFiles e))))))))
               :reopen      (when-supported :app-events
                              (when f
                                (.addAppEventListener (desktop)
                                  (reify AppReopenedListener (appReopened [_ e] (f e))))))
               (illegal-argument "Unknown app handler %s" k))])))

(defn quit-strategy!
  "What happens on a quit request without a :quit handler: :normal-exit
  (System.exit) or :close-all-windows (close windows from back to front, so
  their :window-closing listeners run)."
  [strategy]
  (when-supported :quit-strategy
    (.setQuitStrategy (desktop) (case strategy
                                  :normal-exit       QuitStrategy/NORMAL_EXIT
                                  :close-all-windows QuitStrategy/CLOSE_ALL_WINDOWS))))

;*******************************************************************************
; Taskbar / dock

(defn- taskbar ^Taskbar []
  (when (and (not (java.awt.GraphicsEnvironment/isHeadless)) (Taskbar/isTaskbarSupported))
    (Taskbar/getTaskbar)))

(defn- taskbar-supports? [^Taskbar$Feature feature]
  (boolean (some-> (taskbar) (.isSupported feature))))

(defn dock-icon!
  "Set the application's dock/taskbar icon from a java.awt.Image, an icon, or
  anything (seesaw.icon/icon) accepts."
  [image]
  (if (taskbar-supports? Taskbar$Feature/ICON_IMAGE)
    (let [img (if (instance? java.awt.Image image)
                image
                (.getImage ^javax.swing.ImageIcon ((requiring-resolve 'seesaw.icon/icon) image)))]
      (.setIconImage (taskbar) img)
      true)
    false))

(defn badge!
  "Show text (e.g. an unread count) on the dock/taskbar icon. nil clears it."
  [text]
  (if (taskbar-supports? Taskbar$Feature/ICON_BADGE_TEXT)
    (do (.setIconBadge (taskbar) (some-> text str)) true)
    false))

(defn request-attention!
  "Bounce the dock icon / flash the taskbar entry. With critical? true it
  keeps going until the app is activated."
  ([] (request-attention! false))
  ([critical?]
   (if (taskbar-supports? Taskbar$Feature/USER_ATTENTION)
     (do (.requestUserAttention (taskbar) true (boolean critical?)) true)
     false)))

;*******************************************************************************
; Startup properties

(defn platform-properties!
  "Set the system properties that configure the app on macOS. They only take
  effect if set before the first window (or any AWT class) is created, so call
  this first thing in -main. Options:

    :app-name         Name in the menu bar and dock
    :screen-menu-bar? Put frame menu bars in the macOS screen menu bar
    :appearance       :system, :light or :dark title bars and native dialogs

  JVM flags (-Dapple.awt.application.name=...) are more reliable for the app
  name, since some JDKs read it very early."
  [& {:keys [app-name screen-menu-bar? appearance]}]
  (when app-name
    (System/setProperty "apple.awt.application.name" (str app-name)))
  (when (some? screen-menu-bar?)
    (System/setProperty "apple.laf.useScreenMenuBar" (str (boolean screen-menu-bar?))))
  (when appearance
    (System/setProperty "apple.awt.application.appearance"
                        (case appearance
                          :system "system"
                          :light  "NSAppearanceNameAqua"
                          :dark   "NSAppearanceNameDarkAqua")))
  nil)
