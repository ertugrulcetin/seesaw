;  Copyright (c) Dave Ray, 2011. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this 
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "File chooser and other common dialogs."
      :author "Dave Ray"}
  seesaw.chooser
  (:require [seesaw.color :refer [to-color]]
            [seesaw.options :refer [default-option bean-option apply-options
                                    option-map option-provider]]
            [seesaw.util :refer [illegal-argument]])
  (:import (javax.swing.filechooser FileFilter FileNameExtensionFilter)
           (javax.swing JFileChooser)))

(defn file-filter
  "Create a FileFilter.
  
  Arguments:
  
    description - description of this filter, will show up in the
                  filter-selection box when opening a file choosing dialog.

    accept - a function taking a java.awt.File
             returning true if the file should be shown,
             false otherwise.
  "
  [description accept]
  (proxy [FileFilter] []
    (accept [file]
      (if (accept file)
        true false))
    (getDescription []
      description)))

(def ^{:private true} file-chooser-types {
  :open   JFileChooser/OPEN_DIALOG
  :save   JFileChooser/SAVE_DIALOG
  :custom JFileChooser/CUSTOM_DIALOG
})

(def ^{:private true} file-selection-modes {
  :files-only     JFileChooser/FILES_ONLY
  :dirs-only      JFileChooser/DIRECTORIES_ONLY
  :files-and-dirs JFileChooser/FILES_AND_DIRECTORIES
})

(defn set-file-filters [^JFileChooser chooser filters]
  (.resetChoosableFileFilters chooser)
  (doseq [f filters]
    (.addChoosableFileFilter chooser
      (cond
        (instance? FileFilter f) 
          f

        (and (sequential? f) (sequential? (second f)))
          (FileNameExtensionFilter. (first f) (into-array (second f)))

        (and (sequential? f) (fn? (second f)))
          (apply file-filter f)

        :else
        (illegal-argument "not a valid filter: %s" f)))))

(defn- set-suggested-name [^JFileChooser chooser suggested-name]
  (.setSelectedFile chooser (if (instance? java.io.File suggested-name)
                              suggested-name (java.io.File. (str suggested-name)))))

(def ^{:private true} file-chooser-options 
  (option-map
    (default-option :dir
      (fn [^JFileChooser chooser dir] 
        (.setCurrentDirectory chooser (if (instance? java.io.File dir) dir 
                                          (java.io.File. (str dir))))))
    (default-option :multi?
      #(.setMultiSelectionEnabled ^JFileChooser %1 (boolean %2))
      #(.isMultiSelectionEnabled ^JFileChooser %1))
    (bean-option [:selection-mode :file-selection-mode] JFileChooser file-selection-modes)
    (default-option :filters set-file-filters)
    (default-option :all-files?
      #(.setAcceptAllFileFilterUsed ^JFileChooser %1 (boolean %2))
      #(.isAcceptAllFileFilterUsed ^JFileChooser %1))
    (default-option :suggested-name set-suggested-name)))

(option-provider JFileChooser file-chooser-options)

(def ^{:private true} last-dir (atom nil))

(defn- show-file-chooser [^JFileChooser chooser parent type]
  (case type
    :open (.showOpenDialog chooser parent) 
    :save (.showSaveDialog chooser parent)
          (.showDialog chooser parent (str type))))

(defn- configure-file-chooser [^JFileChooser chooser opts]
  (apply-options chooser opts)
  (when (and @last-dir (not (:dir opts)))
    (.setCurrentDirectory chooser @last-dir))
  chooser)

(defn- remember-chooser-dir [^JFileChooser chooser]
  (reset! last-dir (.getCurrentDirectory chooser))
  chooser)

(defn choose-file
  "Choose a file to open or save. The arguments can take two forms. First, with
  an initial parent component which will act as the parent of the dialog.

      (choose-file dialog-parent ... options ...)

  If the first arg is omitted, the desktop is used as the parent of the dialog:

      (choose-file ... options ...)

  Options can be one of:

    :type The dialog type: :open, :save, or a custom string placed on the Ok button.
          Defaults to :open.
    :dir  The initial working directory. If omitted, the previous directory chosen
          is remembered and used.
    :multi?  If true, multi-selection is enabled and a seq of files is returned.
    :selection-mode The file selection mode: :files-only, :dirs-only and :files-and-dirs.
                    Defaults to :files-only
    :filters A seq of either:

               a seq that contains a filter name and a seq of
               extensions as strings for that filter;

               a seq that contains a filter name and a function
               to be used as accept function (see file-filter);

               a FileFilter (see file-filter).

             The filters appear in the dialog's filter selection in the same
             order as in the seq.
    :all-files? If true, a filter matching all file extensions and files
                without an extension will appear in the filter selection
                of the dialog additionally to the filters specified
                through :filters. The filter usually appears last in the
                selection. If this is not desired set this option to
                false and include an equivalent filter manually at the
                desired position as shown in the examples below. Defaults
                to true.

    :remember-directory? Flag specifying whether to remember the directory for future
                         file-input invocations in case of successful exit. Default: true.
    :success-fn  Function which will be called with the JFileChooser and the File which
                 has been selected by the user. Its result will be returned.
                 Default: return selected File. In the case of MULTI-SELECT? being true,
                 a seq of File instances will be passed instead of a single File.
    :cancel-fn   Function which will be called with the JFileChooser on user abort of the dialog.
                 Its result will be returned. Default: returns nil.

  Examples:

    ; ask & return single file
    (choose-file)

    ; ask & return including a filter for image files and an \"all files\"
    ; filter appearing at the beginning
    (choose-file :all-files? false
                 :filters [(file-filter \"All files\" (constantly true))
                           [\"Images\" [\"png\" \"jpeg\"]]
                           [\"Folders\" #(.isDirectory %)]])

    ; ask & return absolute file path as string
    (choose-file :success-fn (fn [fc file] (.getAbsolutePath file)))

  Returns result of SUCCESS-FN (default: either java.io.File or seq of java.io.File iff multi? set to true)
  in case of the user selecting a file, or result of CANCEL-FN otherwise.
  
  See http://download.oracle.com/javase/6/docs/api/javax/swing/JFileChooser.html
  "
  [& args]
  (let [[parent & {:keys [type remember-directory? success-fn cancel-fn]
                   :or {type :open
                        remember-directory? true
                        success-fn (fn [fc files] files)
                        cancel-fn (fn [fc])}
                   :as opts}] (if (keyword? (first args)) (cons nil args) args)
        parent  (if (keyword? parent) nil parent)
        ^JFileChooser chooser (configure-file-chooser
                                (JFileChooser.)
                                (dissoc
                                  opts
                                  :type
                                  :remember-directory?
                                  :success-fn
                                  :cancel-fn))]
    (when-let [[filter _] (seq (.getChoosableFileFilters chooser))]
      (.setFileFilter chooser filter))
    (let [result (show-file-chooser chooser parent type)
          multi? (.isMultiSelectionEnabled chooser)]
      (cond
        (= result JFileChooser/APPROVE_OPTION)
          (do
            (when remember-directory?
              (remember-chooser-dir chooser))
            (success-fn
              chooser
              (if multi?
                (.getSelectedFiles chooser)
                (.getSelectedFile chooser))))
        :else (cancel-fn chooser)))))

(defn- dialog-owner [parent]
  (when parent
    (let [w (if (instance? java.awt.Window parent)
              parent
              (javax.swing.SwingUtilities/getWindowAncestor
                ((requiring-resolve 'seesaw.core/to-widget) parent)))]
      (when (or (instance? java.awt.Frame w) (instance? java.awt.Dialog w)) w))))

(defn choose-native-file
  "Choose a file with the platform's native file dialog (java.awt.FileDialog),
  e.g. the real Finder dialog on macOS. Like (choose-file), the first argument
  may be a parent widget. Options:

    :type        :open (default) or :save
    :title       Dialog title
    :dir         Initial directory (string or File)
    :file        Initial file name, e.g. a suggested name to save as
    :multi?      Allow selecting several files and return a seq
    :extensions  Only offer files with these extensions, e.g. [\"md\" \"txt\"].
                 Not supported by the Windows dialog.
    :dirs?       Choose folders instead of files (macOS only)

  Returns the java.io.File (or seq of them with :multi?), or nil if
  cancelled.

  See https://docs.oracle.com/javase/8/docs/api/java/awt/FileDialog.html
  "
  [& args]
  (let [[parent & {:keys [type title dir file multi? extensions dirs?] :or {type :open}}]
        (if (keyword? (first args)) (cons nil args) args)
        owner (dialog-owner parent)
        mode  (case type :open java.awt.FileDialog/LOAD :save java.awt.FileDialog/SAVE)
        title (str (or title ""))
        ^java.awt.FileDialog d (cond
                                 (instance? java.awt.Dialog owner) (java.awt.FileDialog. ^java.awt.Dialog owner title (int mode))
                                 :else (java.awt.FileDialog. ^java.awt.Frame owner title (int mode)))
        exts (set (map #(.toLowerCase (str %)) extensions))
        dirs-prop "apple.awt.fileDialogForDirectories"
        old-dirs (System/getProperty dirs-prop)]
    (when dir (.setDirectory d (str (clojure.java.io/file dir))))
    (when file (.setFile d (str file)))
    (.setMultipleMode d (boolean multi?))
    (when (seq exts)
      (.setFilenameFilter d (reify java.io.FilenameFilter
                              (accept [_ _ n]
                                (let [i (.lastIndexOf ^String n ".")]
                                  (and (pos? i) (contains? exts (.toLowerCase (subs n (inc i))))))))))
    (when dirs? (System/setProperty dirs-prop "true"))
    (try
      (.setVisible d true)
      (let [files (seq (.getFiles d))]
        (if multi? files (first files)))
      (finally
        (when dirs?
          (if old-dirs (System/setProperty dirs-prop old-dirs) (System/clearProperty dirs-prop)))
        (.dispose d)))))

(defn choose-color
  "Choose a color with a color chooser dialog. The optional first argument is the
  parent component for the dialog. The rest of the args is a list of key/value 
  pairs:
  
          :color The initial selected color (see seesaw.color/to-color)
          :title The dialog's title
  
  Returns the selected color or nil if canceled.
  
  See:
    http://download.oracle.com/javase/6/docs/api/javax/swing/JColorChooser.html
  "
  [& args]
  (let [[parent & {:keys [color title]
                   :or { title "Choose a color"}
                   :as opts}] (if (keyword? (first args)) (cons nil args) args)
        parent (if (keyword? parent) nil parent)]
    (javax.swing.JColorChooser/showDialog parent title (to-color color))))
