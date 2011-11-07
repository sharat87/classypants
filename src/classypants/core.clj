(ns classypants.core
  (:use seesaw.core
        [seesaw.mig :only (mig-panel)]
        classypants.clipboard
        [classypants.matcher :only (parse-search-str paint-match-status)])
  (:require [clojure.contrib.string :as string])
  (:import [java.io File]
           [java.awt Color]
           [javax.swing JPopupMenu JMenuItem ListSelectionModel]
           [javax.swing.event PopupMenuListener]))

(native!)

(def status-label (label :text  "If you are wearing pants, they better be classy!"
                         :border 5))

(defn set-status
  [level- & strs]
  (let [level (if (keyword? level-)
                level-
                :info)
        strs (if (keyword? level-)
               strs
               (conj strs level))]
    (config! status-label :text (apply str strs))
    (config!* status-label {:foreground "#99F"
                            :background "#009"})))

(defrecord CpEntry [path exists? matches?])

(defn new-cpentry
  [path]
  (CpEntry. path
            (.exists (File. path))
            true))

(defn get-cp-entries
  [classpath]
  (->> (.split classpath ":")
    (remove empty?)
    (map new-cpentry)))

(defn set-cp-clipboard
  [entries]
  (set-clipboard-text (string/join ":" (map :path entries))))

(def current-entries
  (atom (get-cp-entries (System/getProperty "java.class.path"))))

(defn apply-filtered-data!
  [fr]
  (let [search-str (config (select fr [:#filter-input]) :text)
        match-exp (try
                    (parse-search-str search-str)
                    (catch Exception e
                      (set-status :error (.getLocalizedMessage e))))]
    (config! (select fr [:#cp-listbox])
             :model (paint-match-status
                      match-exp
                      @current-entries))))

(defn reset-cplist
  [fr]
  (config! (select fr [:#cp-listbox])
           :model @current-entries))

(defn title-case
  [^String s]
  (str (.toUpperCase (.substring s 0 1))
       (.substring s 1)))

(defmacro defmenu
  [fn-name hotkey & body]
  (let [title (-> (name fn-name)
                (.replace "-action" "")
                (.replace "-" " ")
                title-case)
        args1 ['fr]
        args2 ['e]]
    `(defn ~fn-name
       ~args1
       (action :name ~title
               :key ~hotkey
               :handler (fn ~args2 ~@body)))))

(defmenu copy-action "menu shift C"
  (set-cp-clipboard (paint-match-status
                      (parse-search-str (config (select fr [:#filter-input]) :text))
                      (enumeration-seq
                        (.elements (config (select fr [:#cp-listbox]) :model))))))

(defmenu paste-action "menu shift V"
  (swap! current-entries
         (fn [_] (get-cp-entries (get-clipboard-text))))
  (apply-filtered-data! fr))

(defmenu delete-selection-action "menu shift D"
  (let [cp-listbox (select fr [:#cp-listbox])
        data-model (.getModel cp-listbox)
        selection-model (.getSelectionModel cp-listbox)]
    (swap! current-entries
           (fn [prev-entries]
             (remove nil? (map-indexed
                             (fn [i entry]
                               (if (.isSelectedIndex selection-model i)
                                 nil
                                 entry))
                             prev-entries))))
    (reset-cplist fr)))

(defmenu test-action "menu shift T"
  (set-status :info "hoohaw"))

(defn start-app
  []
  (let [main-frame (frame :title "Classypants"
                          :size [800 :by 600]
                          :on-close :exit)
        error-selected-color (Color. 223 89 255)
        error-unselected-color (Color. 255 120 120)
        cp-listbox (listbox
                     :id :cp-listbox
                     :model @current-entries
                     :renderer (fn [this state]
                                 (let [entry (:value state)]
                                   (.setText this (:path entry))
                                   (.setForeground this (if (:matches? entry)
                                                          Color/black
                                                          (Color. 180 180 180)))
                                   (if-not (:exists? entry)
                                     (.setBackground this (if (:selected? state)
                                                            error-selected-color
                                                            error-unselected-color))))))]
    (.setSelectionMode (.getSelectionModel cp-listbox) ListSelectionModel/MULTIPLE_INTERVAL_SELECTION)
    (config! main-frame
             :content (mig-panel
                        :items [[(menubar
                                   :items [(menu :text "\\m/"
                                                 :items (map
                                                          #(% main-frame)
                                                          [copy-action
                                                           paste-action
                                                           delete-selection-action
                                                           test-action]))])
                                 "split"]
                                [(letfn [(handler [e]
                                           (apply-filtered-data! main-frame))]
                                   (text :id :filter-input
                                     :listen [:changed-update handler
                                              :insert-update handler
                                              :remove-update handler]))
                                 "grow, wrap"]
                                [(scrollable cp-listbox) "push, grow, wrap"]
                                [status-label "growx"]]))
    (.setLocationRelativeTo main-frame nil)
    (show! main-frame)))
