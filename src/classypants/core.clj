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
        err-display (select fr [:#err-display])
        match-exp (try
                    (config! err-display :visible? false)
                    (parse-search-str search-str)
                    (catch Exception e
                      (config! err-display
                               :text (.getLocalizedMessage e)
                               :visible? true)))]
    (config! (select fr [:#cp-listbox])
             :model (paint-match-status
                      match-exp
                      @current-entries))))

(defn reset-cplist
  [fr]
  (config! (select fr [:#cp-listbox])
           :model @current-entries))

(defn show-action-menu
  [e]
  (let [component (.getSource e)
        menu (popup :items [(action :name "Copy"
                                    :key "menu C"
                                    :handler (fn [e]
                                               (prn "Copy!")))
                            "White"])]
    (.show menu component 0 0) ;To get the dimensions calculated
    (.show menu component (- (.getWidth component) (.getWidth menu)) (.getHeight component))
    menu))

(defn delete-handler
  [fr e]
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

(defn copy-handler
  [fr e]
  (prn "Copy!")
  (set-cp-clipboard (paint-match-status
                      (parse-search-str (config (select fr [:#filter-input]) :text))
                      (enumeration-seq
                        (.elements (config (select fr [:#cp-listbox]) :model))))))

(defn paste-handler
  [fr e]
  (swap! current-entries
         (fn [_] (get-cp-entries (get-clipboard-text))))
  (apply-filtered-data! fr))

(defn -main
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
                        :items [[(action :name "Copy"
                                         :handler #(copy-handler main-frame %))
                                 "split"]
                                [(action :name "Paste"
                                         :handler #(paste-handler main-frame %))]
                                [(action :name "Delete Selected"
                                         :handler #(delete-handler main-frame %))
                                 "wrap"]
                                ["Filter:" "split"]
                                [(letfn [(handler [e]
                                           (apply-filtered-data! main-frame))]
                                   (text :id :filter-input
                                     :listen [:changed-update handler
                                              :insert-update handler
                                              :remove-update handler]))
                                 "grow"]
                                [(action :name "\\m/"
                                         :handler show-action-menu)
                                 "wrap"]
                                [(label :id :err-display
                                        :text "Heyo"
                                        :border 5
                                        :foreground "#F99"
                                        :background "#900"
                                        :visible? false)
                                 "growx,wrap"]
                                [(scrollable cp-listbox) "push, grow"]]))
  (show! main-frame)))
