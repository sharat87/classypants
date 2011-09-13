(ns towpath.core
  (:use seesaw.core
        [seesaw.mig :only (mig-panel)]
        towpath.clipboard)
  (:import [java.awt Color]
           [javax.swing ListSelectionModel]
           [java.io File]))

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
  (set-clipboard-text (reduce (fn [a b] (str a ":" b)) entries)))

(def current-entries
  (atom (get-cp-entries (System/getProperty "java.class.path"))))

(defn filter-paths
  [filter-text entries]
  (map
    (fn [entry]
      (assoc entry :matches? (not= (.indexOf (:path entry) filter-text) -1)))
    entries))

(defn apply-filtered-data!
  [fr]
  (config! (select fr [:#cp-listbox])
           :model (filter-paths
                    (config (select fr [:#filter-input]) :text)
                    @current-entries)))

(defn reset-cplist
  [fr]
  (config! (select fr [:#cp-listbox])
           :model @current-entries))

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
  (set-cp-clipboard (filter-paths
                      (config (select fr [:#filter-input]) :text)
                      (enumeration-seq
                        (.elements (config (select fr [:#cp-listbox]) :model))))))

(defn paste-handler
  [fr e]
  (swap! current-entries
         (fn [_] (get-cp-entries (get-clipboard-text))))
  (apply-filtered-data! fr))

(defn -main
  []
  (let [main-frame (frame :title "Towpath"
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
                                [(action :name "< Clear"
                                         :handler (fn [e]
                                                    (config!
                                                      (select main-frame [:#filter-input])
                                                      :text "")))
                                 "wrap"]
                                [(scrollable cp-listbox) "span, grow, push"]])
             :visible? true)))
