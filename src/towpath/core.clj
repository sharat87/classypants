(ns towpath.core
  (:use seesaw.core
        [seesaw.mig :only (mig-panel)]
        towpath.clipboard)
  (:import [java.awt Color]
           [javax.swing ListSelectionModel]
           [java.io File]))

(defn get-cp-entries
  [classpath]
  (remove empty? (.split classpath ":")))

(defn set-cp-clipboard
  [entries]
  (set-clipboard-text (reduce (fn [a b] (str a ":" b))
                              (enumeration-seq (.elements entries)))))

(defn -main
  []
  (let [main-frame (frame :title "Towpath"
                          :size [800 :by 600]
                          :on-close :exit)
        error-unselected-color (Color. 223 89 255)
        error-selected-color (Color. 255 120 120)
        cp-listbox (listbox
                     :model (get-cp-entries (System/getProperty "java.class.path"))
                     :renderer (fn [this state]
                                 (if-not (.exists (File. (:value state)))
                                   (.setBackground this (if (:selected? state)
                                                          error-selected-color
                                                          error-unselected-color)))))]
    (.setSelectionMode (.getSelectionModel cp-listbox) ListSelectionModel/MULTIPLE_INTERVAL_SELECTION)
    (config! main-frame
             :content (mig-panel
                        :constraints ["" "" ""]
                        :items [[(action :name "Copy"
                                         :handler (fn [e]
                                                    (set-cp-clipboard (config cp-listbox :model))))
                                 "split"]
                                [(action :name "Paste"
                                         :handler (fn [e]
                                                    (config! cp-listbox :model (get-cp-entries (get-clipboard-text)))))]
                                [(action :name "Delete Selected"
                                         :handler (fn [e]
                                                    (let [data-model (.getModel cp-listbox)
                                                          selection-model (.getSelectionModel cp-listbox)]
                                                      (loop [i (- (.size data-model) 1)]
                                                        (if (.isSelectedIndex selection-model i)
                                                          (.removeElementAt data-model i))
                                                        (if-not (zero? i)
                                                          (recur (- i 1)))))))
                                 "wrap"]
                                ["Filter:" "split"]
                                [(text :id :filter-input) "grow"]
                                [(action :name "< Clear"
                                         :handler (fn [e]
                                                    (config!
                                                      (select main-frame [:#filter-input])
                                                      :text "")))
                                 "wrap"]
                                [(scrollable cp-listbox) "span, grow, push"]]))
    (show! main-frame)))
