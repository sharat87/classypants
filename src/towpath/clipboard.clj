(ns towpath.clipboard
  (:import [java.awt Toolkit]
           [java.awt.datatransfer Clipboard DataFlavor StringSelection]))

(defn get-clipboard-text
  []
  (let [t (-> (Toolkit/getDefaultToolkit)
            .getSystemClipboard
            (.getContents nil))]
    (if (and t (.isDataFlavorSupported t DataFlavor/stringFlavor))
      (str (.getTransferData t DataFlavor/stringFlavor))
      nil)))

(defn set-clipboard-text
  [text]
  (-> (Toolkit/getDefaultToolkit)
    .getSystemClipboard
    (.setContents (StringSelection. text) nil)))
