(ns classypants.matcher)

(defn str-in?
  "Check if a needle is a substring of haystack"
  [needle haystack]
  (-> haystack
    (.indexOf needle)
    (not= -1)))

(defn matches?
  "Checks if a resource (jar/dir) is considered as matched corresponding to
  the given filter-text"
  [resource filter-text]
  (let [filter-parts (.split filter-text "\\\\s+")]
    (str-in? filter-text (:path resource))))

(defn filter-paths
  [filter-text entries]
  (map
    (fn [entry]
      (assoc entry :matches? (matches? entry filter-text)))
    entries))

