(ns classypants.matcher)

(defn str-in?
  "Check if a needle is a substring of haystack"
  [needle haystack]
  (-> haystack
    (.indexOf needle)
    (not= -1)))

(defn all
  "Return truthy if all items in given seq are truthy"
  [items]
  (reduce #(and %1 %2) items))

(defn split-on
  "Split coll at the first occurance of mark"
  [mark coll]
  (let [[pre post] (split-with #(not= % mark) coll)]
    [pre (rest post)]))

(defn in?
  "Check if elem is present in the coll"
  [elem coll]
  (boolean (first (filter #(= % elem) coll))))

(defn get-spec
  [spec-str]
  (read-string (str "[" spec-str "]")))

(def ^:dynamic *match-haystack* "")

(defn matches?
  "Checks if a resource (jar/dir) is considered as matched corresponding to
  the given filter-text"
  [s]
  (str-in? s *match-haystack*))

(defn digest-search-spec
  [spec]
  (cond
    (nil? spec) nil
    (string? spec) (list `matches? spec)
    (or (keyword? spec) (symbol? spec)) (recur (name spec))
    (= 1 (count spec)) (recur (first spec))
    (in? :and spec) (conj (map digest-search-spec (split-on :and spec)) 'and)
    (in? :or spec) (conj (map digest-search-spec (split-on :or spec)) 'or)
    :else (conj (map digest-search-spec spec) 'or)))

(def parse-search-str (comp digest-search-spec get-spec))

(defn paint-match-status
  "For each given entry, return an entry *painted* in the :matches? key according
  to whether it matches the given match expression or not."
  [match-exp entries]
  (map
    (fn [entry]
      (binding [*match-haystack* (:path entry)]
        (assoc entry :matches? ((comp not false?) (eval match-exp)))))
    entries))
