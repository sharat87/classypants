(ns classypants.matcher
  (:import java.util.zip.ZipFile))

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

(defn endswith?
  [text end]
  (let [text-length (.length text)
        end-length (.length end)]
    (and (> text-length end-length)
         (= end (.substring text (- text-length end-length))))))

(defn files-in-archive
  [filename]
  (map #(.getName %)
       (filter #(not (.isDirectory %))
               (enumeration-seq (.entries (ZipFile. filename))))))

(defn get-spec
  [spec-str]
  (read-string (str "[" spec-str "]")))

(def ^:dynamic *match-resource* "")

(defn matches?
  "Checks if a resource (jar/dir) is considered as matched corresponding to
  the given filter-text"
  [s]
  (str-in? s *match-resource*))

(defn matches-file?
  "Checks if *match-resource* contains the given filename"
  [file]
  (if (endswith? *match-resource* ".jar")
    (let [entries (files-in-archive *match-resource*)]
      (in? file entries))
    false))

(defn digest-search-spec
  [spec]
  (cond
    (or (nil? spec)
        (and (sequential? spec) (empty? spec))) nil
    (string? spec) (list `matches? spec)
    (or (keyword? spec) (symbol? spec)) (recur (name spec))
    (= 1 (count spec)) (recur (first spec))

    ; (:has "filename") <- split stream with :has, pick one from post, construct matcher call
    (in? :has spec) (let [[pre post] (split-on :has spec)
                          digest (filter (comp not nil?)
                                         ['or
                                          (digest-search-spec pre)
                                          (list `matches-file? (name (first post)))
                                          (digest-search-spec (rest post))])]
                      (if (> (count digest) 2)
                        digest
                        (last digest)))

    ; Split with :and/:or and join the parts with an and/or call on recur'ed pre and post
    (in? :and spec) (conj (map digest-search-spec (split-on :and spec)) 'and)
    (in? :or spec) (conj (map digest-search-spec (split-on :or spec)) 'or)

    ; Simply apply 'or on all items
    :else (conj (map digest-search-spec spec) 'or)))

(def parse-search-str (comp digest-search-spec get-spec))

(defn paint-match-status
  "For each given entry, return an entry *painted* in the :matches? key according
  to whether it matches the given match expression or not."
  [match-exp entries]
  (map
    (fn [entry]
      (binding [*match-resource* (:path entry)]
        (assoc entry :matches? ((comp not false?) (eval match-exp)))))
    entries))
