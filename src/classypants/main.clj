(ns classypants.main
  (:use [classypants.core :only (start-app)])
  (:gen-class))

(defn -main
  []
  (start-app))
