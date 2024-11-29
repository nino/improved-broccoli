(ns clj.core
  (:gen-class))
(use 'clj-ical.format)

(write-object
     [:vcalendar
       [:vevent
         [:summary "Bastille Day Partyds lakdsjf ;alsdkjf a;sldkfj asd;lkfjas;dlkjf adslk fjasd;fl kjasdlkf jasdk;flj"]]])

(defn- unroll-lines [input]
  "Replace CRLF followed by space or tab with nothing."
  (clojure.string/replace (clojure.string/replace input "\r\n " "")
                          "\r\n\t" ""))

(defn read-ical [input]
  (-> input (unroll-lines) ()))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(-main)

(def *word* "hello")

(count "hello😅")

(defn count-graphemes
  "Count grapheme clusters in a string using Java's BreakIterator"
  [s]
  (let [iterator (doto (java.text.BreakIterator/getCharacterInstance)
                  (.setText s))]
    (loop [count 0
           pos (.first iterator)]
      (if-let [next (.next iterator)]
        (if (= next -1)
          count
          (recur (inc count) next))
        count))))

;; Example usage:
(count-graphemes "hello") ;; => 5
(count-graphemes "éa🇩🇪") ;; => 1 (combined accent counts as one)
