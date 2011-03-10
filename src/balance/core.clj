(ns balance.core
  )

(def open->close (into {} (map vec (partition 2 "{}()[]"))))
(def close->open (into {} (for [[k v] open->close] [v k])))

;; TODO doesn't understand strings and character literals
(defn new-state [stack c]
  (if-let [closing (open->close c)]
   (conj stack closing) 
   (if-let [opening (close->open c)]
     (let [[first & rest] stack]
       (when-not (= c first)
         (throw (Exception. (str "Mismatched delimiter: saw " c
                                 ", expected " first))))
       rest)
     stack)))

(defn balance [expr]
  (apply str
         ((fn step
            [stack remain]
            (lazy-seq
             (let [[c & more] remain]
               (if-not c
                 stack ; unload the remaining close-braces
                 (cons c (step (stack-state stack c) more))))))
          () expr)))
