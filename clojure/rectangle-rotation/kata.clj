(ns kata)

(defn scale-red [x]
  (-> x
      (/ (Math/sqrt 2))
      (/ 2)
      (int)
      (* 2)
      (+ 1)))

(defn scale-blue [x]
  (-> x
      (/ (Math/sqrt 2))
      (+ 1)
      (/ 2)
      (int)
      (* 2)))

(defn rectangle-rotation [a b]
  (let [area-red  (* (scale-red a) (scale-red b))
       area-blue (* (scale-blue a) (scale-blue b))]
   (+ area-red area-blue)))
