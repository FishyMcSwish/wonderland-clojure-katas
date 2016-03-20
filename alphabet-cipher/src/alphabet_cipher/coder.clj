(ns alphabet-cipher.coder)

(def alphabet (apply str (map char (range 97 123))))

(defn rotated-alphabet [x] 
	(apply str 
		(take 26 
			(drop (.indexOf alphabet x) (cycle alphabet)))))

(defn reverse-rotated-alphabet [x] 
	(apply str 
		(take 26 
			(drop (- 26 (.indexOf alphabet x)) (cycle alphabet)))))

(defn line-up-cipher [keyword message]
	(take (.length message) (cycle keyword)))


(defn get-encoded1 [x y]
	(nth (rotated-alphabet (str x)) (.indexOf alphabet (str y) )))

(defn get-decoded1 [x y]
	(nth (reverse-rotated-alphabet (str x)) (.indexOf alphabet (str y) )))

(defn encode [keyword message]
	(apply str (map get-encoded1 (line-up-cipher keyword message) message)))


(defn decode [keyword message]
	(apply str (map get-decoded1 (line-up-cipher keyword message) message)))

(defn decipher [cipher message]
  "decypherme")

