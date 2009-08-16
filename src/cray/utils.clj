; Math library

(ns cray.utils)

(def epsilon 0.00000001) ; Limit of precision for floating point comparisons
(def pi (. Math PI))

(defmacro sqrt [n] `(. Math sqrt ~n))
(defn abs [#^Double n] (. Math abs n))
(defmacro acos [n] `(. Math acos ~n))	
(defmacro sin [n] `(. Math sin ~n))
(defmacro pow [n m] `(. Math pow ~n ~m))
(defmacro exp [n] `(. Math exp ~n))
(defn smallest-positive [a b] (if (or (< a 0) (< b 0)) (max a b) (min a b)))
(defn pow2 [n] (* n n))
(defn fpeq [a b] (and (< (- a b) epsilon) (< (- b a) epsilon)))
(defn between? [n lower-bound upper-bound] (and (> n lower-bound) (< n upper-bound)))
(defn min-max 
	([[n1 n2]] (min-max n1 n2))
	([n1 n2] (if (< n1 n2) [n1 n2] [n2 n1])))

(defn noise 
  "Generates a random number - TODO replace with a random number source better for 3D (e.g. Perlin noise)"
  ([] (noise -1.0 1.0))
  ([scale] (noise (- 0.0 (* 0.5 scale)) (* 0.5 scale)))
  ([lower-bound upper-bound] 
     (let [noise-range (- lower-bound upper-bound)]
       (* (rand noise-range) (* 0.5 noise-range)))))

(defn combinations 
  "Returns a sequence of all combinations of values in the inputs"
  ([x] (combinations x x))
  ([x y] (for [x-val x y-val y] [x-val y-val])))

; Vector/Matrix library
(defmacro make-vect
  ([x y z] `(let [v# (double-array 3 0)]
     (aset v# 0 (double ~x))
     (aset v# 1 (double ~y))
     (aset v# 2 (double ~z))
     v#))
   ([xyz] `(into-array Double/TYPE ~xyz)))

;(defmacro vx [vect] `(aget ~vect 0))
;(defmacro vy [vect] `(aget ~vect 1))
;(defmacro vz [vect] `(aget ~vect 2))
(defn vx [v] (aget v 0))
(defn vy [v] (aget v 1))
(defn vz [v] (aget v 2))

(defmacro make-colour ([r g b] `(make-vect ~r ~g ~b)))
(defmacro vr [vect] `(aget ~vect 0))
(defmacro vg [vect] `(aget ~vect 1))
(defmacro vb [vect] `(aget ~vect 2))

(def zero-vec (make-vect 0.0 0.0 0.0))

(defn vec-sub [#^doubles v1 #^doubles v2] 
  (make-vect (- (vx v1) (vx v2)) 
	     (- (vy v1) (vy v2)) 
	     (- (vz v1) (vz v2))))

(defn vec-add 
  ([vecs] (reduce vec-add vecs))
  ([#^doubles v1 #^doubles v2] 
  	(make-vect (+ (vx v1) (vx v2)) 
							 (+ (vy v1) (vy v2))
					     (+ (vz v1) (vz v2))))
  ([#^doubles v1 #^doubles v2 & more] 
  	(apply vec-add (vec-add v1 v2) more)))

(defn vec-mul [#^doubles v1 #^doubles v2] 
  (make-vect (* (vx v1) (vx v2)) 
	     (* (vy v1) (vy v2)) 
	     (* (vz v1) (vz v2))))

(defn vec-scale [n #^doubles v] 
  (make-vect (* n (vx v)) 
	     (* n (vy v)) 
	     (* n (vz v))))

(defn vec-length 
  "Calculates the length of a vector"
  [#^doubles v] 
  (sqrt (+ (+ (* (vx v) (vx v)) 
	      (* (vy v) (vy v))) 
	   (* (vz v) (vz v)))))

(defn dot 
  "Calculates the dot product (cosine of the angle between two vectors)"
  [#^doubles v1 #^doubles v2] 
  (+ (+ (* (vx v1) (vx v2)) 
				(* (vy v1) (vy v2))) 
     (* (vz v1) (vz v2)))) ; (Two-operand '+' is faster, replacing dot with a macro costs performance)

(defn cross 
  "Calculates the cross product of two vectors (a vector perpendicular to the plane containing v1 and v2"
  [#^doubles v1 #^doubles v2] 
  (make-vect (- (* (vy v1) (vz v2)) (* (vz v1) (vy v2))) 
	     (- (* (vz v1) (vx v2)) (* (vx v1) (vz v2))) 
	     (- (* (vx v1) (vy v2)) (* (vy v1) (vx v2)))))

(defn normalize 
  "Returns the unit vector (v is scaled so its' length is 1"
  [#^doubles v] (vec-scale (/ 1.0 (vec-length v)) v))

(defn str-vec 
  "Converts vectors to strings, handy for debugging"
  [v]  
  (str (seq v)))

(defn vec-replace-coord
	"Convert a split co-ordinate into the co-ordinates to split the box on"
	[axis value v]
	(make-vect (if (= axis vx) value (vx v))
						 (if (= axis vy) value (vy v))
						 (if (= axis vz) value (vz v))))

(defn axis-next
	"Used for interating through x,y,z axes."
	[axis] (if (= axis vx) 
						vy 
						(if (= axis vy) vz vx)))

(defn make-random-vect 
  ([] (normalize (make-random-vect 1.0)))
  ([scale] (make-vect (noise scale) (noise scale) (noise scale))))

(defn make-matrix 
  "Creates a typical 4x4 matrix for 3D operations"
  [v1 v2 v3] 
  (concat v1 [0.0] v2 [0.0] v3 [0.0 0.0 0.0 0.0 1.0])) 

(defn mat-transform 
  "Transform vector v using matrix m"
  [m v] 
  (make-vect (+ (+ (* (vx v) (nth m 0)) (* (vy v) (nth m 1)) (* (vz v) (nth m 2))) (nth m 3))
	     (+ (* (vx v) (nth m 4)) (* (vy v) (nth m 5)) (* (vz v) (nth m 6)) (nth m 7))
	     (+ (* (vx v) (nth m 8)) (* (vy v) (nth m 9)) (* (vz v) (nth m 10)) (nth m 11))))

; Helper functions
(defmacro col-if [condition success] 
  `(if ~condition ~success zero-vec))

(defn make-grid [size nElements] 
  (combinations (range 0 size (/ size nElements))))

(defn get-image [filename] 
  (. javax.imageio.ImageIO read (java.io.File. filename)))

(defn pack-colour 
  "Convert colour to 32-bit integer in ARGB format"
  [scale-factor c] 
  (let [col (vec-scale (* 255 scale-factor) c)]
    (int (+ (+ 
	     (+ (bit-shift-left 255 24) ; Alpha
		(bit-shift-left (int (vr col)) 16))
	     (bit-shift-left (int (vg col)) 8))
	    (int (vb col))))))

(defn unpack-byte [packed-int byte-index]
  (bit-and 255 (bit-shift-right packed-int (* byte-index 8))))

(def inv-255 (/ 1.0 255.0))

(defn unpack-colour [rgba] 
  (vec-scale inv-255 (make-colour (unpack-byte rgba 2)
				  (unpack-byte rgba 1)
				  (unpack-byte rgba 0))))
  
(defn unpack-normal 
  "Converts a normal that has been packed into RGBA space into a vector"
  [rgba] 
  (vec-add (vec-scale 2 (unpack-colour rgba)) 
	   (make-vect -1.0 -1.0 -1.0)))
  
  
(defn default [a b] (if a a b))


	     