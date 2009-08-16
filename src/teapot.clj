; Demo scene for cray raytracer - Utah teapot

(use 'cray.primitives 'cray.utils 'cray.shaders)

(load-file "teapotsettings.clj")

(defn vertices-ccw? 
	"Returns true if vertices are counter-clockwise, assuming viewer is looking down z-axis."
	[v0 v1 v2] 
  (< (- (* (- (vx v1) (vx v0)) (- (vy v2) (vy v0)))
				(* (- (vx v2) (vx v0)) (- (vy v1) (vy v0)))) 
     0.0))
		
(defn make-triangle-data [vertices v-index]
	(let [index0 (nth v-index 0)
				index1 (nth v-index 1)
				index2 (nth v-index 2)
				v0 (nth vertices index0)
			  v1 (nth vertices index1)
			  v2 (nth vertices index2)
			  is-ccw (vertices-ccw? v0 v1 v2)
			  ccw-v0 (if is-ccw v0 v1)
			  ccw-v1 (if is-ccw v1 v0)
			  edge1 (vec-sub ccw-v1 ccw-v0) 
				edge2 (vec-sub v2 ccw-v0)
				normal (normalize (cross edge1 edge2))]
		{:v0 ccw-v0 :v1 ccw-v1 :v2 v2 :edge1 edge1 :edge2 edge2 :normal normal
		 :index0 (if is-ccw index0 index1) :index1 (if is-ccw index1 index0) :index2 index2}))
	
(defn make-normal-data [tri-data]
	(apply concat (for [{:keys [index0 index1 index2 normal]} tri-data]
		[[index0 normal] [index1 normal] [index2 normal]])))

(defn calc-average-normal [normal-data idx]
	(normalize (reduce #(if (= (first %2) idx) (vec-add %1 (second %2)) %1) zero-vec normal-data))) 
	
(defn make-averaged-normals [normal-data index-set]
	(apply merge (map #(hash-map % (calc-average-normal normal-data %)) index-set)))

(defn set-indices 
	"Takes a sequence of sequences of 3 indices, and returns a set of all individual indices."
	[indices]
	(set (apply concat indices)))

(defn make-triangles [vertices indices]
; Indices at this point are lists of 3 indices defining a triangle.
	(let [tri-data (map #(make-triangle-data vertices %) indices)
				normal-data (make-normal-data tri-data)
				averaged-normals (make-averaged-normals normal-data (set-indices indices))
				tri-shaders [(flat-shader (make-colour 0.5 0.55 0.55))(diffuse-shader-no-shadows 0.7)(specular-shader 0.5)]]
		(map #(array-map :objtype :triangle :v0 (% :v0) :v1 (% :v1) :v2 (% :v2) :edge1 (% :edge1) :edge2 (% :edge2) 
						:shaders tri-shaders
						:normal0 (averaged-normals (% :index0))
						:normal1 (averaged-normals (% :index1))
						:normal2 (averaged-normals (% :index2))
						:normal (% :normal)) tri-data)))
					
(defn load-teapot [filename] ; Hard-coded to format of teapot.off. Needs rewrite.
  (let [lines (seq (.split (slurp filename) "\n"))
				fileinfo (seq (.split (nth lines 1) " "))
				vertex-count (Integer/parseInt (nth fileinfo 0))
				face-count (Integer/parseInt (nth fileinfo 1))
				vertices (map (fn parse [vertex] 
				(make-vect (map #(+ 0.02 (Double/parseDouble (nth vertex %))) (range 3))))			
		      (map #(seq (.split % " ")) (take vertex-count (drop 2 lines))))
				indices (map (fn parse-indices [index] 
		       (map #(Integer/parseInt (nth index %)) (range 3)))
		     		(map #(drop 2 (seq (.split % " "))) (drop (+ 2 vertex-count) lines)))]
	(make-triangles vertices indices)))

(defn get-scene []
	(concat 
		(load-teapot "teapot.off")
  	[(array-map :objtype :light :position (make-vect 1 5 -6) :colour (make-colour 1.0 1.0 1.0))
   	 (array-map :objtype :plane :distance 10.0 :normal (make-vect 0.0 0.0 -1.0) :shaders [(flat-shader (make-colour 0.8 0.77 0.8))])]))
