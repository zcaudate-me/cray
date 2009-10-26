; cray-lib.clj
; Licensed under http://creativecommons.org/licenses/BSD/

(ns cray.tracer 
	(:use cray.utils 
						cray.primitives 
						cray.kdtree
						cray.shaders))

; Ray-tracing functions
(defn closest-object 
  "Returns hit-info for the closest object that 'ray' intersects with, if any"
  [ray objects] 
  (let [hit-info (reduce #(if (= (smallest-positive (%1 :distance) (%2 :distance)) (%1 :distance)) %1 %2) 
								  (intersect-all ray objects))]
		(if (<= (hit-info :distance) 0.0)
			hit-info
			(merge hit-info {:hit-point (calc-point-on-ray ray (hit-info :distance))}))))	

(defn closest-object-tree 
	"Traverses the tree, searching for the closest object to intersect with ray"
	[ray {:keys [leaf primitives box left-node right-node split-pos split-axis]}]
  (if leaf
    (if (nil? (first primitives))
    	missed ; box is empty
    	(closest-object ray primitives))
    (let [intersect-result (aabb-intersect-ray box ray -10000.0 10000.0)]
    	(if (nil? intersect-result)
    		missed
 				(let [[tnear tfar] intersect-result
 							entry-pb (if (> tnear epsilon) 
 										(vec-add (ray :origin) (vec-scale tnear (ray :direction))) ; a ray with external origin
 										(ray :origin)) ; a ray with internal origin			
 							exit-pb (vec-add (ray :origin) (vec-scale tfar (ray :direction)))
 							axis-entry-pb (split-axis entry-pb)
 							axis-exit-pb (split-axis exit-pb)
 							entry-in-left (<= axis-entry-pb split-pos)
 							exit-in-left (<= axis-exit-pb split-pos)
 							left-only (and entry-in-left exit-in-left)
 							left-to-right (and entry-in-left (not exit-in-left))
 							right-only (and (not entry-in-left) (not exit-in-left))
 							right-to-left (not left-to-right)
 							try-both (fn [n1 n2] 
 												 (let [result (closest-object-tree ray n1)
 												 			 try-other-side (or (= result missed) ; didn't hit anything on this side
 												 			 										; test to see whether the ray hit something, but in the wrong box
 												 			 										; e.g. a primitive that spans both boxes
 												 			 										; if so, try and find a closer object in the other box
 												 			 										(and left-to-right (> (split-axis (result :hit-point)) split-pos))
 												 			 										(and right-to-left (< (split-axis (result :hit-point)) split-pos)))
 												 			 result-2 (if try-other-side (closest-object-tree ray n2) missed)]
 												 			 (if (= (smallest-positive (result :distance) (result-2 :distance)) (result :distance))
 												 			 	 result result-2)))]
 					(cond left-only (closest-object-tree ray left-node)
 								right-only (closest-object-tree ray right-node)
 								left-to-right (try-both left-node right-node)
 								right-to-left (try-both right-node left-node)
 								true missed))))))					

; TODO - rewrite to use tree instead of linear search, and to stop when first intersecting object found that is closer
; (unless the closest object is needed for refraction-shadows)
;(defn hit-any-closer? [ray objects light-pos]
;  (let [light-distance (vec-length light-pos)]
;    (default (some #(if (between? (% :distance) 0.0 light-distance) %) 
;		   (intersect-all ray objects))
;      missed)))
								
(defn trace-ray 
	([ray scene lights light-vecs] 
		(trace-ray ray scene lights light-vecs 1.0 0))
	([ray scene lights light-vecs refractive-index trace-depth]
  (col-if (< trace-depth ((scene :settings) :max-trace-depth))
  	(let [hit-info (closest-object-tree ray (scene :scene-tree))]
    	(col-if (> (hit-info :distance) 0.0) 
	    	(let [translated-light-vecs (map #(vec-sub % (hit-info :hit-point)) light-vecs)	
	    			{:keys [settings scene-data scene-tree]} scene
	    			shade-info (array-map
			      	:scene scene
			 		    :ray ray
			     		:object (hit-info :obj)
				      :distance (hit-info :distance)
				      :hit-inside (hit-info :hit-inside)
				      :hit-point (hit-info :hit-point)
				      :normal (get-bumped-normal hit-info)		
			  	    :hit-info hit-info ; TODO - either merge hit-info with shade-info or keep it as a key, instead of separating some values out 
				      :lights lights
				      :light-vecs translated-light-vecs		   
				      :shadow-rays (map #(make-ray (hit-info :hit-point) %) translated-light-vecs)
				      :refractive-index refractive-index
			  	    :trace-depth trace-depth
			  	    :trace-ray trace-ray
			  	    :closest-object-tree closest-object-tree)]
; Each shader is passed the output colour of the previous shader.
	      (reduce #(%2 shade-info %1) (settings :ambient-light) ((hit-info :obj) :shaders))))))))

(defn make-virtual-screen
  "Returns an array vectors containing the top left, top right, bottom left, 
and bottom right co-ordinates of the virtual screen the user is 'looking at'"
  [x-size y-size cam-matrix screen-width] 
  (let [aspect-ratio (/ x-size y-size)
	screen-height (/ screen-width aspect-ratio)
	sw (/ screen-width 2)
	sh (/ screen-height 2)
	screen-rect [(make-vect (- 0.0 sw) sh 0.0) 
		     (make-vect sw sh 0.0) 
		     (make-vect sw (- 0.0 sh) 0.0)
		     (make-vect (- 0.0 sw) (- 0 sh) 0.0)]]
    (map #(mat-transform cam-matrix %) screen-rect)))

(defn make-camera-matrix
 "Returns a matrix used for transforming co-ordinates into the camera's space"
  [cam-look-at cam-viewpoint cam-up]
  (let [cam-z (normalize (vec-scale -1.0 (vec-sub cam-look-at cam-viewpoint)))
	cam-x (cross cam-up (vec-scale -1.0 cam-z))
	cam-y (cross cam-x cam-z)
	m (make-matrix cam-x cam-y cam-z) 
	t (map #(nth m (+ (first %) (* (last %) 4))) 
	       (combinations (range 3)))]
    (concat (take 3 t) [(vx cam-viewpoint)] 
	    (take 3 (drop 3 t)) [(vy cam-viewpoint)]
	    (take 3 (drop 6 t)) [(vz cam-viewpoint)] 
	    [0.0 0.0 0.0 1.0])))

(defn make-start-ray
"Creates the first ray fired into the scene from the viewpoint (subsequent rays are made by reflection/refraction/etc"
  [x y dx dy top-left start-viewpoint [jx jy] dof]
  (let [ray-origin (vec-add start-viewpoint dof) 
	direction (normalize (vec-sub 
			      (vec-add top-left 
				       (vec-scale (+ x jx) dx) 
				       (vec-scale (+ y jy) dy)) 
			      ray-origin))]
    (array-map :origin ray-origin :direction direction)))

(defn make-dof-vecs 
  "Create random offsets for depth-of-field calculations. 
TODO - enhance with more 'even' randomness, circular camera apeture instead of square."
  [amount sample-count oversampling]
  (concat (repeat oversampling zero-vec)
	  (take (- sample-count oversampling) (repeatedly #(make-random-vect amount)))))

(defn set-pixel [image x y x-res samples]
	(aset-int image (+ (* y x-res) x) 
		(pack-colour (/ 1.0 (count samples)) (vec-add samples))))
	
(defn trace-band 
  [image scene band] 
  (let [{:keys [x-res y-res shadow-samples soft-shadow-width look-at viewpoint camera-up
  							depth-of-field depth-of-field-samples oversampling screen-width image-bands]} (scene :settings)
  
  			lights (filter #(= (% :objtype) :light) (scene :scene-data))
  			light-count (count lights)
  			all-lights (take (* shadow-samples light-count) (cycle lights))
  			light-offsets (concat (repeat light-count zero-vec)
												(take (* (dec shadow-samples) light-count)
					      				(repeatedly #(make-random-vect soft-shadow-width))))
		  	light-vecs (map #(vec-add (%1 :position) %2) all-lights light-offsets)
  			cam-matrix (make-camera-matrix look-at viewpoint camera-up)
				virtual-screen (make-virtual-screen x-res y-res cam-matrix screen-width)
				virtual-viewpoint (mat-transform cam-matrix 
					(make-vect 0.0 0.0 (vec-length (vec-sub look-at viewpoint))))
				top-left (nth virtual-screen 0)
				dx (vec-scale (/ 1 x-res) (vec-sub (nth virtual-screen 1) top-left))
				dy (vec-scale (/ 1 y-res) (vec-sub (nth virtual-screen 3) top-left))
				y-start (* (/ band image-bands) y-res)
				y-end (* (/ (inc band) image-bands) y-res)
				sample-count (* oversampling depth-of-field-samples)]
		(dorun (for [x (range x-res)
						     y (range y-start y-end)]
		 (let [oversampling-jitter (make-grid 1 (sqrt sample-count)) 
		       dof-jitter (make-dof-vecs depth-of-field sample-count oversampling)
		       samples (map #(trace-ray (make-start-ray x y dx dy top-left virtual-viewpoint %1 %2) 
																		scene all-lights light-vecs)
				    					oversampling-jitter dof-jitter)]
			(set-pixel image x y x-res samples))))))

(defn build-scene-tree 
	"Build bsp or kd-tree as specified on the settings."
	[scene-data settings]
	(let [{:keys [max-tree-depth min-leaf-primitives scene-file 
				 scene-bounding-box x-res y-res scene-file tree-type]} settings
				primitives (filter #((% :objtype) #{:plane :sphere :triangle}) scene-data)]
		(cond (= tree-type :kd-tree) (make-kd-tree primitives scene-bounding-box max-tree-depth min-leaf-primitives)
					(= tree-type :bsp-tree) (make-bsp-tree primitives scene-bounding-box max-tree-depth min-leaf-primitives)
					true (make-leaf primitives))))

(defn render-bands 
	"Split the image up into bands and assign each one to an agent, then queue them up for execution."
	[scene]
	(let [settings (scene :settings)
				image (make-array Integer/TYPE (* (settings :x-res) (settings :y-res) 3))]
		(doall (pmap #(trace-band image scene %) (range (settings :image-bands)))) 
		image))
 	
(defn trace  
	"Render the scene using multiple threads."
  [scene-data settings]
  (let [scene-tree (build-scene-tree scene-data settings)
  			scene (array-map :scene-data scene-data :scene-tree scene-tree :settings settings)]
		(render-bands scene)))


