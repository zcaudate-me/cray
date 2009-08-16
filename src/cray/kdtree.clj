; kd-tree and bsp tree implementation for cray.clj
(ns cray.kdtree (:use cray.utils cray.primitives))

; A node in a kd or bsp tree is of one of these types:
; Leaf - :primitives :leaf true
; Node - :left :right :box :split-axis :split-pos
 
(defmulti find-extents 
	(fn [axis primitive] (primitive :objtype)))
	
(defmethod find-extents :sphere [axis {:keys [position radius]}]
	[(- (axis position) radius) (+ (axis position) radius)])
 
(defmethod find-extents :triangle [axis {:keys [v0 v1 v2]}]
	[(axis v0) (axis v1) (axis v2)]) 

(defmethod find-extents :default [_ _] [])
   
(defn kd-make-split-list 
	"Returns a list of candidate co-ordinates for splitting the current bounding box"
	[axis box primitives]
	(axis-aabb-intersect-points axis box (reduce concat (map #(find-extents axis %) primitives))))

(defn split-box 
	"Split box into 'right' and 'left' boxes down split-vec, for building kd/bsp trees"
	[box split-axis split-pos]
	(let [left-pos (box :position)
				left-extent (vec-replace-coord split-axis split-pos (box :extent))
				left-size (vec-sub left-extent left-pos)
				right-pos (vec-replace-coord split-axis split-pos left-pos)
				right-extent (box :extent)
				right-size (vec-sub right-extent right-pos)
				left-box (make-aabb left-pos left-size)
				right-box (make-aabb right-pos right-size)]
		[left-box right-box]))

(defn assign-left-right 
	"Assigns each primitive to 'left' or 'right' (or both) boxes, depending on which it intersects"
	[left-box right-box primitives]
  [(aabb-intersect-primitives left-box primitives)
   (aabb-intersect-primitives right-box primitives)])
   
(defn kd-calc-split-cost 
	"Calculates the Surface Area Heuristic for splitting bounding boxes when building kd-trees."
	[parent-box left-box right-box left-count right-count]
	(let [left-area (aabb-surface-area left-box)
				right-area (aabb-surface-area right-box)
				parent-area (aabb-surface-area parent-box) 
				result
		(/ (+ (* left-area left-count) (* right-area right-count)) parent-area)] 
		result))

(defn kd-find-best-split 
	"Returns the best place to split the bounding box, or nil if there isn't one (no primitives in list)."
	[box primitives split-axis]
  (let [split-list (kd-make-split-list split-axis box primitives)
				split-costs 
					(map (fn [split-pos]
						(let [[left-box right-box] (split-box box split-axis split-pos)
									[left-primitives right-primitives] (assign-left-right left-box right-box primitives)
									split-cost (kd-calc-split-cost box left-box right-box 
																								 (count left-primitives)
																								 (count right-primitives))]
							[split-cost split-axis split-pos left-box right-box left-primitives right-primitives])) 
						split-list)
				find-cheapest-split #(if (< (nth %1 0) (nth %2 0)) %1 %2)]
		(reduce find-cheapest-split (first split-costs) (rest split-costs))))

(defn make-leaf [primitives]
	(array-map :leaf true :primitives primitives))

(defn make-branch [left-node right-node box split-pos split-axis]
	(array-map :left-node left-node :right-node right-node :box box :split-pos split-pos :split-axis split-axis))
					
(defn make-bsp-tree
	"Recursively builds bsp-tree"
	([primitives box max-tree-depth min-leaf-primitives] 
		(make-bsp-tree primitives box max-tree-depth min-leaf-primitives vx 0))
	([primitives box max-tree-depth min-leaf-primitives split-axis tree-depth]
		; Test terminating conditions, make leaf if terminating, otherwise make branch
		(if (or (>= tree-depth max-tree-depth)
						(<= (count primitives) min-leaf-primitives))
				(make-leaf primitives)
				(let [next-axis (axis-next split-axis) ; Split the bounding box on the 'next' axis to the last split
							split-pos (+ (split-axis (box :position)) (* 0.5 (split-axis (box :size))))
							[left-box right-box] (split-box box split-axis split-pos)
							[left-primitives right-primitives] (assign-left-right left-box right-box primitives)]
					(make-branch (make-bsp-tree left-primitives left-box max-tree-depth min-leaf-primitives next-axis (inc tree-depth))
										 	 (make-bsp-tree right-primitives right-box max-tree-depth min-leaf-primitives next-axis (inc tree-depth))
										 	 box split-pos split-axis)))))
							
(defn make-kd-tree 
	"Recursively builds nodes for kd-tree." 
  ([primitives box max-tree-depth min-leaf-primitives] 
  	(make-kd-tree primitives box max-tree-depth min-leaf-primitives 0))
  ([primitives box max-tree-depth min-leaf-primitives tree-depth]
		(let [count-primitives (count primitives)
	  			cost-making-leaves count-primitives
	  			box-size (box :size)
    			; Split the bounding box whichever axis the bounding box is largest on
	   			split-axis (cond (and (>= (vx box-size) (vy box-size) (vz box-size))) vx
												   (and (>= (vy box-size) (vx box-size) (vz box-size))) vy
											     :else vz)
	   			[best-split-cost best-split-axis best-split-pos left-box right-box left-primitives right-primitives] 
	   				(kd-find-best-split box primitives split-axis)]
    	; Test terminating conditions, make leaf if terminating, otherwise make branch
    	(if (or (nil? best-split-cost) ; May be nil if there was no way to split the box that fit any primitives in 
     					(> best-split-cost cost-making-leaves) ; Cheaper to make child nodes than leaves
     					(>= tree-depth max-tree-depth)
     					(<= count-primitives min-leaf-primitives))
				(make-leaf primitives)
	   		(make-branch (make-kd-tree left-primitives left-box max-tree-depth min-leaf-primitives (inc tree-depth))
		     			  		 (make-kd-tree right-primitives right-box max-tree-depth min-leaf-primitives (inc tree-depth))
		      					 box best-split-pos split-axis)))))

(defn print-tree [tree depth]
	(let [indent (apply str (repeat depth " "))]
		(if (tree :leaf)
			(println (str indent "Leaf node, depth = " depth ", # primitives = " (count (tree :primitives))))
			(do (println (str indent "Branch node, depth = " depth ", split-pos = " (tree :split-pos) ", split-axis = " (tree :split-axis)))
				(print-tree (tree :left-node) (inc depth))
				(print-tree (tree :right-node) (inc depth))))))


 