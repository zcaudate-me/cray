; cray ray tracer
; Copyright Hamish Hubbard
; Contact: graphics at tu dot be
; Licensed under the BSD license: http://creativecommons.org/licenses/BSD/

; Primitives for cray.clj
; A primitive is defined by three functions:
; 1. A ray-primitive intersection test
; 2. A function to calculate the surface normal at the ray-primitive intersection
; 3. A function to calculate the barycentric co-ordinates of the intersection point, to allow texture mapping (optional).
; 4. A ray-AABB intersection test

(ns cray.primitives (:use cray.utils))

; Ray functions
(defn make-ray 
  "make-ray moves the origin of the ray in the direction of the ray by a tiny amount.
This prevents floating point precision errors from causing the ray to start 
slightly 'inside' the primitive and then intersect with its inside, causing artifacts."
  [origin direction] 
  (let [norm-dir (normalize direction)] 
    (array-map :origin (vec-add origin (vec-scale epsilon direction)) 
      :direction norm-dir)))

(defn calc-point-on-ray
  "Returns a point along the ray at 'distance'."
  [{:keys [origin direction]}	distance]
  (vec-add (vec-scale distance direction)
    origin))

(defn make-reflect-ray 
  "Creates a ray that is reflected off the surface defined at point 'origin' with surface normal 'normal' at that point"
  [origin ray-dir normal] 
  (make-ray origin 
    (vec-sub ray-dir
      (vec-scale (* 2.0 (dot ray-dir normal))
        normal))))

(defn make-refract-ray 
  "Creates a ray that is refracted by the object that 'ray' hit"
  [ray normal hit-point hit-inside refractive-index obj-refractive-index]
  (let [N (if hit-inside (vec-scale -1 normal) normal) ; Need to multiply by -1 if leaving the object
        n (/ refractive-index obj-refractive-index)
        cosI (- 0.0 (dot N (ray :direction)))
        cosT2 (- 1.0 (* n n (- 1.0 (* cosI cosI))))
        T (if (> cosT2 0.0) (vec-add hit-point
                              (vec-scale n (ray :direction))
                              (vec-scale (- (* n cosI) (sqrt cosT2)) N)))]
    (if T (make-ray hit-point T))))

; Ray-primitive intersection tests
(def missed (array-map :obj nil :hit-inside false :distance -1))

(defmulti intersect 
  "Returns an arraymap containing the object, a flag for whether the inside or outside was hit, and the distance to the object."
  (fn [ray obj] (obj :objtype))) 

(defmethod intersect :sphere intersect-sphere [ray obj] 
  (let [sphere-center (vec-sub (ray :origin) (obj :position))
	b (- (dot sphere-center (ray :direction)))
	disc (+ (- (pow2 b) (dot sphere-center sphere-center)) (pow2 (obj :radius)))]
    (cond (< disc 0.0) missed
      true (let [sqrtd (sqrt disc)
                 i1 (- b sqrtd)
                 i2 (+ b sqrtd)]
             (cond (> i2 epsilon) (if (< i1 epsilon)
                                    (array-map :obj obj :hit-inside true :distance i2)
                                    (array-map :obj obj :hit-inside false :distance i1))
               true missed)))))

(defmethod intersect :plane intersect-plane [ray obj]
  (let [Vd (dot (obj :normal) (ray :direction))
        V0 (- 0.0 (+ (dot (obj :normal) (ray :origin))
                    (obj :distance)))]
    (if (or (fpeq Vd 0.0) (< (/ V0 Vd) 0.0)) 
      missed 
      (array-map :obj obj :hit-inside false :distance (/ V0 Vd)))))

(defn intersect-all 
  "Returns a lazy sequence of intersection-test results for all objects"
  [ray objects]
  (map #(intersect ray %) objects))

; See http://www.Graphics.Cornell.EDU/pubs/1997/MT97.pdf or http://www.ddj.com/184404201
(defmethod intersect :triangle intersect-triangle [ray obj] 
  (let [r (cross (ray :direction) (obj :edge2)) ; Start calculating the determinant
        a (dot (obj :edge1) r)] ; a is the determinant
    (cond (fpeq a 0.0) missed ; Triangle is parallel to ray direction
      true (let [f (/ 1.0 a)
                 s (vec-sub (ray :origin) (obj :v0)) ; Translate the first vertex to the ray origin
                 q (cross s (obj :edge1))
                 ; (u,v) are barycentric co-ordinates of the intersection point (values from 0-1 scaled to the size of the triangle)
                 u (* (dot s r) f)]
             (if (between? u 0.0 1.0)
               (let [v (* (dot (ray :direction) q) f)]
                 (if (and (> v 0.0) (< (+ u v) 1.0))
                   (array-map :obj obj
                     :hit-inside false
                     :distance (* (dot (obj :edge2) q) f)
                     :uv [u v])
                   missed))
               missed)))))

; Intersection test for lights or any other objects we don't want to intersect.
(defmethod intersect :default [_ _] missed) 

; Surface normal calculations
(defmulti get-normal (fn [{:keys [obj]}] (obj :objtype)))

(defmethod get-normal :sphere get-normal-sphere [{:keys [obj hit-point]}] 
  (normalize (vec-sub hit-point (obj :position))))

(defmethod get-normal :plane get-normal-plane [{:keys [obj]}] (obj :normal))

; Linear interpolation of normals across the face of the triangle
(defmethod get-normal :triangle get-normal-tri [{:keys [obj uv]}] 
  (let [{:keys [normal0 normal1 normal2]} obj
        [u v] uv]
    (normalize 
      (vec-add normal0
        (vec-scale u (vec-sub normal1 normal0))
        (vec-scale v (vec-sub normal2 normal0))))))

; Axis-aligned bounding boxes for kd-tree/bsp-tree
(defn make-aabb [position size]
  (array-map :position position :size size :extent (vec-add position size)))

; Debugging helper
(defn str-aabb [{:keys [position size extent]}]
  (str "(box position: " (str-vec position) " size: " (str-vec size) " extent: " (str-vec extent) ")"))

(defn axis-aabb-intersect-points 
  "Returns all points that intersect a box, on one axis only"
  [axis {:keys [position extent]} points]
  (let [axis-box-pos (axis position)
        axis-box-extent (axis extent)]
    (filter #(and (> % (- axis-box-pos epsilon)) (< % (+ axis-box-extent epsilon))) points)))

(defmulti aabb-intersect? 
  (fn [box primitive] (primitive :objtype)))

(defn aabb-sphere-distance [axis-box-pos axis-box-extent axis-sphere-pos sphere-radius]
  (cond (< axis-sphere-pos axis-box-pos) (pow2 (- axis-sphere-pos axis-box-pos))
    (> axis-sphere-pos axis-box-extent) (pow2 (- axis-sphere-pos axis-box-extent))
    true 0.0))

(defmethod aabb-intersect? :sphere [box sphere]
  ; min-distance is the square of the distance from the sphere's center to the box
  (let [min-distance-vals (map #(aabb-sphere-distance %1 %2 %3 (sphere :radius))
                            (box :position) (box :extent) (sphere :position))
        min-distance (reduce + min-distance-vals)]
    (<= min-distance (pow2 (sphere :radius)))))

(defn calc-v-min [n d bx] (float (if (> n 0.0) (- 0.0 bx d) (- bx d))))
(defn calc-v-max [n d bx] (float (if (> n 0.0) (- bx d) (- 0.0 bx d))))

; See http://www.cs.lth.se/home/Tomas_Akenine_Moller/code/tribox3.txt
(defn intersect-plane-box? 
  "Plane-AABB interection for a box centered on (0, 0, 0)"
  [normal pos box-extent]
  (let [v-min (make-vect (map calc-v-min normal pos box-extent))
        v-max (make-vect (map calc-v-max normal pos box-extent))]
    (if (> (dot normal v-min) 0.0)
      false
      (>= (dot normal v-max) 0.0))))

(defmethod aabb-intersect? :plane [{:keys [position size extent]} {:keys [normal distance]}]
  (let [point-on-plane (vec-scale distance normal)
        box-half-size (vec-scale 0.5 size)
        box-center (vec-add position box-half-size)
        translated-point (vec-sub point-on-plane box-center)]
    (intersect-plane-box? normal translated-point box-half-size)))

; aabb-triangle intersection test
; See http://www.cs.lth.se/home/Tomas_Akenine_Moller/code/tribox3.txt
(defn do-axis-test 
  [[axis-test a b tv0 tv1 tv2 bhs-1 bhs-2]]
  (let [[min-p max-p] (min-max (axis-test a b tv0 tv1 tv2))
        rad (+ (* (abs a) bhs-1) (* (abs b) bhs-2))]
    (or (> min-p rad) (< max-p (- rad)))))

(defn axis-test-x01 
  [a b tv0 tv1 tv2]
  [(- (* a (vy tv0)) (* b (vz tv0)))
   (- (* a (vy tv2)) (* b (vz tv2)))])

(defn axis-test-x2
  [a b tv0 tv1 tv2]
  [(- (* a (vy tv0)) (* b (vz tv0)))
   (- (* a (vy tv1)) (* b (vz tv1)))])

(defn axis-test-y02
  [a b tv0 tv1 tv2]
  [(+ (* (- a) (vx tv0)) (* b (vz tv0)))
   (+ (* (- a) (vx tv2)) (* b (vz tv2)))])

(defn axis-test-y1
  [a b tv0 tv1 tv2]
  [(+ (* (- a) (vx tv0)) (* b (vz tv0)))
   (+ (* (- a) (vx tv1)) (* b (vz tv1)))])

(defn axis-test-z12
  [a b tv0 tv1 tv2]
  [(- (* a (vx tv1)) (* b (vy tv1)))
   (- (* a (vx tv2)) (* b (vy tv2)))])

(defn axis-test-z0
  [a b tv0 tv1 tv2]
  [(- (* a (vx tv0)) (* b (vy tv0)))
   (- (* a (vx tv1)) (* b (vy tv1)))])

(defn min-max-test [axis v0 v1 v2 box-half-size]
  (let [axis-min (min (axis v0) (axis v1) (axis v2))
        axis-max (max (axis v0) (axis v1) (axis v2))
        axis-bhs (axis box-half-size)]
    (or (> axis-min axis-bhs) (< axis-max (- axis-bhs)))))

(defmethod aabb-intersect? :triangle [box {:keys [v0 v1 v2 edge1 edge2 edge3 normal]}]
  (let [box-half-size (vec-scale 0.5 (box :size))
        box-center (vec-add (box :position) box-half-size)
        tv0 (vec-sub v0 box-center) ; translate triangle vertices to be relative to center of box
        tv1 (vec-sub v1 box-center)
        tv2 (vec-sub v2 box-center)
        edge0 (vec-sub tv1 tv0)
        edge1 (vec-sub tv2 tv1)
        edge2 (vec-sub tv0 tv2)
        tests [[axis-test-x01 (vz edge0) (vy edge0) tv0 tv1 tv2 (vy box-half-size) (vz box-half-size)]
               [axis-test-y02 (vz edge0) (vx edge0) tv0 tv1 tv2 (vx box-half-size) (vz box-half-size)]
               [axis-test-z12 (vy edge0) (vx edge0) tv0 tv1 tv2 (vx box-half-size) (vy box-half-size)]

               [axis-test-x01 (vz edge1) (vy edge1) tv0 tv1 tv2 (vy box-half-size) (vz box-half-size)]
               [axis-test-y02 (vz edge1) (vx edge1) tv0 tv1 tv2 (vx box-half-size) (vz box-half-size)]
               [axis-test-z0 (vy edge1) (vx edge1) tv0 tv1 tv2 (vx box-half-size) (vy box-half-size)]

               [axis-test-x2 (vz edge2) (vy edge2) tv0 tv1 tv2 (vy box-half-size) (vz box-half-size)]
               [axis-test-y1 (vz edge2) (vx edge2) tv0 tv1 tv2 (vx box-half-size) (vz box-half-size)]
               [axis-test-z12 (vy edge2) (vx edge2) tv0 tv1 tv2 (vx box-half-size) (vy box-half-size)]]]
    ; tests return 'true' if they failed
    (not (or (some #(true? %) (map #(do-axis-test %) tests))
           (some #(true? %) (map #(min-max-test % tv0 tv1 tv2 box-half-size) [vx vy vz]))
           (not (intersect-plane-box? (cross edge0 edge1) tv0 box-half-size))))))

(defn aabb-surface-area 
  "Returns the surface area of a box - used for kd-tree splitting heuristic."
  [{:keys [size]}]
  (+ (+ (* (vx size) (vy size))
       (* (vx size) (vz size))
       (* (vy size) (vz size)))))

(defn aabb-intersect-test
  [axis-origin axis-direction axis-box-pos axis-box-extent tnear tfar]
  (let [ray-parallel (= axis-direction 0.0) ; true if ray is parallel to box planes
        ray-outside-planes (or (< axis-origin axis-box-pos)
                             (> axis-origin axis-box-extent))]
    (if ray-parallel
      (if ray-outside-planes nil [tnear tfar])
      (let [t0 (/ (- axis-box-pos axis-origin) axis-direction)
            t1 (/ (- axis-box-extent axis-origin) axis-direction)
            [nearest furthest] (min-max t0 t1)
            new-tnear (max nearest tnear)
            new-tfar (min furthest tfar)]
        (if (or (> new-tnear new-tfar) (< new-tfar 0.0)) ; ray missed slab completely or intersects behind viewpoint
          nil
          [new-tnear new-tfar])))))

(defn aabb-intersect-ray
  "Tests for intersection between box and ray using three-slab method. Return nil if none, [tnear tfar] if there is an intersection"
  [box ray tnear tfar]
  (loop [new-tnear tnear
         new-tfar tfar
         axis vx]
    (let [result (aabb-intersect-test (axis (ray :origin)) (axis (ray :direction))
                   (axis (box :position)) (axis (box :extent))
                   new-tnear new-tfar)]
      (if (or (nil? result) (= axis vz))
        result
        (recur new-tnear new-tfar (axis-next axis))))))

(defn aabb-intersect-primitives [box primitives]
  (filter #(aabb-intersect? box %) primitives))

; returns the amount of space occupied by the sphere. TODO - another method for traingles
; equivalent of eleft/eright values in CalculateRange
(defn sphere-calculate-range [axis {:keys [position radius]}]
  (let [axis-position (axis position)]
    [(- axis-position radius) (+ axis-position radius)]))

; Barycentric co-ordinate calculations for bump/texture mapping
; (u, v) are barycentric co-ordinates (ranging between 0 and u-scale or v-scale on each axis over the co-ordinates of a primitive
; u-scale and v-scale can be used to increase this
(defmulti calc-uv (fn [{:keys [obj]} _ _] (obj :objtype)))	  

(defmethod calc-uv :sphere
  [{:keys [obj] u-scale v-scale]
    (let [vp (get-normal shade-info)
          sphere-up (obj :up)
          sphere-left (obj :left)
          phi (acos (- (dot vp sphere-up)))
          v (* v-scale (/ phi pi))
          sin-phi (sin phi)
          theta (if (fpeq sin-phi 0.0)
                  1.0
                  (/ (acos (clamp (/ (dot vp sphere-left) sin-phi) -1.0 1.0))
                    (* 2.0 pi)))
          u (* u-scale
              (if (fpeq phi 0.0)
                1.0
                (if (> (dot (cross sphere-up sphere-left) vp) 0.0)
                  theta
                  (- 1.0 theta))))]
      [u v]))

(defmethod calc-uv :plane
  [{:keys [obj, hit-point]} u-scale v-scale]
  (let [uAxis (make-vect (vy (obj :normal))
                (vz (obj :normal))
                (- (vx (obj :normal))))
        vAxis (cross uAxis (obj :normal))
        u #^Double (dot hit-point uAxis)
        v #^Double (dot hit-point vAxis)
        absu (rem (abs u) 1.0)
        absv (rem (abs v) 1.0)
        texu (* u-scale (if (< u 0.0) (- 1.0 absu) absu))
        texv (* v-scale (if (< v 0.0) (- 1.0 absv) absv))]
    [texu texv]))

(defmethod calc-uv :triangle
  [{:keys [uv]} u-scale v-scale]
  (let [[u v] uv]
    [(* u u-scale) (* v v-scale)]))

(defmethod calc-uv :plane
  [{:keys [obj, hit-point]} u-scale v-scale]
  (let [uAxis (make-vect (vy (obj :normal))
                (vz (obj :normal))
                (- (vx (obj :normal))))
        vAxis (cross uAxis (obj :normal))
        u #^Double (dot hit-point uAxis)
        v #^Double (dot hit-point vAxis)
        absu (rem (abs u) 1.0)
        absv (rem (abs v) 1.0)
        texu (* u-scale (if (< u 0.0) (- 1.0 absu) absu))
        texv (* v-scale (if (< v 0.0) (- 1.0 absv) absv))]
    [texu texv]))

(defmethod calc-uv :triangle
  [{:keys [uv]} u-scale v-scale]
  (let [[u v] uv]
    [(* u u-scale) (* v v-scale)]))

(defmethod calc-uv :default [_ _ _] [0 0])
