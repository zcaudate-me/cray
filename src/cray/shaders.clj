; Shaders for use with cray.clj

(ns cray.shaders (:use cray.utils cray.primitives))
(import 'java.awt.image.BufferedImage)

; Bump-mapping functions
(defn get-bump [hit-info] 
  (((hit-info :obj) :bumpmap) (calc-uv hit-info 1.0 1.0)))

(defn get-bumped-normal [hit-info]
  (let [normal (get-normal hit-info)]
    (if ((hit-info :obj) :bumpmap)
      (normalize (vec-add (get-bump hit-info) normal))
      normal)))

(defn bump-texture [#^BufferedImage image u-scale v-scale] 
  (fn [[u v]] 
    (let [width (. image getWidth)
	  height (. image getHeight)
	  packed-normal (. image getRGB (max 0 (rem (* u u-scale width) width)) 
			             (max 0 (rem (* v v-scale height) height)))]
      (unpack-normal packed-normal))))

; Shaders - each shader returns a function that calculates shading for an object intersection
(defn flat-shader [colour] (fn [_ colour-acc] (vec-add colour-acc colour)))

(defn texture-shader 
  ([texture amount] (texture-shader texture 1.0 1.0 amount))
  ([texture u-scale v-scale amount]
     (fn [shade-info colour-acc] 
       (vec-add colour-acc 
								(vec-scale amount 
			   									 (texture (calc-uv (shade-info :hit-info) u-scale v-scale)))))))
                          
(defn specular-shader [spec]
  (fn [{:keys [ray,shadow-rays,normal,light-vecs]} colour-acc] 
    (let [samples (map (fn speculars [light-vec]  
			 (let [r (vec-sub light-vec 
					  (vec-scale (* 2 (dot light-vec normal)) 
						     normal))
			       spec-dot (dot (ray :direction) (normalize r))]
			   (col-if (> spec-dot epsilon) 
				   (vec-scale (* (pow spec-dot 20) spec) colour-acc)))) 
		       light-vecs)
	  specular-colour (vec-scale (/ 1 (count shadow-rays)) 
				     (vec-add samples))]
      (vec-add colour-acc specular-colour))))

(defn image-texture [#^BufferedImage image] 
  (fn [[u v]] 
    (let [width (. image getWidth)
	  height (. image getHeight)]
      (unpack-colour (. image getRGB (max 0 (rem (* u width) width)) 
		                    (max 0 (rem (* v height) height)))))))
					  
(defn checkerboard-texture [col1 col2] 
  (fn [[u v]]
    (if (or (and (<= u 0.5) (<= v 0.5)) 
	    (and (> u 0.5) (> v 0.5))) 
      col1
      col2)))
	

(defn reflection-shader [amount]
  (fn [{:keys [trace-depth,hit-point,trace-ray,ray,normal,scene,lights,light-vecs,refractive-index]} colour-acc]  
    (let [reflect-colour (trace-ray (make-reflect-ray hit-point (ray :direction) normal) 
			     													scene lights light-vecs refractive-index (inc trace-depth))]
      (vec-add colour-acc (vec-scale amount reflect-colour)))))

(defn refraction-shader [amount obj-refractive-index density]
  (fn [{:keys [ray,trace-ray,hit-inside,max-trace-depth,normal,refractive-index,direction,hit-point,scene,lights,light-vecs,distance,trace-depth]} colour-acc] 
    (let [refract-ray (make-refract-ray ray normal hit-point hit-inside 
							 													refractive-index obj-refractive-index)]
			(col-if refract-ray
				(let [refract-colour (trace-ray refract-ray scene lights light-vecs refractive-index (inc trace-depth))
			        absorbed-colour (make-vect (map (fn beer [rc oc] (* rc (exp (* oc density (- 0.0 distance)))))
					    								 refract-colour colour-acc))]
								(vec-add colour-acc (vec-scale amount refract-colour))))))) 	    								 
					    								 
(defn calc-diffuse 
  "Calculate diffuse shading for a surface based on the lights in the scene."
  [{ray-direction :direction} normal obj-colour light-colour] 
  (vec-scale (max 0.0 (dot normal ray-direction)) 
	     (vec-mul obj-colour light-colour)))	

(defn in-shade? 
  "Returns true if the the object 'closest' blocks the light from 'light-vec'"
  [{distance :distance} light-vec] 
  (between? distance 0.0 (vec-length light-vec)))

(defn diffuse-shader 
  "Diffuse shading is replaced by 0 if the object is in shadow, or the light refracted thru the nearest object if it is refractive.
Objects with :transparency set to true are treated as refractive.
Could optimise by stopping intersection tests after the first object closer than the light has been found."
  [amount]
  (fn [{:keys [lights,normal,closest-object-tree,trace-ray,scene,refractive-index,trace-depth,shadow-rays,light-vecs]} colour-acc] 
    (let [samples (map (fn [shadow-ray light-vec light] 
			 (let [hit (closest-object-tree shadow-ray (scene :scene-tree))]
	;		 hit-any-closer? shadow-ray objects light-vec)]
	;		    (if (hit :obj);
					(if (in-shade? hit light-vec)
			     (col-if ((hit :obj) :transparency) 
				     (calc-diffuse shadow-ray normal 
						   (trace-ray shadow-ray scene lights light-vecs refractive-index (inc trace-depth)) 
						   (light :colour)))
			     (calc-diffuse shadow-ray normal colour-acc (light :colour))))) 
		       shadow-rays light-vecs lights)
	  			diffuse-colour (vec-scale (/ amount (count samples)) (vec-add samples))]
      (vec-add (vec-scale (- 1.0 amount) colour-acc) diffuse-colour))))

(defn diffuse-shader-no-shadows
	"Diffuse shader with no shadow testing. For use with triangle models to avoid artifacts when part of the model is in its own shadow."
		[amount]
	(fn [{:keys [lights,normal,shadow-rays]} colour-acc] 
    (let [samples (map #(calc-diffuse %1 normal colour-acc (%2 :colour)) shadow-rays lights)
  		  	diffuse-colour (vec-scale (/ amount (count samples)) (vec-add samples))]
      (vec-add (vec-scale (- 1 amount) colour-acc) diffuse-colour))))
			       
; Helpful shader constructing functions
(defn checkerboard-shader 
  "Traditional ray tracing checkerboard"
  [] 
  (texture-shader (checkerboard-texture (make-colour 0.1 0.1 0.1) (make-colour 0.8 0.8 0.8)) 1.0))
	    
(defn image-shader [image] 
  (texture-shader (image-texture image) 0.3))

(defn transparent-shade 
  "A typical set of shaders for a transparent object."
  []
  [(flat-shader (make-colour 0.10 0.10 0.10))
   (specular-shader 4)
   (refraction-shader 1.0 1.2 0.10)])

(defn plain-shade 
  "A typical set of shaders for a plain coloured object"
  [colour]
  [(flat-shader colour)
   (diffuse-shader 0.7)])
