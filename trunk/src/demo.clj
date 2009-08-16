; Pool table demo for cray raytracer

(use 'cray.tracer 'cray.shaders 'cray.utils 'cray.primitives)	

(load-file "demosettings.clj")

(defn get-scene []
  (let [path "./textures/"
	filenames (map #(str path "ball-" % ".jpg") [1 11 2 13 8 4 12 6 15 5 14 3 10 9 7])

	shaders (map #(list (texture-shader (image-texture (get-image %)) 1.0 1.0 0.3) 
			    (diffuse-shader 0.7)
			    (specular-shader 0.5)
			    (reflection-shader 0.15)) 
		     filenames)
	sphere-pitch 0.81649658
	sphere-radius 1
	z-dist (* 2 sphere-pitch sphere-radius)
	z-coords (map #(* z-dist %) 
		      [-2 
		       -1 -1
		       0 0 0  
		       1 1 1 1 
		       2 2 2 2 2])
	x-coords [0 
		  -1 1 
		  -2 0 2 
		  -3 -1 1 3
		  -4 -2 0 2 4]]
	 (concat 
	  (map (fn [shader x z] 
		 (let [up (make-random-vect)
		       left (normalize (cross up (make-random-vect)))]
		       (array-map :objtype :sphere 
				  :position (make-vect x 1.2 z) 
				  :radius sphere-radius
				  :up up
				  :left left
				  :shaders shader))) 
		   shaders x-coords z-coords)
		 [(array-map :objtype :plane :distance 0.2 :normal (normalize (make-vect 0.0 1.0 0.0)) 
			     :bumpmap (image-texture (get-image "textures/felt-normal.jpg")) 
			     :shaders [(image-shader (get-image "textures/felt.jpg")) (diffuse-shader 0.7)])
			(array-map :objtype :light :position (make-vect 3 5 0) :colour (make-colour 3.6 3.5 3.5))
			(array-map :objtype :light :position (make-vect 3 8 8) :colour (make-colour 3.7 3.6 3.4))])))

