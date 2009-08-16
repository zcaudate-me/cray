(ns cray 
	(:gen-class)
	(:use cray.tracer))

;(set! *warn-on-reflection* true)

(import '(java.awt Dimension)
	'(java.awt.image BufferedImage MemoryImageSource ColorModel)
	'(javax.swing JFrame JPanel))

(defn time-trace [scene-data settings]
	(let [start-time (System/nanoTime)
        result (trace scene-data settings)
        elapsed (/ (double (- (System/nanoTime) start-time)) 1000000000)]
  	(println (str "Elapsed time: " elapsed " sec"))
    (println (str "Pixels/sec: " (/ (* (settings :x-res) (settings :y-res)) elapsed)))
 		result))

; get-settings and get-scene must be defined in the scene file passed on the command-line
(declare get-settings get-scene)

(defn -main [& args]
  (println "cray raytracer v1.1")
	(let [scene-file (first *command-line-args*)]
		(if (nil? scene-file)
			(do 
				(println "Please specify a scene to render (e.g. demo.clj")
				(System/exit 0))
			(do
				(println "Rendering:" scene-file)
				(load-file scene-file)
				(let [settings (get-settings)
							{:keys [x-res y-res]} settings
							scene-data (get-scene)
							raw-image (time-trace scene-data settings)
							mis (MemoryImageSource. x-res y-res (. ColorModel getRGBdefault) 
																			raw-image 0 x-res)
							image (. (JPanel.) createImage mis)
							frame (JFrame. "Tracer")
							panel (proxy [JPanel] [] 
								(paint [graphics] 
		      				(. graphics drawImage image 0 0 x-res y-res nil)))]
					(. panel setPreferredSize (Dimension. x-res y-res))
    			(doto frame (.. getContentPane (add panel)) 
	  				.pack .show))))))
	
(-main)
    
