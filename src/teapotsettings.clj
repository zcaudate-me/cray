; Settings for teapot.clj (Utah teapot scene)

(use 'cray.primitives 'cray.utils)

(defn get-settings []
(array-map
	:scene-file "teapot.clj"
	;	"Change this to the name of the scene you want to render")
	:max-trace-depth 4
	;	"Number of times to recurse when following reflections/refractions")
	:image-bands 8
	;	"The image is divided into horizontal slices for processing. Each slice is despatched to a CPU when it becomes free. Set to >= your number of CPU cores.")
	
	; Viewpoint/camera settings
	:viewpoint (make-vect 0.0 1.0 -10.0)
	; "Location that rays are cast from."
	:look-at (make-vect 0.0 1.0 0.0)
	;	"Location of the virtual 'screen' the rays are cast through. Interesting fisheye-type distortion if viewpoint and look-at are close."
	:camera-up (normalize (make-vect 0.0 1.0 0.0))
	;	"'Up' orientation of the camera"
	:screen-width 4.0 
	;	"Width of virtual screen, in world co-ordinates. Virtual screen height is worked out from aspect ratio x-res / y-res"
	:x-res 480
	;	"Size of image, in pixels"
	:y-res 300
	;	"Size of image, in pixels"
	:oversampling 2
	;	"More oversampling = better anti-aliasing (1 = no anti-aliasing)"
	:ambient-light (make-colour 0.1 0.1 0.1)
	;	"This value is added to every pixel in the scene."
	
	; Soft shadow settings
	:shadow-samples 1
	;	"Number of rays cast for soft shadows (1 = sharp shadows, minimum of 32 usually needed to avoid grainy shadows)"
	:soft-shadow-width 2.0
	;	"Amount of scatter for soft shadow rays. More = softer shadows, but grainier unless shadow-samples is increased."
	
	; Depth of field settings
	:depth-of-field 1.0
	;	"Virtual 'apeture size' to simulate a camera lens. (In this case, a square camera lens...) Larger = narrower depth of field."
	:depth-of-field-samples 1
	;	"Number of rays for depth of field. Around 100+ is needed to avoid grain if using depth of field. If using oversampling, can decrease this figure."
	
	; kd-tree settings
	:tree-type :bsp-tree
	;	"If :bsp-tree or :kd-tree, a bsp or kd tree is used to speed intersection tests, otherwise naive raytracing is used"
	:scene-bounding-box (make-aabb (make-vect -20.0 -20.0 -20.0) (make-vect 40.0 40.0 40.0))
	;	"Bounding box for scene if kd-tree is used"
	:max-tree-depth 40
	;	"Maximum depth of the kd-tree. If this depth is reached, any remaining primitives will be stored in a leaf node."
	:min-leaf-primitives 3
	; "Minimum number of primitives in a leaf node - stop creating branches if <= this number of primitives in a node."
	))