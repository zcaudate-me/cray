# Introduction #

cray is a ray tracer written in Clojure as a learning exercise.
[Download](http://code.google.com/p/cray/downloads/list) and run cray.clj with one of the included demo scenes, and after a few seconds or minutes (depending on your machine) an image like one of those shown below should appear.
Change the settings in the settings files to enable soft shadows, depth of field, change resolution, camera position, etc.
Run your JVM with the `-server` command-line option for best performance.

Any comments can be sent to `graphics at tu dot be`

cray currently supports:

### Primitives ###
Spheres, planes, triangles, axis-aligned bounding boxes.
(Freely reusable implementations of ray-(triangle/sphere/plane/AABB) and AABB-(triangle/sphere/plane intersection tests and barycentric co-ordinate calculations if you need them).

### Shading ###
Diffuse, specular, reflection, refraction, texture mapping, bump mapping, normal-averaging, anti-aliasing.

### Distributed ray tracing ###
Depth of field, soft shadows.

### Performance ###
Multi-threading, vector math in Java primitives, kd-trees and bsp-trees for improved rendering speed - O(log n) vs O(n).

## Sample Images ##

![http://tu.be/graphics/8ball-dof.png](http://tu.be/graphics/8ball-dof.png)

Texture/bump mapping, with Monte Carlo depth-of-field simulation, with 200 samples. 'Look-at' camera parameter sets the focus at the 8-ball.

![http://tu.be/graphics/8ball-dof2.png](http://tu.be/graphics/8ball-dof2.png)

As above, but with a deeper depth-of-field setting.

![http://tu.be/graphics/teapot2.png](http://tu.be/graphics/teapot2.png)

The Utah teapot with normal averaging (still some rendering artifacts where triangles are nearly parallel to the viewpoint). BSP-tree results in a 50-100x speedup over naive raytracing.

![http://tu.be/graphics/teapot1.png](http://tu.be/graphics/teapot1.png)

The same teapot model, with normal averaging turned off.

## Images from earlier versions ##

<table border='0'>
<tr><td>
<img src='http://tu.be/graphics/trace5.jpg' />
</td><td>
<img src='http://tu.be/graphics/trace4.jpg' />
</td></tr>
<tr><td>
First results, 83 lines of code. </td>
<td>Anti-aliasing, soft shadows, refraction<br>
</td></tr></table>

![http://tu.be/graphics/trace2.jpg](http://tu.be/graphics/trace2.jpg)

Soft shadows

![http://tu.be/graphics/earth.png](http://tu.be/graphics/earth.png)

Texture mapping

