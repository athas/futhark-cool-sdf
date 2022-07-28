# futhark-cool-sdf

Renders a signed distance function, with gradients computed by AD.

The distance function can be changed at run-time by entering a new one
at the stdin prompt.  Enter a simple math expression that uses the
common operators (`+-/*`) and the functions `sin` and `cos`.  You can
also use the variables `u`, `v` and `t` for spherical coordinates and
time, respectively.

Drag with the mouse to poorly rotate the camera.

Run with `rlwrap` to make the editing experience better.

Interesting expressions you can try:

* `(1+sin(u*20*3+t)*sin(t))/2 + (1+cos(v*20*3+t)*sin(t))/2` (the default).

* `1/2` (constant sphere)

* `sin(u*t+u)`
