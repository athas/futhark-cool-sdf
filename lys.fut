import "lib/github.com/diku-dk/lys/lys"
import "lib/github.com/athas/vector/vspace"

import "interpret"


module evalf32 = mk_eval f32

type var = #t | #u | #v

type inst' = inst var f32

def decode [n] (words: [n]u32) : []inst' =
  let (w,_,acc) =
    loop (w,r,acc) = (0, 0, replicate n (#const 0)) while r < n do
    let simple (acc: *[n]inst') inst =
      (w+1,r+1,acc with [w] = inst)
    in match words[r]
       case 0 ->
         (w+1,
          r+2,
          acc with [w] = #const (f32.from_bits words[r+1]))
       case 1 ->
         (w+1,
          r+2,
          acc with [w] = #var (match words[r+1]
                               case 0 -> #t
                               case 1 -> #u
                               case _ -> #v))
       case 2 -> simple acc #pop
       case 3 -> simple acc #swap
       case 4 -> simple acc #add
       case 5 -> simple acc #sub
       case 6 -> simple acc #mul
       case 7 -> simple acc #div
       case 8 -> simple acc #cos
       case 9 -> simple acc #sin
       case 10 ->
         (w+1,
          r+2,
          acc with [w] = #over (i32.u32 words[r+1]))
       case _ -> assert false (w,r+1,acc)
  in take w acc

def radius_at (program: []inst') t u v : f32 =
  let k = 3
  let var x = match x case #t -> t
                      case #u -> u
                      case #v -> v
  in (evalf32.exec var (replicate k 0) 0 program)[0]

module vec3 = mk_vspace_3d f32
type vec3 = vec3.vector

def uv (p: vec3) : (f32,f32) =
  let d = vec3.normalise p
  in (0.5 + f32.atan2 d.x d.z / (2*f32.pi),
      0.5 + f32.asin d.y / f32.pi)

def sdf program (t: f32) (p: vec3) : f32 =
  let (u,v) = uv p
  in vec3.norm p - radius_at program t u v

type hit = #hit vec3 | #miss

def trace program t (orig: vec3) (dir: vec3) : hit =
  let not_done (i, _) = i < 128
  let march (i, pos) =
    let d = sdf program t pos
    in if d <= 0
       then (1337, pos)
       else (i + 1, pos vec3.+ ((f32.max (d*0.1) 0.01) `vec3.scale` dir))
  in iterate_while not_done march (0,orig)
     |> \(i, hit) -> if i == 1337 then #hit hit else #miss

def grad f x = vjp f x 1f32

def distance_field_normal program t pos =
  vec3.normalise (grad (sdf program t) pos)

def camera_ray origin width height i j =
  let fov = 50
  let target = {x=0,y=0,z=0}
  let eye_dir = vec3.(target - origin)
  let eye_vector = vec3.(normalise eye_dir)
  let vp_right = vec3.normalise (vec3.cross eye_vector {x=0,y=1,z=0})
  let vp_up = vec3.normalise (vec3.cross vp_right eye_vector)
  let fov_radians = f32.pi * (f32.i32 fov / 2) / 180
  let height_width_ratio = f32.i64 height / f32.i64 width
  let half_width = f32.tan fov_radians
  let half_height = height_width_ratio * half_width
  let camera_width = half_width * 2
  let camera_height = half_height * 2
  let pixel_width = camera_width / (f32.i64 width - 1)
  let pixel_height = camera_height / (f32.i64 height - 1)

  let xcomp = vec3.scale ((f32.i64 i * pixel_width) - half_width) vp_right
  let ycomp = vec3.scale ((f32.i64 j * pixel_height) - half_height) vp_up
  in vec3.(normalise (eye_vector + xcomp + ycomp))

def grey (light: f32) : u32 =
  let x = u32.f32(255 * f32.min 1 (f32.max 0 light))
  in (x << 16) | (x << 8) | x

def frame program {x=rot_x,y=rot_y} (width: i64) (height: i64) (t: f32) =
  let f j i =
    let dist = 3
    let origin = vec3.normalise {x=1,y=2,z= -3}
                 |> vec3.rot_y rot_y
                 |> vec3.rot_x rot_x
                 |> vec3.scale dist
    let dir = camera_ray origin width height i j
    in match trace program t origin dir
       case #miss ->
         0xFFFFFF
       case #hit hit ->
      let light_dir = vec3.normalise ({x=10, y=10, z=10} vec3.- hit)
      let light_intensity = light_dir `vec3.dot` distance_field_normal program t hit
      in grey light_intensity
  in tabulate_2d height width f

type~ state = {time: f32, h: i64, w: i64,
               moving: (i64, i64),
               mouse: {x:i32, y:i32},
               rot: {x: f32, y: f32},
               radius: i64,
               paused: bool,
               program: []inst'}

def keydown (key: i32) (s: state) =
  if key == SDLK_RIGHT then s with moving.1 = 1
  else if key == SDLK_LEFT then s with moving.1 = -1
  else if key == SDLK_UP then s with moving.0 = -1
  else if key == SDLK_DOWN then s with moving.0 = 1
  else if key == SDLK_SPACE then s with paused = !s.paused
  else s

def keyup (key: i32) (s: state) =
  if key == SDLK_RIGHT then s with moving.1 = 0
  else if key == SDLK_LEFT then s with moving.1 = 0
  else if key == SDLK_UP then s with moving.0 = 0
  else if key == SDLK_DOWN then s with moving.0 = 0
  else s

def move (x: i64, y: i64) (dx,dy) = (x+dx, y+dy)
def diff (x1: i64, y1: i64) (x2, y2) = (x2 - x1, y2 - y1)

entry render (s: state) =
  frame s.program {x=s.rot.x, y=s.rot.y} s.w s.h s.time

entry key (e: i32) (key: i32) (s: state): state =
  if e == 0 then keydown key s else keyup key s

entry mouse (buttons: i32) (y: i32) (x: i32) (s: state): state =
  s with mouse = {x, y}
    with rot = if buttons != 0
               then {x = s.rot.x - (f32.i32 (x-s.mouse.x) / f32.i64 s.w),
                     y = s.rot.y + (f32.i32 (y-s.mouse.y) / f32.i64 s.h)}
               else s.rot

entry wheel (_dx: i32) (dy: i32) (s: state): state =
  s with radius = i64.max 0 (s.radius + i64.i32 dy)

entry step (td: f32) (s: state): state =
  s with time = s.time + td

entry resize (h: i64) (w: i64) (s: state) : state =
  s with h = h with w = w

entry init (program: []u32) (_seed: u32) (h: i64) (w: i64): state =
  {time = 0, w, h,
   moving = (0,0),
   mouse = {x=0,y=0},
   rot = {x=0,y=0},
   radius = 20,
   paused = false,
   program = decode program
}
