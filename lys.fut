import "lib/github.com/diku-dk/lys/lys"
import "lib/github.com/athas/vector/vspace"

type inst 'a = #const a
             | #dup
             | #pop
             | #swap
             | #add
             | #sub
             | #mul
             | #div
             | #cos
             | #sin
             | #sqrt

local module type num = {
  type t
  val i64 : i64 -> t
  val + : t -> t -> t
  val - : t -> t -> t
  val * : t -> t -> t
  val / : t -> t -> t
  val cos : t -> t
  val sin : t -> t
  val sqrt : t -> t
}

module mk_eval(P: num) : {
  val exec [k][n] : [k]P.t -> (top: i64) -> [n](inst P.t) -> [k]P.t
} = {
  def exec [k][n] (stack: [k]P.t) (top: i64) (code: [n](inst P.t)) =
    let (_, stack') =
      loop (top, stack) = (top, copy stack)
      for inst in code do
        #[unsafe]
        match inst
        case #const v ->
          (top+1, stack with [top] = v)
        case #dup ->
          (top+1, stack with [top] = copy (stack[top-1]))
        case #pop ->
          (top-1, stack)
        case #swap ->
          let (a,b) = copy (stack[top-1], stack[top-2])
          in (top, stack with [top-2] = a with [top-1] = b)
        case #add ->
          (top-1, stack with [top-2] = copy (stack[top-1] P.+ stack[top-2]))
        case #sub ->
          (top-1, stack with [top-2] = copy (stack[top-1] P.- stack[top-2]))
        case #mul ->
          (top-1, stack with [top-2] = copy (stack[top-1] P.* stack[top-2]))
        case #div ->
          (top-1, stack with [top-2] = copy (stack[top-1] P./ stack[top-2]))
        case #cos ->
          (top, stack with [top-1] = copy (P.cos stack[top-1]))
        case #sin ->
          (top, stack with [top-1] = copy (P.sin stack[top-1]))
    in stack'
}

module vec3 = mk_vspace_3d f32
type vec3 = vec3.vector

def uv (p: vec3) : (f32,f32) =
  let d = vec3.normalise p
  in (0.5 + f32.atan2 d.x d.z / (2*f32.pi),
      0.5 + f32.asin d.y / f32.pi)

module evalf32 = mk_eval f32

def radius_at (t: f32) (p: vec3) : f32 =
  let (u,v) = uv p
  let code : [](inst f32) = [#swap,
                             #pop,
                             #const 20,
                             #mul,
                             #const f32.pi,
                             #mul,
                             #add,
                             #sin,
                             #const 1,
                             #add,
                             #const 0.5,
                             #mul]
  let k = 3
  in head (evalf32.exec (replicate k 0 with [0:3] = [t,u,v]) 3 code)

def sdf (t: f32) (p: vec3) : f32 =
  vec3.norm p - radius_at t p

type hit = #hit vec3 | #miss

def trace t (orig: vec3) (dir: vec3) : hit =
  let not_done (i, _) = i < 128
  let march (i, pos) =
    let d = sdf t pos
    in if d <= 0
       then (1337, pos)
       else (i + 1, pos vec3.+ ((f32.max (d*0.1) 0.01) `vec3.scale` dir))
  in iterate_while not_done march (0,orig)
     |> \(i, hit) -> if i == 1337 then #hit hit else #miss

def grad f x = vjp f x 1f32

def distance_field_normal t pos =
  vec3.normalise (grad (sdf t) pos)

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

def frame (width: i64) (height: i64) (t: f32) =
  let f j i =
    let dist = 3
    let origin = dist `vec3.scale` (t `vec3.rot_x` vec3.normalise {x=1,y=1,z= -3})
    let dir = camera_ray origin width height i j
    in match trace t origin dir
       case #miss ->
         0xFFFFFF
       case #hit hit ->
         let light_dir = vec3.normalise ({x=10, y=10, z=10} vec3.- hit)
         let light_intensity = light_dir `vec3.dot` distance_field_normal t hit
         in grey light_intensity
  in tabulate_2d height width f

type state = {time: f32, h: i64, w: i64,
              moving: (i64, i64),
              mouse: (i64, i64),
              radius: i64,
              paused: bool
             }

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
  frame s.w s.h s.time

entry key (e: i32) (key: i32) (s: state): state =
  if e == 0 then keydown key s else keyup key s

entry mouse (_buttons: i32) (x: i32) (y: i32) (s: state): state =
  s with mouse = (i64.i32 y, i64.i32 x)

entry wheel (_dx: i32) (dy: i32) (s: state): state =
  s with radius = i64.max 0 (s.radius + i64.i32 dy)

entry step (td: f32) (s: state): state =
  s with time = s.time + td

entry resize (h: i64) (w: i64) (s: state) : state =
  s with h = h with w = w

entry init (_seed: u32) (h: i64) (w: i64): state =
  {time = 0, w, h,
   moving = (0,0),
   mouse = (0,0),
   radius = 20,
   paused = false
  }

