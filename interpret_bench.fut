import "interpret"

module stack = {
  type inst 'v 'a = #const a
                  | #var v
                  | #pop
                  | #swap
                  | #add
                  | #sub
                  | #mul
                  | #div
                  | #cos
                  | #sin
                  | #over i64

  local module type num = {
    type t
    val i64 : i64 -> t
    val + : t -> t -> t
    val - : t -> t -> t
    val * : t -> t -> t
    val / : t -> t -> t
    val cos : t -> t
    val sin : t -> t
  }

  module mk_eval(P: num) : {
    val exec [k][n] 'v : (var: v -> P.t) -> [k]P.t -> (top: i64) -> [n](inst v P.t) -> [k]P.t
  } = {
    def exec [k][n] 'v (var: v -> P.t) (stack: [k]P.t) (top: i64) (code: [n](inst v P.t)) =
      let (_, stack') =
        loop (top, stack) = (top, copy stack)
        for inst in code do
          #[unsafe]
          match inst
          case #const v ->
            (top+1, stack with [top] = v)
          case #var v ->
            (top+1, stack with [top] = var v)
          case #over i ->
            (top+1, stack with [top] = copy (stack[top-i]))
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

  module evalf32 = mk_eval f32

  type var = #t | #u | #v

  def radius_at t u v : f32 =
    let code : [](inst var f32) =
      [#var #v,            -- V
       #const (20*f32.pi), -- V K
       #mul,               -- V'
       #var #t,            -- V' T
       #add,               -- V'
       #cos,               -- V'
       #var #t,            -- V' T
       #sin,               -- V' T'
       #mul,               -- V'
       #const 1,           -- V' 1
       #add,               -- V'
       #const 0.5,         -- V' 0.5
       #mul,               -- V'

       #var #u,            -- V' U
       #const (20*f32.pi), -- V' U K
       #mul,               -- V' U'
       #var #t,            -- V' U' T
       #add,               -- V' U'
       #sin,               -- V' U'
       #var #t,            -- V' U' T
       #sin,               -- V' U' T'
       #mul,               -- V' U'
       #const 1,           -- V' U' 1
       #add,               -- V' U'
       #const 0.5,         -- V' U' 0.5
       #mul,               -- V' U'
       #add                -- VU
      ]
    let k = 3
    let var x = match x case #t -> t
                        case #u -> u
                        case #v -> v
    in (evalf32.exec var (replicate k 0) 0 code)[0]
}


module evalf32 = mk_eval f32

type var = #t | #u | #v

def radius_at t u v : f32 =
  let code : [](inst var f32) =
    [#var #v,            -- V
     #const (20*f32.pi), -- V K
     #mul,               -- V'
     #var #t,            -- V' T
     #add,               -- V'
     #cos,               -- V'
     #var #t,            -- V' T
     #sin,               -- V' T'
     #mul,               -- V'
     #const 1,           -- V' 1
     #add,               -- V'
     #const 0.5,         -- V' 0.5
     #mul,               -- V'

     #var #u,            -- V' U
     #const (20*f32.pi), -- V' U K
     #mul,               -- V' U'
     #var #t,            -- V' U' T
     #add,               -- V' U'
     #sin,               -- V' U'
     #var #t,            -- V' U' T
     #sin,               -- V' U' T'
     #mul,               -- V' U'
     #const 1,           -- V' U' 1
     #add,               -- V' U'
     #const 0.5,         -- V' U' 0.5
     #mul,               -- V' U'
     #add                -- VU
    ]
  let k = 3
  let var x = match x case #t -> t
                      case #u -> u
                      case #v -> v
  in (evalf32.exec var (replicate k 0) 0 code)[0]

module byhand = {
  def radius_at t u v =
    (1+f32.sin(u*20*f32.pi+t)*f32.sin(t))/2 +
    (1+f32.cos(v*20*f32.pi+t)*f32.sin(t))/2
}

entry bench_interpreter = map3 stack.radius_at

entry bench_byhand = map3 byhand.radius_at

-- ==
-- entry: bench_interpreter bench_byhand
-- random input { [1000000]f32 [1000000]f32 [1000000]f32 }
