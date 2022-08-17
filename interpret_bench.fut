import "interpret"

module stack = {
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

-- ==
-- entry: bench_byhand_grad bench_interpreter_fwd bench_interpreter_rev
-- random input { [1000000]f32 [1000000]f32 [1000000]f32 }

entry bench_interpreter_fwd a b c =
  map3 (\x y z -> (jvp (\(x, y, z) -> stack.radius_at x y z) (x,y,z) (1,0,0),
                   jvp (\(x, y, z) -> stack.radius_at x y z) (x,y,z) (0,1,0),
                   jvp (\(x, y, z) -> stack.radius_at x y z) (x,y,z) (0,0,1))) a b c
  |> unzip3

entry bench_interpreter_rev a b c =
  map3 (\x y z -> vjp (\(x, y, z) -> stack.radius_at x y z) (x,y,z) 1) a b c
  |> unzip3

entry bench_byhand_grad a b c =
  map3 (\x y z -> vjp (\(x, y, z) -> byhand.radius_at x y z) (x,y,z) 1) a b c
  |> unzip3
