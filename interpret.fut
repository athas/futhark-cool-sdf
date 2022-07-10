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
                | #over i32

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
  val exec [k][n] 'v : (var: v -> P.t) -> [k]P.t -> (top: i32) -> [n](inst v P.t) -> [k]P.t
} = {
  def exec [k][n] 'v (var: v -> P.t) (stack: [k]P.t) (top: i32) (code: [n](inst v P.t)) =
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
          (top-1, stack with [top-2] = copy (stack[top-2] P.+ stack[top-1]))
        case #sub ->
          (top-1, stack with [top-2] = copy (stack[top-2] P.- stack[top-1]))
        case #mul ->
          (top-1, stack with [top-2] = copy (stack[top-2] P.* stack[top-1]))
        case #div ->
          (top-1, stack with [top-2] = copy (stack[top-2] P./ stack[top-1]))
        case #cos ->
          (top, stack with [top-1] = copy (P.cos stack[top-1]))
        case #sin ->
          (top, stack with [top-1] = copy (P.sin stack[top-1]))
    in stack'
}
