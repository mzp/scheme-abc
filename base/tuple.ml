module T2 = struct
  type ('a,'b) t = 'a * 'b
  let map1 f (x,y) =
    (f x,y)
  let map2 f (x,y) =
    (x,f y)
  let fst =
    Pervasives.fst
  let snd =
    Pervasives.snd
end

module T3 = struct
  type ('a,'b,'c) t = 'a * 'b * 'c
  let map1 f (x,y,z) =
    (f x,y,z)
  let map2 f (x,y,z) =
    (x,f y,z)
  let map3 f (x,y,z) =
    (x,y,f z)

  let fst (a,_,_) =
    a
  let snd (_,b,_) =
    b
  let trd (_,_,c) =
    c
end
