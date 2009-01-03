module T2 :
  sig
    type ('a,'b) t = 'a * 'b
    val map1 : ('a -> 'c) -> ('a,'b) t -> ('c,'b) t
    val map2 : ('b -> 'c) -> ('a,'b) t -> ('a,'c) t	
  end

module T3 :
  sig
    type ('a,'b,'c) t = 'a * 'b * 'c
    val map1 : ('a -> 'd) -> ('a,'b,'c) t -> ('d,'b,'c) t
    val map2 : ('b -> 'd) -> ('a,'b,'c) t -> ('a,'d,'c) t
    val map3 : ('c -> 'd) -> ('a,'b,'c) t -> ('a,'b,'d) t
  end
