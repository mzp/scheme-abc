#define FOLD(EXPR) ('a -> ([> 'b EXPR]) -> 'a) ->  ('a -> [> 'd EXPR] -> 'e) -> ('a -> 'b -> 'd) -> 'a -> 'b EXPR -> 'e

#define LIFT_NOREC(STMT) ('a -> 'b) -> [< ('a,'c) STMT ] -> [> ('b,'c) STMT ]
#define LIFT(STMT) ('a -> 'b) ->  ('c -> 'd) ->  [< ('a,'c) STMT ] -> [> ('b,'d) STMT ]

#define FOLD_STMT_NOREC(STMT) ('a -> [> ('b,'c) STMT] -> 'a) -> ('a -> [> ('b,'c) STMT] -> 'e) -> 'a -> ('b,'c) STMT -> 'e
#define FOLD_STMT(STMT) ('a -> [> ('b,'c) STMT ] -> 'a) -> ('a -> [> ('b,'d) STMT ] -> 'e) ->  ('a -> 'c -> 'd) -> 'a -> ('b, 'c) STMT -> 'e
