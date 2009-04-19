#define FOLD(EXPR) ('a -> ([> 'b EXPR]) -> 'a) ->  ('a -> [> 'd EXPR] -> 'e) -> ('a -> 'b -> 'd) -> 'a -> 'b EXPR -> 'e

#define LIFT_NOREC_1(STMT) ('a -> 'b) -> [< 'a STMT ] -> [> 'b STMT ]
#define LIFT_NOREC_2(STMT) ('a -> 'b) -> [< ('a,'c) STMT ] -> [> ('b,'c) STMT ]
#define LIFT_2(STMT) ('a -> 'b) ->  ('c -> 'd) ->  [< ('a,'c) STMT ] -> [> ('b,'d) STMT ]

#define FOLD_STMT_NOREC_1(STMT) ('a -> [> 'b STMT] -> 'a) -> ('a -> [> 'b STMT] -> 'e) -> 'a -> 'b STMT -> 'e
#define FOLD_STMT_NOREC_2(STMT) ('a -> [> ('b,'c) STMT] -> 'a) -> ('a -> [> ('b,'c) STMT] -> 'e) -> 'a -> ('b,'c) STMT -> 'e
#define FOLD_STMT_2(STMT) ('a -> [> ('b,'c) STMT ] -> 'a) -> ('a -> [> ('b,'d) STMT ] -> 'e) ->  ('a -> 'c -> 'd) -> 'a -> ('b, 'c) STMT -> 'e
