(* --- Initialize board --- *)
val board = Board.initiate_standard_chess ()

(* --- Print board --- *)
val _ = print "--- Starting Board ---\n"
val _ = Board.print_bit_map ()

(* --- Generate moves --- *)
val moves = MoveGenerator.generate_move_order board

(* --- Print all moves --- *)
val _ = print "\n=== Generated Moves ===\n"
val _ = List.app (fn ((fromR,fromC),(toR,toC),w) =>
                    print ( "(" ^ Int.toString fromR ^ "," ^ Int.toString fromC ^ ") -> " ^
                            "(" ^ Int.toString toR ^ "," ^ Int.toString toC ^ "), weight=" ^
                            Real.toString w ^ "\n")
                 ) moves

val board_2 = Array.fromList [
    Array.fromList [#"r",#" ",#"b",#"q",#"k",#"b",#" ",#"r"],
    Array.fromList [#"p",#"p",#"p",#" ",#" ",#"p",#"p",#"p"],
    Array.fromList [#" ",#"n",#" ",#" ",#"p",#" ",#" ",#" "],
    Array.fromList [#" ",#" ",#" ",#"p",#" ",#" ",#" ",#" "],
    Array.fromList [#" ",#" ",#"P",#" ",#"P",#" ",#" ",#" "],
    Array.fromList [#" ",#" ",#" ",#"N",#" ",#" ",#" ",#" "],
    Array.fromList [#"P",#"P",#" ",#" ",#" ",#"P",#"P",#"P"],
    Array.fromList [#"R",#" ",#"B",#"Q",#"K",#"B",#" ",#"R"]
]

val _ = print "--- Mid Board ---\n"
val _ = Board.print_board(Board.board_to_bitmaps board_2)

val moves = MoveGenerator.generate_move_order (Board.board_to_bitmaps board_2)
val _ = print "\n=== Generated Moves ===\n"
val _ = List.app (fn ((fromR,fromC),(toR,toC),w) =>
                    print ( "(" ^ Int.toString fromR ^ "," ^ Int.toString fromC ^ ") -> " ^
                            "(" ^ Int.toString toR ^ "," ^ Int.toString toC ^ "), weight=" ^
                            Real.toString w ^ "\n")
                 ) moves


val board_3 = Array.fromList [
    Array.fromList [#" ",#" ",#" ",#" ",#"k",#" ",#" ",#" "],
    Array.fromList [#" ",#" ",#" ",#" ",#"p",#" ",#" ",#" "],
    Array.fromList [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
    Array.fromList [#" ",#" ",#" ",#"P",#" ",#" ",#" ",#" "],
    Array.fromList [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
    Array.fromList [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
    Array.fromList [#" ",#" ",#" ",#" ",#"P",#" ",#" ",#" "],
    Array.fromList [#" ",#" ",#" ",#" ",#"K",#" ",#" ",#" "]
]

val _ = print "--- End Board ---\n"
val _ = Board.print_board(Board.board_to_bitmaps board_3)

val moves = MoveGenerator.generate_move_order (Board.board_to_bitmaps board_3)
val _ = print "\n=== Generated Moves ===\n"
val _ = List.app (fn ((fromR,fromC),(toR,toC),w) =>
                    print ( "(" ^ Int.toString fromR ^ "," ^ Int.toString fromC ^ ") -> " ^
                            "(" ^ Int.toString toR ^ "," ^ Int.toString toC ^ "), weight=" ^
                            Real.toString w ^ "\n")
                 ) moves