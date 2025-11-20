

(*--- Initialize board --- *)
val board = Board.initiate_standard_chess ()

(* --- Print board --- *)
val _ = print "--- Starting Board ---\n"
val _ = Board.print_bit_map ()

(* --- Generate moves --- *)
val moves = MoveGenerator.generate_move_order board

(* --- Print all moves --- *)
val _ = print "\n=== Generated Moves ===\n"
val _ = List.app (fn ((fromR,fromC),(toR,toC)) =>
                    print ( "(" ^ Int.toString fromR ^ "," ^ Int.toString fromC ^ ") -> " ^
                            "(" ^ Int.toString toR ^ "," ^ Int.toString toC ^ ")\n")
                 ) moves

val board_2 = Array2.fromList [
    [#"r",#" ",#"b",#"q",#"k",#"b",#" ",#"r"],
    [#"p",#"p",#"p",#" ",#" ",#"p",#"p",#"p"],
    [#" ",#"n",#" ",#" ",#"p",#" ",#" ",#" "],
    [#" ",#" ",#" ",#"p",#" ",#" ",#" ",#" "],
    [#" ",#" ",#"P",#" ",#"P",#" ",#" ",#" "],
    [#" ",#" ",#" ",#"N",#" ",#" ",#" ",#" "],
    [#"P",#"P",#" ",#" ",#" ",#"P",#"P",#"P"],
    [#"R",#" ",#"B",#"Q",#"K",#"B",#" ",#"R"]
]

val _ = print "--- Mid Board ---\n"
val _ = Board.print_board(Board.board_representation board_2)

val moves = MoveGenerator.generate_move_order (Board.board_representation board_2)
val _ = print "\n=== Generated Moves ===\n"
val _ = List.app (fn ((fromR,fromC),(toR,toC)) =>
                    print ( "(" ^ Int.toString fromR ^ "," ^ Int.toString fromC ^ ") -> " ^
                            "(" ^ Int.toString toR ^ "," ^ Int.toString toC ^ ")\n")
                 ) moves


val board_3 = Array2.fromList [
    [#" ",#" ",#" ",#" ",#"k",#" ",#" ",#" "],
    [#" ",#" ",#" ",#" ",#"p",#" ",#" ",#" "],
    [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
    [#" ",#" ",#" ",#"P",#" ",#" ",#" ",#" "],
    [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
    [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
    [#" ",#" ",#" ",#" ",#"P",#" ",#" ",#" "],
    [#" ",#" ",#" ",#" ",#"K",#" ",#" ",#" "]
]

val _ = print "--- End Board ---\n"
val _ = Board.print_board(Board.board_representation board_3)

val moves = MoveGenerator.generate_move_order (Board.board_representation board_3)
val _ = print "\n=== Generated Moves ===\n"
val _ = List.app (fn ((fromR,fromC),(toR,toC)) =>
                    print ( "(" ^ Int.toString fromR ^ "," ^ Int.toString fromC ^ ") -> " ^
                            "(" ^ Int.toString toR ^ "," ^ Int.toString toC ^ ")\n")
                 ) moves



