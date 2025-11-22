(* --- Initialize board --- *)
val board = Board.initiate_standard_chess ()


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




(* ============================================================= *)
(* === PieceList BOARD TESTS (start, middle, end) ============== *)
(* ============================================================= *)

val _ = print "\n\n========== PieceList Tests ==========\n";

(* --- START BOARD --- *)
val _ = print "\n--- PieceList: Standard Board ---\n";
val (w_start, b_start) = PieceList.initiate_standard_chess ();
val board_start = PieceList.print_board (w_start, b_start);

val moves = MoveGeneratorPL.generate_move_order (w_start, b_start);
val _ = print "\n=== Generated Moves ===\n"
val _ = List.app (fn ((fromR,fromC),(toR,toC),w) =>
                    print ( "(" ^ Int.toString fromR ^ "," ^ Int.toString fromC ^ ") -> " ^
                            "(" ^ Int.toString toR ^ "," ^ Int.toString toC ^ "), weight=" ^
                            Real.toString w ^ "\n")
                 ) moves


(* --- MID BOARD --- *)
val board_mid = Array.fromList [
    Array.fromList [#"r",#" ",#"b",#"q",#"k",#"b",#" ",#"r"],
    Array.fromList [#"p",#"p",#"p",#" ",#" ",#"p",#"p",#"p"],
    Array.fromList [#" ",#"n",#" ",#" ",#"p",#" ",#" ",#" "],
    Array.fromList [#" ",#" ",#" ",#"p",#" ",#" ",#" ",#" "],
    Array.fromList [#" ",#" ",#"P",#" ",#"P",#" ",#" ",#" "],
    Array.fromList [#" ",#" ",#" ",#"N",#" ",#" ",#" ",#" "],
    Array.fromList [#"P",#"P",#" ",#" ",#" ",#"P",#"P",#"P"],
    Array.fromList [#"R",#" ",#"B",#"Q",#"K",#"B",#" ",#"R"]
];

val (w_mid, b_mid) = PieceList.array_to_piecelist board_mid;

val _ = print "\n--- PieceList: Mid Board ---\n";
val board_mid = PieceList.print_board (w_mid, b_mid);

val moves = MoveGeneratorPL.generate_move_order (w_mid, b_mid);
val _ = print "\n=== Generated Moves ===\n"
val _ = List.app (fn ((fromR,fromC),(toR,toC),w) =>
                    print ( "(" ^ Int.toString fromR ^ "," ^ Int.toString fromC ^ ") -> " ^
                            "(" ^ Int.toString toR ^ "," ^ Int.toString toC ^ "), weight=" ^
                            Real.toString w ^ "\n")
                 ) moves

                            (* --- END BOARD --- *)
val board_end = Array.fromList [
    Array.fromList [#" ",#" ",#" ",#" ",#"k",#" ",#" ",#" "],
    Array.fromList [#" ",#" ",#" ",#" ",#"p",#" ",#" ",#" "],
    Array.fromList [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
    Array.fromList [#" ",#" ",#" ",#"P",#" ",#" ",#" ",#" "],
    Array.fromList [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
    Array.fromList [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
    Array.fromList [#" ",#" ",#" ",#" ",#"P",#" ",#" ",#" "],
    Array.fromList [#" ",#" ",#" ",#" ",#"K",#" ",#" ",#" "]
];

val (w_end, b_end) = PieceList.array_to_piecelist board_end;

val _ = print "\n--- PieceList: End Board ---\n";
val board_end = PieceList.print_board (w_end, b_end);

val moves = MoveGeneratorPL.generate_move_order (w_end, b_end);
val _ = print "\n=== Generated Moves ===\n"
val _ = List.app (fn ((fromR,fromC),(toR,toC),w) =>
                    print ( "(" ^ Int.toString fromR ^ "," ^ Int.toString fromC ^ ") -> " ^
                            "(" ^ Int.toString toR ^ "," ^ Int.toString toC ^ "), weight=" ^
                            Real.toString w ^ "\n")
                 ) moves

(* ============================================================= *)
(* === End of PieceList Tests ================================= *)
(* ============================================================= *)

fun test_move_generation ()=
    let
        val board = Board.initiate_standard_chess()

        (* Pawn Move *)
        val white_pawn_move = MoveGenerator.apply_move board ((6,4),(4,4))
        val _ = Board.print_board(white_pawn_move)

        val black_pawn_move = MoveGenerator.apply_move white_pawn_move ((1,3),(3,3))
        val _ = Board.print_board(black_pawn_move)

        val white_pawn_move = MoveGenerator.apply_move black_pawn_move ((4,4),(3,3))
        val _ = Board.print_board(white_pawn_move)

        val black_pawn_move = MoveGenerator.apply_move white_pawn_move ((0,2),(5,7))
        val _ = Board.print_board(black_pawn_move)

        val white_pawn_move = MoveGenerator.apply_move black_pawn_move ((3,3),(2,3))
        val _ = Board.print_board(white_pawn_move)

        val black_pawn_move = MoveGenerator.apply_move white_pawn_move ((1,2),(2,3))
        val _ = Board.print_board(black_pawn_move)

        (* val pawn_move = MoveGenerator.apply_move board ((6,4),(4,4))
        val _ = Board.print_board(pawn_move) *)
    in
        ()
    end


val _ =test_move_generation()
