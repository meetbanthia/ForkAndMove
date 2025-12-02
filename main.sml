fun bitmaps_test () =
    let 
        (* Helper to print a header for each test *)
        fun print_header msg = 
            print ("\n========================================\n" ^ msg ^ "\n========================================\n")

        (* Helper to convert move coords to algebraic string (e.g. "e2-e4") *)
        fun move_to_string ((r1,c1),(r2,c2)) =
            let
                fun row_to_rank r = Int.toString (8 - r) (* Row 0 -> Rank 8, Row 7 -> Rank 1 *)
                fun col_to_file c = String.str (Char.chr (c + 97)) (* Col 0 -> 'a', Col 7 -> 'h' *)
            in
                (col_to_file c1) ^ (row_to_rank r1) ^ "-" ^ (col_to_file c2) ^ (row_to_rank r2)
            end



        fun test_scholars_mate () =
            let
                val _ = print_header "TEST 1: Scholar's Mate Sequence (Pawn, Bishop, Queen, Capture)"
                val b0 = Board.initiate_standard_chess()
                val _ = Board.print_board(b0)

                (* 1. e4 *)
                val _ = print "\n--- 1. e4 ((6,4) -> (4,4)) ---\n"
                val b1 = MoveGenerator.apply_move b0 ((6,4),(4,4))
                val _ = Board.print_board(b1)

                (* 1... e5 *)
                val _ = print "\n--- 1... e5 ((1,4) -> (3,4)) ---\n"
                val b2 = MoveGenerator.apply_move b1 ((1,4),(3,4))
                val _ = Board.print_board(b2)

                (* 2. Bc4 *)
                val _ = print "\n--- 2. Bc4 ((7,5) -> (4,2)) ---\n"
                val b3 = MoveGenerator.apply_move b2 ((7,5),(4,2))
                val _ = Board.print_board(b3)

                (* 2... Nc6 *)
                val _ = print "\n--- 2... Nc6 ((0,1) -> (2,2)) ---\n"
                val b4 = MoveGenerator.apply_move b3 ((0,1),(2,2))
                val _ = Board.print_board(b4)

                (* 3. Qh5 *)
                val _ = print "\n--- 3. Qh5 ((7,3) -> (3,7)) ---\n"
                val b5 = MoveGenerator.apply_move b4 ((7,3),(3,7))
                val _ = Board.print_board(b5)

                (* 3... Nf6 *)
                val _ = print "\n--- 3... Nf6 ((0,6) -> (2,5)) ---\n"
                val b6 = MoveGenerator.apply_move b5 ((0,6),(2,5))
                val _ = Board.print_board(b6)

                (* 4. Qxf7# *)
                val _ = print "\n--- 4. Qxf7# ((3,7) -> (1,5)) [Capture] ---\n"
                val b7 = MoveGenerator.apply_move b6 ((3,7),(1,5))
                val _ = Board.print_board(b7)
            in
                ()
            end

        fun test_rook_maneuvers () =
            let
                val _ = print_header "TEST 2: Rook Maneuvers (Pawn opening for Rook file)"
                val b0 = Board.initiate_standard_chess()
                val _ = Board.print_board(b0)
                
                (* 1. a4 *)
                val _ = print "\n--- 1. a4 ((6,0) -> (4,0)) ---\n"
                val b1 = MoveGenerator.apply_move b0 ((6,0),(4,0))
                val _ = Board.print_board(b1)

                (* 1... h5 *)
                val _ = print "\n--- 1... h5 ((1,7) -> (3,7)) ---\n"
                val b2 = MoveGenerator.apply_move b1 ((1,7),(3,7))
                val _ = Board.print_board(b2)

                (* 2. Ra3 *)
                val _ = print "\n--- 2. Ra3 ((7,0) -> (5,0)) ---\n"
                val b3 = MoveGenerator.apply_move b2 ((7,0),(5,0))
                val _ = Board.print_board(b3)

                (* 2... Rh6 *)
                val _ = print "\n--- 2... Rh6 ((0,7) -> (2,7)) ---\n"
                val b4 = MoveGenerator.apply_move b3 ((0,7),(2,7))
                val _ = Board.print_board(b4)
                
                (* 3. Rb3 *)
                val _ = print "\n--- 3. Rb3 ((5,0) -> (5,1)) ---\n"
                val b5 = MoveGenerator.apply_move b4 ((5,0),(5,1))
                val _ = Board.print_board(b5)
            in
                ()
            end

        fun test_knight_captures () =
            let
                val _ = print_header "TEST 3: Knight Captures"
                val b0 = Board.initiate_standard_chess()
                
                (* Setup: 1. e4 d5 *)
                val _ = print "\n--- Setup: 1. e4 d5 ---\n"
                val b1 = MoveGenerator.apply_move b0 ((6,4),(4,4))
                val b2 = MoveGenerator.apply_move b1 ((1,3),(3,3))
                val _ = Board.print_board(b2)

                (* 2. Nc3 *)
                val _ = print "\n--- 2. Nc3 ((7,1) -> (5,2)) ---\n"
                val b3 = MoveGenerator.apply_move b2 ((7,1),(5,2))
                val _ = Board.print_board(b3)
                
                (* 2... dxe4 (Black pawn captures white pawn) *)
                val _ = print "\n--- 2... dxe4 ((3,3) -> (4,4)) [Pawn Capture] ---\n"
                val b4 = MoveGenerator.apply_move b3 ((3,3),(4,4))
                val _ = Board.print_board(b4)

                (* 3. Nxe4 (Knight captures pawn) *)
                val _ = print "\n--- 3. Nxe4 ((5,2) -> (4,4)) [Knight Capture] ---\n"
                val b5 = MoveGenerator.apply_move b4 ((5,2),(4,4))
                val _ = Board.print_board(b5)
            in
                ()
            end

        fun test_bishop_fianchetto () =
            let
                val _ = print_header "TEST 4: Bishop Fianchetto & Blockers"
                val b0 = Board.initiate_standard_chess()
                
                (* 1. g3 *)
                val _ = print "\n--- 1. g3 ((6,6) -> (5,6)) ---\n"
                val b1 = MoveGenerator.apply_move b0 ((6,6),(5,6))
                val _ = Board.print_board(b1)

                (* 1... b6 *)
                val _ = print "\n--- 1... b6 ((1,1) -> (2,1)) ---\n"
                val b2 = MoveGenerator.apply_move b1 ((1,1),(2,1))
                val _ = Board.print_board(b2)

                (* 2. Bg2 (Fianchetto) *)
                val _ = print "\n--- 2. Bg2 ((7,5) -> (6,6)) ---\n"
                val b3 = MoveGenerator.apply_move b2 ((7,5),(6,6))
                val _ = Board.print_board(b3)

                (* 2... Bb7 (Fianchetto) *)
                val _ = print "\n--- 2... Bb7 ((0,2) -> (1,1)) ---\n"
                val b4 = MoveGenerator.apply_move b3 ((0,2),(1,1))
                val _ = Board.print_board(b4)

                (* 3. Bxb7 (Long range capture) *)
                val _ = print "\n--- 3. Bxb7 ((6,6) -> (1,1)) [Long Range Capture] ---\n"
                val b5 = MoveGenerator.apply_move b4 ((6,6),(1,1))
                val _ = Board.print_board(b5)
            in
                ()
            end

        fun test_queen_mixed () =
            let
                val _ = print_header "TEST 5: Queen Centralization"
                val b0 = Board.initiate_standard_chess()

                (* 1. d4 *)
                val _ = print "\n--- 1. d4 ((6,3) -> (4,3)) ---\n"
                val b1 = MoveGenerator.apply_move b0 ((6,3),(4,3))
                val _ = Board.print_board(b1)

                (* 1... d5 *)
                val _ = print "\n--- 1... d5 ((1,3) -> (3,3)) ---\n"
                val b2 = MoveGenerator.apply_move b1 ((1,3),(3,3))
                val _ = Board.print_board(b2)

                (* 2. Qd3 *)
                val _ = print "\n--- 2. Qd3 ((7,3) -> (5,3)) ---\n"
                val b3 = MoveGenerator.apply_move b2 ((7,3),(5,3))
                val _ = Board.print_board(b3)

                (* 2... e5 *)
                val _ = print "\n--- 2... e5 ((1,4) -> (3,4)) ---\n"
                val b4 = MoveGenerator.apply_move b3 ((1,4),(3,4))
                val _ = Board.print_board(b4)

                (* 3. Qb5+ (Check is not validated, but move is legal for piece) *)
                val _ = print "\n--- 3. Qb5+ ((5,3) -> (3,1)) [Diagonal] ---\n"
                val b5 = MoveGenerator.apply_move b4 ((5,3),(3,1))
                val _ = Board.print_board(b5)
                
                (* 3... c6 (Blocking - we just simulate move, not validate block) *)
                val _ = print "\n--- 3... c6 ((1,2) -> (2,2)) ---\n"
                val b6 = MoveGenerator.apply_move b5 ((1,2),(2,2))
                val _ = Board.print_board(b6)
                
                (* 4. Qxb7 (Capture) *)
                val _ = print "\n--- 4. Qxb7 ((3,1) -> (1,1)) [Capture] ---\n"
                val b7 = MoveGenerator.apply_move b6 ((3,1),(1,1))
                val _ = Board.print_board(b7)
            in
                ()
            end

        fun test_king_walk () =
            let
                val _ = print_header "TEST 6: King Walk (Bongcloud style)"
                val b0 = Board.initiate_standard_chess()

                (* 1. e4 *)
                val _ = print "\n--- 1. e4 ((6,4) -> (4,4)) ---\n"
                val b1 = MoveGenerator.apply_move b0 ((6,4),(4,4))
                val _ = Board.print_board(b1)

                (* 1... e5 *)
                val _ = print "\n--- 1... e5 ((1,4) -> (3,4)) ---\n"
                val b2 = MoveGenerator.apply_move b1 ((1,4),(3,4))
                val _ = Board.print_board(b2)

                (* 2. Ke2 *)
                val _ = print "\n--- 2. Ke2 ((7,4) -> (6,4)) ---\n"
                val b3 = MoveGenerator.apply_move b2 ((7,4),(6,4))
                val _ = Board.print_board(b3)

                (* 2... d6 *)
                val _ = print "\n--- 2... d6 ((1,3) -> (2,3)) ---\n"
                val b4 = MoveGenerator.apply_move b3 ((1,3),(2,3))
                val _ = Board.print_board(b4)

                (* 3. Ke3 *)
                val _ = print "\n--- 3. Ke3 ((6,4) -> (5,4)) ---\n"
                val b5 = MoveGenerator.apply_move b4 ((6,4),(5,4))
                val _ = Board.print_board(b5)
            in
                ()
            end

        fun test_pawn_promotion () =
            let
                val _ = print_header "TEST 7: Pawn Promotion (Auto-Queen)"
                val b0 = Board.initiate_standard_chess()

                (* Move a2 pawn all the way to a8. Since apply_move doesn't validate legality, we can just 'teleport' or move quickly *)
                (* Step 1: a2 -> a7 *)
                val _ = print "\n--- Move Pawn a2 -> a7 ((6,0) -> (1,0)) [Setup] ---\n"
                val b1 = MoveGenerator.apply_move b0 ((6,0),(1,0))
                val _ = Board.print_board(b1)

                (* Step 2: a7 -> a8 (Promotion!) *)
                val _ = print "\n--- Move Pawn a7 -> a8 ((1,0) -> (0,1)) [Promotion] ---\n"
                val b2 = MoveGenerator.apply_move b1 ((1,0),(0,1))
                val _ = Board.print_board(b2)

                (* Test Black Promotion *)
                (* Move h7 -> h2 *)
                val _ = print "\n--- Move Pawn h7 -> h2 ((1,2) -> (6,2)) [Setup] ---\n"
                val b3 = MoveGenerator.apply_move b2 ((1,2),(6,2))
                val _ = Board.print_board(b3)

                (* Move h2 -> h1 (Promotion!) *)
                val _ = print "\n--- Move Pawn h2 -> h1 ((6,2) -> (7,1)) [Promotion] ---\n"
                val b4 = MoveGenerator.apply_move b3 ((6,2),(7,1))
                val _ = Board.print_board(b4)
            in
                ()
            end

        (* Run all tests *)
        val _ = test_scholars_mate()
        val _ = test_rook_maneuvers()
        val _ = test_knight_captures()
        val _ = test_bishop_fianchetto()
        val _ = test_queen_mixed()
        val _ = test_king_walk()
        val _ = test_pawn_promotion()
    in
        ()
    end







fun piecelist_tests ()=
    let
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
    in
        ()
    end



(* Execution *)
val _ = bitmaps_test ()

