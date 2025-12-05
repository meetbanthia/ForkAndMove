(* Helper: convert a list of lists to an Array2.array *)
    fun array2_of_lists (rows, cols) board =
        let
            val arr = Array2.array (rows, cols, #" ")  (* Initialize with dummy value *)
            fun fill r [] = ()
            | fill r (row::restRows) =
                    let
                        fun fillCol c [] = ()
                        | fillCol c (x::xs) = (
                                Array2.update(arr, r, c, x);
                                fillCol (c+1) xs
                            )
                    in
                        fillCol 0 row;
                        fill (r+1) restRows
                    end
        in
            fill 0 board;
            arr
        end;


fun apply_move_tests () =
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

fun test_move_ordering_color () =
    let 
        (* Helper to print a header for each test *)
        fun print_header msg = 
            print ("\n========================================\n" ^ msg ^ "\n========================================\n")
        
        (* Helper to convert move coords to algebraic string (e.g. "e2-e4") *)
        fun move_to_string ((r1,c1),(r2,c2)) =
            let
                fun row_to_rank r = Int.toString (8 - r)
                fun col_to_file c = String.str (Char.chr (c + 97))
            in
                (col_to_file c1) ^ (row_to_rank r1) ^ "-" ^ (col_to_file c2) ^ (row_to_rank r2)
            end
            
        val _ = print_header "TEST: Move Ordering by Piece Type"
        
        (* 
           Scenario: White has multiple piece types available to move.
           We want to verify that we can request specific piece types.
        *)
        val b0 = Board.initiate_standard_chess()
        val _ = Board.print_board(b0)
        
        (* 1. Only Pawns *)
        val _ = print "\n--- White Pawns Only ---\n"
        (* generate_color_move_order brep white pawn knight bishop rook king queen *)
        val pawn_moves = MoveGenerator.generate_color_move_order b0 true true false false false false false
        val _ = List.app (fn m => print (move_to_string m ^ " ")) pawn_moves
        val _ = print ("\nCount: " ^ Int.toString (length pawn_moves) ^ "\n")

        (* 2. Only Knights *)
        val _ = print "\n--- White Knights Only ---\n"
        val knight_moves = MoveGenerator.generate_color_move_order b0 true false true false false false false
        val knight_moves_ordered = MoveGenerator.order_moves b0 true knight_moves
        val _ = MoveGenerator.print_ordered_moves knight_moves_ordered
        val _ = List.app (fn m => print (move_to_string m ^ " ")) knight_moves
        val _ = print ("\nCount: " ^ Int.toString (length knight_moves) ^ "\n")

        (* 3. Pawns and Knights *)
        val _ = print "\n--- White Pawns & Knights ---\n"
        val pn_moves = MoveGenerator.generate_color_move_order b0 true true true false false false false
        val _ = List.app (fn m => print (move_to_string m ^ " ")) pn_moves
        val _ = print ("\nCount: " ^ Int.toString (length pn_moves) ^ "\n")
        
        (* 4. Black Pieces (Standard Position) *)
        val _ = print "\n--- Black Pawns Only ---\n"
        val b_pawn_moves = MoveGenerator.generate_color_move_order b0 false true false false false false false
        val _ = List.app (fn m => print (move_to_string m ^ " ")) b_pawn_moves
        val _ = print ("\nCount: " ^ Int.toString (length b_pawn_moves) ^ "\n")
        
        (* 5. Non-existent moves (e.g. Bishops in starting position for White) *)
        val _ = print "\n--- White Bishops Only (Should be empty) ---\n"
        val bishop_moves = MoveGenerator.generate_color_move_order b0 true false false true false false false
        val _ = if null bishop_moves then print "Correct: No bishop moves.\n" else print "Error: Found bishop moves!\n"

    in
        ()
    end

fun test_capture_and_block_logic () =
    let
        (* Helper to print a header for each test *)
        fun print_header msg = 
            print ("\n========================================\n" ^ msg ^ "\n========================================\n")

        fun move_to_string ((r1,c1),(r2,c2)) =
            let
                fun row_to_rank r = Int.toString (8 - r)
                fun col_to_file c = String.str (Char.chr (c + 97))
            in
                (col_to_file c1) ^ (row_to_rank r1) ^ "-" ^ (col_to_file c2) ^ (row_to_rank r2)
            end
            
            (* assume: board_list : (char Array.array Array.array) list *)

            fun run_test_on_board isWhite board  =
                let
                    (* board is already Array2-compatible structure *)
                    val b = Board.board_representation board

                    val moves =
                        MoveGenerator.generate_color_move_order
                            b isWhite false true false false false false

                    val _ = print "\n===========================\n"
                    val _ = print "BOARD:\n"
                    val _ = Board.print_board b
                    val _ = print ("Side to move: " ^ (if isWhite then "White\n" else "Black\n"))
                    val _ = print "\nGenerated Moves:\n"
                    val _ = List.app (fn m => print (move_to_string m ^ " ")) moves
                    val _ = print "\n"

                    val ordered = MoveGenerator.order_moves b isWhite moves

                    val _ = print "\nOrdered Moves:\n"
                    val _ = MoveGenerator.print_ordered_moves ordered
                    val _ = print "\n"
                in
                    ()
                end
            fun run_all_boards boards =
                let 
                    val _ = List.app (run_test_on_board true) boards
                    val _ = List.app (run_test_on_board false) boards
                in
                    ()
                end

        val _ = print_header "TEST: Capture and Friendly Block Logic"

        (* Scenario: White Rook on a1, White Pawn on a2, Black Pawn on a3 *)
        (* Rook should be blocked by a2 (friendly) - cannot move to a2 *)
        (* If Pawn on a2 was Black, Rook could capture a2 *)
        
        (* We'll use a custom board setup to test this *)
        (* Since we can't easily construct arbitrary boards without FEN (which is in Main, not exposed?), 
           we will play moves to reach a state or assume standard board and modify logic if possible. 
           Actually, Board.initiate_standard_chess is hardcoded. 
           However, MoveGenerator.apply_move allows us to teleport pieces if we don't validate legality strictly.
           But apply_move updates bitboards based on FROM and TO. 
           Let's just use standard board and make moves.
        *)

        val b0 = Board.initiate_standard_chess()
        
        (* 
           Test 1: Friendly Fire Prevention
           White Bishop on c1. White Pawn on d2.
           Bishop on c1 should NOT generate move to d2.
        *)
        val _ = print "\n--- Test 1: Friendly Block (Bishop c1 blocked by Pawn d2) ---\n"
        val _ = Board.print_board(b0)
        (* Generate ONLY Bishop moves *)
        val bishop_moves = MoveGenerator.generate_color_move_order b0 true false false true false false false
        val _ = print "\nGenerated Bishop Moves:\n"
        val _ = List.app (fn m => print (move_to_string m ^ " ")) bishop_moves
        val _ = print "\n"
        (* Check if any move targets d2 (6,3) *)
        (* c1 is (7,2). d2 is (6,3). *)
        val moves_to_d2 = List.filter (fn ((r1,c1),(r2,c2)) => r1=7 andalso c1=2 andalso r2=6 andalso c2=3) bishop_moves
        val _ = if null moves_to_d2 then print "PASSED: Bishop cannot move to d2 (occupied by friendly pawn).\n" 
                else print "FAILED: Bishop generated move to friendly square d2!\n"

        (* 
           Test 2: Enemy Capture Generation
           Setup: 
           1. e4 d5
           White Pawn at e4 (4,4). Black Pawn at d5 (3,3).
           White Pawn captures d5? No, pawns capture diagonally.
           Let's use: 1. e4 d5 2. exd5
           White pawn at e4 (4,4). Black pawn at d5 (3,3).
           Wait, e4 is (4,4). d5 is (3,3).
           Pawn at e4 captures diagonally to d5 (3,3) and f5 (3,5).
           If d5 is occupied by Black, e4->d5 should be generated.
        *)
        val _ = print "\n--- Test 2: Enemy Capture (Pawn e4 captures d5) ---\n"
        val b1 = MoveGenerator.apply_move b0 ((6,4),(4,4)) (* 1. e4 *)
        val b2 = MoveGenerator.apply_move b1 ((1,3),(3,3)) (* 1... d5 *)
        val _ = Board.print_board(b2)
        
        val w_pawn_moves = MoveGenerator.generate_color_move_order b2 true true false false false false false
        val _ = print "\nGenerated White Pawn Moves:\n"
        val _ = List.app (fn m => print (move_to_string m ^ " ")) w_pawn_moves
        val _ = print "\n"
        (* Look for move (4,4)->(3,3) *)
        val capture_move = List.filter (fn ((r1,c1),(r2,c2)) => r1=4 andalso c1=4 andalso r2=3 andalso c2=3) w_pawn_moves
        val _ = if not (null capture_move) then print "PASSED: Pawn generated capture move e4xd5.\n"
                else print "FAILED: Pawn missed capture move e4xd5!\n"


        (*
            Test 3: Rook Capture vs Block
            Board b2 has white pawn at e4, black pawn at d5.
            Let's clear the e-file for white rook at e1? No, King is there.
            Let's use a-file. 
            1. a4 (White Pawn a2->a4). a4 is (4,0). a2 is empty.
            Rook at a1 (7,0).
            Moves for Rook a1: a2, a3. a4 is blocked by friendly pawn?
            Wait, standard board:
            a1=R, a2=P.
            Rook cannot move at all vertically.
            Let's move pawn a2 -> a4.
            Now a1=R, a2=empty, a3=empty, a4=P (White).
            Rook moves: a2, a3. Should NOT move to a4.
        *)
        val _ = print "\n--- Test 3: Rook blocked by friendly piece ---\n"
        val b3 = MoveGenerator.apply_move b0 ((6,0),(4,0)) (* 1. a4 *)
        val _ = Board.print_board(b3)
        (* Generate Rook moves *)
        val rook_moves = MoveGenerator.generate_color_move_order b3 true false false false true false false
        val _ = print "\nGenerated Rook Moves:\n"
        val _ = List.app (fn m => print (move_to_string m ^ " ")) rook_moves
        val _ = print "\n"
        (* Check for (7,0) -> (4,0) [a1->a4] *)
        val bad_move = List.filter (fn ((r1,c1),(r2,c2)) => r1=7 andalso c1=0 andalso r2=4 andalso c2=0) rook_moves
        (* Check valid moves: a2 (6,0), a3 (5,0) *)
        val valid_a2 = List.filter (fn ((r1,c1),(r2,c2)) => r1=7 andalso c1=0 andalso r2=6 andalso c2=0) rook_moves
        val valid_a3 = List.filter (fn ((r1,c1),(r2,c2)) => r1=7 andalso c1=0 andalso r2=5 andalso c2=0) rook_moves
        
        val _ = if null bad_move then print "PASSED: Rook blocked by friendly pawn at a4.\n" 
                else print "FAILED: Rook generated move to friendly square a4!\n"
        val _ = if (not (null valid_a2)) andalso (not (null valid_a3)) then print "PASSED: Rook generates valid intermediate moves.\n"
                else print "FAILED: Rook missed valid moves a2 or a3.\n"


        val _ = print "\n--- Test 4: Rook on empty file ---\n"
        val board1 = array2_of_lists (8,8) [
             [#"R",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "]
        ];
        val b1 = Board.board_representation board1
        val rook_moves1 = MoveGenerator.generate_color_move_order b1 true false false false true false false
        val _ = Board.print_board(b1)
        val _ = List.app (fn m => print (move_to_string m ^ " ")) rook_moves1
        val _ = print "\n"
        val bad_move1 = List.filter (fn ((r1,c1),(r2,c2)) => r1=7 andalso c1=0 andalso r2<0) rook_moves1  (* impossible *)
        val _ = if null bad_move1 then print "PASSED: No illegal moves.\n" else print "FAILED: Generated illegal moves!\n"


        val _ = print "\n--- Test 5: Rook blocked by friendly pawn at a4 ---\n"
        val board2 = array2_of_lists (8,8) [
             [#"R",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#"P",#" ",#" ",#" ",#" ",#" ",#" ",#" "],  (* a4 blocks *)
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "]
        ];
        val b2 = Board.board_representation board2
        val rook_moves2 = MoveGenerator.generate_color_move_order b2 true false false false true false false
        val _ = Board.print_board(b2)
        val _ = List.app (fn m => print (move_to_string m ^ " ")) rook_moves2
        val _ = print "\n"


        val _ = print "\n--- Test 6: Rook blocked by enemy queen at a4 with additional bishop---\n"
        val board3 = array2_of_lists (8,8) [
             [#" ",#"R",#" ",#" ",#"B",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#"q",#" ",#" ",#" ",#" ",#" ",#" ",#" "],  (* enemy pawn *)
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#"q",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "]
        ];
        val b3 = Board.board_representation board3
        val rook_moves3 = MoveGenerator.generate_color_move_order b3 true false true true true true true
        val _ = Board.print_board(b3)
        val _ = List.app (fn m => print (move_to_string m ^ " ")) rook_moves3
        val _ = print "\n"
        val rook_moves_ordered = MoveGenerator.order_moves b3 true rook_moves3
        val _ = MoveGenerator.print_ordered_moves rook_moves_ordered



        val _ = print "\n--- Test 7: Rook in middle of empty board ---\n"
        val board4 = array2_of_lists (8,8) [
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#"R",#" ",#" ",#" ",#" ",#" "],  (* c5 *)
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "]
        ];
        val b4 = Board.board_representation board4
        val rook_moves4 = MoveGenerator.generate_color_move_order b4 true false false false true false false
        val _ = Board.print_board(b4)
        val _ = List.app (fn m => print (move_to_string m ^ " ")) rook_moves4
        val _ = print "\n"

    

        val _ = print "\n--- Test 8: Rook completely blocked by friendly pawns ---\n"
        val board5 = array2_of_lists (8,8) [
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#"P",#" ",#" ",#" ",#" ",#" "],  (* c7 *)
             [#" ",#"P",#"R",#"P",#" ",#" ",#" ",#" "],  (* rook at c6 *)
             [#" ",#" ",#"P",#" ",#" ",#" ",#" ",#" "],  (* c5 *)
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "]
        ];
        val b5 = Board.board_representation board5
        val rook_moves5 = MoveGenerator.generate_color_move_order b5 true false false false true false false
        val _ = Board.print_board(b5)
        val _ = List.app (fn m => print (move_to_string m ^ " ")) rook_moves5
        val _ = print "\n"


        val _ = print "\n--- Test 9: Weird combo ---\n"
        val board6 = array2_of_lists (8,8) [
             [#"r",#" ",#" ",#" ",#" ",#" ",#" ",#"k"],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],  (* c7 *)
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],  (* rook at c6 *)
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],  (* c5 *)
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#"Q",#" ",#" ",#" ",#" ",#" ",#" ",#"K"]
        ];
        val b6 = Board.board_representation board6
        val rook_moves6 = MoveGenerator.generate_color_move_order b6 false true true true true true true
        val _ = Board.print_board(b6)
        val _ = List.app (fn m => print (move_to_string m ^ " ")) rook_moves6
        val _ = print "\n"
        val rook_moves_ordered = MoveGenerator.order_moves b6 false rook_moves6
        val _ = MoveGenerator.print_ordered_moves rook_moves_ordered

        val board_list = [
        (* 1 *)
        array2_of_lists (8,8) [
         [#"r",#" ",#"b",#"q",#"k",#"b",#"n",#"r"],
         [#"p",#"p",#"p",#" ",#"p",#"p",#"p",#"p"],
         [#" ",#"n",#" ",#"p",#" ",#" ",#" ",#" "],
         [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
         [#" ",#" ",#" ",#"P",#" ",#" ",#" ",#" "],
         [#" ",#" ",#"N",#" ",#" ",#"N",#" ",#" "],
         [#"P",#"P",#"P",#" ",#"P",#"P",#"P",#"P"],
         [#"R",#" ",#"B",#"Q",#"K",#"B",#" ",#"R"]
        ],

        (* 2 *)
        array2_of_lists (8,8) [
         [#"r",#"n",#"b",#"q",#"k",#"b",#" ",#"r"],
         [#"p",#" ",#"p",#"p",#" ",#"p",#"p",#" "],
         [#" ",#"p",#" ",#" ",#"p",#"n",#" ",#" "],
         [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#"p"],
         [#"P",#" ",#" ",#" ",#"P",#" ",#" ",#" "],
         [#" ",#"N",#"P",#" ",#" ",#" ",#" ",#" "],
         [#" ",#"P",#" ",#"P",#" ",#"P",#"P",#"P"],
         [#"R",#" ",#"B",#"Q",#"K",#"B",#"N",#"R"]
        ],

        (* 3 *)
        array2_of_lists (8,8) [
         [#" ",#" ",#"k",#" ",#" ",#" ",#" ",#" "],
         [#"p",#"p",#" ",#" ",#" ",#"p",#"p",#" "],
         [#" ",#" ",#" ",#"p",#" ",#" ",#" ",#" "],
         [#" ",#"n",#" ",#" ",#" ",#" ",#" ",#" "],
         [#" ",#" ",#"P",#" ",#" ",#" ",#" ",#" "],
         [#" ",#" ",#" ",#"P",#" ",#"N",#" ",#" "],
         [#"P",#"P",#" ",#" ",#" ",#"P",#"P",#"P"],
         [#" ",#" ",#"K",#" ",#" ",#" ",#" ",#" "]
        ],

        (* 4 *)
        array2_of_lists (8,8) [
         [#"r",#" ",#" ",#"q",#"k",#" ",#"b",#"r"],
         [#"p",#"p",#" ",#"p",#" ",#"p",#"p",#"p"],
         [#" ",#" ",#"n",#" ",#"p",#"n",#" ",#" "],
         [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
         [#" ",#" ",#"P",#" ",#"P",#" ",#" ",#" "],
         [#" ",#"N",#" ",#"N",#" ",#" ",#" ",#" "],
         [#"P",#"P",#" ",#"P",#" ",#"P",#"P",#"P"],
         [#"R",#"B",#" ",#"Q",#"K",#"B",#" ",#"R"]
        ],

        (* 5 *)
        array2_of_lists (8,8) [
         [#" ",#" ",#" ",#" ",#"k",#" ",#" ",#" "],
         [#" ",#"p",#" ",#" ",#"p",#" ",#" ",#"p"],
         [#"p",#" ",#"n",#" ",#" ",#" ",#" ",#" "],
         [#" ",#" ",#" ",#"P",#" ",#" ",#" ",#" "],
         [#" ",#" ",#"p",#" ",#" ",#" ",#" ",#" "],
         [#" ",#"N",#" ",#" ",#"N",#" ",#" ",#" "],
         [#"P",#" ",#"P",#" ",#"P",#"P",#"P",#" "],
         [#" ",#" ",#" ",#" ",#"K",#" ",#" ",#" "]
        ],

        (* 6 *)
        array2_of_lists (8,8) [
         [#"r",#" ",#" ",#" ",#"k",#" ",#" ",#"r"],
         [#"p",#"p",#"p",#" ",#"p",#"p",#"p",#" "],
         [#" ",#" ",#"b",#"p",#" ",#" ",#"n",#" "],
         [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
         [#" ",#" ",#"P",#" ",#" ",#" ",#" ",#" "],
         [#" ",#"N",#" ",#"P",#" ",#" ",#" ",#" "],
         [#"P",#"P",#" ",#" ",#"P",#"P",#"P",#"P"],
         [#"R",#" ",#" ",#" ",#"K",#" ",#"N",#"R"]
        ],

        (* 7 *)
        array2_of_lists (8,8) [
         [#" ",#"n",#" ",#"k",#" ",#" ",#" ",#" "],
         [#"p",#" ",#"p",#" ",#" ",#"p",#"p",#"p"],
         [#" ",#"p",#" ",#" ",#"p",#" ",#" ",#" "],
         [#" ",#" ",#" ",#"P",#" ",#" ",#" ",#" "],
         [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
         [#"P",#" ",#"N",#" ",#" ",#"N",#" ",#" "],
         [#" ",#"P",#" ",#"P",#"P",#"P",#"P",#" "],
         [#" ",#" ",#"K",#" ",#" ",#" ",#" ",#" "]
        ],

        (* 8 *)
        array2_of_lists (8,8) [
         [#"r",#"n",#" ",#"q",#"k",#" ",#" ",#" "],
         [#"p",#" ",#"p",#"p",#" ",#"p",#"p",#"p"],
         [#" ",#"p",#"b",#" ",#"p",#" ",#" ",#" "],
         [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
         [#"P",#" ",#"P",#" ",#" ",#" ",#" ",#" "],
         [#" ",#"N",#" ",#"P",#" ",#"N",#" ",#" "],
         [#" ",#"P",#" ",#" ",#"P",#"P",#"P",#"P"],
         [#"R",#" ",#"B",#"Q",#"K",#" ",#" ",#"R"]
        ],

        (* 9 *)
        array2_of_lists (8,8) [
         [#" ",#" ",#"k",#" ",#" ",#" ",#" ",#" "],
         [#"p",#"p",#" ",#" ",#" ",#"p",#" ",#" "],
         [#" ",#"n",#"p",#"p",#" ",#" ",#" ",#" "],
         [#" ",#" ",#" ",#" ",#"P",#" ",#" ",#" "],
         [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
         [#" ",#"N",#" ",#" ",#" ",#" ",#" ",#" "],
         [#"P",#" ",#"P",#"P",#" ",#"P",#"P",#"P"],
         [#" ",#" ",#" ",#" ",#"K",#" ",#" ",#" "]
        ],

        (* 10 *)
        array2_of_lists (8,8) [
         [#"r",#" ",#"b",#" ",#"k",#" ",#"n",#"r"],
         [#"p",#"p",#" ",#"p",#" ",#"p",#" ",#"p"],
         [#" ",#" ",#"n",#" ",#"p",#" ",#" ",#" "],
         [#" ",#" ",#" ",#" ",#" ",#" ",#"p",#" "],
         [#" ",#" ",#"P",#"P",#" ",#" ",#" ",#" "],
         [#" ",#"N",#" ",#" ",#"N",#" ",#" ",#" "],
         [#"P",#"P",#" ",#" ",#"P",#"P",#"P",#"P"],
         [#"R",#" ",#"B",#"Q",#"K",#" ",#" ",#"R"]
        ],

        (* 11 *)
        array2_of_lists (8,8) [
         [#" ",#"n",#" ",#"k",#" ",#" ",#" ",#" "],
         [#"p",#"p",#" ",#"p",#" ",#"p",#" ",#"p"],
         [#" ",#" ",#"b",#" ",#"p",#" ",#" ",#" "],
         [#" ",#" ",#" ",#" ",#"P",#" ",#" ",#" "],
         [#" ",#" ",#"P",#" ",#" ",#" ",#" ",#" "],
         [#" ",#"N",#" ",#"P",#" ",#" ",#" ",#" "],
         [#"P",#" ",#" ",#" ",#"P",#"P",#"P",#"P"],
         [#" ",#" ",#" ",#" ",#"K",#" ",#" ",#" "]
        ],

        (* 12 *)
        array2_of_lists (8,8) [
         [#"r",#" ",#" ",#"q",#"k",#" ",#" ",#"r"],
         [#"p",#"p",#" ",#" ",#"p",#"p",#"p",#" "],
         [#" ",#" ",#"n",#"p",#" ",#" ",#" ",#" "],
         [#" ",#" ",#" ",#" ",#" ",#"P",#" ",#" "],
         [#" ",#" ",#"P",#" ",#" ",#" ",#" ",#"p"],
         [#" ",#"N",#" ",#" ",#" ",#"N",#" ",#" "],
         [#"P",#"P",#" ",#"P",#"P",#"P",#"P",#"P"],
         [#"R",#" ",#"B",#"Q",#"K",#"B",#" ",#" "]
        ],

        (* 13 *)
        array2_of_lists (8,8) [
         [#" ",#" ",#" ",#" ",#"k",#" ",#" ",#"r"],
         [#"p",#" ",#" ",#"p",#" ",#"p",#"p",#"p"],
         [#" ",#"n",#"b",#" ",#"p",#" ",#" ",#" "],
         [#" ",#" ",#" ",#" ",#" ",#" ",#"p",#" "],
         [#" ",#" ",#" ",#"P",#"P",#" ",#" ",#" "],
         [#" ",#"N",#" ",#" ",#" ",#"N",#" ",#" "],
         [#"P",#"P",#"P",#" ",#"P",#"P",#"P",#"P"],
         [#"R",#" ",#"B",#"Q",#"K",#" ",#" ",#"R"]
        ],

        (* 14 *)
        array2_of_lists (8,8) [
         [#"r",#"n",#" ",#" ",#"k",#" ",#"b",#" "],
         [#"p",#" ",#"p",#"p",#" ",#"p",#"p",#" "],
         [#" ",#"p",#" ",#" ",#"p",#" ",#" ",#"n"],
         [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
         [#" ",#" ",#"P",#" ",#" ",#" ",#" ",#" "],
         [#" ",#"N",#" ",#"P",#" ",#" ",#" ",#" "],
         [#"P",#"P",#" ",#" ",#"P",#"P",#"P",#"P"],
         [#"R",#" ",#"B",#"Q",#"K",#"B",#" ",#"R"]
        ],

        (* 15 *)
        array2_of_lists (8,8) [
         [#" ",#" ",#"k",#" ",#" ",#" ",#" ",#" "],
         [#"p",#"p",#"p",#" ",#" ",#"p",#"p",#" "],
         [#" ",#" ",#" ",#"p",#" ",#"n",#" ",#" "],
         [#" ",#" ",#" ",#" ",#"P",#" ",#" ",#" "],
         [#" ",#" ",#" ",#"P",#" ",#" ",#" ",#" "],
         [#" ",#"N",#" ",#" ",#" ",#" ",#" ",#" "],
         [#"P",#"P",#"P",#" ",#" ",#"P",#"P",#"P"],
         [#" ",#" ",#"K",#" ",#" ",#" ",#" ",#" "]
        ],

        (* 16 *)
        array2_of_lists (8,8) [
         [#"r",#" ",#" ",#"q",#"k",#" ",#" ",#"r"],
         [#"p",#" ",#"p",#" ",#"p",#"p",#"p",#" "],
         [#" ",#"n",#" ",#"p",#" ",#" ",#"n",#" "],
         [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
         [#" ",#" ",#"P",#" ",#"P",#"P",#" ",#" "],
         [#" ",#" ",#" ",#"P",#" ",#"N",#" ",#" "],
         [#"P",#"P",#" ",#" ",#" ",#"P",#"P",#"P"],
         [#"R",#"N",#"B",#"Q",#"K",#" ",#" ",#"R"]
        ],

        (* 17 *)
        array2_of_lists (8,8) [
         [#" ",#"n",#" ",#"k",#" ",#"b",#" ",#" "],
         [#"p",#" ",#"p",#"p",#" ",#"p",#"p",#" "],
         [#" ",#"p",#" ",#" ",#"p",#" ",#" ",#" "],
         [#" ",#" ",#" ",#" ",#" ",#" ",#"p",#" "],
         [#" ",#" ",#"P",#" ",#"P",#" ",#" ",#" "],
         [#"P",#" ",#" ",#" ",#"N",#" ",#" ",#" "],
         [#" ",#"P",#"P",#"P",#" ",#"P",#"P",#"P"],
         [#" ",#" ",#"K",#" ",#" ",#" ",#" ",#" "]
        ],

        (* 18 *)
        array2_of_lists (8,8) [
         [#"r",#" ",#"b",#"q",#"k",#" ",#" ",#"r"],
         [#"p",#"p",#" ",#"p",#"p",#"p",#" ",#"p"],
         [#" ",#" ",#"n",#" ",#" ",#" ",#"n",#" "],
         [#" ",#" ",#"p",#" ",#" ",#" ",#" ",#" "],
         [#" ",#" ",#"P",#"P",#" ",#" ",#" ",#" "],
         [#" ",#" ",#"N",#" ",#"N",#" ",#" ",#" "],
         [#"P",#"P",#" ",#" ",#"P",#"P",#"P",#"P"],
         [#"R",#" ",#"B",#"Q",#"K",#" ",#" ",#"R"]
        ],

        (* 19 *)
        array2_of_lists (8,8) [
         [#" ",#" ",#"k",#" ",#" ",#" ",#" ",#" "],
         [#"p",#" ",#"p",#" ",#"p",#" ",#"p",#" "],
         [#" ",#"p",#" ",#"p",#" ",#"n",#" ",#" "],
         [#"p",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
         [#" ",#"P",#"P",#" ",#"P",#" ",#" ",#" "],
         [#" ",#" ",#" ",#"P",#" ",#"N",#" ",#" "],
         [#"P",#" ",#" ",#" ",#"P",#"P",#"P",#"P"],
         [#" ",#" ",#"K",#" ",#" ",#" ",#" ",#" "]
        ],

        (* 20 *)
        array2_of_lists (8,8) [
         [#"r",#" ",#" ",#"q",#"k",#" ",#"n",#"r"],
         [#"p",#"p",#" ",#"p",#"p",#"p",#" ",#" "],
         [#" ",#" ",#"b",#" ",#" ",#"n",#" ",#"p"],
         [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
         [#"P",#" ",#"P",#" ",#" ",#"P",#" ",#" "],
         [#" ",#"N",#" ",#"P",#" ",#" ",#" ",#" "],
         [#" ",#"P",#" ",#" ",#"P",#"P",#"P",#"P"],
         [#"R",#" ",#"B",#"Q",#"K",#" ",#" ",#"R"]
        ], 
        array2_of_lists (8,8) [
            [#"r",#"n",#"b",#"q",#" ",#"r",#"k",#" "],
             [#"p",#"p",#" ",#" ",#"p",#"p",#"p",#"p"],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#"p",#"B",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#"P",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#"N",#" ",#" "],
             [#"P",#"P",#"P",#"P",#" ",#"P",#"P",#"P"],
             [#"R",#"N",#"B",#"Q",#"K",#" ",#" ",#"R"]]
        ];

        val _ = run_all_boards board_list


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


fun evaluate_test () =
    let
        val _ = print "\n\n========== Evaluate Function Tests ==========\n"
        
        (* Initial Board *)
        val b0 = Board.initiate_standard_chess()
        val score0 = Search.eval_position b0 true
        val _ = Board.print_board(b0)
        val _ = print ("Initial Board Score: " ^ Real.toString score0 ^ "\n")
        
        (* 1. e4 *)
        val _ = print "\n--- 1. e4 ((6,4) -> (4,4)) ---\n"
        val b1 = MoveGenerator.apply_move b0 ((6,4),(4,4))
        val score1 = Search.eval_position b1 true
        val _ = Board.print_board(b1)
        val _ = print ("After 1. e4 Score: " ^ Real.toString score1 ^ "\n")

         (* 1... e5 *)
        val _ = print "\n--- 1... e5 ((1,4) -> (3,4)) ---\n"
        val b2 = MoveGenerator.apply_move b1 ((1,4),(3,4))
        val score2 = Search.eval_position b2 true
        val _ = Board.print_board(b2)
        val _ = print ("After 1... e5 Score: " ^ Real.toString score2 ^ "\n")
        
        (* 2. Qf3 (developing queen) *)
        val _ = print "\n--- 2. Qf3 ((7,3) -> (5,5)) ---\n"
        val b3 = MoveGenerator.apply_move b2 ((7,3),(5,5))
        val score3 = Search.eval_position b3 true
        val _ = Board.print_board(b3)
        val _ = print ("After 2. Qf3 Score: " ^ Real.toString score3 ^ "\n")

        (* Capture scenario to test material change *)
        (* White Pawn takes Black Pawn *)
        (* Setup: 1. e4 d5 2. exd5 *)
        val _ = print "\n--- Capture Test: 1. e4 d5 2. exd5 ---\n"
        val b_cap0 = Board.initiate_standard_chess()
        val b_cap1 = MoveGenerator.apply_move b_cap0 ((6,4),(4,4)) (* e4 *)
        val b_cap2 = MoveGenerator.apply_move b_cap1 ((1,3),(3,3)) (* d5 *)
        val b_cap3 = MoveGenerator.apply_move b_cap2 ((4,4),(3,3)) (* exd5 *)
        val score_cap = Search.eval_position b_cap3 true
        val _ = Board.print_board(b_cap3)
        val _ = print ("After Capture (White up a pawn) Score: " ^ Real.toString score_cap ^ "\n")

        (* --- New Threat Tests --- *)
        val _ = print "\n--- Threat Test 1: White Rook threatens Black Pawn ---\n"
        (* Setup: White R(a1), K(h1). Black p(a5), k(h8). *)
        val board_t1 = Array2.fromList [
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#"k"], (* h8 *)
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#"p",#" ",#" ",#" ",#" ",#" ",#" ",#" "], (* a5 *)
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#"R",#" ",#" ",#" ",#" ",#" ",#" ",#"K"]  (* a1, h1 *)
        ];
        val b_t1 = Board.board_representation board_t1
        val score_t1 = Search.eval_position b_t1 false
        val _ = Board.print_board(b_t1)
        (* 
           Material: 
           White: R(500) + K(20000) = 20500
           Black: p(100) + k(20000) = 20100
           Diff: 400.
           
           Threats:
           White R(a1) -> a5 (captures p=100). Threat = 100.
           Black p(a5) -> a4. No capture.
           
           Total Score = 400 + (100 - 0)*0.5 = 450.
        *)
        val _ = print ("Score T1 (R threatens p): " ^ Real.toString score_t1 ^ " (Expected ~450.0)\n")

        val _ = print "\n--- Threat Test 2: Black Rook threatens White Queen ---\n"
        (* Setup: Black r(a8), k(h8). White Q(a1), K(h1). *)
        val board_t2 = Array2.fromList [
             [#"r",#" ",#" ",#" ",#" ",#" ",#" ",#"k"], (* a8, h8 *)
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#"Q",#" ",#" ",#" ",#" ",#" ",#" ",#"K"]  (* a1, h1 *)
        ];
        val b_t2 = Board.board_representation board_t2
        val score_t2 = Search.eval_position b_t2 false
        val _ = Board.print_board(b_t2)
        (* 
           Material:
           White: Q(900) + K = 20900
           Black: r(500) + k = 20500
           Diff: 400.
           
           Threats:
           White Q(a1) -> a8 (captures r=500). Threat = 500.
           Black r(a8) -> a1 (captures Q=900). Threat = 900.
           
           Total Score = 400 + (500 - 900)*0.5 = 400 - 200 = 200.
        *)
        val _ = print ("Score T2 (Mutual Q vs r): " ^ Real.toString score_t2 ^ " (Expected ~200.0)\n")

        val _ = print "\n--- Threat Test 3: Hanging Queen ---\n"
        val board_t3 = Array2.fromList [
             [#" ",#" ",#" ",#" ",#"k",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#"p",#" ",#" ",#" ",#" ",#" "], (* c5 (3,2) *)
             [#" ",#" ",#" ",#"Q",#" ",#" ",#" ",#" "], (* d4 (4,3) *)
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#"K",#" ",#" ",#" "]
        ];
        val b_t3 = Board.board_representation board_t3
        val score_t3_b = Search.eval_position b_t3 false
        val score_t3_w = Search.eval_position b_t3 true
        val _ = Board.print_board(b_t3)
        val _ = print ("Score T3 (Black to move): " ^ Real.toString score_t3_b ^ " (Expected ~ -100.0)\n")
        val _ = print ("Score T3 (White to move): " ^ Real.toString score_t3_w ^ " (Expected ~ 350.0)\n")

        val _ = print "\n--- Threat Test 4: Knight Fork ---\n"
        val board_t4 = Array2.fromList [
             [#" ",#" ",#" ",#" ",#"k",#" ",#" ",#" "],
             [#" ",#" ",#" ",#"r",#" ",#"r",#" ",#" "], (* d7, f7 *)
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#"N",#" ",#" ",#" "], (* e5 *)
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#"K",#" ",#" ",#" "]
        ];
        val b_t4 = Board.board_representation board_t4
        val score_t4_w = Search.eval_position b_t4 true
        val _ = Board.print_board(b_t4)
        val _ = print ("Score T4 (White to move, Fork): " ^ Real.toString score_t4_w ^ " (Expected ~320.0)\n")

        val _ = print "\n--- Threat Test 5: Pinned/Trapped Piece (Simulated) ---\n"
        (* White B on a1. Black R on a8. 
           R attacks B. B attacks nothing (blocked by own pawn or empty? Empty board).
           Material: 330 vs 500 = -170.
           Threat: Black R on a8 attacks a1 (330).
           Black to move: 330 * 1.0 = 330. Total: -170 - 330 = -500.
           White to move: 330 * 0.5 = 165. Total: -170 - 165 = -335.
        *)
        val board_t5 = Array2.fromList [
             [#"r",#" ",#" ",#" ",#"k",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
             [#"B",#" ",#" ",#" ",#"K",#" ",#" ",#" "]
        ];
        val b_t5 = Board.board_representation board_t5
        val score_t5_b = Search.eval_position b_t5 false
        val score_t5_w = Search.eval_position b_t5 true
        val _ = Board.print_board(b_t5)
        val _ = print ("Score T5 (Black to move): " ^ Real.toString score_t5_b ^ " (Expected ~ -500.0)\n")
        val _ = print ("Score T5 (White to move): " ^ Real.toString score_t5_w ^ " (Expected ~ -335.0)\n")

    in
        ()
    end



(* Execution *)
val _ = apply_move_tests ()
val _ = test_move_ordering_color ()
val _ = test_capture_and_block_logic ()
val _ = evaluate_test ()

