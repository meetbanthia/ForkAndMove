(*This need to be implemented after move generator and board rep
This is where parallelism comes into consideration*)

structure  Search:
sig
    val eval_position: Board.brep -> bool -> real

(* node depth maximizingPlayer alpha beta evaluate *)
    val minimax: Board.brep -> int -> bool -> (Board.brep -> bool -> real) -> (Board.brep -> MoveGenerator.move -> Board.brep) -> (Board.brep -> bool -> MoveGenerator.move list) -> (real * ((MoveGenerator.move) option))
    val alpha_beta_search: 'a -> int -> bool -> real -> real -> ('a -> bool -> real) -> ('a -> bool -> 'b list) -> ('a -> 'b -> 'a) -> real * 'b option
end =
struct

    fun set_bit_only i =
        let val setbit0 = Word64.fromInt 1
        in Word64.<< (setbit0, (Word.fromInt i))
        end

    fun add_values_bonuses c bmap =
        let 
            val pt = PieceTable.give_piece_table c
            val piece_score = PieceTable.piece_value 
        in  
            Parallel.reduce 
                op+ 
                0 
                (0,64)
                (fn(i) => 
                    let 
                        val setbit_i = set_bit_only i
                        val row = 7 - (i div 8)
                        val col = 7 - (i mod 8)
                    in
                        case Word64.compare (Word64.andb(bmap, set_bit_only i) , set_bit_only i) of
                            EQUAL => (Array2.sub(pt, row, col) + (piece_score c))
                            | _ => 0
                    end
                )
        end

    fun material bmaps =
        let
            fun f i =
                let 
                    val c = Seq.nth PieceTable.pieces i
                    val bmap = Board.give_piece_bitmap bmaps c
                in 
                    add_values_bonuses c bmap
                end
            val white_side = Parallel.reduce op+ 0 (0,6) f
            val black_side = Parallel.reduce op+ 0 (6,12) f
        in
            white_side - black_side
        end

    fun calculate_threats bmaps isWhiteTurn =
        let
            (* Generate all pseudo-legal moves for both sides *)
            val white_moves = MoveGenerator.generate_color_move_order bmaps true true true true true true true
            val black_moves = MoveGenerator.generate_color_move_order bmaps false true true true true true true
            
            (* We need to know what piece is at a destination square to value the threat *)
            fun get_piece_value_at (P,R,N,B,K,Q,p,r,n,b,k,q) (row,col) isWhiteTarget =
                let
                    val idx = (7-row)*8 + (7-col)
                    val mask = Word64.<< (0w1, Word.fromInt idx)
                    fun has bb = Word64.andb(bb, mask) <> 0w0
                in
                    if isWhiteTarget then
                        if has P then PieceTable.piece_value #"P"
                        else if has N then PieceTable.piece_value #"N"
                        else if has B then PieceTable.piece_value #"B"
                        else if has R then PieceTable.piece_value #"R"
                        else if has Q then PieceTable.piece_value #"Q"
                        else 0 (* Ignore King captures in threats *)
                    else
                        if has p then PieceTable.piece_value #"p"
                        else if has n then PieceTable.piece_value #"n"
                        else if has b then PieceTable.piece_value #"b"
                        else if has r then PieceTable.piece_value #"r"
                        else if has q then PieceTable.piece_value #"q"
                        else 0 (* Ignore King captures in threats *)
                end

            fun max_threat moves targetIsWhite =
                List.foldl (fn ((_, dest), acc) => 
                    Int.max(acc, get_piece_value_at bmaps dest targetIsWhite)
                ) 0 moves
            
            (* White threats against Black pieces *)
            val white_threats = max_threat white_moves false
            (* Black threats against White pieces *)
            val black_threats = max_threat black_moves true
            
            (* Weight threats based on turn *)
            val (w_weight, b_weight) = 
                if isWhiteTurn then (1.0, 0.2)  (* White active, Black passive *)
                else (0.2, 1.0)                 (* White passive, Black active *)
                
            val white_score = Real.fromInt(white_threats) * w_weight
            val black_score = Real.fromInt(black_threats) * b_weight
        in
            (white_score - black_score) (* Full threat value *)
        end

    fun eval_position bmaps isWhiteTurn =
        let
            val material_factor = material bmaps
            val threat_factor = calculate_threats bmaps isWhiteTurn
        in
            Real.fromInt material_factor + threat_factor
        end


    fun minimax_full_parallel node depth maximizingPlayer evaluate apply_move order =
            if depth = 0 then 
                (evaluate node maximizingPlayer, NONE)
            else
                let
                    (* val _ = print ("Enter Depth : " ^ (Int.toString depth) ^ "\n") *)
                    val next_turn = not maximizingPlayer
                    val mo = order node maximizingPlayer

                    val move_order = Seq.fromList mo
                    (* val _ = print ("Generated Move Ordering" ^ "\n") *)

                    val child_boards = Seq.map (fn(i) => apply_move node i) move_order

                    (* val _ = print( "Searching for all childrens" ^ "\n") *)
                    val best_scores_and_moves = Seq.map (fn(i) => minimax_full_parallel i (depth-1) next_turn evaluate apply_move order) child_boards

                    val len = Seq.length best_scores_and_moves
                    val best_move_idx = 
                        if maximizingPlayer then
                            Parallel.reduce 
                                (fn(a,b) => 
                                    let 
                                        val (sa,_) = Seq.nth best_scores_and_moves a
                                        val (sb,_) = Seq.nth best_scores_and_moves b
                                    in
                                        if sa>sb then a
                                        else b
                                    end
                                ) 
                                (~1) (0, len) (fn(i) => i)
                        else 
                            Parallel.reduce 
                                (fn(a,b) => 
                                    let 
                                        val (sa,_) = Seq.nth best_scores_and_moves a
                                        val (sb,_) = Seq.nth best_scores_and_moves b
                                    in
                                        if sa>sb then b
                                        else a
                                    end
                                ) 
                                (~1) (0, len) (fn(i) => i)

                    (* val _ = print ("Found Best Move" ^ "\n") *)
                in
                    if best_move_idx = ~1 then
                        if maximizingPlayer then (Real.negInf, NONE) else (Real.posInf, NONE)
                    else
                        (#1 (Seq.nth best_scores_and_moves best_move_idx), SOME (Seq.nth move_order best_move_idx))
                end 

    fun minimax_evaluate_parallel node depth maximizingPlayer evaluate apply_move order =
            if depth = 0 then 
                (evaluate node maximizingPlayer, NONE)
            else
                let
                    (* val _ = print ("Enter Depth : " ^ (Int.toString depth) ^ "\n") *)
                    val next_turn = not maximizingPlayer
                    val mo = order node maximizingPlayer

                    val move_order = Seq.fromList mo
                    val len = Seq.length move_order
                    val _ = print ((Int.toString len) ^ " ")
                    (* val _ = print ("Generated Move Ordering" ^ "\n") *)

                    val child_boards = Seq.map (fn(i) => apply_move node i) move_order

                    fun f i = #1 (minimax_evaluate_parallel i (depth-1) next_turn evaluate apply_move order)
                    (* val _ = print( "Searching for all childrens" ^ "\n") *)
                    val best_scores = if depth = 1 then  Seq.map f child_boards
                                      else Seq.fromList (List.map f (Seq.toList child_boards))

                    val best_move_idx = 
                        if maximizingPlayer then
                            Parallel.reduce 
                                (fn(a,b) => 
                                    let 
                                        val sa = Seq.nth best_scores a
                                        val sb = Seq.nth best_scores b
                                    in
                                        if sa>sb then a
                                        else b
                                    end
                                ) 
                                (~1) (0, len) (fn(i) => i)
                        else 
                            Parallel.reduce 
                                (fn(a,b) => 
                                    let 
                                        val sa = Seq.nth best_scores a
                                        val sb = Seq.nth best_scores b
                                    in
                                        if sa<sb then a
                                        else b
                                    end
                                ) 
                                (~1) (0, len) (fn(i) => i)

                    (* val _ = print ("Found Best Move" ^ "\n") *)
                in
                    if best_move_idx = ~1 then
                        if maximizingPlayer then (Real.negInf, NONE) else (Real.posInf, NONE)
                    else
                        ((Seq.nth best_scores best_move_idx), SOME (Seq.nth move_order best_move_idx))
                end 

    
    fun alpha_beta_search (node : 'a) depth maximizingPlayer alpha beta (evaluate : 'a -> bool -> real) (next_nodes : 'a -> bool -> 'b list) (next_state : 'a -> 'b -> 'a) =
        if depth = 0 then (evaluate node maximizingPlayer, NONE)
        else
            let
                val moves = Seq.fromList (next_nodes node maximizingPlayer)
                val child_nodes = Seq.map (fn x => (x, next_state node x)) moves
                fun loop i alpha beta best_score best_move =
                    if i >= Seq.length child_nodes orelse beta <= alpha then (best_score, best_move)
                    else
                        let
                            val (move, child_node) = Seq.nth child_nodes i
                            val (score, _) = alpha_beta_search child_node (depth-1) (not maximizingPlayer) alpha beta evaluate next_nodes next_state
                            val (a, b, best_child) =
                                if maximizingPlayer then
                                    if score > best_score then (Real.max(alpha, score), beta, SOME move)
                                    else (Real.max(alpha, score), beta, best_move)
                                else
                                    if score < best_score then (alpha, Real.min(beta, score), SOME move)
                                    else (alpha, Real.min(beta, score), best_move)
                        in
                            if a >= b then (best_score, best_child)
                            else loop (i+1) a b (if maximizingPlayer then Real.max(best_score, score) else Real.min(best_score, score)) best_child
                        end
            in
                if maximizingPlayer then loop 0 alpha beta Real.negInf NONE
                else loop 0 alpha beta Real.posInf NONE
            end

    fun pvs_search node depth maximizingPlayer alpha beta evaluate next_nodes next_state =
        if depth = 0 then evaluate node maximizingPlayer
        else
            let
                val moves = Seq.fromList (next_nodes node maximizingPlayer)
                val child_nodes = Seq.map (fn(x) => next_state node x) moves
                val left_most_node = Seq.nth child_nodes 0
                val score1 = pvs_search left_most_node (depth-1) (not maximizingPlayer) alpha beta evaluate next_nodes next_state
                val rest_scores = Seq.map (fn(x) => alpha_beta_search x (depth-1) (not maximizingPlayer) alpha beta evaluate next_nodes next_state) child_nodes
                val rest_scores = Seq.map #1 rest_scores
                val (a,b) = if maximizingPlayer then (Real.max(score1,alpha), beta) else (alpha, Real.min(beta, score1))
                val f = if maximizingPlayer then Real.max else Real.min
            in
                Parallel.reduce (fn(a,b) => f(a,b)) (Real.fromInt 0) (1,Seq.length child_nodes) (fn(i) => Seq.nth rest_scores i)
            end
end