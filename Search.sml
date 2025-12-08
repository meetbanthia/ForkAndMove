(*This need to be implemented after move generator and board rep
This is where parallelism comes into consideration*)

structure  Search:
sig
    val eval_position: Board.brep -> bool -> real

(* node depth maximizingPlayer alpha beta evaluate *)
    val minimax: 'a -> int -> bool -> ('a -> bool -> real) -> ('a -> 'b -> 'a) -> ('a -> bool -> 'b list) -> (real * (('b) option))
    val alpha_beta_search: 'a -> int -> bool -> real -> real -> ('a -> bool -> real) -> ('a -> bool -> 'b list) -> ('a -> 'b -> 'a) -> (real * (('b) option))
    val pvs_search: 'a -> int -> bool -> real -> real -> ('a -> bool -> real) -> ('a -> bool -> 'b list) -> ('a -> 'b -> 'a) -> (real * (('b) option))
    val minimax_full_parallel_ttt : 'a -> int -> bool -> ('a -> bool -> real) -> ('a -> 'b -> bool -> 'a) -> ('a -> bool -> 'b list) -> (real * ('b option))
    val alpha_beta_search_ttt : 'a -> int -> bool -> real -> real -> ('a -> bool -> real) -> ('a -> bool -> 'b list) -> ('a -> 'b -> bool -> 'a) -> (real * ('b option))
    val pvs_search_ttt : 'a -> int -> bool -> real -> real -> ('a -> bool -> real) -> ('a -> bool -> 'b list) -> ('a -> 'b -> bool -> 'a) -> (real * ('b option))

    (* val alpha_beta_search: 'a -> int -> bool -> real -> real -> ('a -> bool -> real) -> ('a -> bool -> 'b list) -> ('a -> 'b -> 'a) -> real * 'b option *)
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

    fun minimax node depth maximizingPlayer evaluate apply_move order =
            if depth = 0 then 
                (evaluate node maximizingPlayer, NONE)
            else
                let
                    (* val _ = print ("Enter Depth : " ^ (Int.toString depth) ^ "\n") *)
                    val next_turn = not maximizingPlayer
                    val mo = order node maximizingPlayer

                    val move_order = Seq.fromList mo
                    val len = Seq.length move_order
                    (* val _ = print ((Int.toString len) ^ " ") *)
                    (* val _ = print ("Generated Move Ordering" ^ "\n") *)

                    val child_boards = Seq.map (fn(i) => apply_move node i) move_order

                    fun f i = #1 (minimax i (depth-1) next_turn evaluate apply_move order)
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
                                        if sa>=sb then a
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
                                        if sa<=sb then a
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


    
    (* fun alpha_beta_search (node : 'a) depth maximizingPlayer alpha beta (evaluate : 'a -> bool -> real) (next_nodes : 'a -> bool -> 'b list) (next_state : 'a -> 'b -> 'a) =
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
            end *)

    fun alpha_beta_search node depth maximizingPlayer alpha beta evaluate order apply_move =
        if depth = 0 then (evaluate node maximizingPlayer, NONE)
        else
            let
                val moves = Seq.fromList (order node maximizingPlayer)
                val childs = Seq.map (fn i => apply_move node i) moves
                fun loop i a b bci bcs iswhite =
                    if i >= Seq.length childs then (bcs, bci)
                    else
                        let
                            val ithchild = Seq.nth childs i
                            val (ithchild_score, _) = alpha_beta_search ithchild (depth-1) (not iswhite) a b evaluate order apply_move

                            val (new_bci, new_bcs) = 
                                    if ((iswhite andalso ithchild_score > bcs) orelse (not iswhite andalso ithchild_score < bcs)) then 
                                        (i,ithchild_score) 
                                    else 
                                        (bci, bcs)

                            val (new_a, new_b) =
                                    if (iswhite andalso ithchild_score > a) then
                                        (ithchild_score, b)
                                    else if (not iswhite andalso ithchild_score < b) then
                                        (a, ithchild_score)
                                    else 
                                        (a,b)
                        in
                            if new_a >= new_b then (new_bcs, new_bci) 
                            else loop (i+1) new_a new_b new_bci new_bcs iswhite
                        end


                val bs_init = if maximizingPlayer then Real.negInf else Real.posInf
                val (bs, bm_index) = loop 0 alpha beta ~1 bs_init maximizingPlayer
                val bm = if bm_index = ~1 then NONE else SOME( Seq.nth moves bm_index)
            in
                (bs, bm)
            end

    fun pvs_search_no_move node depth maximizingPlayer alpha beta evaluate next_nodes next_state =
        if depth = 0 then evaluate node maximizingPlayer
        else
            let
                val moves = Seq.fromList (next_nodes node maximizingPlayer)
                val child_nodes = Seq.map (fn(x) => next_state node x) moves
                val left_most_node = Seq.nth child_nodes 0
                val score1 = pvs_search_no_move left_most_node (depth-1) (not maximizingPlayer) alpha beta evaluate next_nodes next_state
                val rest_scores = Seq.map (fn(x) => alpha_beta_search x (depth-1) (not maximizingPlayer) alpha beta evaluate next_nodes next_state) child_nodes
                val rest_scores = Seq.map #1 rest_scores
                val (a,b) = if maximizingPlayer then (Real.max(score1,alpha), beta) else (alpha, Real.min(beta, score1))
                val f = if maximizingPlayer then Real.max else Real.min
            in
                Parallel.reduce (fn(a,b) => f(a,b)) (Real.fromInt 0) (1,Seq.length child_nodes) (fn(i) => Seq.nth rest_scores i)
            end

    fun pvs_search node depth maximizingPlayer alpha beta evaluate order apply_move =
    if depth = 0 then
        (evaluate node maximizingPlayer, NONE)
    else
        case (order node maximizingPlayer) of [] => 
            (evaluate node maximizingPlayer, NONE)
            | left_move :: rest_moves =>
            let
                val left_child = apply_move node left_move
                val sibling_seq = Seq.fromList rest_moves
                val n = Seq.length sibling_seq

                val (pv_score, _) = pvs_search left_child (depth-1) (not maximizingPlayer) alpha beta evaluate order apply_move

                val (a0, b0) = if maximizingPlayer then (Real.max(alpha, pv_score), beta) else (alpha, Real.min(beta, pv_score))

                val bestScore0 = pv_score
                val bestMove0  = left_move



                val cutoff = (a0 >= b0) orelse (bestScore0 > beta)
                val best_sib_opt =
                    if cutoff orelse n = 0 then NONE
                    else
                        let
                            val (best_sc, best_idx) =
                            Parallel.reduce
                                (fn ((s1: real,i1),(s2: real,i2)) =>
                                    if maximizingPlayer then
                                        case Real.compare (s1, s2) of
                                        LESS    => (s2, i2)
                                        | _ => (s1, i1)
                                    else
                                        case Real.compare (s1, s2) of
                                        GREATER => (s2, i2)
                                        | _   =>  (s1, i1) 
                                )
                                    (if maximizingPlayer then Real.negInf else Real.posInf, ~1)
                                    (0, n)
                                    (fn i =>
                                        let
                                            val child = apply_move node (Seq.nth sibling_seq i)
                                            val (score, _) = alpha_beta_search child (depth-1) (not maximizingPlayer) a0 b0 evaluate order apply_move
                                        in
                                            (score, i)
                                        end
                                    )
                        in
                            if best_idx = ~1 then NONE else SOME (best_sc, Seq.nth sibling_seq best_idx)
                        end

                val (final_score, final_move) =
                    case best_sib_opt of
                    NONE => (bestScore0, SOME bestMove0)
                    | SOME (sib_score, sib_move) =>
                            if maximizingPlayer then
                                if sib_score > bestScore0 then (sib_score, SOME sib_move)
                                else (bestScore0, SOME bestMove0)
                            else
                                if sib_score < bestScore0 then (sib_score, SOME sib_move)
                                else (bestScore0, SOME bestMove0)

            in
                (final_score, final_move)
            end

fun minimax_full_parallel_ttt node depth maximizingPlayer evaluate (apply_move : 'a -> 'b -> bool -> 'a) order =
    if depth = 0 then (evaluate node maximizingPlayer, NONE)
    else
        let
            val next_turn = not maximizingPlayer
            val moves = order node maximizingPlayer
            val move_seq = Seq.fromList moves

            val child_boards = Seq.map (fn m => apply_move node m maximizingPlayer) move_seq
            val child_results = Seq.map (fn b => minimax_full_parallel_ttt b (depth-1) next_turn evaluate apply_move order) child_boards
            val n = Seq.length child_results

            val best_idx =
                if maximizingPlayer then
                    Parallel.reduce
                        (fn (i,j) =>
                            let
                                val (si,_) = Seq.nth child_results i
                                val (sj,_) = Seq.nth child_results j
                            in if si >= sj then i else j end)
                        (~1) (0,n) (fn k => k)
                else
                    Parallel.reduce
                        (fn (i,j) =>
                            let
                                val (si,_) = Seq.nth child_results i
                                val (sj,_) = Seq.nth child_results j
                            in if si <= sj then i else j end)
                        (~1) (0,n) (fn k => k)
        in
            if best_idx = ~1 then
                if maximizingPlayer then (Real.negInf, NONE) else (Real.posInf, NONE)
            else
                let
                    val (score,_) = Seq.nth child_results best_idx
                    val best_move = Seq.nth move_seq best_idx
                in
                    (score, SOME best_move)
                end
        end

fun alpha_beta_search_ttt node depth maximizingPlayer alpha beta evaluate (order : 'a -> bool -> 'b list) (apply_move : 'a -> 'b -> bool -> 'a) =
    if depth = 0 then (evaluate node maximizingPlayer, NONE)
    else
        let
            val moves = order node maximizingPlayer
            val move_seq = Seq.fromList moves

            fun loop i a b best_idx best_score =
                if i >= Seq.length move_seq then (best_score, best_idx)
                else
                    let
                        val m = Seq.nth move_seq i
                        val child = apply_move node m maximizingPlayer
                        val (score, _) = alpha_beta_search_ttt child (depth-1) (not maximizingPlayer) a b evaluate order apply_move

                        val (new_best_score, new_best_idx) =
                            if maximizingPlayer then
                                if score > best_score then (score, i) else (best_score, best_idx)
                            else
                                if score < best_score then (score, i) else (best_score, best_idx)

                        val new_alpha = if maximizingPlayer then Real.max(a, score) else a
                        val new_beta  = if not maximizingPlayer then Real.min(b, score) else b
                    in
                        if new_alpha >= new_beta then (new_best_score, new_best_idx)
                        else loop (i+1) new_alpha new_beta new_best_idx new_best_score
                    end

            val init_score = if maximizingPlayer then Real.negInf else Real.posInf
            val (best_score_idx_score, best_idx) = loop 0 alpha beta ~1 init_score
        in
            if best_idx = ~1 then (init_score, NONE)
            else (best_score_idx_score, SOME (Seq.nth move_seq best_idx))
        end

fun pvs_search_ttt node depth maximizingPlayer alpha beta evaluate (order : 'a -> bool -> 'b list) (apply_move : 'a -> 'b -> bool -> 'a) =
    if depth = 0 then (evaluate node maximizingPlayer, NONE)
    else
        case order node maximizingPlayer of
            [] => (evaluate node maximizingPlayer, NONE)
          | first_move :: rest_moves =>
            let
                val first_child = apply_move node first_move maximizingPlayer
                val (first_score, _) = pvs_search_ttt first_child (depth-1) (not maximizingPlayer) alpha beta evaluate order apply_move

                val (a0,b0) =
                    if maximizingPlayer then (Real.max(alpha, first_score), beta)
                    else (alpha, Real.min(beta, first_score))

                val bestScore0 = first_score
                val bestMove0  = first_move

                val sibling_seq = Seq.fromList rest_moves
                val n = Seq.length sibling_seq
            val cutoff = (a0 >= b0) orelse (bestScore0 > beta)
            val best_sib_opt =
                    if cutoff orelse n = 0 then NONE
                    else
                    let
                        val (best_sc, best_idx) =
                            Parallel.reduce
                                (fn ((s1: real,i1),(s2: real,i2)) =>
                                    if maximizingPlayer then
                                        case Real.compare (s1, s2) of
                                            GREATER => (s1, i1)
                                        | EQUAL   => if i1 < i2 then (s1, i1) else (s2, i2)
                                        | LESS    => (s2, i2)
                                    else
                                        case Real.compare (s1, s2) of
                                            LESS => (s1, i1)
                                        | EQUAL   => if i1 < i2 then (s1, i1) else (s2, i2)
                                        |  GREATER  => (s2, i2)
                                )
                                (if maximizingPlayer then Real.negInf else Real.posInf, ~1)
                                (0, n)
                                (fn i =>
                                    let
                                        val move = Seq.nth sibling_seq i
                                        val child = apply_move node move maximizingPlayer
                                        val (score, _) = alpha_beta_search_ttt child (depth-1) (not maximizingPlayer) a0 b0 evaluate order apply_move
                                    in
                                        (score, i)
                                    end
                                )
                    in
                        if best_idx = ~1 then NONE
                        else SOME (best_sc, Seq.nth sibling_seq best_idx)
                    end
                val (final_score, final_move) =
                    case best_sib_opt of
                        NONE => (bestScore0, SOME bestMove0)
                      | SOME (sib_score, sib_move) =>
                        if maximizingPlayer then
                            if sib_score > bestScore0 then (sib_score, SOME sib_move)
                            else (bestScore0, SOME bestMove0)
                        else
                            if sib_score < bestScore0 then (sib_score, SOME sib_move)
                            else (bestScore0, SOME bestMove0)
            in
                (final_score, final_move)
            end


end