(*This need to be implemented after move generator and board rep
This is where parallelism comes into consideration*)

structure  Search:
sig
    val eval_position: Board.brep -> real

(* node depth maximizingPlayer alpha beta evaluate *)
    val alpha_beta_search: Board.brep -> int -> bool -> real -> real -> (Board.brep -> real) -> real
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

    fun eval_position bmaps =
        let
            val material_factor = material bmaps
            (*Need to add other factors too like mobility *)
        in
            Real.fromInt material_factor
        end


    (* 
    fun minimax node depth maximizingPlayer evaluate apply_move=
        if depth = 0 then evaluate node
        else
            let
                val next_turn = not maximizingPlayer
                val move_order = node (* list of moves *)
                val child_boards = Parallel.map (fn(i) => apply_move node i) move_order
                val child_scores = Parallel.map (fn(i) => minimax i (depth-1) next_turn evaluate apply_move)
                val score = 
                    if maximizingPlayer then
                        Parallel.reduce (fn(a,b) => Real.max(a,b)) child_scores
                    else 
                        Parallel.reduce (fn(a,b) => Real.min(a,b)) child_scores
            in
                score
            end 
    *)
    
    fun alpha_beta_search node depth maximizingPlayer alpha beta evaluate =
        if depth = 0 then evaluate node
        else
            let   
                val moves = Seq.fromList (MoveGenerator.generate_move_order node)
                val child_nodes = Seq.map (fn(x) => MoveGenerator.apply_move node x) moves            
                fun loop node i alpha beta maximizingPlayer =
                    if i >= (Seq.length child_nodes) orelse beta <= alpha then 
                        if maximizingPlayer then Real.negInf else Real.posInf
                    else
                        let
                            val child_node = Seq.nth child_nodes i
                            val score = alpha_beta_search child_node (depth-1) (not maximizingPlayer) alpha beta evaluate
                            val (a, b) = if maximizingPlayer then (Real.max(alpha, score),beta) else (alpha, Real.min(beta, score))
                            val f = if maximizingPlayer then Real.max else Real.min
                        in
                            f(score, loop child_node (i+1) a b maximizingPlayer)
                        end
            in
                loop node 0 alpha beta maximizingPlayer
            end

    fun pvs_search node depth maximizingPlayer alpha beta evaluate =
        if depth = 0 then evaluate node
        else
            let
                val moves = Seq.fromList (MoveGenerator.generate_move_order node)
                val child_nodes = Seq.map (fn(x) => MoveGenerator.apply_move node x) moves
                val left_most_node = Seq.nth child_nodes 0
                val score1 = pvs_search left_most_node (depth-1) (not maximizingPlayer) alpha beta evaluate
                val rest_scores = Seq.map (fn(x) => alpha_beta_search x (depth-1) (not maximizingPlayer) alpha beta evaluate) child_nodes
                val (a,b) = if maximizingPlayer then (Real.max(score1,alpha), beta) else (alpha, Real.min(beta, score1))
                val f = if maximizingPlayer then Real.max else Real.min
            in
                Parallel.reduce (fn(a,b) => f(a,b)) (Real.fromInt 0) (1,Seq.length child_nodes) (fn(i) => Seq.nth rest_scores i)
            end
end