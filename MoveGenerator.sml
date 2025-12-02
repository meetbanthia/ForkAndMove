structure MoveGenerator : sig

    type move = ((int*int) * (int*int))
    val generate_move_order: Board.brep -> move list
    val apply_move : Board.brep -> move -> Board.brep
    val generate_color_move_order : Board.brep -> bool -> bool-> bool-> bool-> bool-> bool-> bool -> move list
end = 
struct
    type move = ((int*int) * (int*int))

    (* --- Bitboard masks --- *)

    (* Files: a-h *)
    val file_a : Word64.word = 0wx0101010101010101
    val file_b : Word64.word = 0wx0202020202020202
    val file_c : Word64.word = 0wx0404040404040404
    val file_d : Word64.word = 0wx0808080808080808
    val file_e : Word64.word = 0wx1010101010101010
    val file_f : Word64.word = 0wx2020202020202020
    val file_g : Word64.word = 0wx4040404040404040
    val file_h : Word64.word = 0wx8080808080808080

    (* Masks for excluding a file or h file when shifting *)
    val not_a_file : Word64.word = Word64.notb file_a
    val not_h_file : Word64.word = Word64.notb file_h
    val not_ab_file : Word64.word = Word64.notb (Word64.orb(file_a, file_b))
    val not_gh_file : Word64.word = Word64.notb (Word64.orb(file_g, file_h))
    
    (* Ranks: 1-8, rank1 = bottom row *)
    val rank1 : Word64.word = 0wx00000000000000FF
    val rank2 : Word64.word = 0wx000000000000FF00
    val rank3 : Word64.word = 0wx0000000000FF0000
    val rank4 : Word64.word = 0wx00000000FF000000
    val rank5 : Word64.word = 0wx000000FF00000000
    val rank6 : Word64.word = 0wx0000FF0000000000
    val rank7 : Word64.word = 0wx00FF000000000000
    val rank8 : Word64.word = 0wxFF00000000000000

    (* Masks for excluding top/bottom ranks if needed *)
    val not_rank_1 = Word64.notb rank1
    val not_rank_8 = Word64.notb rank8

    val not_lr_rank = not_rank_1
    val not_fr_rank = not_rank_8




    (* --- Shift constants --- *)
    val s1  = Word.fromInt 1
    val s8  = Word.fromInt 8
    val s7  = Word.fromInt 7
    val s9  = Word.fromInt 9

    (* --- Bitboard helpers --- *)
    fun north b = Word64.<< (b, s8)
    fun south b = Word64.>> (b, s8)

    fun east b = Word64.<< (Word64.andb (b, not_h_file), s1)
    fun west b = Word64.>> (Word64.andb (b, not_a_file), s1)
    fun northeast b = Word64.<< (Word64.andb (b, not_h_file), s9)
    fun northwest b = Word64.<< (Word64.andb (b, not_a_file), s7)
    fun southeast b = Word64.>> (Word64.andb (b, not_h_file), s7)
    fun southwest b = Word64.>> (Word64.andb (b, not_a_file), s9)
    

    (* --- Convert bit index to (row,col) --- *)

    fun indexToCoord i = (7 - (i div 8), 7 - (i mod 8))
    fun coord_to_index (r,c) = (7-r)*8 + (7-c)

    (* --- Get occupancy bitboards --- *)
    fun occupancy (P,R,N,B,K,Q,p,r,n,b,k,q) =
        let
            val white = Word64.orb(P, Word64.orb(R, Word64.orb(N, Word64.orb(B, Word64.orb(K,Q)))))
            val black = Word64.orb(p, Word64.orb(r, Word64.orb(n, Word64.orb(b, Word64.orb(k,q)))))
        in (white, black) end

    (* --- Pawn moves --- *)
    fun white_pawn_moves P ooc_white occ_black =
        let
            val empty = Word64.notb (Word64.orb(ooc_white, occ_black))
            (* 1-step moves: any pawn can move north if empty *)
            val one_step = Word64.andb(north P, empty)

            (* 2-step moves: only from rank 2 and if 1-step is empty *)
            val rank_3_pawn = Word64.andb(one_step, rank3)
            val two_step = Word64.andb(north rank_3_pawn, empty)

            (* Captures *)
            val attacksL = Word64.andb(northwest P, occ_black)
            val attacksR = Word64.andb(northeast P, occ_black)
        in
            Word64.orb(one_step, Word64.orb(two_step, Word64.orb(attacksL, attacksR)))
        end

    fun black_pawn_moves p ooc_white occ_black =
        let
            val empty = Word64.notb (Word64.orb(ooc_white, occ_black))
            (* 1-step moves *)
            val one_step = Word64.andb(south p, empty)
            val rank_7_pawns = Word64.andb(p, rank7)
            val one_step_from_rank7 = Word64.andb(south rank_7_pawns, empty)
            val two_step = Word64.andb(south one_step_from_rank7, empty)
            (* 2-step moves: only from rank 7 and if 1-step is empty *)
            val rank_6_pawns = Word64.andb(one_step, rank6)
            val two_step = Word64.andb(south rank_6_pawns, empty)

            (* Captures *)
            val attacksL = Word64.andb(southwest p, ooc_white)
            val attacksR = Word64.andb(southeast p, ooc_white)
        in
            Word64.orb(one_step, Word64.orb(two_step, Word64.orb(attacksL, attacksR)))
        end


    (* --- Knight moves --- *)
    fun knight_moves n occOwn =
        let

            val n1 = Word64.<< (Word64.andb(n, not_h_file), Word.fromInt 15)
            val n2 = Word64.<< (Word64.andb(n, not_a_file), Word.fromInt 17)
            val n3 = Word64.<< (Word64.andb(n, not_gh_file), Word.fromInt 6)
            val n4 = Word64.<< (Word64.andb(n, not_ab_file), Word.fromInt 10)
            val n5 = Word64.andb(Word64.>> (n, Word.fromInt 15), not_h_file)
            val n6 = Word64.andb(Word64.>> (n, Word.fromInt 17), not_a_file)
            val n7 = Word64.andb(Word64.>> (n, Word.fromInt 6), not_gh_file)
            val n8 = Word64.andb(Word64.>> (n, Word.fromInt 10), not_ab_file)
            val moves = Word64.orb(n1,
                        Word64.orb(n2,
                        Word64.orb(n3,
                        Word64.orb(n4,
                        Word64.orb(n5,
                        Word64.orb(n6,
                        Word64.orb(n7, n8)))))))
        in
            Word64.andb(moves, Word64.notb occOwn)
        end


    (* --- Sliding pieces --- *)
    fun slide shift mask b blockers =
        let
            fun loop b acc =
                let val next = shift (Word64.andb (b, mask)) in
                    if next = 0w0 then acc
                    else
                        let val acc' = Word64.orb(acc, next)
                        in if Word64.andb(next, blockers) <> 0w0 then acc' else loop next acc' end
                end
        in loop b 0w0 end

    fun rook_moves b ooc_white occ_black isWhite =
        let
            val blockers = Word64.orb(ooc_white, occ_black)
            val own = if isWhite then ooc_white else occ_black
            val moves = Word64.orb(
                slide north not_lr_rank b blockers,
                Word64.orb(
                    slide south not_fr_rank b blockers,
                    Word64.orb(
                        slide east not_h_file b blockers,
                        slide west not_a_file b blockers
                    )
                )
            )
        in Word64.andb(moves, Word64.notb own) end

    fun bishop_moves b ooc_white occ_black isWhite =
        let
            val blockers = Word64.orb(ooc_white, occ_black)
            val own = if isWhite then ooc_white else occ_black
            val moves = Word64.orb(
                slide northeast not_h_file b blockers,
                Word64.orb(
                    slide northwest not_a_file b blockers,
                    Word64.orb(
                        slide southeast not_h_file b blockers,
                        slide southwest not_a_file b blockers
                    )
                )
            )
        in Word64.andb(moves, Word64.notb own) end

    fun queen_moves b ooc_white occ_black isWhite =
        Word64.orb(rook_moves b ooc_white occ_black isWhite,
                   bishop_moves b ooc_white occ_black isWhite)

    fun king_moves k occOwn =
        Word64.andb(
            Word64.orb(north k, Word64.orb(south k, Word64.orb(east k, Word64.orb(west k,
            Word64.orb(northeast k, Word64.orb(northwest k, Word64.orb(southeast k, southwest k))))))),
            Word64.notb occOwn)

    (* --- Bitboard to coordinates --- *)
    fun bitboard_to_moves b =
        let
            fun loop i acc =
                if i >= 64 then acc
                else
                    let val bit = Word64.andb(b, Word64.<< (0w1, Word.fromInt i))
                    in loop (i+1) (if bit<>0w0 then indexToCoord i::acc else acc) end
        in loop 0 [] end

    (* --- Generate moves per piece --- *)
    fun generate_pawn_moves P ooc_white occ_black isWhite =
        let
            val squares = bitboard_to_moves P
            fun aux [] acc = acc
              | aux (sq::rest) acc =
                  let
                      (* this indexing is reverse ig *)
                      val idx = coord_to_index sq

                      
                      val bb = Word64.<< (0w1, Word.fromInt idx)
                      val targets = if isWhite then white_pawn_moves bb ooc_white occ_black
                                    else black_pawn_moves bb ooc_white occ_black
                      val moves = List.map (fn t => (sq,t)) (bitboard_to_moves targets)
                  in aux rest (moves @ acc) end
        in aux squares [] end

    fun generate_knight_moves n occOwn =
        let
            val squares = bitboard_to_moves n
            fun aux [] acc = acc
              | aux (sq::rest) acc =
                  let
                      val idx = coord_to_index sq
                      val bb = Word64.<< (0w1, Word.fromInt idx)
                      val targets = knight_moves bb occOwn
                      val moves = List.map (fn t => (sq,t)) (bitboard_to_moves targets)
                  in aux rest (moves @ acc) end
        in aux squares [] end

    fun generate_sliding_moves b ooc_white occ_black isWhite pieceFunc =
        let
            val squares = bitboard_to_moves b
            fun aux [] acc = acc
              | aux (sq::rest) acc =
                  let
                      val idx = coord_to_index sq
                      val bb = Word64.<< (0w1, Word.fromInt idx)
                      val targets = pieceFunc bb ooc_white occ_black isWhite
                      val moves = List.map (fn t => (sq,t)) (bitboard_to_moves targets)
                  in aux rest (moves @ acc) end
        in aux squares [] end

    fun generate_king_moves k occOwn =
        let
            val squares = bitboard_to_moves k
            fun aux [] acc = acc
              | aux (sq::rest) acc =
                  let
                      val idx = coord_to_index sq
                      val bb = Word64.<< (0w1, Word.fromInt idx)
                      val targets = king_moves bb occOwn

                      val moves = List.map (fn t => (sq,t)) (bitboard_to_moves targets)
                  in aux rest (moves @ acc) end
        in aux squares [] end

    (* --- Generate all moves for both colors --- *)
    fun generate_piece_moves (P,R,N,B,K,Q,p,r,n,b,k,q) =
        let
            val (ooc_white, occ_black) = occupancy (P,R,N,B,K,Q,p,r,n,b,k,q)
            (* White moves *)
            val w_p_moves = generate_pawn_moves P ooc_white occ_black true
            val w_n_moves = generate_knight_moves N ooc_white
            val w_k_moves = generate_king_moves K ooc_white
            val w_r_moves = generate_sliding_moves R ooc_white occ_black true rook_moves
            val w_b_moves = generate_sliding_moves B ooc_white occ_black true bishop_moves
            val w_q_moves = generate_sliding_moves Q ooc_white occ_black true queen_moves
            (* Black moves *)
            val b_P_moves = generate_pawn_moves p ooc_white occ_black false
            val b_N_moves = generate_knight_moves n occ_black
            val b_K_moves = generate_king_moves k occ_black
            val b_R_moves = generate_sliding_moves r ooc_white occ_black false rook_moves
            val b_B_moves = generate_sliding_moves b ooc_white occ_black false bishop_moves
            val b_Q_moves = generate_sliding_moves q ooc_white occ_black false queen_moves
        in
            w_p_moves @ w_n_moves @ w_k_moves @ w_r_moves @ w_b_moves @ w_q_moves
            @ b_P_moves @ b_N_moves @ b_K_moves @ b_R_moves @ b_B_moves @ b_Q_moves
        end

    fun generate_move_order brep =
        generate_piece_moves brep

    fun generate_color_move_order brep white pawn knight bishop rook king queen = 
        let
            val (P,R,N,B,K,Q,p,r,n,b,k,q) = brep
            val (ooc_white, occ_black) = occupancy (P,R,N,B,K,Q,p,r,n,b,k,q)
            (* White moves *)
            val w_p_moves = generate_pawn_moves P ooc_white occ_black true
            val w_n_moves = generate_knight_moves N ooc_white
            val w_k_moves = generate_king_moves K ooc_white
            val w_r_moves = generate_sliding_moves R ooc_white occ_black true rook_moves
            val w_b_moves = generate_sliding_moves B ooc_white occ_black true bishop_moves
            val w_q_moves = generate_sliding_moves Q ooc_white occ_black true queen_moves
            (* Black moves *)
            val b_P_moves = generate_pawn_moves p ooc_white occ_black false
            val b_N_moves = generate_knight_moves n occ_black
            val b_K_moves = generate_king_moves k occ_black
            val b_R_moves = generate_sliding_moves r ooc_white occ_black false rook_moves
            val b_B_moves = generate_sliding_moves b ooc_white occ_black false bishop_moves
            val b_Q_moves = generate_sliding_moves q ooc_white occ_black false queen_moves
        in
            []
                @ (if white andalso pawn   then w_p_moves else [])
                @ (if white andalso knight then w_n_moves else [])
                @ (if white andalso king   then w_k_moves else [])
                @ (if white andalso rook   then w_r_moves else [])
                @ (if white andalso bishop then w_b_moves else [])
                @ (if white andalso queen  then w_q_moves else [])
                @ (if (not white) andalso pawn   then b_P_moves else [])
                @ (if (not white) andalso knight then b_N_moves else [])
                @ (if (not white) andalso king   then b_K_moves else [])
                @ (if (not white) andalso rook   then b_R_moves else [])
                @ (if (not white) andalso bishop then b_B_moves else [])
                @ (if (not white) andalso queen  then b_Q_moves else [])
        end
    (* --- Apply move --- *)

    fun apply_move brep ((fromR,fromC),(toR,toC)) =
        let
            fun remove_piece (P,R,N,B,K,Q,p,r,n,b,k,q) location =
                let
                val mask = Word64.notb(Word64.<<(0w1, Word.fromInt location))
                in
                    (Word64.andb(P,mask),
                    Word64.andb(R,mask),
                    Word64.andb(N,mask),
                    Word64.andb(B,mask),
                    Word64.andb(K,mask),
                    Word64.andb(Q,mask),
                    Word64.andb(p,mask),
                    Word64.andb(r,mask),
                    Word64.andb(n,mask),
                    Word64.andb(b,mask),
                    Word64.andb(k,mask),
                    Word64.andb(q,mask)
                    )
                end
            val (P,R,N,B,K,Q,p,r,n,b,k,q) = brep
            fun update bb from_index to_index =
                Word64.orb(Word64.andb(bb, Word64.notb(Word64.<< (0w1, Word.fromInt from_index))),
                           Word64.<< (0w1, Word.fromInt to_index))
            val from_index = coord_to_index (fromR, fromC)
            val to_index = coord_to_index (toR, toC)
            val (P,R,N,B,K,Q,p,r,n,b,k,q) = remove_piece (P,R,N,B,K,Q,p,r,n,b,k,q) to_index
            val from_mask = Word64.<< (0w1, Word.fromInt from_index)
            val to_mask = Word64.<< (0w1, Word.fromInt to_index)

            val is_white_pawn = Word64.andb(P, from_mask) <> 0w0
            val is_black_pawn = Word64.andb(p, from_mask) <> 0w0

            val is_white_promotion = is_white_pawn andalso toR = 0
            val is_black_promotion = is_black_pawn andalso toR = 7

            val P' = if is_white_pawn then
                        if is_white_promotion then Word64.andb(P, Word64.notb from_mask)
                        else update P from_index to_index
                     else P
            val N' = if Word64.andb(N, from_mask) <> 0w0 then update N from_index to_index else N
            val R' = if Word64.andb(R, from_mask) <> 0w0 then update R from_index to_index else R
            val B' = if Word64.andb(B, from_mask) <> 0w0 then update B from_index to_index else B
            val Q' = if Word64.andb(Q, from_mask) <> 0w0 then update Q from_index to_index
                     else if is_white_promotion then Word64.orb(Q, to_mask)
                     else Q
            val K' = if Word64.andb(K, from_mask) <> 0w0 then update K from_index to_index else K

            val p' = if is_black_pawn then
                        if is_black_promotion then Word64.andb(p, Word64.notb from_mask)
                        else update p from_index to_index
                     else p
            val n' = if Word64.andb(n, from_mask) <> 0w0 then update n from_index to_index else n
            val r' = if Word64.andb(r, from_mask) <> 0w0 then update r from_index to_index else r
            val b' = if Word64.andb(b, from_mask) <> 0w0 then update b from_index to_index else b
            val q' = if Word64.andb(q, from_mask) <> 0w0 then update q from_index to_index
                     else if is_black_promotion then Word64.orb(q, to_mask)
                     else q
            val k' = if Word64.andb(k, from_mask) <> 0w0 then update k from_index to_index else k
        in
            (P',R',N',B',K',Q',p',r',n',b',k',q')
        end

end
