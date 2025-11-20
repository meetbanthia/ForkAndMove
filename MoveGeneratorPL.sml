structure MoveGeneratorPL : sig
    type move = ((int*int) * (int*int) * real)
    val generate_move_order: PieceList.piece_list * PieceList.piece_list -> move list
    val apply_move : PieceList.piece_list * PieceList.piece_list -> move -> PieceList.piece_list * PieceList.piece_list
end = struct

type move = ((int*int) * (int*int) * real)

(* Convert PieceList.piece_list (16-tuple of Word8.word) -> int list *)
fun pl_to_int_list pl =
    let
        val (a0,a1,a2,a3,a4,a5,a6,a7,
             a8,a9,a10,a11,a12,a13,a14,a15) = pl
    in
        [Word8.toInt a0, Word8.toInt a1, Word8.toInt a2, Word8.toInt a3,
         Word8.toInt a4, Word8.toInt a5, Word8.toInt a6, Word8.toInt a7,
         Word8.toInt a8, Word8.toInt a9, Word8.toInt a10, Word8.toInt a11,
         Word8.toInt a12, Word8.toInt a13, Word8.toInt a14, Word8.toInt a15]
    end

(* Convert int list (len 16) -> PieceList.piece_list *)
fun int_list_to_pl lst =
    let
        fun nth i = List.nth (lst, i)
    in
        (Word8.fromInt (nth 0),  Word8.fromInt (nth 1),  Word8.fromInt (nth 2),  Word8.fromInt (nth 3),
         Word8.fromInt (nth 4),  Word8.fromInt (nth 5),  Word8.fromInt (nth 6),  Word8.fromInt (nth 7),
         Word8.fromInt (nth 8),  Word8.fromInt (nth 9),  Word8.fromInt (nth 10), Word8.fromInt (nth 11),
         Word8.fromInt (nth 12), Word8.fromInt (nth 13), Word8.fromInt (nth 14), Word8.fromInt (nth 15))
    end

(* Convert square number (1..64) to (rank,file) with 0-based rank,file *)
fun square_to_coord sq =
    if sq = 0 then (0,0)
    else
        let
            val rank = (sq - 1) div 8
            val file = (sq - 1) mod 8
        in
            (rank, file)
        end

(* Convert (rank,file) to square number 1..64 *)
fun coord_to_square (r,f) = r*8 + f + 1

(* Check bounds *)
fun in_bounds (r,f) = r>=0 andalso r<8 andalso f>=0 andalso f<8

(* occupied: is square number sq present in piece_list pl? *)
fun occupied (sq:int) (pl:PieceList.piece_list) : bool =
    List.exists (fn x => x = sq) (pl_to_int_list pl)

(* occupied_any: check in both piece lists (white+black combined) *)
fun occupied_any (sq:int) (wpl,bpl) =
    List.exists (fn x => x = sq) ((pl_to_int_list wpl) @ (pl_to_int_list bpl))

(* Generate all moves for a single piece type at (r,f) *)
fun moves_for_piece (r,f,pt,isWhite,wpl,bpl) =
    let
        (* Knight jumps *)
        val knight_offsets = [(2,1),(1,2),(~1,2),(~2,1),(~2,~1),(~1,~2),(1,~2),(2,~1)]

        (* King offsets *)
        val king_offsets = [(1,0),(0,1),(~1,0),(0,~1),(1,1),(1,~1),(~1,1),(~1,~1)]

        (* Sliding directions *)
        val rook_dirs = [(1,0),(~1,0),(0,1),(0,~1)]
        val bishop_dirs = [(1,1),(1,~1),(~1,1),(~1,~1)]
        val queen_dirs = rook_dirs @ bishop_dirs

        val own_pl = if isWhite then wpl else bpl
        val opp_pl = if isWhite then bpl else wpl

        (* Generate sliding moves returning list of (rank,file) destinations *)
        fun slide_dirs dirs =
            let
                fun slide_dir (dr,df) =
                    let
                        fun loop nr nf acc =
                            if not (in_bounds(nr,nf)) then acc
                            else
                                let
                                    val tsq = coord_to_square (nr,nf)
                                in
                                    if occupied tsq own_pl then acc
                                    else if occupied tsq opp_pl then (nr,nf)::acc
                                    else loop (nr+dr) (nf+df) ((nr,nf)::acc)
                                end
                    in
                        loop (r+dr) (f+df) []
                    end
            in
                List.concat (List.map slide_dir dirs)
            end

        (* Helper: build list of valid target coords from offsets *)
        fun valid_from_offsets offs =
            let
                fun map_offset (dr,df) = (r+dr,f+df)
                fun filter_valid coords = List.filter (fn (nr,nf) =>
                        in_bounds(nr,nf)
                        andalso not (occupied (coord_to_square (nr,nf)) own_pl)
                    ) coords
            in
                filter_valid (List.map map_offset offs)
            end

        val target_coords =
            case pt of
                 #"N" => valid_from_offsets knight_offsets
               | #"n" => valid_from_offsets knight_offsets
               | #"K" => valid_from_offsets king_offsets
               | #"k" => valid_from_offsets king_offsets
               | #"R" => slide_dirs rook_dirs
               | #"r" => slide_dirs rook_dirs
               | #"B" => slide_dirs bishop_dirs
               | #"b" => slide_dirs bishop_dirs
               | #"Q" => slide_dirs queen_dirs
               | #"q" => slide_dirs queen_dirs
               | #"P" =>
                     let
                        val dir = if isWhite then 1 else ~1
                        val forward1 = (r+dir,f)
                        val forward2 = (r+2*dir,f)
                        val captures = [(r+dir,f+1),(r+dir,f-1)]

                        val allOcc = (pl_to_int_list wpl) @ (pl_to_int_list bpl)
                        fun occ sq = List.exists (fn x => x = sq) allOcc

                        val moves1 =
                            (if in_bounds forward1 andalso not (occ (coord_to_square forward1))
                             then [forward1] else [])

                        val moves2 =
                            (if ((r=1 andalso isWhite) orelse (r=6 andalso not isWhite))
                                andalso in_bounds forward2
                                andalso not (occ (coord_to_square forward2))
                             then [forward2] else [])

                        val cap_moves = List.filter (fn c =>
                             in_bounds c andalso occupied (coord_to_square c) (if isWhite then bpl else wpl)) captures

                     in
                        moves1 @ moves2 @ cap_moves
                     end
               | #"p" =>
                     let
                        val dir = if isWhite then 1 else ~1
                        val forward1 = (r+dir,f)
                        val forward2 = (r+2*dir,f)
                        val captures = [(r+dir,f+1),(r+dir,f-1)]

                        val allOcc = (pl_to_int_list wpl) @ (pl_to_int_list bpl)
                        fun occ sq = List.exists (fn x => x = sq) allOcc

                        val moves1 =
                            (if in_bounds forward1 andalso not (occ (coord_to_square forward1))
                             then [forward1] else [])

                        val moves2 =
                            (if ((r=1 andalso isWhite) orelse (r=6 andalso not isWhite))
                                andalso in_bounds forward2
                                andalso not (occ (coord_to_square forward2))
                             then [forward2] else [])

                        val cap_moves = List.filter (fn c =>
                             in_bounds c andalso occupied (coord_to_square c) (if isWhite then bpl else wpl)) captures

                     in
                        moves1 @ moves2 @ cap_moves
                     end
               | _ => []
    in
        List.map (fn toC => ((r,f),toC,1.0)) target_coords
    end

(* Generate all moves for a side *)
fun generate_side_moves (plOwn, plOpp, isWhite) =
    let
        val squares = pl_to_int_list plOwn
        fun idxs n acc = if n<0 then acc else idxs (n-1) (n::acc)
        val piece_indices = idxs 15 []  (* 0..15 *)
        fun loop [] acc = acc
          | loop (idx::rest) acc =
            let
                val sq = List.nth (squares, idx)
                val (r,f) = square_to_coord sq
                val pt =
                    if isWhite then
                        (if idx<8 then #"P"
                         else if idx<10 then #"N"
                         else if idx<12 then #"B"
                         else if idx<14 then #"R"
                         else if idx=14 then #"Q"
                         else #"K")
                    else
                        (if idx<8 then #"p"
                         else if idx<10 then #"n"
                         else if idx<12 then #"b"
                         else if idx<14 then #"r"
                         else if idx=14 then #"q"
                         else #"k")
                val piece_moves = moves_for_piece(r,f,pt,isWhite,plOwn,plOpp)
            in
                loop rest (piece_moves @ acc)
            end
    in
        loop piece_indices []
    end

(* Public: generate all moves *)
fun generate_move_order (wpl,bpl) =
    let
        val white_moves = generate_side_moves(wpl,bpl,true)
        val black_moves = generate_side_moves(bpl,wpl,false)
    in
        white_moves @ black_moves
    end

(* Apply a move to piece_lists *)
fun apply_move (wpl,bpl) (((fr,ff),(tr,tf),_)) =
    let
        val fromSq = coord_to_square (fr,ff)
        val toSq = coord_to_square (tr,tf)

        fun move_in_pl pl =
            let
                val lst = pl_to_int_list pl
                val new_lst = List.map (fn x => if x = fromSq then toSq else x) lst
            in
                int_list_to_pl new_lst
            end

        val whiteSquares = pl_to_int_list wpl
    in
        if List.exists (fn x => x = fromSq) whiteSquares then
            (move_in_pl wpl, bpl)
        else
            (wpl, move_in_pl bpl)
    end

end
