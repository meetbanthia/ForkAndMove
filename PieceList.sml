structure PieceList : sig
    type piece_list =
        Word8.word * Word8.word * Word8.word * Word8.word *
        Word8.word * Word8.word * Word8.word * Word8.word *
        Word8.word * Word8.word * Word8.word * Word8.word *
        Word8.word * Word8.word * Word8.word * Word8.word

    val array_to_piecelist : (char array array) -> piece_list * piece_list
    val initiate_standard_chess : unit -> piece_list * piece_list

    val print_board : piece_list * piece_list -> unit
end =
struct

type piece_list =
    Word8.word * Word8.word * Word8.word * Word8.word * Word8.word * Word8.word * Word8.word * Word8.word * Word8.word * Word8.word * Word8.word * Word8.word * Word8.word * Word8.word * Word8.word * Word8.word

val zero = Word8.fromInt 0

fun empty_list () : piece_list =
    (zero, zero, zero, zero, zero, zero, zero, zero,
     zero, zero, zero, zero, zero, zero, zero, zero)

fun update16 (pl: piece_list, idx: int, w: Word8.word) =
    let 
        val (a0,a1,a2,a3,a4,a5,a6,a7,
       a8,a9,a10,a11,a12,a13,a14,a15) = pl
    in
    case idx of
            0  => (w, a1,a2,a3,a4,a5,a6,a7, a8,a9,a10,a11,a12,a13,a14,a15)
        | 1  => (a0, w,a2,a3,a4,a5,a6,a7, a8,a9,a10,a11,a12,a13,a14,a15)
        | 2  => (a0,a1, w,a3,a4,a5,a6,a7, a8,a9,a10,a11,a12,a13,a14,a15)
        | 3  => (a0,a1,a2, w,a4,a5,a6,a7, a8,a9,a10,a11,a12,a13,a14,a15)
        | 4  => (a0,a1,a2,a3, w,a5,a6,a7, a8,a9,a10,a11,a12,a13,a14,a15)
        | 5  => (a0,a1,a2,a3,a4, w,a6,a7, a8,a9,a10,a11,a12,a13,a14,a15)
        | 6  => (a0,a1,a2,a3,a4,a5, w,a7, a8,a9,a10,a11,a12,a13,a14,a15)
        | 7  => (a0,a1,a2,a3,a4,a5,a6, w, a8,a9,a10,a11,a12,a13,a14,a15)

        | 8  => (a0,a1,a2,a3,a4,a5,a6,a7, w,a9,a10,a11,a12,a13,a14,a15)
        | 9  => (a0,a1,a2,a3,a4,a5,a6,a7, a8, w,a10,a11,a12,a13,a14,a15)
        | 10 => (a0,a1,a2,a3,a4,a5,a6,a7, a8,a9, w,a11,a12,a13,a14,a15)
        | 11 => (a0,a1,a2,a3,a4,a5,a6,a7, a8,a9,a10, w,a12,a13,a14,a15)
        | 12 => (a0,a1,a2,a3,a4,a5,a6,a7, a8,a9,a10,a11, w,a13,a14,a15)
        | 13 => (a0,a1,a2,a3,a4,a5,a6,a7, a8,a9,a10,a11,a12, w,a14,a15)
        | 14 => (a0,a1,a2,a3,a4,a5,a6,a7, a8,a9,a10,a11,a12,a13, w,a15)
        | 15 => (a0,a1,a2,a3,a4,a5,a6,a7, a8,a9,a10,a11,a12,a13,a14, w)
        | _ => pl 
    end


(* board indices 1..64, rank1 = 1..8 *)
fun square_of (rank:int, file:int) =
    Word8.fromInt (rank * 8 + file + 1)

fun white_index c =
    case c of
        #"P" => SOME 0
      | #"N" => SOME 8
      | #"B" => SOME 9
      | #"R" => SOME 10
      | #"Q" => SOME 11
      | #"K" => SOME 12
      | _    => NONE

fun black_index c =
    case c of
        #"p" => SOME 0
      | #"n" => SOME 8
      | #"b" => SOME 9
      | #"r" => SOME 10
      | #"q" => SOME 11
      | #"k" => SOME 12
      | _    => NONE

fun array_to_piecelist brd =
    let
        fun walk (r, f, wpl, bpl,
                  wp, wn, wb, wr, wqk,
                  bp, bn, bb, br, bqk) =
            if r = 8 then (wpl, bpl)
            else
                let
                    val c = Array.sub (Array.sub(brd, r), f)
                    val sq = square_of (7 - r, f)

                    (* White pieces *)
                    val (wpl', wp', wn', wb', wr', wqk') =
                        case c of
                            #"P" => (update16(wpl, wp, sq), wp+1, wn, wb, wr, wqk)
                          | #"N" => (update16(wpl, 8 + wn, sq), wp, wn+1, wb, wr, wqk)
                          | #"B" => (update16(wpl, 10 + wb, sq), wp, wn, wb+1, wr, wqk)
                          | #"R" => (update16(wpl, 12 + wr, sq), wp, wn, wb, wr+1, wqk)
                          | #"Q" => (update16(wpl, 14, sq), wp, wn, wb, wr, wqk)
                          | #"K" => (update16(wpl, 15, sq), wp, wn, wb, wr, wqk)
                          | _    => (wpl, wp, wn, wb, wr, wqk)

                    (* Black pieces *)
                    val (bpl', bp', bn', bb', br', bqk') =
                        case c of
                            #"p" => (update16(bpl, bp, sq), bp+1, bn, bb, br, bqk)
                          | #"n" => (update16(bpl, 8 + bn, sq), bp, bn+1, bb, br, bqk)
                          | #"b" => (update16(bpl, 10 + bb, sq), bp, bn, bb+1, br, bqk)
                          | #"r" => (update16(bpl, 12 + br, sq), bp, bn, bb, br+1, bqk)
                          | #"q" => (update16(bpl, 14, sq), bp, bn, bb, br, bqk)
                          | #"k" => (update16(bpl, 15, sq), bp, bn, bb, br, bqk)
                          | _    => (bpl, bp, bn, bb, br, bqk)

                    (* next square *)
                    val (next_r, next_f) =
                        if f = 7 then (r+1, 0) else (r, f+1)
                in
                    walk (next_r, next_f,
                          wpl', bpl', wp', wn', wb', wr', wqk',
                          bp', bn', bb', br', bqk')
                end

        val w0 = empty_list()
        val b0 = empty_list()
    in
        walk (0,0, w0,b0, 0,0,0,0,0, 0,0,0,0,0)
    end




fun initiate_standard_chess () =
    let
        val brd = Array.tabulate (8, fn _ => Array.array(8,#"."))
        fun put(r,f,c) = Array.update(Array.sub(brd,r), f, c)

        (* White pieces on rank 1 *)
        val _ = (put(0,0,#"R"); put(0,1,#"N"); put(0,2,#"B"); put(0,3,#"Q");
                 put(0,4,#"K"); put(0,5,#"B"); put(0,6,#"N"); put(0,7,#"R"))
        val _ = List.app (fn f => put(1,f,#"P")) (List.tabulate(8, fn x=>x))

        (* Black pieces on rank 8 *)
        val _ = (put(7,0,#"r"); put(7,1,#"n"); put(7,2,#"b"); put(7,3,#"q");
                 put(7,4,#"k"); put(7,5,#"b"); put(7,6,#"n"); put(7,7,#"r"))
        val _ = List.app (fn f => put(6,f,#"p")) (List.tabulate(8, fn x=>x))
    in
        array_to_piecelist brd
    end

fun print_board (white_pl, black_pl) =
    let
        (* extract nth entry from piece_list *)
        fun get (pl, idx) =
            let 
                val (a0,a1,a2,a3,a4,a5,a6,a7, a8,a9,a10,a11,a12,a13,a14,a15) = pl;
            in
              List.nth ([a0,a1,a2,a3,a4,a5,a6,a7,
                         a8,a9,a10,a11,a12,a13,a14,a15], idx)
            end

        (* Handle multiple pieces of same type *)
        fun white_char idx =
            if idx < 8 then #"P"          (* pawns 0-7 *)
            else if idx < 10 then #"N"     (* knights 8-9 *)
            else if idx < 12 then #"B"     (* bishops 10-11 *)
            else if idx < 14 then #"R"     (* rooks 12-13 *)
            else if idx = 14 then #"Q"     (* queen 14 *)
            else if idx = 15 then #"K"     (* king 15 *)
            else #"?"

        fun black_char idx =
            if idx < 8 then #"p"
            else if idx < 10 then #"n"
            else if idx < 12 then #"b"
            else if idx < 14 then #"r"
            else if idx = 14 then #"q"
            else if idx = 15 then #"k"
            else #"?"

        val board = Array.tabulate (64, fn _ => #".")
        
        fun place_white idx =
            let val sq = Word8.toInt (get (white_pl, idx))
            in  if sq <> 0 then Array.update(board, sq-1, white_char idx) else () end

        fun place_black idx =
            let val sq = Word8.toInt (get (black_pl, idx))
            in  if sq <> 0 then Array.update(board, sq-1, black_char idx) else () end

        val _ = List.app place_white (List.tabulate(16, fn i => i))
        val _ = List.app place_black (List.tabulate(16, fn i => i))

        fun print_rank r =
            (List.app
                (fn f => print (String.str (Array.sub(board, 8*r + f)) ^ " "))
                (List.tabulate(8, fn x => x));
             print "\n")
    in
        List.app print_rank (List.tabulate(8, fn i => 7 - i))
    end

end