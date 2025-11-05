(*Can be optimized for parallelism *)

structure  Board:
sig
    type brep = (Word64.word * Word64.word * Word64.word * Word64.word * Word64.word * Word64.word * Word64.word * Word64.word * Word64.word * Word64.word * Word64.word * Word64.word)
    val board_to_bitmaps: (char array array) -> brep
    val initiate_standard_chess: unit -> brep
    val give_piece_bitmap: brep -> char -> Word64.word


    (*This is only for verification purpose*)
    val print_bit_map: unit -> unit
end =
struct
    type brep = (Word64.word * Word64.word * Word64.word * Word64.word * Word64.word * Word64.word * Word64.word * Word64.word * Word64.word * Word64.word * Word64.word * Word64.word)

    (* This function convert any chess board to
    bitmaps for each type of pieces of white and black *)
    fun board_to_bitmaps chessboard =
        (* Intialize bitmaps for each piece type of black and white *)
        (* P, R, N, B, K, Q, p, r, n, b, k, q *)
        let
            (* loop from bottom-right(i=0) to top-left(i=63) *)
            (* i=0 corresponds to least signficant bit and i=63 corresponds to most signficant bit *)
            fun loop i (P, R, N, B, K, Q, p, r, n, b, k, q) stride =
                if i >= 64 then
                    (P, R, N, B, K, Q, p, r, n, b, k, q)
                else
                    let 
                        val row = 7 - (i div 8)
                        val col = 7 - (i mod 8)
                        val ni = i+1
                    in
                        (* check current position is board and piece present there*)
                        case Array.sub(Array.sub(chessboard, row), col) of
                              #"P" => loop ni ((Word64.orb (P, stride)), R, N, B, K, Q, p, r, n, b, k, q) (Word64.<< (stride, (Word.fromInt 1)))
                            | #"R" => loop ni (P, (Word64.orb (R, stride)), N, B, K, Q, p, r, n, b, k, q) (Word64.<< (stride, (Word.fromInt 1)))
                            | #"N" => loop ni (P, R, (Word64.orb (N, stride)), B, K, Q, p, r, n, b, k, q) (Word64.<< (stride, (Word.fromInt 1))) 
                            | #"B" => loop ni (P, R, N, (Word64.orb (B, stride)), K, Q, p, r, n, b, k, q) (Word64.<< (stride, (Word.fromInt 1))) 
                            | #"K" => loop ni (P, R, N, B, (Word64.orb (K, stride)), Q, p, r, n, b, k, q) (Word64.<< (stride, (Word.fromInt 1))) 
                            | #"Q" => loop ni (P, R, N, B, K, (Word64.orb (Q, stride)), p, r, n, b, k, q) (Word64.<< (stride, (Word.fromInt 1))) 
                            | #"p" => loop ni (P, R, N, B, K, Q, (Word64.orb (p, stride)), r, n, b, k, q) (Word64.<< (stride, (Word.fromInt 1))) 
                            | #"r" => loop ni (P, R, N, B, K, Q, p, (Word64.orb (r, stride)), n, b, k, q) (Word64.<< (stride, (Word.fromInt 1))) 
                            | #"n" => loop ni (P, R, N, B, K, Q, p, r, (Word64.orb (n, stride)), b, k, q) (Word64.<< (stride, (Word.fromInt 1))) 
                            | #"b" => loop ni (P, R, N, B, K, Q, p, r, n, (Word64.orb (b, stride)), k, q) (Word64.<< (stride, (Word.fromInt 1))) 
                            | #"k" => loop ni (P, R, N, B, K, Q, p, r, n, b, (Word64.orb (k, stride)), q) (Word64.<< (stride, (Word.fromInt 1))) 
                            | #"q" => loop ni (P, R, N, B, K, Q, p, r, n, b, k, (Word64.orb (q, stride))) (Word64.<< (stride, (Word.fromInt 1)))
                            | _ =>    loop ni (P, R, N, B, K, Q, p, r, n, b, k, q) (Word64.<< (stride, (Word.fromInt 1)))
                    end
        in
            loop 0 ((Word64.fromInt 0), (Word64.fromInt 0), (Word64.fromInt 0), (Word64.fromInt 0), (Word64.fromInt 0), (Word64.fromInt 0), (Word64.fromInt 0), (Word64.fromInt 0), (Word64.fromInt 0), (Word64.fromInt 0), (Word64.fromInt 0), (Word64.fromInt 0)) (Word64.fromInt 1)
        end

    (* This function is just to initialise the initial state of chess board
    and find its bitmpas *)
    fun initiate_standard_chess () =
        let 
            (* cap alphabets denotes white and small alphabets denote black *)
            (*can initalise with any chess board here*)
            (*we can even add a feature to take board as a command line arg*)
            val chessboard = Array.fromList [
                                    Array.fromList [#"r",#"n",#"b",#"q",#"k",#"b",#"n",#"r"],
                                    Array.fromList [#"p",#"p",#"p",#"p",#"p",#"p",#"p",#"p"],
                                    Array.fromList [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
                                    Array.fromList [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
                                    Array.fromList [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
                                    Array.fromList [#" ",#" ",#" ",#" ",#" ",#" ",#" ",#" "],
                                    Array.fromList [#"P",#"P",#"P",#"P",#"P",#"P",#"P",#"P"],
                                    Array.fromList [#"R",#"N",#"B",#"Q",#"K",#"B",#"N",#"R"]
                                ]
            (* val chessboard = Array.fromList [
                                Array.fromList [#"r",#"n",#"b",#"q",#"k",#"b",#" ",#"r"],
                                Array.fromList [#"p",#"p",#"p",#" ",#"p",#"p",#"p",#"p"],
                                Array.fromList [#" ",#" ",#" ",#"n",#" ",#" ",#" ",#" "],
                                Array.fromList [#" ",#" ",#" ",#"p",#" ",#" ",#" ",#" "],
                                Array.fromList [#" ",#" ",#"P",#" ",#"P",#" ",#" ",#" "],
                                Array.fromList [#" ",#" ",#" ",#"N",#" ",#" ",#" ",#" "],
                                Array.fromList [#"P",#"P",#" ",#" ",#" ",#"P",#"P",#"P"],
                                Array.fromList [#"R",#"N",#"B",#"Q",#"K",#"B",#" ",#"R"]
                            ]  *)

        in
            board_to_bitmaps chessboard
        end



    (* this function initialises the chess board, and calculates its bitmaps
    and then again find the print the board so that to check if bitmaps were calculated properly.*)
    fun print_bit_map () =
        let 
            val (P, R, N, B, K, Q, p, r, n, b, k, q) = initiate_standard_chess ()
            val bmaps = [P, R, N, B, K, Q, p, r, n, b, k, q]
            val pieces = [#"P", #"R", #"N", #"B", #"K", #"Q", #"p", #"r", #"n", #"b", #"k", #"q"]

            val board = Array2.array(8,8,#" ");

            fun loop2 bitmap i c =
                if i>=64 then ()
                else
                    let
                        val row = 7 - (i div 8)
                        val col = 7 - (i mod 8)
                        val _ = if Word64.compare (Word64.andb (bitmap, (Word64.fromInt 1)), Word64.fromInt 1) = EQUAL then
                                    Array2.update(board, row, col, c)
                                else ()
                    in
                        loop2 (Word64.>> (bitmap, (Word.fromInt 1))) (i+1) c
                    end

            fun loop1 bmaps pieces =
                if null bmaps orelse null pieces then ()
                else
                    let
                        val _ = loop2 (hd bmaps) 0 (hd pieces)
                    in
                        loop1 (tl bmaps) (tl pieces)
                    end

            val _ = loop1 bmaps pieces

            fun display_board i =
                if i >= 64 then ()
                else
                    let 
                        val row = i div 8
                        val col = i mod 8
                        val _ = print (Char.toString (Array2.sub(board, row, col)) ^ " ")
                        val _ = if col = 7 then print("\n") else ()
                    in
                        display_board (i+1)
                    end
        in
            display_board 0
        end

    fun give_piece_bitmap bmaps c =
        let 
            val (P, R, N, B, K, Q, p, r, n, b, k, q) = bmaps 
        in
            case c of 
                #"P" => P
                | #"R" => R
                | #"N" => N
                | #"B" => B
                | #"K" => K
                | #"Q" => Q
                | #"p" => p
                | #"r" => r
                | #"n" => n
                | #"b" => b
                | #"k" => k
                | #"q" => q
                | _ => (Word64.fromInt 0)
end