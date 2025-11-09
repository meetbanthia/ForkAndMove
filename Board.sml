(*Can be optimized for parallelism *)

structure  Board:
sig
    type brep = (Word64.word * Word64.word * Word64.word * Word64.word * Word64.word * Word64.word * Word64.word * Word64.word * Word64.word * Word64.word * Word64.word * Word64.word)
    val board_to_bitmaps: (char array array) -> brep
    val initiate_standard_chess: unit -> brep


    (*This is only for verification purpose*)
    val print_bit_map: unit -> unit
    val print_board: brep -> unit
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

        fun print_board (P, R, N, B, K, Q, p, r, n, b, k, q) =
    let
        (* Helper function to check if a specific bit is set in a bitmap *)
        fun is_set (bitmap, idx) =
            Word64.compare (Word64.andb (bitmap, Word64.<< (Word64.fromInt 1, Word.fromInt idx)), Word64.fromInt 0) <> EQUAL

        (* Function to display the board row by row, as was done in the original print_bit_map *)
        fun display_board i =
            if i >= 64 then ()
            else
                let
                    val row = 7 - (i div 8)  (* Calculate row from index *)
                    val col = 7 - (i mod 8)  (* Calculate column from index *)
                    (* Determine which piece is in this position *)
                    val piece_char =
                        if is_set (P, i) then #"P"
                        else if is_set (R, i) then #"R"
                        else if is_set (N, i) then #"N"
                        else if is_set (B, i) then #"B"
                        else if is_set (K, i) then #"K"
                        else if is_set (Q, i) then #"Q"
                        else if is_set (p, i) then #"p"
                        else if is_set (r, i) then #"r"
                        else if is_set (n, i) then #"n"
                        else if is_set (b, i) then #"b"
                        else if is_set (k, i) then #"k"
                        else if is_set (q, i) then #"q"
                        else #" "  (* Empty space *)

                    val _ = print (Char.toString(piece_char) ^ " ")  (* Print the piece character with a space *)
                    val _ = if col = 7 then print("\n") else ()  (* Print a new line at the end of the row *)
                in
                    display_board (i + 1)  (* Recur for the next position *)
                end

    in
        display_board 0  (* Start from the first position (index 0) *)
    end
end