structure  BoardRep:
sig
    val board_to_bitmaps: (char array array) -> (word * word * word * word * word * word * word * word * word * word * word * word)
    val initiate_standard_chess: () -> (word * word * word * word * word * word * word * word * word * word * word * word)
end =
struct

    (* This function convert any chess board to
    bitmaps for each type of pieces of white and black *)
    fun board_to_bitmaps chessboard =
        (* Intialize bitmaps for each piece type of black and white *)
        (* WP, WR, WN, WB, WK, WQ, BP, BR, BN, BB, BK, BQ *)
        let

            (* loop from bottom-right(i=0) to top-left(i=63) *)
            (* i=0 corresponds to least signficant bit and i=63 corresponds to most signficant bit *)
            fun loop (i:int) (WP:word) (WR:word) (WN:word) (WB: word) (WK:word) (WQ:word) (BP:word) (BR:word) (BN:word) (BB:word) (BK:word) (BQ:word) (stride:word) =
                if i >= 64 then
                    (WP, WR, WN, WB, WK, WQ, BP, BR, BN, BB, BK, BQ)
                else
                    let 
                        val row = 7 - (i div 8)
                        val col = 7 - (i mod 8)
                    in
                        (* check current position is board and piece present there*)
                        case Array.sub(Array.sub(chessboard, row), col) of
                              #"P" => loop (i+1) (Word.orb(WP, stride)) WR WN WB WK WQ BP BR BN BB BK BQ (Word.<<(stride, (Word.fromInt 1)))
                            | #"R" => loop (i+1) WP (Word.orb(WR, stride)) WN WB WK WQ BP BR BN BB BK BQ (Word.<<(stride, (Word.fromInt 1)))
                            | #"N" => loop (i+1) WP WR (Word.orb(WN, stride)) WB WK WQ BP BR BN BB BK BQ (Word.<<(stride, (Word.fromInt 1))) 
                            | #"B" => loop (i+1) WP WR WN (Word.orb(WB, stride)) WK WQ BP BR BN BB BK BQ (Word.<<(stride, (Word.fromInt 1))) 
                            | #"K" => loop (i+1) WP WR WN WB (Word.orb(WK, stride)) WQ BP BR BN BB BK BQ (Word.<<(stride, (Word.fromInt 1))) 
                            | #"Q" => loop (i+1) WP WR WN WB WK (Word.orb(WQ, stride)) BP BR BN BB BK BQ (Word.<<(stride, (Word.fromInt 1))) 
                            | #"p" => loop (i+1) WP WR WN WB WK WQ (Word.orb(BP, stride)) BR BN BB BK BQ (Word.<<(stride, (Word.fromInt 1))) 
                            | #"r" => loop (i+1) WP WR WN WB WK WQ BP (Word.orb(BR, stride)) BN BB BK BQ (Word.<<(stride, (Word.fromInt 1))) 
                            | #"n" => loop (i+1) WP WR WN WB WK WQ BP BR (Word.orb(BN, stride)) BB BK BQ (Word.<<(stride, (Word.fromInt 1))) 
                            | #"b" => loop (i+1) WP WR WN WB WK WQ BP BR BN (Word.orb(BB, stride)) BK BQ (Word.<<(stride, (Word.fromInt 1))) 
                            | #"k" => loop (i+1) WP WR WN WB WK WQ BP BR BN BB (Word.orb(BK, stride)) BQ (Word.<<(stride, (Word.fromInt 1))) 
                            | #"q" => loop (i+1) WP WR WN WB WK WQ BP BR BN BB BK (Word.orb(BQ, stride)) (Word.<<(stride, (Word.fromInt 1)))
                            | _ =>    loop (i+1) WP WR WN WB WK WQ BP BR BN BB BK BQ (Word.<< (stride, (Word.fromInt 1)))
                    end
        in
            loop 0 (Word.fromInt 0) (Word.fromInt 0) (Word.fromInt 0) (Word.fromInt 0) (Word.fromInt 0) (Word.fromInt 0) (Word.fromInt 0) (Word.fromInt 0) (Word.fromInt 0) (Word.fromInt 0) (Word.fromInt 0) (Word.fromInt 0) (Word.fromInt 1)
        end

    (* This function is just to initialise the initial state of chess board
    and find its bitmpas *)
    fun initiate_standard_chess () =
        let 
            (* cap alphabets denotes white and small alphabets denote black *)
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
        in
            board_to_bitmaps chessboard
        end
end