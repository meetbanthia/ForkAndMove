structure  PieceTable:
sig
    val pieces: char Seq.t
    val piece_table_pawn_w : int Array2.array
    val piece_table_rook_w: int Array2.array
    val piece_table_knight_w: int Array2.array
    val piece_table_bishop_w: int Array2.array
    val piece_table_queen_w: int Array2.array
    val piece_table_king_middle_w: int Array2.array
    val piece_table_king_end_w: int Array2.array
    val piece_table_pawn_b: int Array2.array
    val piece_table_rook_b: int Array2.array
    val piece_table_knight_b: int Array2.array
    val piece_table_bishop_b: int Array2.array
    val piece_table_queen_b: int Array2.array
    val piece_table_king_middle_b: int Array2.array
    val piece_table_king_end_b: int Array2.array

    val give_piece_table : char -> int Array2.array
    val piece_value: char -> int
end =
struct
    val pieces = Seq.fromArray (Array.fromList([#"P", #"R", #"N", #"B", #"K", #"Q", #"p", #"r", #"n", #"b", #"k", #"q"]))

    val piece_table_pawn_w =
            Array2.fromList [
                [0,  0,  0,  0,  0,  0,  0,  0],
                [50, 50, 50, 50, 50, 50, 50, 50],
                [10, 10, 20, 30, 30, 20, 10, 10],
                [5,  5, 10, 25, 25, 10,  5,  5],
                [0,  0,  0, 20, 20,  0,  0,  0],
                [5, ~5,~10,  0,  0,~10, ~5,  5],
                [5, 10, 10,~20,~20, 10, 10,  5],
                [0,  0,  0,  0,  0,  0,  0,  0]
            ]

    val piece_table_pawn_b =
            Array2.fromList [
                [0,  0,  0,  0,  0,  0,  0,  0],
                [5, 10, 10,~20,~20, 10, 10,  5],
                [5, ~5,~10,  0,  0,~10, ~5,  5],
                [0,  0,  0, 20, 20,  0,  0,  0],
                [5,  5, 10, 25, 25, 10,  5,  5],
                [10, 10, 20, 30, 30, 20, 10, 10],
                [50, 50, 50, 50, 50, 50, 50, 50],
                [0,  0,  0,  0,  0,  0,  0,  0]
            ]


    val piece_table_knight_w =
        Array2.fromList [
            [~50,~40,~30,~30,~30,~30,~40,~50],
            [~40,~20,  0,  0,  0,  0,~20,~40],
            [~30,  0, 10, 15, 15, 10,  0,~30],
            [~30,  5, 15, 20, 20, 15,  5,~30],
            [~30,  0, 15, 20, 20, 15,  0,~30],
            [~30,  5, 10, 15, 15, 10,  5,~30],
            [~40,~20,  0,  5,  5,  0,~20,~40],
            [~50,~40,~30,~30,~30,~30,~40,~50]
        ]

    val piece_table_knight_b =
        Array2.fromList [
            [~50,~40,~30,~30,~30,~30,~40,~50],
            [~40,~20,  0,  5,  5,  0,~20,~40],
            [~30,  5, 10, 15, 15, 10,  5,~30],
            [~30,  0, 15, 20, 20, 15,  0,~30],
            [~30,  5, 15, 20, 20, 15,  5,~30],
            [~30,  0, 10, 15, 15, 10,  0,~30],
            [~40,~20,  0,  0,  0,  0,~20,~40],
            [~50,~40,~30,~30,~30,~30,~40,~50]
        ]


    val piece_table_bishop_w =
        Array2.fromList [
            [~20,~10,~10,~10,~10,~10,~10,~20],
            [~10,  0,  0,  0,  0,  0,  0,~10],
            [~10,  0,  5, 10, 10,  5,  0,~10],
            [~10,  5,  5, 10, 10,  5,  5,~10],
            [~10,  0, 10, 10, 10, 10,  0,~10],
            [~10, 10, 10, 10, 10, 10, 10,~10],
            [~10,  5,  0,  0,  0,  0,  5,~10],
            [~20,~10,~10,~10,~10,~10,~10,~20]
        ]

    val piece_table_bishop_b =
        Array2.fromList [
            [~20,~10,~10,~10,~10,~10,~10,~20],
            [~10,  5,  0,  0,  0,  0,  5,~10],
            [~10, 10, 10, 10, 10, 10, 10,~10],
            [~10,  0, 10, 10, 10, 10,  0,~10],
            [~10,  5,  5, 10, 10,  5,  5,~10],
            [~10,  0,  5, 10, 10,  5,  0,~10],
            [~10,  0,  0,  0,  0,  0,  0,~10],
            [~20,~10,~10,~10,~10,~10,~10,~20]
        ]


    val piece_table_rook_w =
        Array2.fromList [
            [ 0,  0,  0,  0,  0,  0,  0,  0],
            [ 5, 10, 10, 10, 10, 10, 10,  5],
            [~5,  0,  0,  0,  0,  0,  0, ~5],
            [~5,  0,  0,  0,  0,  0,  0, ~5],
            [~5,  0,  0,  0,  0,  0,  0, ~5],
            [~5,  0,  0,  0,  0,  0,  0, ~5],
            [~5,  0,  0,  0,  0,  0,  0, ~5],
            [ 0,  0,  0,  5,  5,  0,  0,  0]
        ]

    val piece_table_rook_b =
        Array2.fromList [
            [ 0,  0,  0,  5,  5,  0,  0,  0],
            [~5,  0,  0,  0,  0,  0,  0, ~5],
            [~5,  0,  0,  0,  0,  0,  0, ~5],
            [~5,  0,  0,  0,  0,  0,  0, ~5],
            [~5,  0,  0,  0,  0,  0,  0, ~5],
            [~5,  0,  0,  0,  0,  0,  0, ~5],
            [ 5, 10, 10, 10, 10, 10, 10,  5],
            [ 0,  0,  0,  0,  0,  0,  0,  0]
        ]


    val piece_table_queen_w =
        Array2.fromList [
            [~20,~10,~10, ~5, ~5,~10,~10,~20],
            [~10,  0,  0,  0,  0,  0,  0,~10],
            [~10,  0,  5,  5,  5,  5,  0,~10],
            [ ~5,  0,  5,  5,  5,  5,  0, ~5],
            [  0,  0,  5,  5,  5,  5,  0, ~5],
            [~10,  5,  5,  5,  5,  5,  0,~10],
            [~10,  0,  5,  0,  0,  0,  0,~10],
            [~20,~10,~10, ~5, ~5,~10,~10,~20]
        ]

    val piece_table_queen_b =
        Array2.fromList [
            [~20,~10,~10, ~5, ~5,~10,~10,~20],
            [~10,  0,  5,  0,  0,  0,  0,~10],
            [~10,  5,  5,  5,  5,  5,  0,~10],
            [  0,  0,  5,  5,  5,  5,  0, ~5],
            [ ~5,  0,  5,  5,  5,  5,  0, ~5],
            [~10,  0,  0,  0,  0,  0,  0,~10],
            [~10,  0,  0,  0,  0,  0,  0,~10],
            [~20,~10,~10, ~5, ~5,~10,~10,~20]
        ]


    val piece_table_king_middle_w =
        Array2.fromList [
            [~30,~40,~40,~50,~50,~40,~40,~30],
            [~30,~40,~40,~50,~50,~40,~40,~30],
            [~30,~40,~40,~50,~50,~40,~40,~30],
            [~30,~40,~40,~50,~50,~40,~40,~30],
            [~20,~30,~30,~40,~40,~30,~30,~20],
            [~10,~20,~20,~20,~20,~20,~20,~10],
            [ 20, 20,  0,  0,  0,  0, 20, 20],
            [ 20, 30, 10,  0,  0, 10, 30, 20]
        ]

    val piece_table_king_middle_b =
        Array2.fromList [
            [ 20, 30, 10,  0,  0, 10, 30, 20],
            [ 20, 20,  0,  0,  0,  0, 20, 20],
            [~10,~20,~20,~20,~20,~20,~20,~10],
            [~20,~30,~30,~40,~40,~30,~30,~20],
            [~30,~40,~40,~50,~50,~40,~40,~30],
            [~30,~40,~40,~50,~50,~40,~40,~30],
            [~30,~40,~40,~50,~50,~40,~40,~30],
            [~30,~40,~40,~50,~50,~40,~40,~30]
        ]


    val piece_table_king_end_w =
        Array2.fromList [
            [~50,~40,~30,~20,~20,~30,~40,~50],
            [~30,~20,~10,  0,  0,~10,~20,~30],
            [~30,~10, 20, 30, 30, 20,~10,~30],
            [~30,~10, 30, 40, 40, 30,~10,~30],
            [~30,~10, 30, 40, 40, 30,~10,~30],
            [~30,~10, 20, 30, 30, 20,~10,~30],
            [~30,~30,  0,  0,  0,  0,~30,~30],
            [~50,~30,~30,~30,~30,~30,~30,~50]
        ]

    val piece_table_king_end_b =
        Array2.fromList [
            [~50,~30,~30,~30,~30,~30,~30,~50],
            [~30,~30,  0,  0,  0,  0,~30,~30],
            [~30,~10, 20, 30, 30, 20,~10,~30],
            [~30,~10, 30, 40, 40, 30,~10,~30],
            [~30,~10, 30, 40, 40, 30,~10,~30],
            [~30,~10, 20, 30, 30, 20,~10,~30],
            [~30,~20,~10,  0,  0,~10,~20,~30],
            [~50,~40,~30,~20,~20,~30,~40,~50]
        ]


    fun piece_value #"P" = 100
    | piece_value #"p" = 100
    | piece_value #"N" = 320
    | piece_value #"n" = 320
    | piece_value #"B" = 330
    | piece_value #"b" = 330
    | piece_value #"R" = 500
    | piece_value #"r" = 500
    | piece_value #"Q" = 900
    | piece_value #"q" = 900
    | piece_value #"K" = 20000
    | piece_value #"k" = 20000
    | piece_value _    = 0


    

    fun give_piece_table c =
        case c of 
            #"P" => piece_table_pawn_w
            | #"R" => piece_table_rook_w
            | #"N" => piece_table_knight_w
            | #"B" => piece_table_bishop_w
            | #"K" => piece_table_king_middle_w
            | #"Q" => piece_table_queen_w
            | #"p" => piece_table_pawn_b
            | #"r" => piece_table_rook_b
            | #"n" => piece_table_knight_b
            | #"b" => piece_table_bishop_b
            | #"k" => piece_table_king_middle_b
            | #"q" => piece_table_queen_b
            | _ => Array2.fromList []

end