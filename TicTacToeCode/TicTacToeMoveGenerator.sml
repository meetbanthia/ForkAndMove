structure TicTacToeMoveGenerator :
sig
    type move = int
    val generate_move_order: TicTacToeBoard.board -> move list
    val apply_move : TicTacToeBoard.board -> move -> bool -> TicTacToeBoard.board
    val generate_color_move_order : TicTacToeBoard.board -> bool -> bool -> bool -> bool -> bool -> bool -> bool -> move list
    val order_moves : TicTacToeBoard.board -> bool -> move list -> (int * move) list
    val print_ordered_moves : TicTacToeBoard.board list -> unit
    val generate_ordered_moves : TicTacToeBoard.board -> bool -> move list
    val eval_position_tictactoe: TicTacToeBoard.board -> bool -> real
end =
struct

    type move = int

    fun hasWon player board =
        let
            val (c0,c1,c2,c3,c4,c5,c6,c7,c8) = TicTacToeBoard.decodeBoard board
            fun checkLine [p1,p2,p3] = p1 = player andalso p2 = player andalso p3 = player
        in
            List.exists checkLine [[c0,c1,c2],[c3,c4,c5],[c6,c7,c8],
                                   [c0,c3,c6],[c1,c4,c7],[c2,c5,c8],
                                   [c0,c4,c8],[c2,c4,c6]]
        end


    fun eval_position_tictactoe board is_x_turn =
        let
            val player = if is_x_turn then 1 else 2
            val opponent = if player = 1 then 2 else 1
        in
            if hasWon player board then 1.0
            else if hasWon opponent board then ~1.0
            else 0.0
        end

    fun eval_move board is_x_turn move = 
    let
        val player = if is_x_turn then 1 else 2
        val opponent = if player = 1 then 2 else 1
        val new_board = TicTacToeBoard.setCell board move player
        val opp_board = TicTacToeBoard.setCell board move opponent
    in
        if hasWon player new_board then 10
        else if hasWon opponent opp_board then 5
        else if move = 5 then 3 else 0
    end
    

    fun order_moves board is_x_turn moves =
        let
            val player = if is_x_turn then 1 else 2
            val scored_moves = List.map (fn m =>
                let
                    val score = eval_move board is_x_turn m
                in
                    (score, m)
                end) moves

            (* Simple insertion sort *)
            fun insert (x as (s1, m1)) [] = [x]
            | insert (x as (s1, m1)) ((s2, m2)::rest) =
                    if s1 >= s2 then x :: (s2, m2)::rest
                    else (s2, m2) :: insert x rest

            fun sort [] = []
            | sort (x::xs) = insert x (sort xs)
        in
            sort scored_moves
        end

fun generate_move_order board =
    let
        val (c0,c1,c2,c3,c4,c5,c6,c7,c8) = TicTacToeBoard.decodeBoard board
        val cells = [(0,c0),(1,c1),(2,c2),(3,c3),(4,c4),(5,c5),(6,c6),(7,c7),(8,c8)]
    in
        if hasWon 1 board then [] else if hasWon 2 board then [] else List.map #1 (List.filter (fn (_,v) => v = 0) cells)
    end

fun generate_ordered_moves board is_x_turn =
    List.map #2 (order_moves board is_x_turn (generate_move_order board))

fun apply_move board move is_x_turn =
    let
        val player = if is_x_turn then 1 else 2
    in
        TicTacToeBoard.setCell board move player
    end

    fun generate_color_move_order board _ _ _ _ _ _ _ =
        generate_move_order board

    fun print_ordered_moves boards =
        List.app (fn board =>
            let
                val moves = generate_move_order board
            in
                print ("Ordered moves for board: " ^
                       String.concatWith "," (List.map Int.toString moves) ^ "\n")
            end) boards

end
