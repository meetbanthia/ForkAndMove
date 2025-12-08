structure TicTacToe :
sig
    val tictactoe: unit -> unit
end =
struct

fun find_best_move board depth maximizingPlayer use_alphabeta use_pvs =
    if use_alphabeta then
        Search.alpha_beta_search_ttt board depth maximizingPlayer Real.negInf Real.posInf
            TicTacToeMoveGenerator.eval_position_tictactoe
            TicTacToeMoveGenerator.generate_ordered_moves
            TicTacToeMoveGenerator.apply_move
    else if use_pvs then
        Search.pvs_search_ttt board depth maximizingPlayer Real.negInf Real.posInf
            TicTacToeMoveGenerator.eval_position_tictactoe
            TicTacToeMoveGenerator.generate_ordered_moves
            TicTacToeMoveGenerator.apply_move
    else
        Search.minimax_full_parallel_ttt board depth maximizingPlayer
            TicTacToeMoveGenerator.eval_position_tictactoe
            TicTacToeMoveGenerator.apply_move
            TicTacToeMoveGenerator.generate_ordered_moves

fun has_flag key =
        List.exists (fn s => s = ("-" ^ key)) (CommandLine.arguments())

fun run () =
    let
        val depth = CommandLineArgs.parseInt "depth" 4
        val moves = CommandLineArgs.parseInt "moves" 9
        val use_alphabeta = has_flag "alphabeta"
        val use_pvs = has_flag "pvs"
        val board0 = 0

        fun game_loop board maximizingPlayer iter limit =
            if iter > limit then ()
            else
                let
                    val _ = print ("\n" ^ (if maximizingPlayer then "X" else "O") ^
                                   "'s move (Depth " ^ Int.toString depth ^ ")...\n")
                    val (score, best_move_opt) = find_best_move board depth maximizingPlayer use_alphabeta use_pvs
                in
                    case best_move_opt of
                        SOME move =>
                            let
                                val new_board = TicTacToeBoard.setCell board move (if maximizingPlayer then 1 else 2)
                                val _ = print ("Move chosen: " ^ Int.toString move ^ "\n")
                                val _ = print ("Score: " ^ Real.toString score ^ "\n")
                                val _ = TicTacToeBoard.printBoard new_board
                            in
                                game_loop new_board (not maximizingPlayer) (iter + 1) limit
                            end
                      | NONE =>
                            let
                                val _ = print ("\nNo legal moves for " ^
                                               (if maximizingPlayer then "X" else "O") ^
                                               ". Game over.\n")
                            in () end
                end
    in
        game_loop board0 true 0 moves
    end

fun tictactoe () = 
    let
    val moves_test = TicTacToeMoveGenerator.generate_ordered_moves 0 true
    val _ = List.app (fn m => print (Int.toString m ^ " ")) moves_test
    val b = TicTacToeBoard.decodeBoard 0
    val _ = print "123"
    val _ = print (String.concatWith "," (List.map Int.toString [#1 b, #2 b, #3 b, #4 b, #5 b, #6 b, #7 b, #8 b, #9 b]))
    val _ = run()
    in
    ()
    end

end