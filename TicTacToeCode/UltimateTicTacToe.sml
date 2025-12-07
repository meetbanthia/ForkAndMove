structure UltimateTicTacToeMain :
sig
    val ultimatetictactoe : unit -> unit
end =
struct

fun has_flag key =
    List.exists (fn s => s = ("-" ^ key)) (CommandLine.arguments())

fun find_best_move board depth maximizingPlayer use_alphabeta use_pvs =
    if use_alphabeta then
        Search.alpha_beta_search_ttt board depth maximizingPlayer Real.negInf Real.posInf
            UltimateTicTacToeMoveGen.eval_position_ultimatetictactoe
            UltimateTicTacToeMoveGen.generate_ordered_moves
            UltimateTicTacToeMoveGen.apply_move

    else if use_pvs then
        Search.pvs_search_ttt board depth maximizingPlayer Real.negInf Real.posInf
            UltimateTicTacToeMoveGen.eval_position_ultimatetictactoe
            UltimateTicTacToeMoveGen.generate_ordered_moves
            UltimateTicTacToeMoveGen.apply_move

    else
        Search.minimax_full_parallel_ttt board depth maximizingPlayer
            UltimateTicTacToeMoveGen.eval_position_ultimatetictactoe
            UltimateTicTacToeMoveGen.apply_move
            UltimateTicTacToeMoveGen.generate_ordered_moves


fun run () =
    let
        val depth = CommandLineArgs.parseInt "depth" 3
        val moves = CommandLineArgs.parseInt "moves" 9
        val use_alphabeta = has_flag "alphabeta"
        val use_pvs = has_flag "pvs"

        (* empty ultimate board *)
        val board0 = (0,0,0,0,0,0,0,0,0)

        fun game_loop board maximizingPlayer iter limit =
            if iter > limit then ()
            else
                let
                    val _ = print ("\n" ^
                                   (if maximizingPlayer then "X" else "O") ^
                                   "'s move (Depth " ^
                                   Int.toString depth ^ ")...\n")

                    val (score, best_move_opt) =
                        find_best_move board depth maximizingPlayer use_alphabeta use_pvs

                in
                    case best_move_opt of
                        SOME (sub, cell) =>
                            let
                                val new_board =
                                    UltimateTicTacToe.applyMove board (sub, cell)
                                        (if maximizingPlayer then true else false)

                                val _ = print ("Move chosen: (" ^
                                               Int.toString sub ^ "," ^
                                               Int.toString cell ^ ")\n")

                                val _ = print ("Score: " ^ Real.toString score ^ "\n")
                                val _ = UltimateTicTacToe.printBoard new_board
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


(* test-driver similar to TicTacToe.tictactoe() *)
fun ultimatetictactoe () =
    let
        val test_moves =
            UltimateTicTacToeMoveGen.generate_ordered_moves
                (UltimateTicTacToe.emptyBoard ()) true

        val _ =
            List.app (fn (sb,cell) =>
                          print ("(" ^
                                 Int.toString sb ^ "," ^
                                 Int.toString cell ^ ") "))
                     test_moves

        val _ =
            print "\nInitial board:\n"

        val b0 = UltimateTicTacToe.emptyBoard ()
        val _ = UltimateTicTacToe.printBoard b0

        val _ = run ()
    in
        ()
    end

end
