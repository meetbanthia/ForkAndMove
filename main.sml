(* Start your chess engine here*)

structure Main =
struct
    (* Helper to convert move coords to algebraic string (e.g. "e2-e4") *)
    fun move_to_string ((r1,c1),(r2,c2)) =
        let
            fun row_to_rank r = Int.toString (8 - r) (* Row 0 -> Rank 8, Row 7 -> Rank 1 *)
            fun col_to_file c = String.str (Char.chr (c + 97)) (* Col 0 -> 'a', Col 7 -> 'h' *)
        in
            (col_to_file c1) ^ (row_to_rank r1) ^ "-" ^ (col_to_file c2) ^ (row_to_rank r2)
        end

    (* Helper to read file content *)
    fun read_file filename =
        let
            val file = TextIO.openIn filename
            val content = TextIO.inputAll file
            val _ = TextIO.closeIn file
            (* Remove trailing newline if present *)
            val clean_content = 
                if String.size content > 0 andalso String.sub(content, String.size content - 1) = #"\n" then
                    String.substring(content, 0, String.size content - 1)
                else content
        in
            clean_content
        end

    (* --- FEN Parser --- *)
    (* Converts a FEN string (piece placement part) to Board.brep *)
    fun fen_to_board fen =
        let
            val board_arr = Array2.array(8, 8, #" ")
            
            (* Helper to fill board from FEN *)
            fun parse_fen chars row col =
                case chars of
                    [] => ()
                  | #"/"::rest => parse_fen rest (row + 1) 0
                  | c::rest =>
                        if Char.isDigit c then
                            let val skip = (Char.ord c) - (Char.ord #"0")
                            in parse_fen rest row (col + skip) end
                        else
                            (Array2.update(board_arr, row, col, c);
                             parse_fen rest row (col + 1))
            
            val fen_parts = String.tokens (fn c => c = #" ") fen
            val placement = if null fen_parts then "" else hd fen_parts
            val _ = parse_fen (String.explode placement) 0 0
        in
            Board.board_representation board_arr
        end

    (* Helper to check for command line flags *)
    fun has_flag key =
        List.exists (fn s => s = ("-" ^ key)) (CommandLine.arguments())

    fun find_best_move board depth maximizing_player use_alphabeta =
        if use_alphabeta then
            Search.alpha_beta_search board depth maximizing_player Real.negInf Real.posInf Search.eval_position MoveGenerator.generate_ordered_moves MoveGenerator.apply_move
        else
            Search.minimax_full_parallel board depth maximizing_player Search.eval_position MoveGenerator.apply_move MoveGenerator.generate_ordered_moves

    fun run () =
        let
            (* Default FEN: Standard starting position *)
            val default_fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq"
            
            (* Parse command line args *)
            val fen_arg = CommandLineArgs.parseString "fen" ""
            val file_arg = CommandLineArgs.parseString "file" ""
            val depth = CommandLineArgs.parseInt "depth" 4
            val moves = CommandLineArgs.parseInt "moves" 10
            
            (* Search algorithm flags *)
            val use_alphabeta = has_flag "alphabeta"
            val use_minimax = has_flag "minimax" (* Explicit flag, though currently default *)
            
            (* Determine source of FEN *)
            val fen =
            if file_arg <> "" then 
                (print ("Reading FEN from file: " ^ file_arg ^ "\n");
                 read_file file_arg)
            else if fen_arg <> "" then 
                fen_arg
            else 
                default_fen

        (* Extract turn from FEN *)
        val tokens = String.tokens (fn c => c = #" ") fen
        val is_white = 
            if length tokens >= 2 then 
                List.nth(tokens, 1) = "w"
            else 
                true (* Default to white if not specified *)

        (* Reconstruct minimal FEN (Placement + Turn) *)
        val minimal_fen = 
            if length tokens >= 2 then
                List.nth(tokens, 0) ^ " " ^ List.nth(tokens, 1)
            else
                fen (* Fallback if parsing fails or already minimal *)

        val _ = print "\n========================================\n"
        val _ = print "      Chess Engine Initialization       \n"
        val _ = print ("FEN: " ^ minimal_fen ^ "\n")
        val _ = print ("Turn: " ^ (if is_white then "White" else "Black") ^ "\n")
        val _ = print ("Depth: " ^ Int.toString depth ^ "\n")
        val _ = print ("Algorithm: " ^ (if use_alphabeta then "Alpha-Beta" else "Minimax") ^ "\n")
        val _ = print "========================================\n"

        (* Initialize board *)
        val board0 = fen_to_board fen
        val _ = Board.print_board board0

        (* Recursive loop to play the game *)
        fun game_loop board turn iter limit =
            if iter > limit then ()
            else
            let
                val _ = print ("\n" ^ (if turn then "White" else "Black") ^ "'s move (Depth " ^ Int.toString depth ^ ")...\n")
                val (score, best_move_opt) = find_best_move board depth turn use_alphabeta
            in
                case best_move_opt of
                    SOME m =>
                        let
                            val _ = print ("\n" ^ "Move chosen: " ^ move_to_string m ^ "\n")
                            val _ = print ("Score: " ^ Real.toString score ^ "\n")
                            val new_board = MoveGenerator.apply_move board m
                            val _ = print "\n"
                            val _ = Board.print_board new_board
                        in
                            game_loop new_board (not turn) (iter + 1) limit
                        end
                  | NONE =>
                        (print ("\nNo legal moves found for " ^ (if turn then "White" else "Black") ^ ". Game over.\n");
                         ())
            end
    in
        game_loop board0 is_white 1 moves
    end
end

val _ = Main.run()
