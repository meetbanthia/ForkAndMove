structure UltimateTicTacToeMoveGen : 
sig
  type move = int * int
  val generate_move_order : UltimateTicTacToe.board -> move list
  val apply_move : UltimateTicTacToe.board -> move -> bool -> UltimateTicTacToe.board
  val order_moves : UltimateTicTacToe.board -> bool -> move list -> (real * move) list
  val print_ordered_moves : UltimateTicTacToe.board list -> unit
  val generate_ordered_moves : UltimateTicTacToe.board -> bool -> move list
  val eval_position_ultimatetictactoe : UltimateTicTacToe.board -> bool -> real
end =
struct
  type move = int * int

fun generate_move_order ((a,b,c,d,e,f,g,h,i) : UltimateTicTacToe.board) : (int * int) list =
    let
        val subs = [a,b,c,d,e,f,g,h,i]
        fun oneSub (sb, idx) =
            let
                val cells = TicTacToeMoveGenerator.generate_move_order sb
            in
                List.map (fn cell => (idx, cell)) cells
            end
    in
        #1 (List.foldl (fn (sb, (acc, idx)) => (acc @ oneSub(sb, idx), idx+1)) ([], 0) subs)
    end

  fun apply_move (board: UltimateTicTacToe.board) (move: move) (isPlayerX: bool) : UltimateTicTacToe.board =
    let
      val (subBoardIdx, positionInSubBoard) = move
      val subBoard = case subBoardIdx of
          0 => #1 board
        | 1 => #2 board
        | 2 => #3 board
        | 3 => #4 board
        | 4 => #5 board
        | 5 => #6 board
        | 6 => #7 board
        | 7 => #8 board
        | 8 => #9 board
        | n => raise Fail "Invalid Index"
    in
      let
        val newSubBoard = TicTacToeMoveGenerator.apply_move subBoard positionInSubBoard isPlayerX
      in
        case subBoardIdx of
            0 => (newSubBoard, #2 board, #3 board, #4 board, #5 board, #6 board, #7 board, #8 board, #9 board)
          | 1 => (#1 board, newSubBoard, #3 board, #4 board, #5 board, #6 board, #7 board, #8 board, #9 board)
          | 2 => (#1 board, #2 board, newSubBoard, #4 board, #5 board, #6 board, #7 board, #8 board, #9 board)
          | 3 => (#1 board, #2 board, #3 board, newSubBoard, #5 board, #6 board, #7 board, #8 board, #9 board)
          | 4 => (#1 board, #2 board, #3 board, #4 board, newSubBoard, #6 board, #7 board, #8 board, #9 board)
          | 5 => (#1 board, #2 board, #3 board, #4 board, #5 board, newSubBoard, #7 board, #8 board, #9 board)
          | 6 => (#1 board, #2 board, #3 board, #4 board, #5 board, #6 board, newSubBoard, #8 board, #9 board)
          | 7 => (#1 board, #2 board, #3 board, #4 board, #5 board, #6 board, #7 board, newSubBoard, #9 board)
          | 8 => (#1 board, #2 board, #3 board, #4 board, #5 board, #6 board, #7 board, #8 board, newSubBoard)
          | n => raise Fail "Invalid Index"
      end
    end

    fun insert (x as (s1, m1)) [] = [x]
      | insert (x as (s1, m1)) ((s2, m2)::rest) =
        if s1 >= s2 then x :: (s2, m2)::rest
        else (s2, m2) :: insert x rest

    fun sort [] = []
      | sort (x::xs) = insert x (sort xs)

  fun order_moves (board: UltimateTicTacToe.board) (isPlayerX: bool) (moves: move list) : (real * move) list =
    let
      fun evaluate_move (i , j) =
        let
          val new_i = TicTacToeMoveGenerator.apply_move i j isPlayerX
          val score = TicTacToeMoveGenerator.eval_position_tictactoe new_i isPlayerX
        in
          (score, (i, j))
        end
    in
      List.map evaluate_move moves
    end

  fun print_ordered_moves (boards: UltimateTicTacToe.board list) =
    List.app (fn board => UltimateTicTacToe.printBoard board) boards

  fun generate_ordered_moves (board: UltimateTicTacToe.board) (isPlayerX: bool) : move list =
    let
      val validMoves = generate_move_order board
      val orderedMoves = order_moves board isPlayerX validMoves
    in
      List.map #2 orderedMoves 
    end

  fun eval_position_ultimatetictactoe (board: UltimateTicTacToe.board) (isPlayerX: bool) : real =
    let
      val scores = List.tabulate (9, fn i =>
        let
          val subBoard = case i of
              0 => #1 board
            | 1 => #2 board
            | 2 => #3 board
            | 3 => #4 board
            | 4 => #5 board
            | 5 => #6 board
            | 6 => #7 board
            | 7 => #8 board
            | 8 => #9 board
            | n  => raise Fail "Invalid Index"
        in
          TicTacToeMoveGenerator.eval_position_tictactoe subBoard isPlayerX
        end
      )
    in
      List.foldl (fn (score, acc) => acc + score) 0.0 scores
    end
end
