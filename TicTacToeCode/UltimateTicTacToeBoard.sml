signature ULTIMATETICTACTOE =
sig
  type board 
  val getCell : board -> int -> int -> TicTacToeBoard.cell
  val setCell : board -> int -> int -> int -> board
  val printBoard : board -> unit
  val applyMove : board -> int * int -> bool -> board
  val emptyBoard : unit -> board
end

structure UltimateTicTacToe : ULTIMATETICTACTOE =
struct
  type board = TicTacToeBoard.board * TicTacToeBoard.board * TicTacToeBoard.board *
               TicTacToeBoard.board * TicTacToeBoard.board * TicTacToeBoard.board *
               TicTacToeBoard.board * TicTacToeBoard.board * TicTacToeBoard.board
  fun emptyBoard () = (0,0,0,0,0,0,0,0,0)
  fun getCell (b: board) (i: int) (j: int) =
    case b of
      (sb1, sb2, sb3, sb4, sb5, sb6, sb7, sb8, sb9) =>
        let
          val subBoard = case i of
              0 => sb1
            | 1 => sb2
            | 2 => sb3
            | 3 => sb4
            | 4 => sb5
            | 5 => sb6
            | 6 => sb7
            | 7 => sb8
            | 8 => sb9
            | n => raise Fail "Invalid Index"
        in
          TicTacToeBoard.getCell subBoard j
        end

  fun setCell (b: board) (i: int) (j: int) (c: int) =
    case b of
      (sb1, sb2, sb3, sb4, sb5, sb6, sb7, sb8, sb9) =>
        let
          val newSB = case i of
              0 => TicTacToeBoard.setCell (j) c sb1
            | 1 => TicTacToeBoard.setCell (j) c sb2
            | 2 => TicTacToeBoard.setCell (j) c sb3
            | 3 => TicTacToeBoard.setCell (j) c sb4
            | 4 => TicTacToeBoard.setCell (j) c sb5
            | 5 => TicTacToeBoard.setCell (j) c sb6
            | 6 => TicTacToeBoard.setCell (j) c sb7
            | 7 => TicTacToeBoard.setCell (j) c sb8
            | 8 => TicTacToeBoard.setCell (j) c sb9
            | n => raise Fail "Invalid Index"
        in
          case i of
              0 => (newSB, sb2, sb3, sb4, sb5, sb6, sb7, sb8, sb9)
            | 1 => (sb1, newSB, sb3, sb4, sb5, sb6, sb7, sb8, sb9)
            | 2 => (sb1, sb2, newSB, sb4, sb5, sb6, sb7, sb8, sb9)
            | 3 => (sb1, sb2, sb3, newSB, sb5, sb6, sb7, sb8, sb9)
            | 4 => (sb1, sb2, sb3, sb4, newSB, sb6, sb7, sb8, sb9)
            | 5 => (sb1, sb2, sb3, sb4, sb5, newSB, sb7, sb8, sb9)
            | 6 => (sb1, sb2, sb3, sb4, sb5, sb6, newSB, sb8, sb9)
            | 7 => (sb1, sb2, sb3, sb4, sb5, sb6, sb7, newSB, sb9)
            | 8 => (sb1, sb2, sb3, sb4, sb5, sb6, sb7, sb8, newSB)
            | n => raise Fail "Invalid Index"
        end

  (* Print the board by printing all 9 sub-boards *)
  fun printBoard (b: board) =
    let
      val (sb1, sb2, sb3, sb4, sb5, sb6, sb7, sb8, sb9) = b
    in
      print("Sub-board 1:\n");
      TicTacToeBoard.printBoard sb1;
      print("Sub-board 2:\n");
      TicTacToeBoard.printBoard sb2;
      print("Sub-board 3:\n");
      TicTacToeBoard.printBoard sb3;
      print("Sub-board 4:\n");
      TicTacToeBoard.printBoard sb4;
      print("Sub-board 5:\n");
      TicTacToeBoard.printBoard sb5;
      print("Sub-board 6:\n");
      TicTacToeBoard.printBoard sb6;
      print("Sub-board 7:\n");
      TicTacToeBoard.printBoard sb7;
      print("Sub-board 8:\n");
      TicTacToeBoard.printBoard sb8;
      print("Sub-board 9:\n");
      TicTacToeBoard.printBoard sb9
    end


  fun applyMove (b: board) (move: int * int) (isPlayerX: bool) =
    let
      val (i, j) = move
      val subBoard = case i of
          0 => #1 b
        | 1 => #2 b
        | 2 => #3 b
        | 3 => #4 b
        | 4 => #5 b
        | 5 => #6 b
        | 6 => #7 b
        | 7 => #8 b
        | 8 => #9 b
        | n => raise Fail "Invalid Index"
    in
      let
        val newSubBoard = TicTacToeMoveGenerator.apply_move subBoard j isPlayerX
      in
        case i of
            0 => (newSubBoard, #2 b, #3 b, #4 b, #5 b, #6 b, #7 b, #8 b, #9 b)
          | 1 => (#1 b, newSubBoard, #3 b, #4 b, #5 b, #6 b, #7 b, #8 b, #9 b)
          | 2 => (#1 b, #2 b, newSubBoard, #4 b, #5 b, #6 b, #7 b, #8 b, #9 b)
          | 3 => (#1 b, #2 b, #3 b, newSubBoard, #5 b, #6 b, #7 b, #8 b, #9 b)
          | 4 => (#1 b, #2 b, #3 b, #4 b, newSubBoard, #6 b, #7 b, #8 b, #9 b)
          | 5 => (#1 b, #2 b, #3 b, #4 b, #5 b, newSubBoard, #7 b, #8 b, #9 b)
          | 6 => (#1 b, #2 b, #3 b, #4 b, #5 b, #6 b, newSubBoard, #8 b, #9 b)
          | 7 => (#1 b, #2 b, #3 b, #4 b, #5 b, #6 b, #7 b, newSubBoard, #9 b)
          | 8 => (#1 b, #2 b, #3 b, #4 b, #5 b, #6 b, #7 b, #8 b, newSubBoard)
          | n => raise Fail "Invalid Index"
      end
    end
end
