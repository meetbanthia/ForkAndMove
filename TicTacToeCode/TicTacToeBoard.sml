  signature TICTACTOE =
  sig
    type board
    type cell
    val encodeBoard : cell * cell * cell * cell * cell * cell * cell * cell * cell -> int
    val decodeBoard : int -> cell * cell * cell * cell * cell * cell * cell * cell * cell
    val getCell : int -> int -> cell
    val setCell : int -> int -> cell -> int
    val printBoard : int -> unit
  end

structure TicTacToeBoard : TICTACTOE =
struct
  type cell = int
  type board = int
  fun cellToInt c = c
  fun intToCell c = c

  fun pow3 0 = 1
    | pow3 n = 3 * pow3 (n - 1)

  fun encodeBoard (c0, c1, c2, c3, c4, c5, c6, c7, c8) =
    cellToInt c0 * pow3 0 +
    cellToInt c1 * pow3 1 +
    cellToInt c2 * pow3 2 +
    cellToInt c3 * pow3 3 +
    cellToInt c4 * pow3 4 +
    cellToInt c5 * pow3 5 +
    cellToInt c6 * pow3 6 +
    cellToInt c7 * pow3 7 +
    cellToInt c8 * pow3 8

  fun decodeBoard n =
    let
      fun getCell i = intToCell ((n div pow3 i) mod 3)
    in
      (getCell 0, getCell 1, getCell 2,
       getCell 3, getCell 4, getCell 5,
       getCell 6, getCell 7, getCell 8)
    end

  fun getCell boardInt pos =
    intToCell ((boardInt div pow3 pos) mod 3)

  fun setCell boardInt pos value =
    let
      val oldVal = getCell boardInt pos
      val diff = cellToInt value - cellToInt oldVal
    in
      boardInt + diff * pow3 pos
    end

  fun printBoard boardInt =
    let
      val (c0, c1, c2, c3, c4, c5, c6, c7, c8) = decodeBoard boardInt
      fun cellToChar 0 = "."
        | cellToChar 1 = "X"
        | cellToChar 2 = "0"
        | cellToChar _ = raise Fail "Invalid cell value"
    in
      print (cellToChar c0 ^ cellToChar c1 ^ cellToChar c2 ^ "\n");
      print (cellToChar c3 ^ cellToChar c4 ^ cellToChar c5 ^ "\n");
      print (cellToChar c6 ^ cellToChar c7 ^ cellToChar c8 ^ "\n")
    end
end
