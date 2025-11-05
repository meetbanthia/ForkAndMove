(*This need to be implemented after move generator and board rep
This is where parallelism comes into consideration*)


structure  Search:
sig
    val search: Board.brep -> MoveGenerator.move
    val eval_position: Board.brep -> real
end =
struct

    fun set_bit_only i =
        let val setbit0 = Word64.fromInt 1
        in Word64.<< (setbit0, (Word.fromInt i))
        end

    fun add_values_bonuses c bmap =
        let 
            val pt = PieceTable.give_piece_table c
            val piece_score = PieceTable.piece_value 
        in  
            Parallel.reduce 
                op+ 
                0 
                (0,64)
                (fn(i) => 
                    let 
                        val setbit_i = set_bit_only i
                        val row = 7 - (i div 8)
                        val col = 7 - (i mod 8)
                    in
                        case Word64.compare (Word64.andb(bmap, set_bit_only i) , set_bit_only i) of
                            EQUAL => (Array2.sub(pt, row, col) + piece_score)
                            | _ => 0
                    end
                )
        end

    fun material bmaps =
        let
            fun f i =
                let 
                    val c = Seq.nth PieceTable.pieces i
                    val bmap = give_piece_bitmap c
                in 
                    add_values_bonuses c bmap
                end
            val white_side = Parallel.reduce op+ 0 (0,6) f
            val black_side = Parallel.reduce op+ 0 (6,12) f
        in
            white - black
        end

    fun eval_postion bmaps =
        let
            val material_factor = material bmaps

            (*Need to add other factors too like mobility *)
        in
            material_factor

    fun search bitmaps = 

end