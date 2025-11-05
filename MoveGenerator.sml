(* This need to be implemented *)

structure  MoveGenerator:
sig
    type move = ((int*int) * (int*int) * real)

    (*Maybe Parallelism here?*)
    val generate_move_order: BoardRep.brep -> move array

    val apply_move : brep -> move -> brep
end =
struct
    type move = ((int*int) * (int*int) * real)
    fun generate_move_order bitmaps = 


    (* This need to be implemented *)
    fun apply_move bitmaps mv =

end