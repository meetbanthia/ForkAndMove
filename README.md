# ForkAndMove
Parallelised chess engine using Alpha–Beta pruning to explore multiple game states simultaneously. Building in SML/MaPLe to achieve CPU speedup through parallel search, caching, and pruning optimisations.


Instructions to Use:

To check if bitmpas are generated correctly, add a line in main.sml to call function print_bit_maps and see if the board printed was the board intialised. Add below line in main.sml

    main.sml :|
    val _ = BoardRep.print_bit_map () 
 
    Run:
    make main
    ./main