# ForkAndMove: Parallel Chess Engine in MPL

A parallel chess engine implemented in MPL (MaPLe), designed to explore parallel search algorithms on the chessboard.

## Overview

ForkAndMove is a chess engine built to leverage the parallelism features of the MPL language. It includes:
- **Bitboard Representation**: Efficient 64-bit word representation for board state and piece movement.
- **Move Generation**: Logic to generate legal chess moves for all piece types.
- **Parallel Search**: Implementation of Minimax and Alpha-Beta pruning algorithms (currently in development) designed to run in parallel.
- **FEN Support**: Ability to load board states using Forsyth-Edwards Notation (FEN).

## Project Structure

- **`Board.sml`**: Defines the bitboard representation (`brep`) and utilities for board manipulation and printing.
- **`MoveGenerator.sml`**: Implements move generation logic for all pieces using bitwise operations.
- **`Search.sml`**: Contains the search algorithms (`alpha_beta_search`, `minimax`) and evaluation function.
- **`main.sml`**: Entry point for the engine. Handles FEN input, invokes the search, and displays results.
- **`PieceTable.sml`**: Piece-square tables for static board evaluation.
- **`lib/` & `lib-local/`**: MPL library dependencies and local helpers.

## Getting Started

### Prerequisites
- **MPL Compiler**: You need the MPL compiler installed and available in your path.

### Building
To compile the project, run:
```bash
make main
```
This will generate an executable named `main`.

### Usage

Run the engine by providing a board state via a file or direct FEN string.

**1. Using a FEN file:**
Create a text file (e.g., `board.txt`) containing a FEN string:
```text
rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w
```
Then run:
```bash
./main -file board.txt
```

**2. Using a FEN string directly:**
```bash
./main -fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w"
```

**3. Default (Standard Start):**
```bash
./main
```

**Options:**
- `-depth <n>`: Set the search depth (default is 4).

## Current Status
- Move generation and board representation are implemented.
- Parallel Alpha-Beta search structure is in place.
- The engine currently mocks the "best move" selection while the search integration is being finalized.

## Contributors
- Meet Banthia - MSCS, NYU Courant
- Pranav Sharma - MSCS, NYU Courant

