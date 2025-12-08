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
- **`Search.sml`**: Contains the search algorithms (`alpha_beta_search`, `minimax`, `pvs_search`) and evaluation function.
- **`main.sml`**: Entry point for the engine. Handles FEN input, invokes the search, and displays results. By default runs chess.
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

The engine can play a game starting from a specific board state provided via a file or direct FEN string. It supports different search algorithms and game settings.

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

### Command Line Arguments

| Flag | Description | Default |
|------|-------------|---------|
| `-fen "<string>"` | Set the initial board state using FEN notation. | Standard Start |
| `-file <path>` | Read FEN string from a file. | None |
| `-depth <n>` | Set the search depth for the AI. | 4 |
| `-moves <n>` | Set the maximum number of moves to simulate in the game loop. | 10 |
| `-alphabeta` | Use the **Alpha-Beta Pruning** search algorithm. | Disabled (Minimax used by default) |
| `-minimax` | Use the **Minimax** search algorithm. | Enabled (Default) |
| `-pvs` | Use the **Principal Variation Splitting** search algorithm. | Disabled (Minimaxed used by default) |
| `-mode` | Optional flag for game mode. By default it is set to chess (2 for TicTacToe and 3 for Ultimate TicTacToe) | Chess |

**Examples:**

Run a game with Alpha-Beta pruning at depth 6 for 20 moves:
```bash
./main -alphabeta -depth 6 -moves 20
```

Run an Ultimate TicTacToe game with PVS search at depth 4 for 100 moves:
```bash
./main -pvs -depth 4 -moves 100 -mode 3
```

## Verification

To verify the correctness of the Alpha-Beta implementation, you can run both the Minimax and Alpha-Beta algorithms on the same game configuration and diff their outputs. Since both should yield the same best moves (just with different performance), their outputs should match identically except for the algorithm name.

Run the following command:
```bash
./main @mpl procs 64 -- -depth 4 -moves 1 -alphabeta > ab.txt && ./main @mpl procs 64 -- -depth 4 -moves 1 -minimax > mm.txt && diff ab.txt mm.txt
```

**Expected Output:**
The `diff` command should report only a single line difference corresponding to the algorithm name printed in the header:
```
7c7
< Algorithm: Alpha-Beta
---
> Algorithm: Minimax
```

## Current Status
- **Core**: Bitboard representation and move generation are fully implemented.
- **Evaluation**: Material counting and threat detection (forks, pins, direct attacks).
- **Search**: Parallel Minimax and Alpha-Beta pruning implementations are functional.
- **Gameplay**: The engine can play against itself or simulate games from a given position.

## Contributors
- Meet Banthia - MSCS, NYU Courant
- Pranav Sharma - MSCS, NYU Courant

