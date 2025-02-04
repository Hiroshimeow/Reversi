# Reversi in Common Lisp

This project implements the game Reversi (also known as Othello) in Common Lisp. It includes a human vs. human mode, and a human vs. computer mode with AI powered by minimax with alpha-beta pruning. The AI strategy is customizable through a weight matrix, and the code includes functions for optimizing these weights using a gradient descent approach.

## Features

*   **Reversi Game Logic:** Complete implementation of Reversi rules, including move validation, stone flipping, and game termination.
*   **Human vs. Human Mode:** Allows two human players to play against each other.
*   **Human vs. Computer Mode:** Play against an AI opponent with adjustable difficulty (search depth).
*   **Minimax AI with Alpha-Beta Pruning:** Efficient AI algorithm for making strategic decisions.
*   **Customizable Strategy:** The AI's strategy is determined by a weight matrix that can be modified to adjust its play style.
*   **Weight Optimization (Gradient Descent):** Functions for optimizing the AI's strategy by iteratively adjusting the weight matrix based on game results.
*   **Board Representation:** Uses a 2D array to represent the game board.
*   **File Input/Output:** Functions to save and load game states from files (for external integration or training).

## Code Structure

The code is organized into several sections:

*   **Constants:** Defines constants for board size, stone representation, and initial board state.
*   **Basic Board Functions:** Functions for creating, accessing, modifying, and printing the game board.
*   **Move Validation:** Functions for checking the validity of moves and flipping stones.
*   **Game Flow:** Functions for managing the game loop, handling player turns, and determining the game result.
*   **AI Implementation:**
    *   `eval-node`: Evaluation function to estimate the value of a game state.
    *   `minimax` & `alpha-beta`: Implementations of the minimax algorithm with alpha-beta pruning.
    *   `expand-node`: Generates the possible moves from a given game state.
*   **Strategy Optimization:**
    *   `loss-function`: Calculates the loss based on game results.
    *   `gradient`: Computes the gradient of the loss function with respect to the strategy weights.
    *   `update-strategy-weights`: Updates the strategy weights using gradient descent.
*   **File I/O:** Functions for reading and writing board states and moves to files.
*   **Main Functions:**
    *   `othello`:  Main function to start the game.
    *   `main-loop`: Function to perform the game iterations.
    *   `initialize`: Initialize important variables.

## Usage

1.  **Load the Code:** Load the Lisp code into your Common Lisp environment (e.g., SBCL, CLISP).

2.  **Play the Game:**

    *   **Human vs. Computer:** Run the `(othello sente-gote yomi-depth)` function.
        *   `sente-gote`:  Specify if the computer plays first (1), second (-1), or if its a test to output the state of the board (0).
        *   `yomi-depth`:  The search depth for the AI (higher values increase difficulty but also computation time). A depth of 2 is set as default.
    *   **Running Main-loop** Run function `(main-loop sente-gote yomi-depth)` to start the loop

3.  **Customize the AI Strategy:**

    *   Modify the `*strategy-weights*` array to change the AI's preferences for different board positions.

4.  **Optimize the AI Strategy (Experimental):**

    *   Run the `(run-optimization-loop num-iterations learning-rate board state)` function.
        *   `num-iterations`: The number of iterations for the gradient descent optimization.
        *   `learning-rate`: The learning rate for gradient descent.
        *   `board`: the board that the last result was calulated on
        *   `state`: state of game on the board

## Constants
* Board-size: Size of the board to use (default 8x8)
* Board-Sente: Player 1
* Board-Gote: Player -1
* Board-Empty: 0, represnts the current board position

## Notes

*   This code is written in Common Lisp and requires a Common Lisp implementation to run.
*   The AI's strategy optimization is an experimental feature and may require tuning of the learning rate and other parameters to achieve good results.
*   File I/O functions are designed to interface with a command server for testing purposes.
*   The code includes functions for playing against an AI opponent with adjustable difficulty and the ability to optimize the AI's strategy using gradient descent.

## Author
h20074

## License

[Choose a license, e.g., MIT License] (Replace with actual license details if applicable)