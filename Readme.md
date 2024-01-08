run dune build to build the game, then run dune exec src/terminal.exe to run the game. No outside libraries are used in this project.

apologize for the confusion. Here's the README in Markdown format with additional formatting:

markdown
Copy code
# **Mahjong Game in OCaml**

This is a Mahjong game implemented in OCaml. The project includes functionalities for drawing tiles, discarding tiles, handling riichi, checking for winning combinations (ron), and more. The game simulates a basic round of Mahjong, with players drawing and discarding tiles.

## **Getting Started**

To run the Mahjong game, follow these steps:

1. **Make sure you have OCaml installed on your machine.**

2. **Clone the repository:**

   ```bash
   git clone https://github.com/your-username/mahjong-ocaml.git

   
I apologize for the confusion. Here's the README in Markdown format with additional formatting:

markdown
Copy code
# **Mahjong Game in OCaml**

This is a Mahjong game implemented in OCaml. The project includes functionalities for drawing tiles, discarding tiles, handling riichi, checking for winning combinations (ron), and more. The game simulates a basic round of Mahjong, with players drawing and discarding tiles.

## **Getting Started**

To run the Mahjong game, follow these steps:

1. **Make sure you have OCaml installed on your machine.**

2. **Clone the repository:**

   ```bash
   git clone https://github.com/your-username/mahjong-ocaml.git
Navigate to the project directory:

bash
Copy code
cd mahjong-ocaml
Compile and run the game:

bash
Copy code
ocamlc -o mahjong game.ml
./mahjong
Game Rules and Features
The game follows standard Mahjong rules for drawing and discarding tiles.
Players can perform actions such as chii, pong, kan, and skip.
Riichi (ready hand) is supported, and players can declare riichi if conditions are met.
The game checks for winning combinations (ron) and ends the round if a player wins.
Code Structure
The code is organized into modules and follows a functional programming style.
Good coding practices, such as immutability and type annotations, are adhered to.

How to Play
## Draw a Tile:

Players take turns drawing a tile from the tile pool.

draw_handles player_number game_state
## Discard a Tile:

Players choose a tile to discard. Under riichi, the player must discard the tile they just drew.

discard_handles player_number game_state
## Perform Actions:

Players can perform actions such as chii, pong, kan, or skip, based on the available options.

chii_pong_kang_handles player_number discarded_tile available_tiles game_state
## Declare Riichi:

Players can declare riichi if they meet the conditions.

check_p_riichi player_number game_state
## Check for Ron:

The game checks for winning combinations (ron) and ends the round if a player wins.


handle_ron player_number tile_to_check game_state
## Simulate a Round:

Simulates a complete round of Mahjong, including drawing, discarding, and handling actions.

simulate_round game_state
