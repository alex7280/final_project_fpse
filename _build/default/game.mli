open Types

module type Game = sig
  (*this Game can be seen as a Record that contain several attributes. It
   will have different attributes compared to game in the game.ml file of ocaml-mjlib *)
  type t = game
  (* this fun initialize a game, giving the game its initial setup values*)
  val setup : unit -> game

  (*this fun sets up the players that is playing the game. The player variable refers to player in ocaml-mjlib*)
  val players : game -> player -> player -> game
  (*this is called if a player ask for the assistance tool*)
  val players_assist : game -> player -> game
  (* this sets up the dora of the game*)
  val dora : game -> game
  (* this initializes a rock-paper-scissor shoot game. The player returned indicates the winner*)
  val rps_game : game -> player
  (* this sets up the pai-shan of the game-- no pai in pai-shan can be distributed to players*)
  val pai_shan : game -> pai list
  (* this sets up the li-dora of the game, which is based on pai-shan*)
  val li_dora: game -> game
  (*this distributes 30 tiles to each players in random fashion. no tiles in pai-shan will be distributed*)
  val distribute : game -> pai list -> (player * pai list)
  (*this allows a player to select 13 pai of the 30 pais they are distributed, and make the remaining 17 pais be the 
pais that they play. The bool value indicates if the assistance tool is selected. *)
  val select_pai : game -> player -> bool -> pai list -> (player * pai list)

  (*this finds the remaining 17 pais that the players need to play, and feeds in the 13 pais the player selected, 17 pais the player will play*)
  val feed_game_selection: game -> player -> pai list -> pai list -> game
  (*when both players have done the selection, game starts*)
  val finish_selection: game -> game
  (*check if the game can be continued, based on the remaining pai of the player.*)
  val check_playable: game -> player -> pai list -> bool

  (*each player plays a pai in their turn. The player will have the pai removed from their playable pai.*)

  val play_pai: game-> player -> pai -> pai list -> (game*pai list)
  (*after the player plays the pai, check if his opponent reaches 4 fan (using the fan functions) and if so, his opponent wins and we end the match and return it. Else, continue the game*)
  val check_win: game -> pai -> player -> pai list -> (bool * game)
  (*if the match results in a tie, then edit the game such that the winner of next match will receive 2x points*)
  val tie_happens: game -> game
  (*if the match results in a player winning, edit the scores of the players. The li-dora will be revealed and taken into account.*)
  val win_happens: game -> int -> tile -> player -> (game*player)
  (* when the 4 matches are completed, the total score will be computed, and winner will be decided*)
  val compute_result: game -> player -> player -> player

end
