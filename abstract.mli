module type Game = sig
  (*this Game can be seen as a Record that contain several attributes. It 
   will have different attributes compared to game in the game.ml file of ocaml-mjlib *)
  type t = game
  (* this fun initialize a game, giving the game its initial setup values*)
  val setup : unit -> game
  
  (*this fun sets up the players that is playing the game)
  
  

end
