open Core;;

let start_message = 
"Welcome to the game! This is two people mahjong, a simplified version of the traditional riichi mahjong. Enjoy!"


let start_game () = 
  let curr_state = ref Game.init_game in
  while Game.in_prog !curr_state do 
     curr_state := Game.simulate_round !curr_state;
  done
  

let guide = 
"
Tiles: Wind 1-4 represent Tong fong, Nan fong, Xia fong, Pei fong,respectively.
       Dragon 1-3 represent Bai, Chong, Fa, respectively.
       Man/Pin/Suo 1-9

Discard tile: type discard (tile to discard). Eg discard Wind 1
Chii: type chii (option) or skip
Pong: type pong (option) or skip
Kan : type kan (option) or skip
Quit: type quit

Enter start!
"

let () = 

  print_endline(start_message);
  print_endline("\n\n\n\n"^guide);
  match In_channel.(input_line_exn stdin) with 
  | "start" -> start_game ()
  | "quit" -> print_endline("thanks for playing the game")
  | _ -> print_endline("invalid, run the game again and enter start.")
