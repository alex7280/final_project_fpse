open Core;;

type state = {
    (*simulates the unused tile*)
    mutable paiShan : Tile.t list;
    (*player1*)
    p1 : Player.t;
    (*player2*)
    p2 : Player.t;
    (*turn player*)
    mutable t_player : int;
    (*list of dora*)
    mutable dora_list : Tile.t list;
    (* is game still running*)
    mutable in_progress : bool;
    mutable round_num : int ;
  }
type t = state

let in_prog (s:state) = s.in_progress
                    
(*gets a tile from the list at the specified position, return remaining list*)  
let rec get_a_tile (t_list: Tile.t list) (pos:int) (new_list:Tile.t list) = 
  match t_list with 
  | [] -> failwith("should not happen")
  | x::xs -> if pos = 0 then (x,new_list @xs) 
             else get_a_tile xs (pos-1) (x::new_list)
           
let rec get_n_tiles_helper ( t_list : Tile.t list) (num:int) (extracted : Tile.t list) (is_rand:bool) = 
  match num with 
  | 0 -> (t_list,extracted)
  | _ -> match is_rand with 
         | true -> let r = Random.int (List.length t_list) in
                   let (t,remained) = get_a_tile t_list r [] in 
                   get_n_tiles_helper remained (num-1) (t:: extracted) is_rand
         | false -> let (t,remained) = get_a_tile t_list 0 [] in 
                    get_n_tiles_helper remained (num-1) (t::extracted) is_rand
(*get num tiles from a list, in a random/fixed fashion*)
let get_n_tiles (t_list: Tile.t list) (num:int) (is_rand:bool) = 
  match num with 
  | 0 -> (t_list,[])
  | _ -> get_n_tiles_helper t_list num [] is_rand
(*shuffle tiles*)
let shuffle (t_list : Tile.t list) = 
  match get_n_tiles t_list (List.length t_list) true with 
  | (_,extracted) -> extracted
(*initialize game. Give each player 13 tiles to start with*)               
let init_game = 
  let new_state = { 
      paiShan = shuffle (Tile.generate_game_tiles ());
      p1 = Player.class_constructor 1 false true [] [];
      p2 = Player.class_constructor 2 false true [] [];
      t_player = 1;
      in_progress = true;
      dora_list = Tile.construct_fake Man 1::[];
      round_num = 0 ;
    } in  
  let (left_tiles,fst_hand) = get_n_tiles new_state.paiShan 13 true in
  new_state.p1.total_tiles <- fst_hand ;
  let (left_tiles_snd,second_hand) = get_n_tiles left_tiles 13 true in 
  new_state.p2.total_tiles <- second_hand;
  let (left_tiles_dora,init_dora) = get_n_tiles left_tiles_snd 1 true in 
  new_state.dora_list <- [List.nth_exn init_dora 0] ;
  new_state.paiShan <- left_tiles_dora;
  new_state
    
[@@@coverage off]
(*show information of a player*)
let show_myself_info (p: Player.t) = 
  print_endline ("\n My info:"
                 ^ "\nNon fulu:\n");
  Tile.print_tile_list (Tile.get_all_non_fulu p.total_tiles);
  print_endline "\n Fulu:";
  Tile.print_tile_list (Tile.get_all_fulu p.total_tiles); 
  print_endline "\n ron info:";
  Player.print_ron_info p.r_info;
  print_endline "\n discarded:";
  Tile.print_tile_list p.discarded;
  print_string("\n")
  
let show_other_info (p: Player.t) = 
  print_endline ("\n Opponent info");
  print_endline ("\n fulu:");
  Tile.print_tile_list (Tile.get_all_fulu p.total_tiles);
  print_string("\n");
  print_endline "\n discarded";
  Tile.print_tile_list p.discarded;
  print_string("\n")
  
let show_info (player_to_see:int) (my_id:int) (g:state)= 
  if (player_to_see = my_id) then (match player_to_see with 
                                   | 1 -> show_myself_info (g.p1)
                                   | 2 -> show_myself_info (g.p2)
                                   | _ -> failwith("should be dealt at command line")
                                  )
  else (match player_to_see with 
        | 1 -> show_other_info (g.p1)
        | 2 -> show_other_info (g.p2)
        | _ -> failwith("should be dealt at command line")
       )
(*simulates a player drawing a tile*)
let draw_handles (which:int) (g:state) =
  let p = ref g.p1 in
  if which =1 then p := g.p1
  else p := g.p2;
  if (List.length g.paiShan <= 0) then failwith("this should have been catched")
  else Player.turn_draw_tile !p (List.nth_exn g.paiShan 0);
  print_endline ("You drawed\n");
  Tile.print_tile (List.nth_exn g.paiShan 0);
  g.paiShan <- shuffle (List.sub g.paiShan ~pos:(1) ~len: (List.length g.paiShan -1));
  g
  
(*checks if a player can chii or pong or kan, and if possible, handles player's chii pong kan or skip.*)
let rec chii_pong_kang_handles (which:int) (discard:Tile.t) (avail:Tile.t list) (g:state) = 
  let p = ref g.p1 in
  if which =1 then p := g.p1
  else p := g.p2;
  let options = Tile.find_all_ke_shun (Tile.sort_hand_add_tile (Tile.get_all_non_fulu avail) discard) discard in
  match List.length options with 
  | 0 -> print_endline("You can't do any action on the discarded tile."); g
  | _ -> print_string("\n");
         print_endline("You can perform the following chii/pong/kan");
         Tile.print_tile_list_list options;
         print_endline("As a remainder, your hand (dark tiles aka non fulu tiles)is:");
         Tile.print_tile_list (Tile.get_all_non_fulu !p.total_tiles);
         print_endline("\n and your fulu tiles is:");
         Tile.print_tile_list (Tile.get_all_fulu !p.total_tiles);
         print_endline("\n");
         try(
           match Action.read_str (In_channel.(input_line_exn stdin) ) with 
           | Chii n -> let k = n-1 in 
                       if (k>=0) && (k <= List.length options -1)
                       then (Player.update_ron_info !p (List.nth_exn options k );
                             !p.mensei <- false;
                             let acc_non_fulu = Tile.sort_hand (discard::!p.total_tiles) in 
                             !p.total_tiles <- Player.set_fulu acc_non_fulu (List.nth_exn options k);
                             if (List.length (List.nth_exn options k) = 4) 
                             then (let after_kan = draw_handles which g in
                                   let (left_tiles_dora,init_dora) = get_n_tiles g.paiShan 1 true in
                                   after_kan.dora_list <- List.nth_exn init_dora 0 :: after_kan.dora_list ;
                                   after_kan.paiShan <- left_tiles_dora;
                                   print_endline("you kan - ed, so you drawed a tile and displayed a dora");
                                   after_kan)
                             else (print_endline("you chii-ed!");
                                   g))
                       else (print_endline("invalid chii, enter again");
                             chii_pong_kang_handles which discard avail g)
           | Skip -> print_endline("skip chii");
                     g
           | Quit -> g.in_progress <- false;
                     g
                     
           | _ ->print_endline("invalid command, enter again");
                 chii_pong_kang_handles which discard avail g)
         with 
         | _ -> print_endline("invalid command, enter again");
                chii_pong_kang_handles which discard avail g
(*handles a player's discard. Under riichi, player discard what he drawed.*)
let rec discard_handles (which:int) (g:state) = 
  let p = ref g.p1 in 
  if which =1 then p := g.p1 
  else p := g.p2;
  let first = List.nth_exn !p.total_tiles 0 in  
  match !p.richii with 
  | true -> print_endline("under richii,discard tile u have drawn\n");
            !p.total_tiles <- Tile.delete_one_tile !p.total_tiles first [];
            !p.discarded <- first::!p.discarded;
            g.round_num <- g.round_num + 1;
            g
  | false -> print_endline("You will need to select a tile to discard\n");
             print_endline("As a remainder, your non fulu tiles are\n ");
             Tile.print_tile_list (Tile.get_all_non_fulu !p.total_tiles);
             print_endline("\n and your fulu tiles are:");
             Tile.print_tile_list (Tile.get_all_fulu !p.total_tiles);
             print_endline("\n");
             try (
               match Action.read_str (In_channel.(input_line_exn stdin)) with 
               | Discard (kind,number) -> 
                  if Tile.have_tile (Tile.get_all_non_fulu !p.total_tiles) kind number 
                  then (print_endline("You have selected to discard\n");
                        !p.total_tiles <- Tile.sort_hand ( Tile.get_all_fulu !p.total_tiles @
                                                             Tile.delete_one_tile (Tile.get_all_non_fulu !p.total_tiles) (Tile.construct_fake kind number) []);
                        !p.discarded <- Tile.construct_fake kind number ::!p.discarded;
                        g.round_num <- g.round_num + 1;
                        g)
                  else (print_endline("Dont discard a tile you dont have lol\n");
                        discard_handles which g) 
               | Quit -> g.in_progress <- false;
                         g
               | _ ->print_endline("invalid command, enter again");
                     discard_handles which g)
             with 
             | _ ->print_endline("invalid command, enter again");
                   discard_handles which g
(*check if a player is able to riichi,and riichi if possible*)
let check_p_riichi (which:int) (g:state) = 
  let t_list = if which = 1 then Player.get_riichi g.p1 
               else Player.get_riichi g.p2 in 
  if (which = 1) && not (g.p1.mensei) then g
  else if (which = 2 ) && not (g.p2.mensei) then g
  else(
    match List.length t_list with 
    | 0 -> g
    | _ -> print_endline("You have ri-chiied, wait until these tiles appear");
           Tile.print_tile_list t_list;
           match which with 
           | 1 -> g.p1.richii <- true;
                  g
           | 2 -> g.p2.richii <- true;
                  g
           | _ -> failwith("stop")
  )
(*this checks if a player is able to ron. If so, end the game and output the yakus that the player achieved*)
let handle_ron (which_player:int) (to_check:Tile.t) (g:state) = 
  let p = ref g.p1 in 
  if which_player = 1 then p := g.p1 
  else p:= g.p2;
  let add_to_check = to_check :: !p.total_tiles in
  match Player.ron_and_cal_yaku add_to_check !p with 
  | (_,0) -> g
  | (yaku,n) ->let num = ref n in  
               print_endline("You win");
               List.iter g.dora_list ~f:(fun x-> List.iter !p.total_tiles 
                                                   ~f:(fun y -> if Tile.check_equal y x then 
                                                                  num := !num + 1;
                 ));
               if !num <=3 then (print_endline("You ron... But not an exciting one.");
                                 List.iter yaku ~f:(fun x-> print_endline(Player.string_of_yaku x));
                                 g.in_progress <- false;
                                 g)
               else if !num >= 4 && !num <= 6 then (print_endline("Man-Gan! 8000 den!");
                                                    List.iter yaku ~f:(fun x-> print_endline(Player.string_of_yaku x));
                                                    g.in_progress <- false;
                                                    g)
               else if !num >= 6 && !num <= 10 then (print_endline("Hale-Man! 12000 den!");
                                                     List.iter yaku ~f:(fun x-> print_endline(Player.string_of_yaku x));
                                                     g.in_progress <- false;
                                                     g)
               else if !num >= 10 && !num <= 13 then (print_endline("San-pei-man ?!! 18000 den ?!") ;
                                                      List.iter yaku ~f:(fun x-> print_endline(Player.string_of_yaku x));
                                                      g.in_progress <- false;
                                                      g)
               else if !num >= 13 then (print_endline ("Yaku-man!!! 24000 den!!! Congrats!");
                                        List.iter yaku ~f:(fun x-> print_endline(Player.string_of_yaku x));
                                        g.in_progress <- false;
                                        g)
               else if !num >= 26 then (print_endline ("W-Yaku-man or above-- congrats!!!!!!");
                                        List.iter yaku ~f:(fun x-> print_endline(Player.string_of_yaku x));
                                        g.in_progress <- false;
                                        g)
               else (failwith("what is happening");)
(*when no chii action is available, proceed as normal*)
let no_action_available (which:int) (g:state) (last_disc:Tile.t) = 
  let after_check_lastp_disc = handle_ron which last_disc g in
  if not after_check_lastp_disc.in_progress then after_check_lastp_disc
  else(
    let after_draw_g = draw_handles which g in
    let curr_player = (if which = 1 then g.p1 else g.p2) in
    let drawn_tile = List.nth_exn (curr_player.total_tiles) 0 in
    let after_ron_g = handle_ron which drawn_tile after_draw_g in
    if after_ron_g.in_progress 
    then let after_discard_g = discard_handles which after_ron_g in
         if after_discard_g.in_progress then let after_richii_g = check_p_riichi which after_discard_g in
                                             after_richii_g.t_player <- (after_richii_g.t_player mod 2  +1);
                                             after_richii_g
         else after_discard_g
    else after_ron_g
  )
  
(*when a chii action is available, handle if the player chii-ed or not. If he chiied, first handles the chii, and skip to discarding tiles. 
  Otherwise, proceed as normal*)
let action_available (g:state) (last_disc:Tile.t) (which:int) (p:Player.t ref)= 
  let after_check_lastp_disc = handle_ron which last_disc g in
  if not after_check_lastp_disc.in_progress then after_check_lastp_disc
  else (let fulu_list = Tile.get_all_fulu !p.total_tiles in
        let chii_handle = chii_pong_kang_handles which last_disc (!p.total_tiles)
                            after_check_lastp_disc in
        if not (List.length (Tile.get_all_fulu !p.total_tiles) = List.length fulu_list) 
        then (print_endline("after check chii");
              let after_discard_g = discard_handles which chii_handle in
              if after_discard_g.in_progress 
              then (let after_richii_g = check_p_riichi which after_discard_g in
                    after_richii_g.t_player <- (after_richii_g.t_player mod 2  +1);
                    after_richii_g)
              else after_discard_g
             )
        else let after_draw_g = draw_handles which chii_handle in
             let curr_player = (if which = 1 then g.p1 else g.p2) in
             let drawn_tile = List.nth_exn (curr_player.total_tiles) 0 in
             let after_ron_g = handle_ron which drawn_tile after_draw_g in
             if after_ron_g.in_progress 
             then let after_discard_g = discard_handles which after_ron_g in
                  if after_discard_g.in_progress then let after_richii_g = check_p_riichi which after_discard_g in
                                                      after_richii_g.t_player <- (after_richii_g.t_player mod 2  +1);
                                                      after_richii_g
                  else after_discard_g
             else after_ron_g
       )
  
(*this simulates the whole "round" of game, including draw, discard, riichii, check ron ,etc*)
let simulate_round_helper_action (g:state) (last_disc:Tile.t) (which:int) = 
  let p = ref g.p1 in
  if which = 1 then p := g.p1
  else p:= g.p2;
  let combine_curr = last_disc :: !p.total_tiles in
  match Tile.find_all_ke_shun combine_curr last_disc with
  | [] -> no_action_available which g last_disc
  | _ -> action_available g last_disc which p

let display_basic_info (g:state) = 
  print_endline("\n dora list");

  List.iter g.dora_list ~f:(fun x-> Tile.print_tile x; print_string ("  "));
  print_endline("round number: " ^ string_of_int g.round_num)

(*simulate how a round goes. When the game just starts, skip checking ron and go directly to draw. Otherwise, use helper function to proceed*)
let simulate_round (g:state) = 
  display_basic_info g;
  if g.t_player = 1 
  then ( show_other_info g.p2;
         show_myself_info g.p1)
  else ( show_other_info g.p1;
         show_myself_info g.p2);
  let which = g.t_player in 
  match g.round_num with
  | 0 -> let after_draw_g = draw_handles which g in 
         let curr_player = (if which = 1 then g.p1 else g.p2) in 
         let drawn_tile = List.nth_exn (curr_player.total_tiles) 0 in 
         let after_ron_g = handle_ron which drawn_tile after_draw_g in 
         if after_ron_g.in_progress 
         then let after_discard_g = discard_handles which after_ron_g in 
              if after_discard_g.in_progress then let after_richii_g = check_p_riichi which after_discard_g in 
                                                  after_richii_g.t_player <- (after_richii_g.t_player mod 2  +1);
                                                  after_richii_g
              else after_discard_g
         else after_ron_g
  | _ -> 
     let last_disc = (match which with
                      | 1 -> List.nth_exn g.p2.discarded 0
                      | 2 -> List.nth_exn g.p1.discarded 0
                      | _ -> Tile.construct_fake Man 1
                     ) in 
     simulate_round_helper_action g last_disc which

[@@@coverage on]
