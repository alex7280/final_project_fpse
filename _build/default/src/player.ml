open Core;;


type player = {
    p_id : int;
    (* indicates if player riichi-ed or not*)
    mutable richii : bool;
    (*indicate if a player is still mensei aka never performed chii, pong, gang*)
    mutable mensei : bool;

    (*records discarded tiles*)
    mutable discarded : Tile.tile list;
    (*records tiles that the player has right now*)
    mutable total_tiles : Tile.tile list;
}

type t = player

let get_all_fulu (p:player) = List.filter ~f:(fun x -> Tile.get_fulu x) p.total_tiles

let get_all_non_fulu (p:player) = List.filter ~f:(fun x -> not (Tile.get_fulu x)) p.total_tiles

let turn_draw_tile (p:player) (to_add:Tile.t) = 
  p.total_tiles <- Tile.sort_hand_add_tile p.total_tiles to_add

let play_tile (p:player) (to_play:Tile.t) = 
  let hand = p.total_tiles in 
  let () = Tile.change_discarded to_play in
  p.total_tiles <- Tile.sort_hand (Tile.delete_one_tile hand to_play []);
  p.discarded <- Tile.sort_hand_add_tile (p.discarded) to_play

let class_constructor (i:int) (r:bool) (m:bool) (t_list: Tile.t list) (dis: Tile.t list) = {
  p_id = i;
  richii = r;
  mensei = m;
  total_tiles = t_list;
  discarded = dis;
  }
 
let user_chii_pong_kan (t:Tile.t) (p:player) (which_triplet:int) = 
  let hand = p.total_tiles in 
  let all_ke_shun = Tile.find_all_ke_shun hand t in
  match all_ke_shun with
  | [] -> ()
  | _ -> let triplet = List.nth all_ke_shun which_triplet in 
         (match triplet with
         | None -> ()
         | Some x -> p.total_tiles <- Tile.delete_list_of_tile x hand;
                     p.mensei <- false;
                     ()
         )

(*this type helps to decide the information needed for ron. Also facilitate deleting tuples, for example*)
type ron_info = {
    mutable shun_list : Tile.t list list;
    mutable pong_list : Tile.t list list;
    mutable kan_list : Tile.t list list;
    mutable pair_ron: Tile.t list list;
}

let add_tuple (to_add:Tile.t) (new_rep: (Tile.t * int) array)= 
  (*let rec add_tuple_helper (to_add:tile) (temp: tile list) (new_rep:tile list) =
    match new_rep with
    | [] -> 
*)
  let found = ref false in 
  let () = Array.iteri ~f:(fun i (tile,count) -> if (Tile.get_number tile = to_add.number) && (Tile.kind_equal (Tile.get_kind tile) to_add.kind)
                                        then found := true; Array.set new_rep i (tile,count+1)
             (*let _ = count = count + 1 in 
                                             ()*)
             ) new_rep in 
  match !found with 
  | false -> let new_array = Array.create ~len:(Array.length new_rep + 1) (Array.get new_rep 0) in 
             let () = Array.iteri ~f:(fun i x -> Array.set new_array (i+1) x ) new_rep in
             let () = Array.set new_array 0 (to_add,1) in
             new_array
  | true -> new_rep
  
let rec alternate_rep_helper (orig_rep:Tile.t list) (new_rep: (Tile.t * int) array) = 
  match orig_rep with
  | [] -> new_rep
  | x::xs -> alternate_rep_helper xs (add_tuple x new_rep)

let alternate_rep (orig_rep:Tile.t list) = 
  let new_rep = Array.create ~len:1 (List.nth_exn orig_rep 0, 1) in  
  match orig_rep with
  | [] -> new_rep
  | _::xs -> alternate_rep_helper xs new_rep

(*let rec get_count (t:Tile.t) (new_rep: (Tile.t * int) array) = 
  match Array.to_list new_rep with
  | [] -> 0
  | x::xs -> (match x with
              | (tile,count) -> if Tile.check_equal tile t then count
                                else get_count t (Array.of_list xs)
             )
(*let clean_up_after_delete_helper(new_rep: (Tile.t * int) array) (acc_rep: (Tile.t * int) array)= 
  match Array.length new_rep with
  | 0 -> acc_rep
  | _ -> let found = ref false in 
                                 let new_array = Array.make (Array.length acc_rep - 1) (Array.get acc_rep 0) in
                                 let () = Array.iteri 
                                            (fun i x -> (match !found with
                                                         | false -> (match x with
                                                                     | (_,count) ->if (count = 0) then found := true
                                                                                   else Array.set new_array i 
                                                                                 (Array.get new_rep 0)
                                                                    )
                                                         | true -> Array.set new_array (i-1) (Array.get new_rep 0)
                                                        )) new_rep in 
                                             (match !found with
                                              | false -> new_array
                                              | true -> clean_up_after_delete new_array
                                             )
                                            )*)
                            
let rec clean_up_after_delete (new_rep: (Tile.t * int) array) =
  let clean_up_after_delete_helper(new_rep: (Tile.t * int) array) (acc_rep: (Tile.t * int) array)=
  match Array.length new_rep with
  | 0 -> acc_rep
  | _ -> let found = ref false in
                                 let new_array = Array.make (Array.length acc_rep - 1) (Array.get acc_rep 0) in
                                 let () = Array.iteri
                                            (fun i x -> (match !found with
                                                         | false -> (match x with
                                                                     | (_,count) ->if (count = 0) then found := true
                                                                                   else Array.set new_array i
                                                                                 (Array.get new_rep 0)
                                                                    )
                                                         | true -> Array.set new_array (i-1) (Array.get new_rep 0)
                                                        )) new_rep in
                                             (match !found with
                                              | false -> new_array
                                              | true -> clean_up_after_delete new_array
                                             )
                                             in
 
  clean_up_after_delete_helper new_rep (Array.make (Array.length new_rep) (Array.get new_rep 0))

let rec remove_first_nth (count:int) (new_rep: (Tile.t * int) array) =
  if count = 0 then new_rep
  else (match Array.length new_rep with
        | 0 -> failwith("impossible")
        | _ -> remove_first_nth (count-1) (Array.sub new_rep 1 (Array.length new_rep))
       )
let dec_count (dec:int) (t:Tile.t) (new_rep: (Tile.t * int) array) = 
  Array.iteri (fun i x -> (match x with
                       | (tile,count) -> if Tile.check_equal tile t then Array.set new_rep i (tile,count-dec)
                                  )
             ) new_rep

let rec rem_first_shun (i:int) (shun:Tile.t list) (sorted: Tile.t list) (acc: Tile.t list) = 
  if i = 0 then acc @ sorted
  else (match (shun,sorted) with
        | (x::xs),(y::ys) -> if Tile.check_equal x y then rem_first_shun (i-1) xs ys acc
                             else rem_first_shun i shun ys  (acc @ [y])
        | _ -> failwith("impossible") )

let dec_count_multiple (i:int) (new_rep: (Tile.t * int) array) = 
  if i = 0 then clean_up_after_delete new_rep
  else ( match Array.length new_rep with
         | 0 -> failwith("impossible")
         | _ -> match Array.get new_rep 0 with
                (tile,count) -> let () = Array.set new_rep 0 (tile,count-1) in
                                new_rep

(*let new_array = Array.make (Array.length acc_rep + 1) (Array.get new_rep 0) in
                                let () = Array.iteri (fun i x -> Array.set new_array (i+1) x ) in
                                let () = Array.set new_array 0 x in
                                dec_count_multiple (i-1) (Array.sub new_rep 1 (Array.length new_rep)) new_array*)
       )

let rec get_three (i:int) (acc_rep: (Tile.t * int) array) (acc: Tile.t list)= 
  if i = 0 then acc_rep
  else (match Array.to_list acc_rep with
        | [] -> failwith "impossible"
        | (tile,_)::xs -> get_three (i-1) (Array.of_list xs) (acc@[tile])
       )*)

let rec print_alt_display (acc_rep: (Tile.t * int) array) = 
  match Array.length acc_rep with
  | 0 -> ()
  | _ -> let (tile,count) = Array.get acc_rep 0 in 
         Tile.print_tile tile;
         print_endline (string_of_int count);
         print_alt_display (Array.sub acc_rep ~pos:1 ~len:(Array.length acc_rep))

let tanyao_check (t_list:Tile.t list) = 
  let all_tanyao = ref true in 
  List.iter ~f:(fun x -> if not (Tile.check_non_yao x) then all_tanyao := false) t_list;
  match !all_tanyao with 
  | true -> false
  | false -> true

let non_tanyao_check (t_list: Tile.t list) = 
  let all_yao = ref true in
  List.iter ~f:(fun x -> if (Tile.check_non_yao x) then all_yao := false) t_list;
  all_yao

let hun_yisu_check (t_list:Tile.t list) = 
  let man_list = Tile.find_kind t_list Man in
  let pin_list = Tile.find_kind t_list Pin in
  let suo_list = Tile.find_kind t_list Suo in 
  let dragon_list = Tile.find_kind t_list Dragon in 
  let wind_list = Tile.find_kind t_list Wind in 
  if (List.length man_list =0) && (List.length pin_list = 0) && (not (List.length dragon_list = 0) || not (List.length wind_list = 0)) then true
  else if (List.length pin_list = 0 ) && (List.length suo_list = 0) && ( not (List.length dragon_list = 0) || not (List.length wind_list = 0)) then true
  else if (List.length man_list = 0 ) && (List.length suo_list = 0) && (not (List.length dragon_list = 0) || not (List.length wind_list = 0)) then true
  else false

let qing_yisu_check (t_list:Tile.t list) = 
    let man_list = Tile.find_kind t_list Man in
  let pin_list = Tile.find_kind t_list Pin in
  let suo_list = Tile.find_kind t_list Suo in
  let dragon_list = Tile.find_kind t_list Dragon in
  let wind_list = Tile.find_kind t_list Wind in
  if (List.length man_list =0) && (List.length pin_list = 0) && (List.length dragon_list = 0) && (List.length wind_list = 0) then true
  else if (List.length pin_list = 0 ) && (List.length suo_list = 0) && (List.length dragon_list = 0) && (List.length 
wind_list = 0) then true
  else if (List.length man_list = 0 ) && (List.length suo_list = 0) && (List.length dragon_list = 0) && (List.length
wind_list = 0) then true
  else false



let rec has_dragon_triplet (acc_rep: (Tile.t * int) array) = 
  (*let empty_array = Array.make 0 (List.nth t_list 0, 0) in *)
  (*let acc_rep = alternate_rep t_list empty_array in*)
  match (Array.length acc_rep) with
  | 0 -> false
  | _ -> let (tile,count) = (Array.get acc_rep 0) in
         if (Tile.kind_equal tile.kind Dragon) && (count >2) then true
         else has_dragon_triplet (Array.sub acc_rep ~pos:1 ~len:(Array.length acc_rep ))

let one_pair_left(acc_rep:(Tile.t * int) array) = 
  print_endline(string_of_int (Array.length acc_rep));
  let non_pair_exist = ref false in 
  let pair_count = Array.fold acc_rep ~init:(0) ~f:(fun acc (_,count) -> if count = 2 then (acc+1)
                                                                            else let () = (non_pair_exist := true) in
                                                                            acc) in 
  (*print_endline(string_of_int pair_count ^ "pair_count");
  print_endline(Bool.to_string !non_pair_exist ^ "non pair exist");
  *)
  if not(pair_count = 1) then false
  else if (Bool.equal !non_pair_exist true) then false
  else true

(*let rec check_ron_helper (t_list:Tile.t list) (info:ron_info) = 
  Array.iter ~f:(fun x-> let operate_list = Tile.find_all_ke_shun t_list in
                         if List.is_empty operate_list then
                           (if one_pair_left ( alternate_rep t_list) then (true,info)
                            else (false,[]))
                         else
                           let all_ron =
                           (List.iter ~f:(fun x -> let (new_t_list,ron_info) = operate t_list x in
                                                   let can_ron = check_ron new_t_list in
                                                   ))
                              operate_list*)
let operate (t_list:Tile.t list ) (x:Tile.t list)=
  let new_t_list = Tile.delete_list_of_tile t_list x in
  (new_t_list,x)
  (*let fst_ele = List.nth_exn x 0 in 
  let snd_ele = List.nth_exn x 1 in
  if (Tile.check_equal fst_ele snd_ele) then (if (List.length x) = 3 then 
  *)
let add_info (x: Tile.t list) (info_list: ron_info list) = 
  print_endline("akagi");
  let fst_ele = List.nth_exn x 0 in
  let snd_ele = List.nth_exn x 1 in
  if (Tile.check_equal fst_ele snd_ele) then (if (List.length x) = 3 then 
                                                List.iter info_list ~f:(fun y-> y.pong_list <- x::y.pong_list)
                                              else List.iter info_list ~f:(fun y -> y.kan_list <- x::y.kan_list)
                                             )
  else List.iter info_list ~f:(fun y -> y.shun_list <- x::y.shun_list)


let rec check_ron (t_list:Tile.t list) = 
  let ron_info = ref [] in 
  let ron_ref = ref false in
  List.iter t_list ~f:(fun x-> let operate_list = Tile.find_all_ke_shun t_list x in
                         if (List.is_empty operate_list) then 
                           let () = Tile.print_tile_list t_list in
                           let () = print_endline("fuk") in
                           (if one_pair_left ( alternate_rep t_list) then (ron_ref := true;
                            print_endline("ron")))
                         else 
                           List.iter operate_list ~f:(fun y -> 
                               (*print_endline( string_of_int (List.length y));
                               Tile.print_tile_list y;*)
                               let (new_t_list,info_ele) = operate t_list y in
                                                   (*print_endline( string_of_int (List.length new_t_list));*)
                                                   let (ron_tf,info_list) = check_ron new_t_list in 
                                                   print_endline (Bool.to_string ron_tf);
                                                   if (Bool.equal ron_tf true) 
                                                   then ( add_info info_ele info_list; 
                                                     (*let added_info = add_info info_ele info_list in 
                                                          ron_info := added_info @ (!ron_info);*)
                                                     (*ron_info := (info_ele::info_list)::(!ron_info);*)
                                                         ron_ref := true)
                           ));
  (!ron_ref,!ron_info)



(*let thirteen_orphan_helper (t_list: Tile.t list) (unfound_orphan: Tile.t list)= 


let thirteen_orphan ( t_list:Tile.t list) = 
*)

(* 
let has_qi_tui_helper( acc_rep: (Tile.t * int) array) = 

let has_qi_tui (acc_rep: (Tile.t * int) array) = 
  match (Array.length acc_rep) with 
  | 0 -> true
  | 
*)
(** The type [yaku] represents the types of yaku player can achieve *)
type yaku = Riichi | Tanyao | Hunyise | Dragontriplet | Qinyise | Chankan
          | None


let string_of_yaku yaku = 
  match yaku with
  | Riichi -> "Riichi"
  | Tanyao -> "Tanyao"
  | Hunyise -> "Hunyise"
  | Qinyise -> "Qinyise"
  | Chankan -> "ChunKan"
  | Dragontriplet -> "Dragon Triplet"
  | None -> ""
  
  (*
let check_yaku (p:player) (t:Tile.t) (acc_rep: (Tile.t * int) array) =   
  comb.riichied 
  || is_tanyao new_l 
  || is_hunyise new_l 
  || is_dragons acc_rep
  || is_seven_pairs comb.info
*)
let set_riichi (p:player) = 
  match p.richii with
  | true -> failwith("impossible")
  | false -> p.richii <- true





