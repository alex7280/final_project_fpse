open Core;;


(*five kind of tiles*)
type kind = Man | Pin | Suo | Dragon | Wind [@@deriving equal]

type tile = {
    id : int;
    kind : kind;
    number : int;
    (* indicates if this tile is chii, pong or kang*)
    mutable fulu : bool;
    (*indicate if this tile is played*)
    mutable discarded : bool;
    mutable dora : bool;
}

let class_constructor (i:int) (k:kind) (num:int) (f:bool) (d:bool) (dora:bool)= {
    id = i;
    kind = k;
    number = num;
    fulu = f;
    discarded = d;
    dora = dora
}

let construct_fake (k:kind) (num:int) = {
    id = 0;
    kind = k;
    number = num;
    fulu = false;
    discarded = false;
    dora = false;
  }

type t = tile

let get_kind (t:tile) = 
  t.kind


let get_fulu (t:tile) = 
  t.fulu

let get_number (t:tile) = 
  t.number

let get_discarded (t:tile) = 
  t.discarded



let change_fulu (to_change:tile) = 
  to_change.fulu <- true

let set_nonfulu (to_change:tile) = 
  to_change.fulu <- false

let change_discarded (to_change:tile) = 
  to_change.discarded <- true

let kind_equal (k1: kind) (k2:kind) = 
  equal_kind k1 k2

let generate_game_tiles _ = 
  let id_gen = ref 1 in 
  let generated_man_tiles = List.fold [1;2;3;4;5;6;7;8;9] ~init:[] 
                              ~f:(fun acc x -> let t = class_constructor !id_gen Man x false false false in 
                                               let t1 = class_constructor (!id_gen+1) Man x false false false in
                                               let t2 = class_constructor (!id_gen+2) Man x false false false in
                                               let t3 = class_constructor (!id_gen+3) Man x false false false in
                                               id_gen := !id_gen + 4;
                                          acc @[t;t1;t2;t3]) in 
  let generated_pin_tiles = List.fold [1;2;3;4;5;6;7;8;9] ~init:generated_man_tiles 
                          ~f:(fun acc x -> let t = class_constructor !id_gen Pin x false false false in
                                               let t1 = class_constructor (!id_gen+1) Pin x false false false in
                                               let t2 = class_constructor (!id_gen+2) Pin x false false false in
                                               let t3 = class_constructor (!id_gen+3) Pin x false false false in
                                               id_gen := !id_gen + 4;
                                          acc @[t;t1;t2;t3]) in 
  let generated_suo_tiles = List.fold [1;2;3;4;5;6;7;8;9] ~init:generated_pin_tiles
                          ~f:(fun acc x -> let t = class_constructor !id_gen Suo x false false false in
                                               let t1 = class_constructor (!id_gen+1) Suo x false false false in
                                               let t2 = class_constructor (!id_gen+2) Suo x false false false in
                                               let t3 = class_constructor (!id_gen+3) Suo x false false false in
                                               id_gen := !id_gen + 4;
                                          acc @[t;t1;t2;t3]) in
  let generated_wind_tiles = List.fold [1;2;3;4] ~init:generated_suo_tiles 
                           ~f:(fun acc x-> let t = class_constructor !id_gen Wind x false false false in
                                               let t1 = class_constructor (!id_gen+1) Wind x false false false in
                                               let t2 = class_constructor (!id_gen+2) Wind x false false false in
                                               let t3 = class_constructor (!id_gen+3) Wind x false false false in
                                               id_gen := !id_gen + 4;
                                          acc @[t;t1;t2;t3]) in
  List.fold [1;2;3] ~init:generated_wind_tiles
    ~f:(fun acc x-> let t = class_constructor !id_gen Dragon x false false false in
                    let t1 = class_constructor (!id_gen+1) Dragon x false false false in
                    let t2 = class_constructor (!id_gen+2) Dragon x false false false in
                    let t3 = class_constructor (!id_gen+3) Dragon x false false false in
                    id_gen := !id_gen + 4;
                    acc @[t;t1;t2;t3])

let check_pin_suo_man (k_list : kind list) = 
  let man_exist = ref false in 
  let pin_exist = ref false in
  let suo_exist = ref false in 
  List.iter k_list ~f:(fun x -> if equal_kind x Man then man_exist := true
                                else if equal_kind x Pin then pin_exist := true
                                else if equal_kind x Suo then suo_exist := true);
  !man_exist && !pin_exist && !suo_exist

let check_laotou (to_check:tile) = 
  match to_check.kind with 
  | Dragon -> false
  | Wind -> false
  | _ -> (match to_check.number with 
          | 1 -> true
          | 9 -> true
          | _ -> false
         )
let is_wind_dragon (to_check:tile) = 
  match to_check.kind with 
  | Dragon -> true
  | Wind -> true
  | _ -> false 

let is_green_tile (to_check: tile) = 
  match to_check.kind with 
  | Dragon -> if to_check.number = 3 then true
              else false
  | Suo -> if (to_check.number = 2) || (to_check.number = 3) || (to_check.number = 4 ) || (to_check.number = 6) 
            || (to_check.number = 8) then true 
           else false
  | _ -> false 


(*check the tile does not belong to wind/dragon/ has number 1 or 9*)
let check_non_yao (to_check:tile) = 
  match to_check.kind with
  | Man -> if not (to_check.number = 1) && not (to_check.number = 9) then true
           else false
  | Pin -> if not (to_check.number = 1) && not (to_check.number = 9) then true
           else false
  | Suo -> if not (to_check.number = 1) && not (to_check.number = 9) then true
           else false
  | _ -> false

(*check  a list contains lao tou tile*)
let contain_lao_tou_list (to_check:tile list ) = 
  List.fold to_check ~init:false ~f:(fun acc x -> check_laotou x || acc)

let contain_non_yao_list (to_check:tile list) = 
  List.fold to_check ~init:false ~f:(fun acc x -> check_non_yao x || acc)


 
(*string rep of tiles*)
let tile_to_string (to_print:tile):string = 
  match to_print.kind with
    | Man -> "Man"^(string_of_int to_print.number)
    | Pin -> "Pin"^(string_of_int to_print.number)
    | Suo -> "Suo"^(string_of_int to_print.number)
    | Dragon -> (match to_print.number with
                   | 1 -> "Bai / Dragon 1"
                   | 2 -> "Chong / Dragon 2"
                   | 3 -> "Fa / Dragon 3"
                   | _ -> "impossible"
                )
    | Wind -> (match to_print.number with 
                 | 1 -> "Tong / Wind 1"
                 | 2 -> "Nan / Wind 2"
                 | 3 -> "Xia / Wind 3"
                 | 4 -> "Pei / Wind 4"
                 | _ -> "impossible"
              )

let tile_list_to_string (to_print: tile list): string = 
  List.fold to_print ~init:"" ~f:(fun acc x -> let string_rep = tile_to_string x in 
                                  String.concat [acc;" ";string_rep])

let tile_list_list_to_string ( to_print: tile list list) : string = 
  List.fold to_print ~init:"" ~f:(fun acc x -> let string_rep = tile_list_to_string x in
                                  String.concat [acc;" ";string_rep])



let print_tile (to_print:tile) =
  match to_print.kind with
    | Man -> print_string("Man"^(string_of_int to_print.number) ^ "   ")
    | Pin -> print_string("Pin"^(string_of_int to_print.number) ^ "   ")
    | Suo -> print_string("Suo"^(string_of_int to_print.number) ^ "   ")
    | Dragon -> (match to_print.number with
                   | 1 -> print_string("Bai / Dragon 1"^ "   ")
                   | 2 -> print_string("Chong / Dragon 2"^ "   ")
                   | 3 -> print_string("Fa / Dragon 3"^ "   ")
                   | _ -> print_string("impossible")
                )
    | Wind -> (match to_print.number with
                 | 1 -> print_string("Tong / Wind 1"^ "   ")
                 | 2 -> print_string("Nan / Wind 2"^ "   ")
                 | 3 -> print_string("Xia / Wind 3"^ "   ")
                 | 4 -> print_string("Pei / Wind 4"^ "   ")
                 | _ -> print_string("impossible")
              )

(*print out a list of tiles*)
let print_tile_list (to_print: tile list) = 
  List.iter ~f:(fun x -> 
                                      print_tile x) to_print


let print_tile_list_list ( to_print: tile list list ) : unit=
  let num = ref 1 in
  List.iter to_print ~f:(fun x -> print_string (string_of_int !num^ "\n");
                                  print_tile_list x;
                                  print_string("\n\n");
                                  num := !num + 1) 

(*check if three tiles all have same kind*)
let same_kind (first:tile) (second:tile) (third:tile):bool =
  if (equal_kind first.kind second.kind) && (equal_kind first.kind third.kind) then true
  else false

(*check if a list of list of Tiles have the same type .*)
let same_kind_across_shun(shun_list: tile list list) = 
  match shun_list with 
  | [] -> true
  | x::xs -> if List.length xs > 0 then 
             (List.fold xs ~init:true ~f:(fun acc y -> if equal_kind (List.nth_exn y 0).kind (List.nth_exn x 0).kind
                                                       then true && acc
                                                       else false ))
             else true
(*check if a list of tiles have same kind*)
let same_kind_list (t_list:tile list) : bool = 
  if List.length t_list = 0 then true
  else (
  let cur = List.nth_exn t_list 0 in 
  let cur_kind = cur.kind in 
  List.fold ~init:(true) ~f:(fun acc x -> if (equal_kind cur_kind x.kind) then (acc && true)
                                        else false
    ) t_list
  )
(*check if the three tiles from a shun, such as 1 2 3 Man*)
let check_shun (first:tile) (second:tile) (third:tile):bool = 
  let adj small big = 
    if small.number = big.number -1 then true
    else false in
  (*if (same_kind first second third)*)  (adj first second) && (adj second third)

(*check if two tiles are equal in kind and number*)
let check_equal (first:tile) (second:tile):bool = 
  if ((equal_kind first.kind second.kind) && (first.number = second.number)) then true
  else false


(*note this function requires all three tiles to be same kind*)
let sort_same_kind (l:tile list):(tile list) = 
  List.sort l ~compare:(fun x y -> if x.number < y.number then -1
                        else if x.number = y.number then 0
                        else 1) 

(*find the specificied kind of tiles in the hand*)
let find_kind (l:tile list) (t:kind):(tile list) = 
  List.filter ~f:(fun x -> if (equal_kind x.kind t) then true
                        else false) l 

(*check if a list of tiles are equal. Facilitate yiipeikou*)
let check_equal_list (first:tile list) (second:tile list): bool =
  if (not (same_kind_list first) || not (same_kind_list second)) then false
  else (let sorted_first = sort_same_kind first in
        let sorted_second = sort_same_kind second in
        if not ((List.length sorted_first) = (List.length sorted_second)) then false
        else (List.foldi sorted_first ~init:(true)
                ~f:(fun i acc x -> if check_equal x (List.nth_exn sorted_second i) then true && acc
                else false)))

(*find a specific kind of tiles of hand. Then sort it from smallest to largest*)
let sort_hand_onekind (l:tile list) (t:kind):(tile list) = 
  let kind_list = find_kind l t in 
  sort_same_kind kind_list

let sort_hand (l:tile list):(tile list) = 
  let sorted_man_list = sort_hand_onekind l Man in 
  let sorted_pin_list = sort_hand_onekind l Pin in 
  let sorted_suo_list = sort_hand_onekind l Suo in
  let sorted_dragon_list = sort_hand_onekind l Dragon in 
  let sorted_wind_list = sort_hand_onekind l Wind in 
  List.concat [sorted_man_list;sorted_pin_list;sorted_suo_list;sorted_dragon_list;sorted_wind_list]

let get_all_non_fulu (t_list: tile list) =
  let not_sorted = List.fold t_list ~init:[] ~f:(fun acc x-> if not(x.fulu) then acc@[x] 
                                     else acc) in
  sort_hand not_sorted

let get_all_fulu (t_list: tile list) = 
  let not_sorted = List.fold t_list ~init:[] ~f:(fun acc x -> if x.fulu then acc@[x]
                                                       else acc) in 
  sort_hand not_sorted


(*default sort the hand once we add a tile to hand, hand is always sorted to the player*)
let sort_hand_add_tile (l:tile list) (to_set:tile) :tile list = 
  let added_list = to_set::l in 
  sort_hand added_list 

let have_tile (l:tile list) (to_check_kind:kind) (to_check_num:int) : bool = 
  List.fold l ~init:false ~f:(fun acc x -> if equal_kind x.kind to_check_kind &&
                                           x.number = to_check_num then true 
                                           else acc || false )

(*given a tile, delete all tiles with the same property*)
let delete_all_tile (l:tile list) (to_del:tile): tile list = 
  List.filter ~f:(fun x-> if (x.number = to_del.number) && (equal_kind x.kind to_del.kind) then false
                       else true) l

(*delete only one tile of the same one inputted*)
let rec delete_one_tile (l:tile list) (to_del:tile) (acc:tile list): tile list = 
  match l with
    | [] -> sort_hand acc
    | ele::ele_list -> if (equal_kind ele.kind to_del.kind) && (ele.number = to_del.number) && not (ele.fulu)
                       then 
                         sort_hand (acc@ele_list)
                       else delete_one_tile ele_list to_del acc@[ele]



let rec delete_list_of_tile (l:tile list) (to_del:tile list): tile list = 
  match to_del with
  | [] -> l
  | x::xs -> let new_l = delete_one_tile l x [] in 
             delete_list_of_tile new_l xs
(*let sort_ (first: tile) (second:tile) (third:tile): (tile,tile,tile) = 
  let a = first.number in
  let b = second.number in 
  let c = third.number in 
  if ((a >= b) && (b>=c)) then (a,b,c)
  else if ((a>= b) && (b<=c)) then (if a>=c then (a,c,b) else (c,a,b))
  else if ((b>=a) && (a>=c)) then (b,a,c)
  else if *)
(* for shun, there are three possibilities. Either the input tile is the tile with lowest number of shun, such as it is 1 in the 1-2-3 shun, or it is in the middle of shun such as 2 in the 1-2-3 shun,or it is at last of shun. Deal with these cases seperately and append them all together.*)
let get_low_shun (l:tile list) (to_set:tile) (first_diff:int) (sec_diff:int) : tile list option = 
  let same_kind = find_kind l to_set.kind in
  let one_higher_list = List.filter ~f:(fun x -> 
                                              if (x.number = (to_set.number + first_diff)) then true
                                              else false) same_kind in
  let two_higher_list = List.filter ~f:(fun x -> if (x.number = (to_set.number + sec_diff)) then true
                                              else false) same_kind in
  if (List.length one_higher_list >= 1) && (List.length two_higher_list >=1) then 
    Some [to_set;List.nth_exn one_higher_list 0;List.nth_exn two_higher_list 0]
  else None

let get_shun_from_option( opt_list : tile list option) : tile list = 
  match opt_list with 
  | None -> []
  | Some l -> l

let get_all_shun (l:tile list) (to_set:tile): tile list list option= 
  let get_when_is_low = get_shun_from_option( get_low_shun l to_set 1 2) in
  let get_when_is_mid = get_shun_from_option( get_low_shun l to_set (-1) 1 )in
  let get_when_is_high = get_shun_from_option( get_low_shun l to_set (-2) (-1) )in
  Some [get_when_is_low;get_when_is_mid;get_when_is_high]
(*  match get_when_is_low with
  | None -> (match get_when_is_mid with
             | None -> (match get_when_is_high with
                        | None -> None
                        | Some l3 -> Some [get_when_is_low;get_when_is_mid;get_when_is_high]
                       )
             | Some l2 -> Some [get_when_is_low;get_when_is_mid;get_when_is_high]
            )
  | Some l -> Some [get_when_is_low;get_when_is_mid;get_when_is_high]
*)


  (*if (List.length get_when_is_low >= 1) || (List.length get_when_is_mid >=1) || (List.length get_when_is_high)>= 1 then
    Some [get_when_is_low;get_when_is_mid;get_when_is_high]
  else None*)
  
let get_pong (l:tile list) (to_set:tile): tile list option= 
  let same_tile_list = List.filter ~f:(fun x -> if (equal_kind x.kind to_set.kind) && (x.number = to_set.number) then true
                                             else false) l in
  if List.length same_tile_list >= 3 then Some [to_set;to_set;to_set]
  else None

let get_kan (l:tile list) (to_set:tile): tile list option=
  let same_tile_list = List.filter ~f:(fun x -> if (equal_kind x.kind to_set.kind) && (x.number = to_set.number) then true
                                             else false) l in
  if List.length same_tile_list >= 4 then Some [to_set;to_set;to_set;to_set]
  else None

let check_chii (l:tile list) (to_set:tile): bool =
  match get_all_shun l to_set with
  | Some _ -> true
  | None -> false
let check_tui (l:tile list) (to_set:tile):bool = 
  let one_higher_list = List.filter ~f:(fun x -> if (x.number = to_set.number) && (equal_kind x.kind to_set.kind) then true
                                              else false) l in
  if List.length one_higher_list >= 1 then true
  else false

let check_pong (l:tile list) (to_set:tile): bool = 
  (*let same_tile_list = List.filter (fun x -> if (x.kind = to_set.kind) && (x.number = to_set.number) then true
                                             else false) l in
  if List.length same_tile_list >= 3 then true
  else false*)
  match get_pong l to_set with
  | Some _ -> true
  | None -> false


(*check quad tuples*)
let check_kan ( l: tile list) (to_set:tile):bool = 
  let same_tile_list = List.filter ~f:(fun x -> if (equal_kind x.kind to_set.kind) && (x.number = to_set.number) then true
                                             else false) l in
  if List.length same_tile_list >= 4 then true
  else false
(*let find_ke (l:tile list) (to_set:tile):bool = 
  let exist_ke = ref false in
  let 
  let exist_tui = ref false in 
  let () = List.iter l ~f:( fun x -> (match !exist_tui with
                                      | false -> if check_equal x to_set then exist_tui := true)
                                      | true -> if check_equal 
*)
let get_list_shun_from_option (opt_list:tile list list option) =
  match opt_list with
  | None -> []
  | Some l -> l

let remove_empty_lists (l:tile list list) = 
  List.fold l ~init:[] ~f:(fun acc x -> if not (List.is_empty x) then x::acc
    else acc)


let rec compare_tile_list (first:tile list) (second:tile list) = 
  if not(List.length first = List.length second) then failwith("cant compare")
  else (match List.length first with 
        | 0 -> 0 
        | _ -> let fst_first = List.nth_exn first 0 in 
               let fst_second = List.nth_exn second 0 in 
               if fst_first.number > fst_second.number then 1
               else if fst_first.number < fst_second.number then -1 
               else ( compare_tile_list
                        (List.sub first ~pos:1 ~len:(List.length first -1)) 
                        (List.sub second ~pos:1 ~len:(List.length second -1))
                    ))

let sort_ke_shun (l:tile list list) = 
  List.map l ~f:(fun l -> sort_same_kind l)

let find_all_ke_shun(l:tile list) (to_set:tile):(tile list list) = 
  match to_set.kind with
    | Dragon -> let have_empty = [get_shun_from_option (get_pong l to_set)
                                 ;get_shun_from_option (get_kan l to_set)] in
                let no_empty = remove_empty_lists have_empty in
                sort_ke_shun no_empty
    | Wind -> let have_empty = [get_shun_from_option(get_pong l to_set)
                               ; get_shun_from_option (get_kan l to_set)] in
              let no_empty = remove_empty_lists have_empty in
              sort_ke_shun no_empty
    | _ -> let have_empty =  (get_shun_from_option (get_pong l to_set))
             ::get_list_shun_from_option (get_all_shun l to_set) in
           
             let have_empty2 = (get_shun_from_option (get_kan l to_set))
                                 ::have_empty in
             let no_empty = remove_empty_lists have_empty2 in
             sort_ke_shun no_empty
    (*| Pin -> let have_empty = (get_shun_from_option (get_pong l to_set))
             ::get_list_shun_from_option (get_all_shun l to_set) in
             let have_empty2 = (get_shun_from_option (get_kan
             remove_empty_lists have_empty
    | Suo -> let have_empty = (get_shun_from_option (get_pong l to_set))
             ::get_list_shun_from_option (get_all_shun l to_set) in
             remove_empty_lists have_empty*)

let generate_man_pin_suo (k:kind):tile list =
  let pai_list = ref [] in
  let one_nine_list = [1;2;3;4;5;6;7;8;9] in
  List.iter one_nine_list ~f:(fun x -> let constructed = construct_fake k x in
                                       pai_list := constructed::(!pai_list););
  !pai_list

let generate_wind : tile list = 
  let tong = construct_fake Wind 1 in
  let nan = construct_fake Wind 2 in
  let xia = construct_fake Wind 3 in
  let pei = construct_fake Wind 4 in 
  [tong;nan;xia;pei]

let generate_dragon : tile list = 
  let bai = construct_fake Dragon 1 in 
  let chong = construct_fake Dragon 2 in 
  let fa = construct_fake Dragon 3 in
  [bai;chong;fa]

let generate_all_yao : tile list = 
  let man_1 = construct_fake Man 1 in
  let man_9 = construct_fake Man 9 in 
  let pin_1 = construct_fake Pin 1 in 
  let pin_9 = construct_fake Pin 9 in 
  let suo_1 = construct_fake Suo 1 in 
  let suo_9 = construct_fake Suo 9 in
  let wind_list = generate_wind in
  let dragon_list = generate_dragon in 
  ([man_1;man_9;pin_1;pin_9;suo_1;suo_9] @ wind_list) @dragon_list 

let generate_all_pai : tile list = 
  let man_list = generate_man_pin_suo Man in 
  let pin_list = generate_man_pin_suo Pin in 
  let suo_list = generate_man_pin_suo Suo in 
  let wind_list = generate_wind in
  let dragon_list = generate_dragon in 
  man_list @ pin_list @suo_list @ wind_list @ dragon_list

