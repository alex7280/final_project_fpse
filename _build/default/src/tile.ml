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
}

let class_constructor (i: int)(k:kind) (num:int) (f:bool) (d:bool) = {
    id = i;
    kind = k;
    number = num;
    fulu = f;
    discarded = d;
}
type t = tile

let get_kind (t:tile) = 
  t.kind

let get_id (t:tile) = 
  t.id

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

(*string rep of tiles*)
let tile_to_string (to_print:tile):string = 
  match to_print.kind with
    | Man -> "Man"^(string_of_int to_print.number)
    | Pin -> "Pin"^(string_of_int to_print.number)
    | Suo -> "Suo"^(string_of_int to_print.number)
    | Dragon -> (match to_print.number with
                   | 1 -> "Bai"
                   | 2 -> "Chong"
                   | 3 -> "Fa"
                   | _ -> "impossible"
                )
    | Wind -> (match to_print.number with 
                 | 1 -> "Tong"
                 | 2 -> "Nan"
                 | 3 -> "Xia"
                 | 4 -> "Pei"
                 | _ -> "impossible"
              )

let print_tile (to_print:tile) =
  match to_print.kind with
    | Man -> print_endline("Man"^(string_of_int to_print.number))
    | Pin -> print_endline("Pin"^(string_of_int to_print.number))
    | Suo -> print_endline("Suo"^(string_of_int to_print.number))
    | Dragon -> (match to_print.number with
                   | 1 -> print_endline("Bai")
                   | 2 -> print_endline("Chong")
                   | 3 -> print_endline("Fa")
                   | _ -> print_endline("impossible")
                )
    | Wind -> (match to_print.number with
                 | 1 -> print_endline("Tong")
                 | 2 -> print_endline("Nan")
                 | 3 -> print_endline("Xia")
                 | 4 -> print_endline("Pei")
                 | _ -> print_endline("impossible")
              )

(*print out a list of tiles*)
let print_tile_list (to_print: tile list) = 
  List.iter ~f:(fun x -> print_tile x) to_print
(*check if three tiles all have same kind*)
let same_kind (first:tile) (second:tile) (third:tile):bool =
  if (equal_kind first.kind second.kind) && (equal_kind first.kind third.kind) then true
  else false

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

(*default sort the hand once we add a tile to hand, hand is always sorted to the player*)
let sort_hand_add_tile (l:tile list) (to_set:tile):tile list = 
  let added_list = to_set::l in 
  sort_hand added_list 

(*given a tile, delete all tiles with the same property*)
let delete_all_tile (l:tile list) (to_del:tile): tile list = 
  List.filter ~f:(fun x-> if (x.number = to_del.number) && (equal_kind x.kind to_del.kind) then false
                       else true) l

(*delete only one tile of the same one inputted*)
let rec delete_one_tile (l:tile list) (to_del:tile) (acc:tile list): tile list = 
  match l with
    | [] -> acc
    | ele::ele_list -> if (equal_kind ele.kind to_del.kind) && (ele.number = to_del.number) then acc@ele_list
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


let find_all_ke_shun(l:tile list) (to_set:tile):(tile list list) = 
  match to_set.kind with
    | Dragon -> let have_empty = [get_shun_from_option (get_pong l to_set)] in
                remove_empty_lists have_empty
    | Wind -> let have_empty = [get_shun_from_option(get_pong l to_set)] in
              remove_empty_lists have_empty
    | Man -> let have_empty =  (get_shun_from_option (get_pong l to_set))
             ::get_list_shun_from_option (get_all_shun l to_set) in
             remove_empty_lists have_empty
    | Pin -> let have_empty = (get_shun_from_option (get_pong l to_set))
             ::get_list_shun_from_option (get_all_shun l to_set) in
             remove_empty_lists have_empty
    | Suo -> let have_empty = (get_shun_from_option (get_pong l to_set))
             ::get_list_shun_from_option (get_all_shun l to_set) in
             remove_empty_lists have_empty
