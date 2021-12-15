open Core;;

type ron_info = {
    mutable shun_list : Tile.t list list;
    mutable pong_list : Tile.t list list;
    mutable kan_list : Tile.t list list;
    mutable pair_ron: Tile.t list;
}

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
    mutable r_info : ron_info;
}

type t = player

let get_all_fulu (p:player) = List.filter ~f:(fun x -> Tile.get_fulu x) p.total_tiles

let get_all_non_fulu (p:player) = List.filter ~f:(fun x -> not (Tile.get_fulu x)) p.total_tiles

let turn_draw_tile (p:player) (to_add:Tile.t) = 
  p.total_tiles <- to_add::p.total_tiles

let play_tile (p:player) (to_play:Tile.t) = 
  let hand = p.total_tiles in 
  let () = Tile.change_discarded to_play in
  p.total_tiles <- Tile.sort_hand (Tile.delete_one_tile hand to_play []);
  p.discarded <- Tile.sort_hand_add_tile (p.discarded) to_play
let empty_ron_info_const () = {
  shun_list = [];
  pong_list = [];
  kan_list = [];
  pair_ron = [];
}
let class_constructor (i:int) (r:bool) (m:bool) (t_list: Tile.t list) (dis: Tile.t list) = {
  p_id = i;
  richii = r;
  mensei = m;
  total_tiles = t_list;
  discarded = dis;
  r_info = empty_ron_info_const ();
  }
 
let rec set_fulu_helper (remain: Tile.t list) (to_set_fulu: Tile.t list) (passed: Tile.t list)= 
  match to_set_fulu with 
  | [] -> Tile.sort_hand (passed @ remain)
  | x::xs ->  
         if Tile.check_equal x (List.nth_exn remain 0)
         then (Tile.change_fulu x;
               if List.length remain >= 1 then 
                 (set_fulu_helper (List.sub remain ~pos:1 ~len:(List.length remain -1)) 
                              xs
                              passed@[x])
               else (set_fulu_helper [] xs
                              passed@[x])
         )
         else set_fulu_helper (List.sub remain ~pos:1 ~len:(List.length remain -1))
                               to_set_fulu
                               passed@[List.nth_exn remain 0]
         

let set_fulu (p:Tile.t list) (to_set_fulu: Tile.t list) = 
  set_fulu_helper (Tile.sort_hand p) (Tile.sort_hand to_set_fulu) []

let update_ron_info (p:player) (to_put: Tile.t list) = 
  match List.length to_put with 
  | 3 -> if Tile.check_equal (List.nth_exn to_put 0) (List.nth_exn to_put 1) then 
         p.r_info.pong_list <- to_put:: p.r_info.pong_list 
         else p.r_info.shun_list <- to_put :: p.r_info.shun_list
  | 4 -> p.r_info.kan_list <- to_put :: p.r_info.kan_list 
  | _ -> failwith("chen mo nian dai")


let user_chii_pong_kan (t:Tile.t) (p:player) (which_triplet:int) = 
  let hand = p.total_tiles in 
  let all_ke_shun = Tile.find_all_ke_shun hand t in
  match all_ke_shun with
  | [] -> ()
  | _ -> let triplet = List.nth all_ke_shun which_triplet in 
         (match triplet with
         | None -> ()
         | Some x -> p.total_tiles <- (set_fulu (p.total_tiles) x) ;
                     p.mensei <- false;
                     ()
         )

(*this type helps to decide the information needed for ron. Also facilitate deleting tuples, for example*)


let ron_info_constructor (shun_list :Tile.t list list) (pong_list : Tile.t list list) (kan_list : Tile.t list list) (pair_ron : Tile.t list) = {
  shun_list = shun_list;
  pong_list = pong_list;
  kan_list = kan_list;
  pair_ron = pair_ron
}

let empty_ron_info_const () = {
  shun_list = [];
  pong_list = [];
  kan_list = [];
  pair_ron = [];
}
let print_ron_info (r:ron_info) = 
  let shun = r.shun_list in 
  List.iter shun ~f:(fun x -> Tile.print_tile_list x;
    print_endline("\n"));
  let pong = r.pong_list in 
  List.iter pong ~f:(fun y -> Tile.print_tile_list y;
    print_endline("\n"));
  let kan = r.kan_list in 
  List.iter kan ~f:(fun z -> Tile.print_tile_list z;
    print_endline("\n"));
  Tile.print_tile_list r.pair_ron


let print_ron_info_list (r_list: ron_info list) = 
  List.iter r_list ~f:(fun x -> print_ron_info x)

(*this removes the duplicate ron infos. For example, if a ron info have 1 2 3 man as its shun_list and another has 2 3 1 man as its shun_list (and pong/kan/pair are same), we consider them as same ron info, and remove one of them.*) 

let check_shun_list (first: Tile.t list list) (second: Tile.t list list) = 
  let sorted_fst_ele = List.map first ~f:(fun x -> Tile.sort_same_kind x) in 
  let sorted_snd_ele = List.map second ~f:(fun y -> Tile.sort_same_kind y) in 
  let sorted_fst_whole = List.sort sorted_fst_ele ~compare:(fun x y -> Tile.compare_tile_list x y) in
  let sorted_snd_whole = List.sort sorted_snd_ele ~compare:(fun x y -> Tile.compare_tile_list x y) in
  if not (List.length sorted_fst_whole = List.length sorted_snd_whole) then false
  else List.foldi sorted_fst_whole ~init:true ~f:(fun i acc x-> let snd_ele = List.nth_exn sorted_snd_whole i in 
                                                                if (Tile.compare_tile_list snd_ele x = 0) then true && acc
                                                                else false )

let check_pair (first:Tile.t list) (second: Tile.t list) = 
  if (not (List.length first = List.length second) || not (List.length first = 2)) then false 
  else let first_first = List.nth_exn first 0 in 
       let second_first = List.nth_exn second 0 in 
       Tile.check_equal first_first second_first


let check_same_ron_info ( first: ron_info) (second:ron_info) = 
  let shun_list_same = check_shun_list first.shun_list second.shun_list in 
  let pong_list_same = check_shun_list first.pong_list second.pong_list in 
  let pair_ron_same = check_pair first.pair_ron second.pair_ron in
  shun_list_same && pong_list_same && pair_ron_same

let sort_shun_list (first:Tile.t list list) = 
  let sorted_fst_ele = List.map first ~f:(fun x -> Tile.sort_same_kind x ) in 
  List.sort sorted_fst_ele ~compare:(fun x y -> Tile.compare_tile_list x y) 

let sort_ron_info (r_list: ron_info list) = 
  List.map r_list ~f:(fun x-> let sorted_shun_list = sort_shun_list x.shun_list in
                              let sorted_pong_list = sort_shun_list x.pong_list in 
                              ron_info_constructor sorted_shun_list sorted_pong_list x.kan_list x.pair_ron
                              )
let rec simplify_ron_info_index (r_list : ron_info list) (index :int ) = 
 match (index = (List.length r_list -1 )) with
 | true -> r_list 
 | false -> (match r_list with
             | [] -> r_list 
             | _ -> let x = List.nth_exn r_list index in 
                    let xs = List.foldi r_list ~init:[] ~f:(fun i acc y -> if (i = index) then acc 
                                                                    else acc@[y]) in 

                     let is_dup = List.fold xs ~init:false ~f:( fun acc y -> if check_same_ron_info y x then true
                                                                     else false || acc) in
             if is_dup then simplify_ron_info_index xs 0
             else (match List.length r_list with
                   | 1 -> r_list
                   | _ -> simplify_ron_info_index r_list (index+ 1) 
                  )
            )



let simplify_ron_info (r_list : ron_info list) = 
  match r_list with
  | [] -> r_list 
  | x::xs -> let is_dup = List.fold xs ~init:false ~f:( fun acc y -> if check_same_ron_info y x then true 
                                                                     else false || acc) in 
             if is_dup then simplify_ron_info_index xs 0 
             else (match List.length r_list with
                   | 1 -> r_list
                   | _ -> simplify_ron_info_index r_list 1 
                  )

let concat_two_ron_info (first: ron_info) (second: ron_info) = 
  let concat_shun = List.concat [first.shun_list;second.shun_list] in 
  let concat_pong = List.concat [first.pong_list;second.pong_list] in 
  let concat_kan = List.concat [first.kan_list;second.kan_list] in 
  let concat_pair = List.concat [first.pair_ron;second.pair_ron] in 
  ron_info_constructor concat_shun concat_pong concat_kan concat_pair


(*let add_tuple (to_add:Tile.t) (new_rep: (Tile.t * int) array)= 
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

let rec print_alt_display (acc_rep: (Tile.t * int) array) = 
  match Array.length acc_rep with
  | 0 -> ()
  | _ -> let (tile,count) = Array.get acc_rep 0 in 
         Tile.print_tile tile;
         print_endline (string_of_int count);
         print_alt_display (Array.sub acc_rep ~pos:1 ~len:(Array.length acc_rep))
*)
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

let zi_yisu_check (t_list:Tile.t list) = 
  let man_list = Tile.find_kind t_list Man in
  let pin_list = Tile.find_kind t_list Pin in
  let suo_list = Tile.find_kind t_list Suo in
  if (List.length man_list = 0 ) && (List.length pin_list = 0) && (List.length suo_list = 0) then true
  else false

let has_dragon_triplet (acc_rep: Tile.t list) = 
  (*let empty_array = Array.make 0 (List.nth t_list 0, 0) in *)
  (*let acc_rep = alternate_rep t_list empty_array in*)
  let bai_count = ref 0 in 
  let chong_count = ref 0 in 
  let fa_count = ref 0 in 
  List.iter acc_rep ~f:(fun x -> if Tile.check_equal x (Tile.construct_fake Dragon 1) then bai_count := !bai_count+1
                                 else if Tile.check_equal x (Tile.construct_fake Dragon 2 ) 
                                 then chong_count := !chong_count + 1
                                 else if Tile.check_equal x (Tile.construct_fake Dragon 3) 
                                 then fa_count := !fa_count + 1
                                 else ());
  (!bai_count >= 3)|| (!chong_count >= 3) || (!fa_count >= 3)
let one_pair_left(acc_rep:Tile.t list) = 
  if List.length acc_rep <> 2 then false
  else (if Tile.check_equal (List.nth_exn acc_rep 0)
        (List.nth_exn acc_rep 1) then true
  else false
       )
let operate (t_list:Tile.t list ) (x:Tile.t list)=
  let new_t_list = Tile.delete_list_of_tile t_list x in
  (new_t_list,x)

let add_info (x: Tile.t list) (info_list: ron_info list) = 
  let fst_ele = List.nth_exn x 0 in
  let snd_ele = List.nth_exn x 1 in
  if (Tile.check_equal fst_ele snd_ele) then (if (List.length x) = 3 then 
                                                List.iter info_list ~f:(fun y-> y.pong_list <- x::y.pong_list)
                                              else List.iter info_list ~f:(fun y -> y.kan_list <- x::y.kan_list)
                                             )
  else List.iter info_list ~f:(fun y -> y.shun_list <- x::y.shun_list)

let ron_info_to_string (r: ron_info) = 
  let shun_str = List.fold r.shun_list ~init:"" ~f:(fun acc x -> let l_String = Tile.tile_list_to_string x in 
                                                     String.concat [acc;"\n";l_String]
                                                     ) in 
  let pong_str = List.fold r.pong_list ~init:shun_str ~f:(fun acc x -> let l_String = Tile.tile_list_to_string x in
                                                     String.concat [acc;"\n";l_String]
                                                     ) in
  let kang_str = List.fold r.kan_list ~init:pong_str ~f:(fun acc x -> let l_String = Tile.tile_list_to_string x in
                                                     String.concat [acc;"\n";l_String]
                                                     ) in
  let pair_str = Tile.tile_list_to_string r.pair_ron in 
  String.concat [kang_str;"\n";pair_str]
  
let rec check_ron_helper (t_list:Tile.t list) = 
  let ron_info = ref [] in 
  let ron_ref = ref false in
  List.iter t_list ~f:(fun x-> let operate_list = Tile.find_all_ke_shun t_list x in
                         if (List.is_empty operate_list) then 
                           
                           (
                             if one_pair_left ( t_list) then (ron_ref := true;
                                                                          ron_info := (ron_info_constructor [] [] [] t_list)::[];
                            ))
                         else 
                           List.iter operate_list ~f:(fun y -> 
                               (*print_endline( string_of_int (List.length y));
                               Tile.print_tile_list y;*)
                               let (new_t_list,info_ele) = operate t_list y in
                                                   (*print_endline( string_of_int (List.length new_t_list));*)
                                                   let (ron_tf,info_list) = check_ron_helper new_t_list in 
                                                  if (Bool.equal ron_tf true) 
                                                   then ( add_info info_ele info_list; 
                                                          ron_info := simplify_ron_info (info_list @ !ron_info);
                                                          (*print_ron_info_list (!ron_info);*) 
                                                     (*let added_info = add_info info_ele info_list in 
                                                          ron_info := added_info @ (!ron_info);*)
                                                     (*ron_info := (info_ele::info_list)::(!ron_info);*)
                                                         ron_ref := true)
                           ));
  (!ron_ref,!ron_info)

let check_ron (t_list_have_fulu:Tile.t list) =
  let ron_info = ref [] in
  let ron_ref = ref false in
  let t_list = Tile.get_all_non_fulu t_list_have_fulu in 
  List.iter t_list ~f:(fun x-> let operate_list = Tile.find_all_ke_shun t_list x in
                         if (List.is_empty operate_list) then

                           (if one_pair_left ( t_list) then (ron_ref := true;
                                                                          ron_info := (ron_info_constructor [] [] [] t_list)::[];
                            ))
                         else
                           List.iter operate_list ~f:(fun y ->
                               (*print_endline( string_of_int (List.length y));
                               Tile.print_tile_list y;*)
                               let (new_t_list,info_ele) = operate t_list y in
                                                   (*print_endline( string_of_int (List.length new_t_list));*)
                                                   let (ron_tf,info_list) = check_ron_helper new_t_list in
                                                  if (Bool.equal ron_tf true)
                                                   then ( add_info info_ele info_list;
                                                          ron_info := simplify_ron_info (info_list @ !ron_info);
                                                     
                                                         ron_ref := true)
                           ));
  (!ron_ref,sort_ron_info !ron_info)

let rec thirteen_orphan_helper (t_list: Tile.t list) (unfound_orphan: Tile.t list)= 
  match unfound_orphan with 
  | [] -> if List.is_empty t_list then true 
          else false 
  | x::xs -> let count = List.fold t_list ~init:0 ~f:(fun acc y -> if Tile.check_equal x y then (acc+1) 
                                                       else acc
               ) in
             if not (count = 1) then false
             else (let new_list = List.fold t_list ~init:[] ~f:(fun acc y -> if Tile.check_equal x y then acc
                                                              else acc@[y]) in 
                   thirteen_orphan_helper new_list xs)



let thirteen_orphan ( t_list:Tile.t list) = 
  let unfound = Tile.generate_all_yao in 
  thirteen_orphan_helper t_list unfound

let rec has_qi_tui (acc_rep: (Tile.t * int) array) = 
  match (Array.length acc_rep) with 
  | 0 -> true
  | _ -> let ele = Array.get acc_rep 0 in 
         if (snd ele = 2) then has_qi_tui (Array.sub acc_rep ~pos:1 ~len:(Array.length acc_rep -1 ))
         else false

let has_pinfu (info:ron_info) = 
  if (List.length (info.shun_list) < 4 ) then false
  else ( List.fold info.shun_list ~init:true ~f:(fun acc x -> let y = List.nth_exn x 0 in 
                                                          if Bool.equal y.fulu true then false
                                                          else true && acc))
 
let has_tuitui (info: ron_info) = 
  if (List.length info.pong_list + List.length info.kan_list < 4) then false
  else true

let has_dai_san_gan (info: ron_info) = 
  if (List.length info.pong_list + List.length info.kan_list < 3 ) then false
  else (let is_Bai = ref false in
        let is_Chong = ref false in
        let is_Fa = ref false in 
        List.iter info.pong_list ~f:(fun x -> let pos_ele =  List.nth_exn x 0 in
                                              if Tile.check_equal pos_ele (Tile.construct_fake Dragon 0) then is_Bai := true;
                                              if Tile.check_equal pos_ele (Tile.construct_fake Dragon 1) then is_Chong := true;
                                              if Tile.check_equal pos_ele (Tile.construct_fake Dragon 2 ) then is_Fa := true;
                                              );
        List.iter info.kan_list ~f:(fun x -> let pos_ele =  List.nth_exn x 0 in
                                              if Tile.check_equal pos_ele (Tile.construct_fake Dragon 0) then is_Bai := true;
                                              if Tile.check_equal pos_ele (Tile.construct_fake Dragon 1) then is_Chong := true;
                                              if Tile.check_equal pos_ele (Tile.construct_fake Dragon 2 ) then is_Fa := true;
                                              );
        !is_Bai && !is_Chong && !is_Fa
       )

let is_qing_lao_tou (info: ron_info) = 
  let laotou = ref true in 
  if (List.length info.pong_list + List.length info.kan_list < 4) then false
  else (List.iter info.pong_list ~f:(fun x -> List.iter x ~f:(fun y -> if not(Tile.check_laotou y) then laotou := false ));
        !laotou)

let append_missing (fong: bool list)  = 
  let missing_fong = ref 0 in
  let is_missing = List.foldi fong ~init:[] ~f:(fun i acc x -> if (not x) then (missing_fong := i;
                               x::acc)
                   else acc) in
  match (List.length is_missing) with
  | 1 -> (true,!missing_fong) 
  | _ -> (false,-88)
  

let is_Xiao_su_xi (info: ron_info) = 
  let tong = ref false in 
  let nan = ref false in 
  let xia = ref false in 
  let pei = ref false in 
  List.iter info.pong_list ~f:(fun x -> let pos_ele = List.nth_exn x 0 in 
                                        if (Tile.check_equal pos_ele (Tile.construct_fake Wind 0)) then tong := true;
                                        if (Tile.check_equal pos_ele (Tile.construct_fake Wind 1)) then nan := true;
                                        if (Tile.check_equal pos_ele (Tile.construct_fake Wind 2)) then xia := true;
                                        if (Tile.check_equal pos_ele (Tile.construct_fake Wind 3)) then pei := true;
    );
  List.iter info.kan_list ~f:(fun x -> let pos_ele = List.nth_exn x 0 in
                                        if (Tile.check_equal pos_ele (Tile.construct_fake Wind 0)) then tong := true;
                                        if (Tile.check_equal pos_ele (Tile.construct_fake Wind 1)) then nan := true;
                                        if (Tile.check_equal pos_ele (Tile.construct_fake Wind 2)) then xia := true;
                                        if (Tile.check_equal pos_ele (Tile.construct_fake Wind 3)) then pei := true;
    );
  let tf_list = [!tong;!nan;!xia;!pei] in 
  let get_missing= append_missing tf_list in 
  match get_missing with 
  | (true,k) -> let missing_fong = Tile.construct_fake Wind (k+1) in 
                let pair_tile = List.nth_exn info.pair_ron 0 in 
                Tile.check_equal pair_tile missing_fong
                
  | (false,_) -> false 


let is_Dai_su_xi (info: ron_info) = 
  let tong = ref false in
  let nan = ref false in
  let xia = ref false in
  let pei = ref false in
  List.iter info.pong_list ~f:(fun x -> let pos_ele = List.nth_exn x 0 in
                                        if (Tile.check_equal pos_ele (Tile.construct_fake Wind 0)) then tong := true;
                                        if (Tile.check_equal pos_ele (Tile.construct_fake Wind 1)) then nan := true;
                                        if (Tile.check_equal pos_ele (Tile.construct_fake Wind 2)) then xia := true;
                                        if (Tile.check_equal pos_ele (Tile.construct_fake Wind 3)) then pei := true;
    );
  List.iter info.kan_list ~f:(fun x -> let pos_ele = List.nth_exn x 0 in
                                        if (Tile.check_equal pos_ele (Tile.construct_fake Wind 0)) then tong := true;
                                        if (Tile.check_equal pos_ele (Tile.construct_fake Wind 1)) then nan := true;
                                        if (Tile.check_equal pos_ele (Tile.construct_fake Wind 2)) then xia := true;
                                        if (Tile.check_equal pos_ele (Tile.construct_fake Wind 3)) then pei := true;
    );
  !tong && !nan && !xia && !pei

let check_equal_list_nofulu (first: Tile.t list) (second: Tile.t list) = 
  let check_fst_fulu = List.nth_exn first 0 in
  let check_snd_fulu = List.nth_exn second 0 in 
  if (check_fst_fulu.fulu || check_snd_fulu.fulu) then false
  else (Tile.check_equal_list first second)

let yi_pei_kou (info: ron_info) = 
  if (List.length info.shun_list < 2 ) then false 
  else (match List.length info.shun_list with 
        | 2 -> let fst_shun = List.nth_exn info.shun_list 0 in 
               let snd_shun = List.nth_exn info.shun_list 1 in 
               check_equal_list_nofulu fst_shun snd_shun
        | 3 -> let fst_shun = List.nth_exn info.shun_list 0 in 
               let snd_shun = List.nth_exn info.shun_list 1 in 
               let thr_shun = List.nth_exn info.shun_list 2 in 
               (check_equal_list_nofulu fst_shun snd_shun) || (check_equal_list_nofulu snd_shun thr_shun)
        | 4 -> let fst_shun = List.nth_exn info.shun_list 0 in
               let snd_shun = List.nth_exn info.shun_list 1 in
               let thr_shun = List.nth_exn info.shun_list 2 in
               let for_shun = List.nth_exn info.shun_list 3 in 
               (check_equal_list_nofulu fst_shun snd_shun) || (check_equal_list_nofulu snd_shun thr_shun) || (check_equal_list_nofulu thr_shun for_shun)

        | _ -> failwith("baba bubu dede la")
       )

let two_pei_kou (info: ron_info) = 
  if (List.length info.shun_list < 4) then false
  else (let fst_shun = List.nth_exn info.shun_list 0 in
               let snd_shun = List.nth_exn info.shun_list 1 in
               let thr_shun = List.nth_exn info.shun_list 2 in
               let for_shun = List.nth_exn info.shun_list 3 in
        (check_equal_list_nofulu fst_shun snd_shun) && (check_equal_list_nofulu thr_shun for_shun))

let check_all_tiles_number_eq (first:Tile.t list) (second:Tile.t list) (third:Tile.t list) = 
  List.foldi first ~init:true ~f:(fun i acc x-> let snd_ele = (List.nth_exn second i) in
                                 let thr_ele = (List.nth_exn third i) in 
                                 if Tile.check_pin_suo_man [x.kind;snd_ele.kind;thr_ele.kind] then
                                 ((x.number = snd_ele.number) && (snd_ele.number = thr_ele.number)) && acc 
                                 else false
    )

let san_su_tong_shun (info: ron_info) = 
  match List.length info.shun_list with
  | 3 -> let fst_shun = List.nth_exn info.shun_list 0 in
         let snd_shun = List.nth_exn info.shun_list 1 in
         let thr_shun = List.nth_exn info.shun_list 2 in
         check_all_tiles_number_eq fst_shun snd_shun thr_shun  
  | 4 -> let fst_shun = List.nth_exn info.shun_list 0 in
         let snd_shun = List.nth_exn info.shun_list 1 in
         let thr_shun = List.nth_exn info.shun_list 2 in
         let for_shun  = List.nth_exn info.shun_list 3 in 
         check_all_tiles_number_eq fst_shun snd_shun thr_shun ||
         check_all_tiles_number_eq fst_shun snd_shun for_shun ||
         check_all_tiles_number_eq fst_shun thr_shun for_shun ||
         check_all_tiles_number_eq snd_shun thr_shun for_shun 
  | _ -> false

let san_su_tong_ke (info: ron_info) =
  match List.length info.pong_list with
  | 3 -> let fst_shun = List.nth_exn info.pong_list 0 in
         let snd_shun = List.nth_exn info.pong_list 1 in
         let thr_shun = List.nth_exn info.pong_list 2 in
         check_all_tiles_number_eq fst_shun snd_shun thr_shun
  | 4 -> let fst_shun = List.nth_exn info.pong_list 0 in
         let snd_shun = List.nth_exn info.pong_list 1 in
         let thr_shun = List.nth_exn info.pong_list 2 in
         let for_shun  = List.nth_exn info.pong_list 3 in
         check_all_tiles_number_eq fst_shun snd_shun thr_shun ||
         check_all_tiles_number_eq fst_shun snd_shun for_shun ||
         check_all_tiles_number_eq fst_shun thr_shun for_shun ||
         check_all_tiles_number_eq snd_shun thr_shun for_shun
  | _ -> false

let get_an_ke (pong_l: Tile.t list list) = 
  List.fold pong_l ~init:0 ~f:(fun acc x -> let first_t = List.nth_exn x 0 in 
                                 if Tile.get_fulu first_t then acc
                                 else acc + 1)

let san_an_ke (info: ron_info) = 
  match (List.length info.pong_list + List.length info.kan_list) with 
  | 3 -> let an_ke = get_an_ke info.pong_list in
         let an_kan = get_an_ke info.kan_list in 
         if (an_ke + an_kan = 3) then true
         else false 
  | 4 -> let an_ke = get_an_ke info.pong_list in
         let an_kan = get_an_ke info.kan_list in
         if (an_ke + an_kan = 3) then true
         else false
  | _ -> false

let si_an_ke (info: ron_info) = 
  match (List.length info.pong_list + List.length info.kan_list) with 
  | 4 -> let an_ke = get_an_ke info.pong_list in
         let an_kan = get_an_ke info.kan_list in
         if (an_ke + an_kan = 4) then true
         else false
  | _ -> false

let compose_one_to_nine (first: Tile.t list) (second: Tile.t list) (third:Tile.t list) = 
  if Tile.same_kind_across_shun [first;second;third] 
  then let init_first = List.nth_exn first 0 in 
       let init_second = List.nth_exn second 0 in 
       let init_third = List.nth_exn third 0 in 
       (match (init_first.number,init_second.number,init_third.number) with 
       | (1,4,7) -> true
       | _ -> false 
       )
  else false

let yi_cu (info: ron_info) = 
  match (List.length info.shun_list) with 
  | 3 -> let first_shun = List.nth_exn info.shun_list 0 in 
         let second_shun = List.nth_exn info.shun_list 1 in 
         let third_shun = List.nth_exn info.shun_list 2 in 
         compose_one_to_nine first_shun second_shun third_shun
  | 4 -> let first_shun = List.nth_exn info.shun_list 0 in 
         let second_shun =  List.nth_exn info.shun_list 1 in 
         let third_shun = List.nth_exn info.shun_list 2 in 
         let fourth_shun = List.nth_exn info.shun_list 3 in 
         (compose_one_to_nine first_shun second_shun third_shun ||
         compose_one_to_nine first_shun second_shun fourth_shun ||
         compose_one_to_nine first_shun third_shun fourth_shun ||
         compose_one_to_nine second_shun third_shun fourth_shun)
  | _ -> false


let han_chan_dai (info: ron_info) = 
  let all_shun = info.shun_list in 
  let shun_contains_laotou = List.fold all_shun ~init:true ~f:(fun acc x -> Tile.contain_lao_tou_list x && acc) in
  let all_pong = info.pong_list in 
  let pong_contains_nonyao = List.fold all_pong ~init:true ~f:(fun acc x -> Tile.contain_non_yao_list x && acc) in 
  let all_kan = info.kan_list in 
  let kan_contains_nonyao = List.fold all_kan ~init:true ~f:(fun acc x -> Tile.contain_non_yao_list x && acc) in 
  let pair_contains_nonyao = Tile.contain_non_yao_list info.pair_ron in 
  shun_contains_laotou && pong_contains_nonyao && kan_contains_nonyao && pair_contains_nonyao

let han_lao_tou (info: ron_info) = 
  if (List.length info.shun_list > 0 ) then false 
  else (let all_pong = info.pong_list in
        let pong_contains_nonyao = List.fold all_pong ~init:true ~f:(fun acc x -> Tile.contain_non_yao_list x && acc) in
        let all_kan = info.kan_list in
        let kan_contains_nonyao = List.fold all_kan ~init:true ~f:(fun acc x -> Tile.contain_non_yao_list x && acc) in
        let pair_contains_nonyao = Tile.contain_non_yao_list info.pair_ron in
        pong_contains_nonyao && kan_contains_nonyao && pair_contains_nonyao
       )

let xio_san_gan(info:ron_info) = 
  if (List.length info.pong_list + List.length info.kan_list < 2 ) then false
  else (
    let is_Bai = ref false in
    let is_Chong = ref false in
    let is_Fa = ref false in
    List.iter info.pong_list ~f:(fun x -> let pos_ele = List.nth_exn x 0 in
                                          if Tile.check_equal pos_ele (Tile.construct_fake Dragon 0) then is_Bai := true;
                                          if Tile.check_equal pos_ele (Tile.construct_fake Dragon 1) then is_Chong := true;
                                          if Tile.check_equal pos_ele (Tile.construct_fake Dragon 2 ) then is_Fa := true;
      );
    List.iter info.kan_list ~f:(fun x -> let pos_ele = List.nth_exn x 0 in
                                         if Tile.check_equal pos_ele (Tile.construct_fake Dragon 0) then is_Bai := true;
                                         if Tile.check_equal pos_ele (Tile.construct_fake Dragon 1) then is_Chong := true;
                                         if Tile.check_equal pos_ele (Tile.construct_fake Dragon 2 ) then is_Fa := true;
      );
    let tf_list = [!is_Bai;!is_Chong;!is_Fa] in
    let get_missing= append_missing tf_list in
    match get_missing with
    | (true,k) -> let missing_dragon = Tile.construct_fake Dragon (k+1) in
                  let pair_tile = List.nth_exn info.pair_ron 0 in
                  Tile.check_equal pair_tile missing_dragon
                  
    | (false,_) -> false
)

let chan_kan (info:ron_info) = 
  
  let shun_contain = List.fold info.shun_list ~init:true ~f:(fun acc x -> Tile.contain_lao_tou_list x && acc) in
  let pong_contain = List.fold info.pong_list ~init:true ~f:(fun acc x -> Tile.contain_lao_tou_list x && acc) in 
  let pair_contain = Tile.contain_lao_tou_list info.pair_ron in 
  shun_contain && pong_contain && pair_contain 
  

let green_yi_su (t_list :Tile.t list ) = 
  List.fold t_list ~init:true ~f:(fun acc x -> Tile.is_green_tile x && acc)

    (** The type [yaku] represents the types of yaku player can achieve *)
type yaku = Riichi | Tanyao | Hunyise | Qinyise | Chankan
          | Dragontriplet | Ziyise | Daisangan | Chinlaotou | Xiosuxi | Daisuxi | 
          Pinfu | Yipeiko | Lanpeiko | Sanseshun | Sanseke | Sananke | Yicu | Hanchan 
          | Hanlaotou | Xiosangan | Greenyise | Sianke | Tuitui | Thirteen | Chitui | None

let fan_of_yaku yaku = 
  match yaku with 
  | Riichi -> 1
  | Tanyao -> 1
  | Hunyise -> 3
  | Qinyise -> 6
  | Chankan -> 3
  | Dragontriplet -> 1
  | Ziyise -> 13
  | Daisangan -> 13
  | Chinlaotou -> 13
  | Xiosuxi -> 13
  | Daisuxi -> 26
  | Pinfu -> 1
  | Yipeiko -> 1
  | Lanpeiko -> 3
  | Sanseshun -> 2
  | Sanseke -> 2
  | Sananke -> 2
  | Yicu -> 2
  | Hanchan -> 2
  | Hanlaotou -> 2
  | Xiosangan -> 2
  | Greenyise -> 13
  | Sianke -> 13
  | Tuitui -> 2
  | Thirteen -> 13
  | Chitui -> 2
  | None -> 0

let string_of_yaku yaku = 
  match yaku with
  | Riichi -> "Riichi"
  | Tanyao -> "Tanyao"
  | Hunyise -> "Hunyise"
  | Qinyise -> "Qinyise"
  | Chankan -> "ChunKan"
  | Dragontriplet -> "Dragon Triplet"
  | Ziyise -> "Ziyise"
  | Daisangan -> "Dai San Gan"
  | Chinlaotou -> "Chin lao tou"
  | Xiosuxi -> "Xio su xi"
  | Daisuxi -> "Dai su xi"
  | Pinfu -> "Pin fu"
  | Yipeiko -> "Yi Pei Ko"
  | Lanpeiko -> "Lan Pei Ko"
  | Sanseshun -> "San se shun"
  | Sanseke -> "San se ke"
  | Sananke -> "San an ke"
  | Yicu -> "Yi cu"
  | Hanchan -> "Han chan"
  | Hanlaotou -> "Han lao tou"
  | Xiosangan -> "Xio san gan"
  | Greenyise -> "Green yi se"
  | Sianke -> "Si an ke"
  | Tuitui -> "Tui tui fu"
  | Thirteen -> "Thirteen"
  | Chitui -> "Chi tui"
  | None -> ""

let cal_point_yaku (yaku_list: yaku list) = 
  List.fold yaku_list ~init:0 ~f:(fun acc x -> acc + fan_of_yaku x )

let cal_yaku_man (hand : ron_info) (t : Tile.t list) = 
  let all_yaku_man = ref [] in 
  let () = (match zi_yisu_check t with
  | true -> all_yaku_man := Ziyise:: !all_yaku_man
  | false -> () ) in 
  let () = (match has_dai_san_gan hand with
  | true -> all_yaku_man := Daisangan:: !all_yaku_man
  | false -> ()) in 
  let () = (match is_qing_lao_tou hand with 
  | true -> all_yaku_man := Chinlaotou :: !all_yaku_man
  | false -> ()) in 
  let () = (match is_Xiao_su_xi hand with 
  | true -> all_yaku_man := Xiosuxi :: !all_yaku_man 
  | false -> ()) in 
  let () = (match is_Dai_su_xi hand with 
  | true -> all_yaku_man := Daisuxi :: !all_yaku_man
  | false -> ()) in 
  let () = (match si_an_ke hand with 
  | true -> all_yaku_man := Sianke :: !all_yaku_man
  | false -> ()) in 
  let () = (match green_yi_su t with 
  | true -> all_yaku_man := Greenyise :: !all_yaku_man
  | false -> ()) in
  !all_yaku_man

let cal_non_yaku_man( hand: ron_info) (t: Tile.t list) (p:player) = 
  let all_fan = ref [] in 
  let () = (match p.richii with 
  | true -> all_fan := Riichi :: !all_fan
  | false -> ()) in 
  let () = (match tanyao_check t with 
  | true -> all_fan := Tanyao :: !all_fan 
  | false -> ()) in 
  let () = (match hun_yisu_check t with 
  | true -> all_fan := Hunyise :: !all_fan 
  | false -> ()) in 
  let () = (match qing_yisu_check t with 
  | true -> all_fan := Qinyise :: !all_fan 
  | false -> ()) in 
  let () = (match chan_kan hand with 
  | true -> all_fan := Chankan :: !all_fan 
  | false -> (match han_lao_tou hand with 
              | true -> all_fan := Hanlaotou :: !all_fan 
              | false -> (match han_chan_dai hand with 
                          | true -> all_fan := Hanchan :: !all_fan
                          | false  -> ()))) in 
  let () = (match san_su_tong_shun hand with 
  | true -> all_fan := Sanseshun :: !all_fan 
  | false -> ()) in 
  let () = (match san_su_tong_ke hand with 
  | true -> all_fan := Sanseke :: !all_fan 
  | false -> ()) in 
  let () = (match yi_cu hand with 
  | true -> all_fan := Yicu :: !all_fan 
  | false -> ()) in 
  let () = (match xio_san_gan hand with  
  | true -> all_fan := Xiosangan :: !all_fan 
  | false -> ()) in 
  let () = (match has_tuitui hand with  
  | true -> all_fan := Tuitui :: !all_fan 
  | false -> ()) in 
  let () = (match has_dragon_triplet ( t) with 
  | true -> all_fan := Dragontriplet :: !all_fan 
  | false -> ()) in 
  let () = (match has_pinfu hand with 
  | true -> all_fan := Pinfu :: !all_fan 
  | false -> ()) in 
  let () = (match yi_pei_kou hand with 
  | true -> all_fan := Yipeiko :: !all_fan 
  | false -> ()) in 
  let () = (match two_pei_kou hand with 
  | true -> all_fan := Lanpeiko :: !all_fan 
  | false -> ()) in 
  !all_fan



let cal_yaku (t_list:Tile.t list) (p:player) (info:ron_info list) = 
  let yaku_list = List.map info ~f:(fun x ->
                                              let yaku_man_list = cal_yaku_man x t_list in
                                              if List.length yaku_man_list > 0 then (true,yaku_man_list)
                                              else (let non_yaku_man_list = cal_non_yaku_man x t_list p in
                                                    if List.length non_yaku_man_list > 0 then (true, non_yaku_man_list)
                                                    else (false,[])
                                                    )
                                              ) in
                   let yaku_fan_point = List.map yaku_list ~f:( fun (tf,y_list) ->
                                                                if not(tf) then (y_list,0)
                                                                else (y_list,cal_point_yaku y_list)
                                                                ) in
                   let max_fan_point = List.max_elt yaku_fan_point ~compare:(fun (_,p1) (_,p2) -> if p1 > p2 then 1
                                                                         else if p1 = p2 then 0
                                                                         else -1) in
                   match max_fan_point with
                   | Some (_,0) -> ([],0)
                   | Some (yaku,point) -> (yaku,point)
                   | None -> ([],0)

(*when we calculate yaku, we put each yaku that the ron_info or t_list (they are the same thing) tells us *)
let ron_and_cal_yaku (t_list:Tile.t list) (p:player)= 
  let result = check_ron t_list in 
  match result with 
  | (false,_) -> ([],0)
  | (true,info) -> cal_yaku t_list p info 


let get_riichi (p:player) = 
  let t_list = p.total_tiles in 
  let all_tiles = Tile.generate_all_pai in 
  List.fold all_tiles ~init:[] ~f:(fun acc x -> let added_list = Tile.sort_hand_add_tile t_list x in 
                                                let ron_result = check_ron added_list in 
                                                (match ron_result with 
                                                 | (true,_) -> x::acc
                                                 | (false,_) -> acc
                                                ))


let set_riichi (p:player) = 
  match p.richii with
  | true -> failwith("impossible")
  | false -> p.richii <- true





