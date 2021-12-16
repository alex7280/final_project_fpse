open Core;;
open OUnit2;;

module T = Tile
module P = Player   
module G = Game
module A = Action

let test_read_str _ = 
  (match A.read_str "chii 1" with
  | Chii k -> assert_equal k 1
  | _ -> assert_equal 0 1);
  (match A.read_str "discard Man 1" with 
  | Discard (_,num) -> assert_equal num 1
  | _ -> assert_equal 0 1);
  (match A.read_str "skip" with 
  | Skip -> assert_equal 0 0 
  | _ -> assert_equal 0 1);
  match A.read_str "quit" with 
  | Quit -> assert_equal 0 0 
  | _ -> assert_equal 0 1 

let initialize_game _ = 
  let g = G.init_game in 
  assert_equal (List.length g.p1.total_tiles) 13;
  let tile1 = T.class_constructor 0 Man 1 false false false in
  let tile2 = T.class_constructor 0 Man 2 false false false in
  let tile3 = T.class_constructor 0 Man 3 false false false in
  let (_,t_list) = G.get_n_tiles [tile1;tile2;tile3] 3 false in
  assert_equal (List.length t_list) 3;
  assert_equal (List.length (G.shuffle t_list)) 3

let is_wind_dragon_test _ = 
  let tile1 = T.class_constructor 0 Dragon 1 false false false in
  let tile2 = T.class_constructor 0 Wind 1 false false false in
  assert_equal( T.is_wind_dragon tile1) true;
  assert_equal (T.is_wind_dragon tile2) true

let contain_yao_list_and_other_funs_test _ = 
  let tile1 = T.class_constructor 0 Man 1 false false false in 
  let tile2 = T.class_constructor 0 Man 2 false false false in
  let tile3 = T.class_constructor 0 Man 3 false false false in
  assert_equal (T.contain_yao_list [tile1;tile2;tile3]) true;
  assert_equal (T.contain_non_yao_list [tile1;tile2;tile3]) true;
  assert_equal (T.same_kind tile1 tile2 tile3) true;
  assert_equal (T.have_tile [tile1;tile2;tile3] Man 1) true




let test_constructor _ = 
  let tile1 = T.class_constructor 0 Dragon 1 false false false in 
  let tile2 = T.class_constructor 0 Man 1 false false false in
  assert_equal (T.tile_to_string tile1) "Bai / Dragon 1";
  assert_equal (T.tile_to_string tile2) "Man1"
  
let test_fulu _ = 
  let tile1=T.class_constructor 0 Dragon 1 false false false in
  T.change_fulu tile1;
  assert_equal (T.get_fulu tile1) true

let test_check_non_yao _ = 
  let tile1 = T.class_constructor  0 Dragon 1 false false false in
  assert_equal (T.check_yao tile1) true

let test_check_shun _ = 
  let tile1 = T.class_constructor 0 Man 1 false false false in
  let tile2 = T.class_constructor 0 Man 2 false false false in
  let tile3 = T.class_constructor 0 Man 3 false false false in
  assert_equal (T.check_shun tile1 tile2 tile3) true

let test_check_equal _ = 
  let tile1 = T.class_constructor 0 Man 1 false false false in
  let tile2 = T.class_constructor 0 Man 1 false false false in
  assert_equal (T.check_equal tile1 tile2) true
      
let test_sort_same_kind _ = 
  let tile1 = T.class_constructor 0 Man 7 false false false in
  let tile2 = T.class_constructor 0 Man 3 false false false in
  let tile3 = T.class_constructor 0 Man 5 false false false in
  let tile4 = T.class_constructor 0 Man 1 false false false in
  let tile_list = T.sort_same_kind [tile1;tile2;tile3;tile4] in
  let new_tile_2 = List.nth tile_list 1 in 
  let new_tile_3 = List.nth tile_list 2 in
  let l3 = (match new_tile_2 with
  | Some l1 -> l1 
  | None -> failwith("impossible")) in
  let l4 = (match new_tile_3 with
  | Some l2 -> l2 
  | None -> failwith("impossible")) in 
  assert_equal (T.check_equal tile3 l4) true;
  assert_equal (T.check_equal tile2 l3) true
  
let test_sort_hand _ = 
  let tile1 = T.class_constructor 0 Man 7 false false false in
  let tile2 = T.class_constructor 0 Man 3 false false false in
  let tile3 = T.class_constructor 0 Man 5 false false false in
  let tile4 = T.class_constructor 0 Man 1 false false false in
  let tile5 = T.class_constructor 0 Pin 7 false false false in
  let tile6 = T.class_constructor 0 Pin 3 false false false in
  let tile7 = T.class_constructor 0 Pin 5 false false false in
  let tile8 = T.class_constructor 0 Pin 1 false false false in
  let tile_list = T.sort_hand [tile1;tile2;tile3;tile4;tile5;tile6;tile7;tile8] in
  let new_tile_2 = List.nth tile_list 1 in
  let new_tile_3 = List.nth tile_list 2 in
  let new_tile_4 = List.nth tile_list 5 in
  let new_tile_5 = List.nth tile_list 6 in 
  let l3 = (match new_tile_2 with
  | Some l1 -> l1
  | None -> failwith("impossible")) in
  let l4 = (match new_tile_3 with
  | Some l2 -> l2
  | None -> failwith("impossible")) in
  let l5 = (match new_tile_4 with
  | Some l3 -> l3
  | None -> failwith("impossible")) in
  let l6 = (match new_tile_5 with
  | Some l4 -> l4
  | None -> failwith("impossible")) in
  assert_equal (T.check_equal tile6 l5) true;
  assert_equal (T.check_equal tile7 l6) true;
  assert_equal (T.check_equal tile3 l4) true;
  assert_equal (T.check_equal tile2 l3) true

let test_delete_all_tile _ = 
  let tile1 = T.class_constructor 0 Man 7 false false false in
  let tile2 = T.class_constructor 0 Man 3 false false false in
  let tile3 = T.class_constructor 0 Man 5 false false false in
  let tile4 = T.class_constructor 0 Man 1 false false false in
  let tile5 = T.class_constructor 0 Man 1 false false false in
  let tile_list = T.delete_all_tile [tile1;tile2;tile3;tile4;tile5] tile4 in
  assert_equal (List.length tile_list) 3 

let test_delete_a_tile _ = 
  let tile1 = T.class_constructor 0 Man 7 false false false in
  let tile2 = T.class_constructor 0 Man 3 false false false in
  let tile3 = T.class_constructor 0 Man 5 false false false in
  let tile4 = T.class_constructor 0 Man 1 false false false in
  let tile5 = T.class_constructor 0 Man 1 false false false in
  let tile_list = T.delete_one_tile [tile1;tile2;tile3;tile4;tile5] tile4 [] in
  assert_equal (List.length tile_list) 4

let test_get_all_shun _ = 
  let tile1 = T.class_constructor 0 Man 5 false false false in
  let tile2 = T.class_constructor 0 Man 4 false false false in
  let tile3 = T.class_constructor 0 Man 3 false false false in
  let tile4 = T.class_constructor 0 Man 2 false false false in
  let tile5 = T.class_constructor 0 Man 1 false false false in
  let shun_list = T.get_all_shun [tile1;tile2;tile3;tile4;tile5] tile3 in
  let list = match shun_list with 
  | Some k -> k
  | None -> failwith("no") in
  assert_equal (List.length list) 3

let test_get_pong _ =
  let tile1 = T.class_constructor 0 Man 1 false false false in
  let tile2 = T.class_constructor 0 Man 1 false false false in
  let tile3 = T.class_constructor 0 Man 3 false false false in
  let tile4 = T.class_constructor 0 Man 2 false false false in
  let tile5 = T.class_constructor 0 Man 1 false false false in
  let shun_list = T.get_pong [tile1;tile2;tile3;tile4;tile5] tile1 in
  let list = match shun_list with
  | Some k -> k
  | None -> failwith("no") in
  assert_equal (List.length list) 3

let test_get_kang _ =
  let tile1 = T.class_constructor 0 Man 1 false false false in
  let tile2 = T.class_constructor 0 Man 1 false false false in
  let tile3 = T.class_constructor 0 Man 1 false false false in
  let tile4 = T.class_constructor 0 Man 2 false false false in
  let tile5 = T.class_constructor 0 Man 1 false false false in
  let shun_list = T.get_kan [tile1;tile2;tile3;tile4;tile5] tile1 in
  let list = match shun_list with
  | Some k -> k
  | None -> failwith("no") in
  assert_equal (List.length list) 4

let test_get_all_ways _ = 
  let tile1 = T.class_constructor 0 Man 5 false false false in
  let tile2 = T.class_constructor 0 Man 4 false false false in
  let tile3 = T.class_constructor 0 Man 3 false false false in
  let tile4 = T.class_constructor 0 Man 2 false false false in
  let tile5 = T.class_constructor 0 Man 1 false false false in
  let tile6 = T.class_constructor 0 Man 1 false false false in
  let tile7 = T.class_constructor 0 Man 1 false false false in
  let all_list = T.find_all_ke_shun [tile1;tile2;tile3;tile4;tile5;tile6;tile7] tile5 in 
  assert_equal all_list all_list

let test_check_ron _ = 
  let tile1 = T.class_constructor 0 Man 1 false false false in
  let tile2 = T.class_constructor 0 Man 1 false false false in
  let tile3 = T.class_constructor 0 Man 2 false false false in
  let tile4 = T.class_constructor 0 Man 2 false false false in
  let tile5 = T.class_constructor 0 Man 3 false false false in
  let tile6 = T.class_constructor 0 Man 3 false false false in
  let tile7 = T.class_constructor 0 Man 7 false false false in
  let tile8 = T.class_constructor 0 Man 7 false false false in 
  let (ron_tf,_) = P.check_ron [tile1;tile2;tile3;tile4;tile5;tile6;tile7;tile8] in
  assert_equal ron_tf true;

  let tile81 = T.class_constructor 0 Man 1 false false false in
  let tile82 = T.class_constructor 0 Man 1 false false false in
  let tile83 = T.class_constructor 0 Man 2 false false false in
  let tile84 = T.class_constructor 0 Man 2 false false false in
  let tile85 = T.class_constructor 0 Man 3 false false false in
  let tile86 = T.class_constructor 0 Man 3 false false false in
  let tile87 = T.class_constructor 0 Man 4 false false false in
  let tile88 = T.class_constructor 0 Man 4 false false false in
  let (eron_tf,_) = P.check_ron [tile81;tile82;tile83;tile84;tile85;tile86;tile87;tile88] in
  assert_equal eron_tf true;


  let tile9 = T.class_constructor 0 Man 1 false false false in
  let tile10 = T.class_constructor 0 Man 2 false false false in
  let tile11 = T.class_constructor 0 Man 3 false false false in
  let tile12 = T.class_constructor 0 Man 7 false false false in
  let tile13 = T.class_constructor 0 Man 7 false false false in
  let (all_list,_) = P.check_ron [tile9;tile10;tile11;tile12;tile13] in 
  assert_equal all_list true
  
let stresstest_check_ron _ = 
  let tile1 = T.class_constructor 0 Man 1 false false false in
  let tile2 = T.class_constructor 0 Man 1 false false false in
  let tile3 = T.class_constructor 0 Man 2 false false false in
  let tile4 = T.class_constructor 0 Man 3 false false false in
  let tile5 = T.class_constructor 0 Man 4 false false false in
  let tile6 = T.class_constructor 0 Pin 2 false false false in
  let tile7 = T.class_constructor 0 Pin 2 false false false in
  let tile8 = T.class_constructor 0 Pin 2 false false false in
  let tile9 = T.class_constructor 0 Pin 3 false false false in
  let tile10 = T.class_constructor 0 Pin 4 false false false in
  let tile11 = T.class_constructor 0 Pin 5 false false false in
  let tile12 = T.class_constructor 0 Pin 4 false false false in
  let tile13 = T.class_constructor 0 Pin 5 false false false in
  let tile14 = T.class_constructor 0 Pin 6 false false false in
  let (tf,_) = P.check_ron [tile1;tile2;tile3;tile4;tile5;tile6;tile7;tile8;tile9;tile10;tile11;tile12;tile13;tile14] 
  in    
  assert_equal tf true

let set_fulu_test _ = 
  let tile81 = T.class_constructor 0 Man 1 false false false in
  let tile82 = T.class_constructor 0 Man 1 false false false in
  let tile83 = T.class_constructor 0 Man 2 false false false in
  let tile84 = T.class_constructor 0 Man 2 false false false in
  let tile85 = T.class_constructor 0 Man 3 false false false in
  let tile86 = T.class_constructor 0 Man 3 false false false in
  let tile87 = T.class_constructor 0 Man 4 false false false in
  let tile88 = T.class_constructor 0 Man 4 false false false in
  let list = P.set_fulu_helper [tile81;tile82;tile83;tile84;tile85;tile86;tile87;tile88] [tile84;tile86;tile88] [] in
  assert_equal (List.length list) 8 

let get_num_test _ = 
  let t1 = T.construct_fake Man 1 in 
  assert_equal (T.get_number t1) 1;
  assert_equal (T.get_discarded t1) false;
  assert_equal (T.kind_equal t1.kind T.Man) true

let generate_game_tiles_test _ = 
  let t1 = T.generate_game_tiles () in 
  assert_equal (List.length t1) 136

let check_tiles_test _ =
  let t1 = T.construct_fake Man 1 in 
  assert_equal (T.check_laotou t1) true;
  assert_equal (T.is_green_tile t1) false

let contain_lao_tou_list_test _ = 
  let t1 = T.construct_fake Man 1 in
  let t2 = T.construct_fake Man 2 in
  let t3 = T.construct_fake Man 3 in
  assert_equal (T.contain_lao_tou_list [t1;t2;t3]) true;
  assert_equal (T.contain_non_yao_list [t1;t2;t3]) true

let same_kind_across_shun_test _ = 
  let t1 = T.construct_fake Man 1 in
  let t2 = T.construct_fake Man 2 in
  let t3 = T.construct_fake Man 3 in
  let t4 = T.construct_fake Pin 3 in 
  assert_equal (T.same_kind_across_shun [[t1;t2;t3]]) true;
  assert_equal (T.same_kind_across_shun [[t1;t2;t4]]) true

let get_all_fulu_test _ = 
  let t1 = T.construct_fake Man 1 in
  let t2 = T.construct_fake Man 2 in
  let t3 = T.construct_fake Man 3 in
  let p = P.class_constructor 10 false false [t1;t2;t3] [] in
  assert_equal (P.get_all_fulu p) [];
  assert_equal (P.get_all_non_fulu p) [t1;t2;t3]

let turn_draw_tile_test _ = 
  let t1 = T.construct_fake Man 2 in
  let t2 = T.construct_fake Man 3 in
  let t3 = T.construct_fake Man 4 in
  let p = P.class_constructor 10 false false [t1;t2;t3] [] in
  let add = T.construct_fake Man 1 in
  P.turn_draw_tile p add;
  assert_equal (List.length p.total_tiles) 4

let play_tile_test _ = 
  let t1 = T.construct_fake Man 2 in
  let t2 = T.construct_fake Man 3 in
  let t3 = T.construct_fake Man 4 in
  let p = P.class_constructor 10 false false [t1;t2;t3] [] in
  P.play_tile p t1;
  assert_equal (List.length p.total_tiles) 2

let test_user_chii_pong_kan _ = 
  let t1 = T.construct_fake Man 2 in
  let t2 = T.construct_fake Man 3 in
  let t3 = T.construct_fake Man 4 in
  let p = P.class_constructor 10 false false [t1;t2;t3] [] in
  P.user_chii_pong_kan t1 p 0;
  
  assert_equal (List.length p.total_tiles) 3

let test_check_shun_list _ = 
  let t1 = T.construct_fake Man 2 in
  let t2 = T.construct_fake Man 3 in
  let t3 = T.construct_fake Man 4 in
  let t_list = [[t1;t2;t3]] in 
  let t1_list = [[t3;t2;t1]] in 
  assert_equal (P.check_shun_list t_list t1_list) true

let test_check_pair _ = 
  let t1 = T.construct_fake Man 2 in 
  let t2 = T.construct_fake Man 3 in
  let t_list = [t1;t1] in
  let t_list1 = [t1;t1] in 
  let t_list2 = [t2;t2] in 
  assert_equal (P.check_pair t_list t_list1) true;
  assert_equal (P.check_pair t_list2 t_list2) true

let test_update_ron_info _ =
  let t1 = T.construct_fake Man 2 in
  let t2 = T.construct_fake Man 3 in
  let t3 = T.construct_fake Man 4 in
  let p = P.class_constructor 10 false false [t1;t2;t3] [] in
  P.update_ron_info p [t1;t2;t3];
  let t4 = T.construct_fake Man 6 in
  let t5 = T.construct_fake Man 7 in
  let t6 = T.construct_fake Man 8 in
  P.update_ron_info p [t4;t5;t6];
  assert_equal (List.length p.r_info.shun_list) 2
  
let test_concat_two_ron_info _ = 
  let t1 = T.construct_fake Man 2 in
  let t2 = T.construct_fake Man 3 in
  let t3 = T.construct_fake Man 4 in
  let t4 = T.construct_fake Man 6 in
  let t5 = T.construct_fake Man 7 in
  let t6 = T.construct_fake Man 8 in
  let r1 = P.ron_info_constructor [[t1;t2;t3]] [] [] [] in
  let r2 = P.ron_info_constructor [[t4;t5;t6]] [] [] [] in 
  let r3 = P.concat_two_ron_info r1 r2 in 
  assert_equal (List.length r3.shun_list) 2

let tanyao_check_test _ = 
  let t1 = T.construct_fake Man 2 in
  let t2 = T.construct_fake Man 3 in
  let t3 = T.construct_fake Man 4 in
  let t4 = T.construct_fake Man 6 in
  let t5 = T.construct_fake Man 7 in
  let t6 = T.construct_fake Man 8 in
  assert_equal (P.tanyao_check [t1;t2;t3;t4;t5;t6]) true

let hun_yisu_check_test _ = 
  let t1 = T.construct_fake Man 1 in
  let t2 = T.construct_fake Man 1 in
  let t3 = T.construct_fake Man 1 in
  let t4 = T.construct_fake Wind 1 in
  let t5 = T.construct_fake Wind 1 in
  let t6 = T.construct_fake Wind 1 in
  let t7 = T.construct_fake Dragon 2 in 
  let t8 = T.construct_fake Dragon 2 in 
  assert_equal (P.hun_yisu_check [t1;t2;t3;t4;t5;t6;t7;t8]) true;
  let t11 = T.construct_fake Pin 1 in
  let t12 = T.construct_fake Pin 1 in
  let t13 = T.construct_fake Pin 1 in
  let t14 = T.construct_fake Wind 1 in
  let t15 = T.construct_fake Wind 1 in
  let t16 = T.construct_fake Wind 1 in
  let t17 = T.construct_fake Dragon 2 in
  let t18 = T.construct_fake Dragon 2 in
  assert_equal (P.hun_yisu_check [t11;t12;t13;t14;t15;t16;t17;t18]) true;
  let t21 = T.construct_fake Suo 1 in
  let t22 = T.construct_fake Suo 1 in
  let t23 = T.construct_fake Suo 1 in
  let t24 = T.construct_fake Wind 1 in
  let t25 = T.construct_fake Wind 1 in
  let t26 = T.construct_fake Wind 1 in
  let t27 = T.construct_fake Dragon 2 in
  let t28 = T.construct_fake Dragon 2 in
  assert_equal (P.hun_yisu_check [t21;t22;t23;t24;t25;t26;t27;t28]) true

let qing_yisu_test _ = 
  let t1 = T.construct_fake Man 1 in
  let t2 = T.construct_fake Man 1 in
  let t3 = T.construct_fake Man 1 in
  let t11 = T.construct_fake Pin 1 in
  let t12 = T.construct_fake Pin 1 in
  let t13 = T.construct_fake Pin 1 in
   let t21 = T.construct_fake Pin 1 in
  let t22 = T.construct_fake Pin 1 in
  let t23 = T.construct_fake Pin 1 in
  assert_equal (P.qing_yisu_check [t1;t2;t3]) true;
  assert_equal (P.qing_yisu_check [t11;t12;t13]) true;
  assert_equal (P.qing_yisu_check [t21;t22;t23]) true
  
let zi_yisu_test _ = 
  let t1 = T.construct_fake Man 1 in
  let t24 = T.construct_fake Wind 1 in
  let t25 = T.construct_fake Wind 1 in
  let t26 = T.construct_fake Wind 1 in
  assert_equal (P.zi_yisu_check [t24;t25;t26]) true;
  assert_equal (P.zi_yisu_check [t1;t24;t25;t26]) false

let dragon_triplet_test _ = 
  let faker = T.construct_fake Wind 1 in 
  let t24 = T.construct_fake Dragon 1 in
  let t25 = T.construct_fake Dragon 1 in
  let t26 = T.construct_fake Dragon 1 in
  assert_equal (P.has_dragon_triplet [faker;t24;t25;t26]) true

let check_pin_fu _ = 
  let t1 = T.construct_fake Man 1 in
  let t2 = T.construct_fake Man 2 in
  let t3 = T.construct_fake Man 3 in
  let t4 = T.construct_fake Pin 1 in
  let t5 = T.construct_fake Pin 2 in
  let t6 = T.construct_fake Pin 3 in
  let t7 = T.construct_fake Suo 1 in
  let t8 = T.construct_fake Suo 2 in
  let t9 = T.construct_fake Suo 3 in
  let t10 = T.construct_fake Man 5 in
  let t11 = T.construct_fake Man 6 in
  let t12 = T.construct_fake Man 7 in
  let t13 = T.construct_fake Man 9 in
  let t14 = T.construct_fake Man 9 in
  let r1 = P.ron_info_constructor [[t1;t2;t3];[t4;t5;t6];[t7;t8;t9];[t10;t11;t12]] [] [] [t13;t14] in
  assert_equal (P.has_pinfu r1 ) true

let pin_fu_false _ =
  let t1 = T.class_constructor 0 Man 1 true false false in
  let t2 = T.construct_fake Man 2 in
  let t3 = T.construct_fake Man 3 in
  let t4 = T.construct_fake Pin 1 in
  let t5 = T.construct_fake Pin 2 in
  let t6 = T.construct_fake Pin 3 in
  let t7 = T.construct_fake Suo 1 in
  let t8 = T.construct_fake Suo 2 in
  let t9 = T.construct_fake Suo 3 in
  let t10 = T.construct_fake Man 5 in
  let t11 = T.construct_fake Man 6 in
  let t12 = T.construct_fake Man 7 in
  let t13 = T.construct_fake Man 9 in
  let t14 = T.construct_fake Man 9 in
  let r1 = P.ron_info_constructor [[t1;t2;t3];[t4;t5;t6];[t7;t8;t9];[t10;t11;t12]] [] [] [t13;t14] in
  assert_equal (P.has_pinfu r1 ) false

let dai_san_gan_false _ = 
  let t1 = T.class_constructor 0 Man 1 true false false in
  let t2 = T.construct_fake Man 2 in
  let t3 = T.construct_fake Man 3 in
  let t4 = T.construct_fake Pin 1 in
  let t5 = T.construct_fake Pin 2 in
  let t6 = T.construct_fake Pin 3 in
  let t7 = T.construct_fake Suo 1 in
  let t8 = T.construct_fake Suo 2 in
  let t9 = T.construct_fake Suo 3 in
  let t10 = T.construct_fake Man 5 in
  let t11 = T.construct_fake Man 6 in
  let t12 = T.construct_fake Man 7 in
  let t13 = T.construct_fake Man 9 in
  let t14 = T.construct_fake Man 9 in
  let r1 = P.ron_info_constructor [[t1;t2;t3];[t4;t5;t6];[t7;t8;t9];[t10;t11;t12]] [] [] [t13;t14] in
  assert_equal (P.has_dai_san_gan r1 ) false

let has_dai_san_gan_test_more _ =
  let t1 = T.construct_fake Dragon 3 in
  let t2 = T.construct_fake Dragon 3 in
  let t3 = T.construct_fake Dragon 3 in
  let t4 = T.construct_fake Dragon 2 in
  let t5 = T.construct_fake Dragon 2 in
  let t6 = T.construct_fake Dragon 2 in
  let t77 = T.construct_fake Dragon 2 in 
  let t7 = T.construct_fake Suo 1 in
  let t8 = T.construct_fake Suo 1 in
  let t9 = T.construct_fake Suo 1 in
  let t10 = T.construct_fake Dragon 1 in
  let t11 = T.construct_fake Dragon 1 in
  let t12 = T.construct_fake Dragon 1 in
  let t13 = T.construct_fake Dragon 1 in
  let t14 = T.construct_fake Man 9 in
  let t15 = T.construct_fake Man 9 in
  let r1 = P.ron_info_constructor [] [[t1;t2;t3];[t7;t8;t9]] [[t4;t5;t6;t77];[t10;t11;t12;t13]] [t14;t15] in
  assert_equal (P.has_dai_san_gan r1) true

let has_tui_tui_false _ = 
  let t1 = T.construct_fake Man 1 in
  let t2 = T.construct_fake Man 2 in
  let t3 = T.construct_fake Man 3 in
  let t4 = T.construct_fake Pin 1 in
  let t5 = T.construct_fake Pin 2 in
  let t6 = T.construct_fake Pin 3 in
  let t7 = T.construct_fake Suo 1 in
  let t8 = T.construct_fake Suo 2 in
  let t9 = T.construct_fake Suo 3 in
  let t10 = T.construct_fake Man 5 in
  let t11 = T.construct_fake Man 6 in
  let t12 = T.construct_fake Man 7 in
  let t13 = T.construct_fake Man 9 in
  let t14 = T.construct_fake Man 9 in
  let r1 = P.ron_info_constructor [[t1;t2;t3];[t4;t5;t6];[t7;t8;t9];[t10;t11;t12]] [] [] [t13;t14] in
  assert_equal (P.has_tuitui r1 ) false

let is_Xiao_su_xi_test_more _ =
  let t1 = T.construct_fake Wind 3 in
  let t2 = T.construct_fake Wind 3 in
  let t3 = T.construct_fake Wind 3 in
  let t4 = T.construct_fake Wind 2 in
  let t5 = T.construct_fake Wind 2 in
  let t6 = T.construct_fake Wind 2 in
  let t7 = T.construct_fake Suo 1 in
  let t8 = T.construct_fake Suo 1 in
  let t9 = T.construct_fake Suo 1 in
  let t10 = T.construct_fake Wind 1 in
  let t11 = T.construct_fake Wind 1 in
  let t12 = T.construct_fake Wind 1 in
  let t13 = T.construct_fake Wind 1 in
  let t14 = T.construct_fake Wind 4 in
  let t15 = T.construct_fake Wind 4 in
  let r1 = P.ron_info_constructor [] [[t1;t2;t3];[t4;t5;t6];[t7;t8;t9]] [[t10;t11;t12;t13]] [t14;t15] in
  assert_equal (P.is_Xiao_su_xi r1) true

let is_Xiao_su_xi_test_more1 _ =
  let t1 = T.construct_fake Wind 2 in
  let t2 = T.construct_fake Wind 2 in
  let t3 = T.construct_fake Wind 2 in
  let t33 = T.construct_fake Wind 2 in 
  let t4 = T.construct_fake Wind 4 in
  let t5 = T.construct_fake Wind 4 in
  let t6 = T.construct_fake Wind 4 in
  let t66 = T.construct_fake Wind 4 in
  let t7 = T.construct_fake Suo 1 in
  let t8 = T.construct_fake Suo 1 in
  let t9 = T.construct_fake Suo 1 in
  let t10 = T.construct_fake Wind 1 in
  let t11 = T.construct_fake Wind 1 in
  let t12 = T.construct_fake Wind 1 in
  let t13 = T.construct_fake Wind 1 in
  let t14 = T.construct_fake Wind 3 in
  let t15 = T.construct_fake Wind 3 in
  let r1 = P.ron_info_constructor [] [[t7;t8;t9]] [[t1;t2;t3;t33];[t4;t5;t6;t66];[t10;t11;t12;t13]] [t14;t15] in
  assert_equal (P.is_Xiao_su_xi r1) true

let is_Dai_su_xi_test_more _ =
  let t1 = T.construct_fake Wind 3 in
  let t2 = T.construct_fake Wind 3 in
  let t3 = T.construct_fake Wind 3 in
  let t4 = T.construct_fake Wind 2 in
  let t5 = T.construct_fake Wind 2 in
  let t6 = T.construct_fake Wind 2 in
  let t66 = T.construct_fake Wind 2 in 
  let t7 = T.construct_fake Wind 4 in
  let t8 = T.construct_fake Wind 4 in
  let t9 = T.construct_fake Wind 4 in
  let t99 = T.construct_fake Wind 4 in
  let t10 = T.construct_fake Wind 1 in
  let t11 = T.construct_fake Wind 1 in
  let t12 = T.construct_fake Wind 1 in
  let t13 = T.construct_fake Wind 1 in
  let t14 = T.construct_fake  Dragon 1 in
  let t15 = T.construct_fake Dragon 1 in
  let r1 = P.ron_info_constructor [] [[t1;t2;t3]] [[t4;t5;t6;t66];[t7;t8;t9;t99];[t10;t11;t12;t13]] [t14;t15] in
  assert_equal (P.is_Dai_su_xi r1) true

let san_an_ke_test_more _ =
  let t1 = T.construct_fake Man 1 in
  let t2 = T.construct_fake Man 2 in
  let t3 = T.construct_fake Man 3 in
  let t4 = T.construct_fake Wind 2 in
  let t5 = T.construct_fake Wind 2 in
  let t6 = T.construct_fake Wind 2 in
  let t7 = T.construct_fake Wind 4 in
  let t8 = T.construct_fake Wind 4 in
  let t9 = T.construct_fake Wind 4 in
  let t10 = T.construct_fake Wind 3 in
  let t11 = T.construct_fake Wind 3 in
  let t12 = T.construct_fake Wind 3 in
  let t13 = T.construct_fake Wind 3 in
  let t14 = T.construct_fake  Dragon 1 in
  let t15 = T.construct_fake Dragon 1 in
  let r1 = P.ron_info_constructor [[t1;t2;t3]] [] [[t4;t5;t6;t6];[t7;t8;t9;t9];[t10;t11;t12;t13]] [t14;t15] in
  assert_equal (P.san_an_ke r1) true

let has_tui_tui_test _ = 
  let t1 = T.construct_fake Man 1 in
  let t2 = T.construct_fake Man 1 in
  let t3 = T.construct_fake Man 1 in
  let t4 = T.construct_fake Pin 1 in
  let t5 = T.construct_fake Pin 1 in
  let t6 = T.construct_fake Pin 1 in
  let t7 = T.construct_fake Suo 1 in
  let t8 = T.construct_fake Suo 1 in
  let t9 = T.construct_fake Suo 1 in
  let t10 = T.construct_fake Man 5 in
  let t11 = T.construct_fake Man 5 in
  let t12 = T.construct_fake Man 5 in
  let t13 = T.construct_fake Man 5 in
  let t14 = T.construct_fake Man 9 in
  let t15 = T.construct_fake Man 9 in 
  let r1 = P.ron_info_constructor [] [[t1;t2;t3];[t4;t5;t6];[t7;t8;t9]] [[t10;t11;t12;t13]] [t14;t15] in
  assert_equal (P.has_tuitui r1) true

let has_dai_san_gan_test _ = 
  let t1 = T.construct_fake Dragon 1 in
  let t2 = T.construct_fake Dragon 1 in
  let t3 = T.construct_fake Dragon 1 in
  let t4 = T.construct_fake Dragon 2 in
  let t5 = T.construct_fake Dragon 2 in
  let t6 = T.construct_fake Dragon 2 in
  let t7 = T.construct_fake Suo 1 in
  let t8 = T.construct_fake Suo 1 in
  let t9 = T.construct_fake Suo 1 in
  let t10 = T.construct_fake Dragon 3 in
  let t11 = T.construct_fake Dragon 3 in
  let t12 = T.construct_fake Dragon 3 in
  let t13 = T.construct_fake Dragon 3 in
  let t14 = T.construct_fake Man 9 in
  let t15 = T.construct_fake Man 9 in
  let r1 = P.ron_info_constructor [] [[t1;t2;t3];[t4;t5;t6];[t7;t8;t9]] [[t10;t11;t12;t13]] [t14;t15] in
  assert_equal (P.has_dai_san_gan r1) true 
  


let is_qing_lao_tou_test _ = 
  let t1 = T.construct_fake Man 1 in
  let t2 = T.construct_fake Man 1 in
  let t3 = T.construct_fake Man 1 in
  let t4 = T.construct_fake Pin 1 in
  let t5 = T.construct_fake Pin 1 in
  let t6 = T.construct_fake Pin 1 in
  let t7 = T.construct_fake Suo 1 in
  let t8 = T.construct_fake Suo 1 in
  let t9 = T.construct_fake Suo 1 in
  let t10 = T.construct_fake Pin 9 in
  let t11 = T.construct_fake Pin 9 in
  let t12 = T.construct_fake Pin 9 in
  let t13 = T.construct_fake Pin 9 in
  let t14 = T.construct_fake Man 9 in
  let t15 = T.construct_fake Man 9 in
  let r1 = P.ron_info_constructor [] [[t1;t2;t3];[t4;t5;t6];[t7;t8;t9]] [[t10;t11;t12;t13]] [t14;t15] in
  assert_equal (P.is_qing_lao_tou r1) true

let is_Xiao_su_xi_test _ = 
  let t1 = T.construct_fake Wind 1 in
  let t2 = T.construct_fake Wind 1 in
  let t3 = T.construct_fake Wind 1 in
  let t4 = T.construct_fake Wind 2 in
  let t5 = T.construct_fake Wind 2 in
  let t6 = T.construct_fake Wind 1 in
  let t7 = T.construct_fake Suo 1 in
  let t8 = T.construct_fake Suo 1 in
  let t9 = T.construct_fake Suo 1 in
  let t10 = T.construct_fake Wind 3 in
  let t11 = T.construct_fake Wind 3 in
  let t12 = T.construct_fake Wind 3 in
  let t13 = T.construct_fake Wind 3 in
  let t14 = T.construct_fake Wind 4 in
  let t15 = T.construct_fake Wind 4 in
  let r1 = P.ron_info_constructor [] [[t1;t2;t3];[t4;t5;t6];[t7;t8;t9]] [[t10;t11;t12;t13]] [t14;t15] in
  assert_equal (P.is_Xiao_su_xi r1) true

let is_Dai_su_xi_test _ =
  let t1 = T.construct_fake Wind 1 in
  let t2 = T.construct_fake Wind 1 in
  let t3 = T.construct_fake Wind 1 in
  let t4 = T.construct_fake Wind 2 in
  let t5 = T.construct_fake Wind 2 in
  let t6 = T.construct_fake Wind 2 in
  let t7 = T.construct_fake Wind 4 in
  let t8 = T.construct_fake Wind 4 in
  let t9 = T.construct_fake Wind 4 in
  let t10 = T.construct_fake Wind 3 in
  let t11 = T.construct_fake Wind 3 in
  let t12 = T.construct_fake Wind 3 in
  let t13 = T.construct_fake Wind 3 in
  let t14 = T.construct_fake  Dragon 1 in
  let t15 = T.construct_fake Dragon 1 in
  let r1 = P.ron_info_constructor [] [[t1;t2;t3];[t4;t5;t6];[t7;t8;t9]] [[t10;t11;t12;t13]] [t14;t15] in
  assert_equal (P.is_Dai_su_xi r1) true;
  assert_equal (P.si_an_ke r1) true
  
let san_su_tong_shun_test _ = 
  let t1 = T.construct_fake Man 1 in
  let t2 = T.construct_fake Man 2 in
  let t3 = T.construct_fake Man 3 in
  let t4 = T.construct_fake Pin 1 in
  let t5 = T.construct_fake Pin 2 in
  let t6 = T.construct_fake Pin 3 in
  let t7 = T.construct_fake Suo 1 in
  let t8 = T.construct_fake Suo 2 in
  let t9 = T.construct_fake Suo 3 in
  let t10 = T.construct_fake Wind 3 in
  let t11 = T.construct_fake Wind 3 in
  let t12 = T.construct_fake Wind 3 in
  let t13 = T.construct_fake Wind 3 in
  let t14 = T.construct_fake  Dragon 1 in
  let t15 = T.construct_fake Dragon 1 in
  let r1 = P.ron_info_constructor [[t1;t2;t3];[t4;t5;t6];[t7;t8;t9]] [] [[t10;t11;t12;t13]] [t14;t15] in
  assert_equal (P.san_su_tong_shun r1) true

let san_se_tong_ke_test _ = 
  let t1 = T.construct_fake Man 1 in
  let t2 = T.construct_fake Man 1 in
  let t3 = T.construct_fake Man 1 in
  let t4 = T.construct_fake Pin 1 in
  let t5 = T.construct_fake Pin 1 in
  let t6 = T.construct_fake Pin 1 in
  let t7 = T.construct_fake Suo 1 in
  let t8 = T.construct_fake Suo 1 in
  let t9 = T.construct_fake Suo 1 in
  let t10 = T.construct_fake Wind 3 in
  let t11 = T.construct_fake Wind 3 in
  let t12 = T.construct_fake Wind 3 in
  let t14 = T.construct_fake  Dragon 1 in
  let t15 = T.construct_fake Dragon 1 in
  let r1 = P.ron_info_constructor [] [[t1;t2;t3];[t4;t5;t6];[t7;t8;t9];[t10;t11;t12]] [] [t14;t15] in
  assert_equal (P.san_su_tong_ke r1) true

let yi_cu_test _ = 
  let t1 = T.construct_fake Man 1 in
  let t2 = T.construct_fake Man 2 in
  let t3 = T.construct_fake Man 3 in
  let t4 = T.construct_fake Man 4 in
  let t5 = T.construct_fake Man 5 in
  let t6 = T.construct_fake Man 6 in
  let t7 = T.construct_fake Man 7 in
  let t8 = T.construct_fake Man 8 in
  let t9 = T.construct_fake Man 9 in
  let t10 = T.construct_fake Wind 3 in
  let t11 = T.construct_fake Wind 3 in
  let t12 = T.construct_fake Wind 3 in
  let t13 = T.construct_fake Wind 3 in
  let t14 = T.construct_fake  Dragon 1 in
  let t15 = T.construct_fake Dragon 1 in
  let r1 = P.ron_info_constructor [[t1;t2;t3];[t4;t5;t6];[t7;t8;t9]] [] [[t10;t11;t12;t13]] [t14;t15] in
  assert_equal (P.yi_cu r1) true

let test_han_chan_dai _ = 
  let t1 = T.construct_fake Wind 1 in
  let t2 = T.construct_fake Wind 1 in
  let t3 = T.construct_fake Wind 1 in
  let t4 = T.construct_fake Wind 2 in
  let t5 = T.construct_fake Wind 2 in
  let t6 = T.construct_fake Wind 2 in
  let t7 = T.construct_fake Wind 4 in
  let t8 = T.construct_fake Wind 4 in
  let t9 = T.construct_fake Wind 4 in
  let t10 = T.construct_fake Wind 3 in
  let t11 = T.construct_fake Wind 3 in
  let t12 = T.construct_fake Wind 3 in
  let t13 = T.construct_fake Wind 3 in
  let t14 = T.construct_fake  Dragon 1 in
  let t15 = T.construct_fake Dragon 1 in
  let r1 = P.ron_info_constructor [] [[t1;t2;t3];[t4;t5;t6];[t7;t8;t9]] [[t10;t11;t12;t13]] [t14;t15] in
  assert_equal (P.han_chan_dai r1) true

let test_han_lao_tou _ = 
  let t1 = T.construct_fake Man 1 in
  let t2 = T.construct_fake Man 1 in
  let t3 = T.construct_fake Man 1 in
  let t4 = T.construct_fake Wind 2 in
  let t5 = T.construct_fake Wind 2 in
  let t6 = T.construct_fake Wind 2 in
  let t7 = T.construct_fake Pin 1 in
  let t8 = T.construct_fake Pin 1 in
  let t9 = T.construct_fake Pin 1 in
  let t10 = T.construct_fake Pin 9 in
  let t11 = T.construct_fake Pin 9 in
  let t12 = T.construct_fake Pin 9 in
  let t13 = T.construct_fake Pin 9 in
  let t14 = T.construct_fake  Dragon 1 in
  let t15 = T.construct_fake Dragon 1 in
  let r1 = P.ron_info_constructor [] [[t1;t2;t3];[t4;t5;t6];[t7;t8;t9]] [[t10;t11;t12;t13]] [t14;t15] in
  assert_equal (P.han_lao_tou r1) true

let test_xio_san_gan _ = 
  let t1 = T.construct_fake Dragon 1 in
  let t2 = T.construct_fake Dragon 1 in
  let t3 = T.construct_fake Dragon 1 in
  let t4 = T.construct_fake Dragon 2 in
  let t5 = T.construct_fake Dragon 2 in
  let t6 = T.construct_fake Dragon 2 in
  let t7 = T.construct_fake Suo 1 in
  let t8 = T.construct_fake Suo 1 in
  let t9 = T.construct_fake Suo 1 in
  let t10 = T.construct_fake Wind 3 in
  let t11 = T.construct_fake Wind 3 in
  let t12 = T.construct_fake Wind 3 in
  let t13 = T.construct_fake Wind 3 in
  let t14 = T.construct_fake Dragon 3 in
  let t15 = T.construct_fake Dragon 3 in
  let r1 = P.ron_info_constructor [] [[t1;t2;t3];[t4;t5;t6];[t7;t8;t9]] [[t10;t11;t12;t13]] [t14;t15] in
  assert_equal (P.xio_san_gan r1) true

let test_chan_kan _ =
  let t1 = T.construct_fake Man 1 in
  let t2 = T.construct_fake Man 2 in
  let t3 = T.construct_fake Man 3 in
  let t4 = T.construct_fake Man 7 in
  let t5 = T.construct_fake Man 8 in
  let t6 = T.construct_fake Man 9 in
  let t7 = T.construct_fake Suo 1 in
  let t8 = T.construct_fake Suo 2 in
  let t9 = T.construct_fake Suo 3 in
  let t10 = T.construct_fake Suo 9 in
  let t11 = T.construct_fake Suo 9 in
  let t12 = T.construct_fake Suo 9 in
  let t14 = T.construct_fake Suo 1 in
  let t15 = T.construct_fake Suo 1 in
  let r1 = P.ron_info_constructor [[t1;t2;t3];[t4;t5;t6];[t7;t8;t9]] [[t10;t11;t12]] [] [t14;t15] in
  assert_equal (P.chan_kan r1) true

let test_green_yisu _ = 
  let t7 = T.construct_fake Suo 2 in
  let t8 = T.construct_fake Suo 2 in
  let t9 = T.construct_fake Suo 2 in
  let t10 = T.construct_fake Suo 4 in
  let t11 = T.construct_fake Suo 4 in
  let t12 = T.construct_fake Suo 4 in
  let t14 = T.construct_fake Dragon 3 in
  let t15 = T.construct_fake Dragon 3 in
  assert_equal (P.green_yi_su [t7;t8;t9;t10;t11;t12;t14;t15]) true

let cal_yaku_test _ = 
  let t1 = T.construct_fake Dragon 1 in
  let t2 = T.construct_fake Dragon 1 in
  let t3 = T.construct_fake Dragon 1 in
  let t4 = T.construct_fake Dragon 2 in
  let t5 = T.construct_fake Dragon 2 in
  let t6 = T.construct_fake Dragon 2 in
  let t7 = T.class_constructor 0 Suo 1 true false false in
  let t8 = T.class_constructor 0 Suo 1 true false false in
  let t9 = T.class_constructor 0 Suo 1 true false false in
  let t10 = T.construct_fake Wind 3 in
  let t11 = T.construct_fake Wind 3 in
  let t12 = T.construct_fake Wind 3 in
  let t13 = T.construct_fake Wind 3 in
  let t14 = T.construct_fake Dragon 3 in
  let t15 = T.construct_fake Dragon 3 in
  let r1 = P.ron_info_constructor [] [[t1;t2;t3];[t4;t5;t6];[t7;t8;t9]] [[t10;t11;t12;t13]] [t14;t15] in
  let p = P.class_constructor 0 false false [t1;t2;t3;t4;t5;t6;t7;t8;t9;t10;t11;t12;t13;t14;t15] [] in 
  let (_,num) = P.cal_yaku [t1;t2;t3;t4;t5;t6;t7;t8;t9;t10;t11;t12;t13;t14;t15] p [r1] in 
  assert_equal num 10



let ron_cal_yaku_test _ = 
  let t1 = T.construct_fake Dragon 1 in
  let t2 = T.construct_fake Dragon 1 in
  let t3 = T.construct_fake Dragon 1 in
  let t4 = T.construct_fake Dragon 2 in
  let t5 = T.construct_fake Dragon 2 in
  let t6 = T.construct_fake Dragon 2 in
  let t7 = T.construct_fake Suo 1 in
  let t8 = T.construct_fake Suo 1 in
  let t9 = T.construct_fake Suo 1 in
  let t10 = T.construct_fake Dragon 3 in
  let t11 = T.construct_fake Dragon 3 in
  let t12 = T.construct_fake Dragon 3 in
  let t13 = T.construct_fake Dragon 3 in
  let t14 = T.construct_fake Man 9 in
  let t15 = T.construct_fake Man 9 in
  let p = P.class_constructor 0 false false [t1;t2;t3;t4;t5;t6;t7;t8;t9;t10;t11;t12;t13;t14;t15] [] in
  let (_,num) = P.ron_and_cal_yaku [t1;t2;t3;t4;t5;t6;t7;t8;t9;t10;t11;t12;t13;t14;t15] p  in
  assert_equal num 26

let yi_pei_kou_test _ = 
  let t1 = T.construct_fake Man 1 in
  let t2 = T.construct_fake Man 2 in
  let t3 = T.construct_fake Man 3 in
  let t4 = T.construct_fake Man 1 in
  let t5 = T.construct_fake Man 2 in
  let t6 = T.construct_fake Man 3 in
  let t7 = T.construct_fake Suo 1 in
  let t8 = T.construct_fake Suo 1 in
  let t9 = T.construct_fake Suo 1 in
  let t10 = T.construct_fake Dragon 3 in
  let t11 = T.construct_fake Dragon 3 in
  let t12 = T.construct_fake Dragon 3 in
  let t13 = T.construct_fake Dragon 3 in
  let t14 = T.construct_fake Man 9 in
  let t15 = T.construct_fake Man 9 in
  let r1 = P.ron_info_constructor [[t1;t2;t3];[t4;t5;t6]] [[t7;t8;t9]] [[t10;t11;t12;t13]] [t14;t15] in
  assert_equal (P.yi_pei_kou r1) true

let yi_pei_kou_more _ = 
  let t1 = T.construct_fake Man 1 in
  let t2 = T.construct_fake Man 2 in
  let t3 = T.construct_fake Man 3 in
  let t4 = T.construct_fake Man 1 in
  let t5 = T.construct_fake Man 2 in
  let t6 = T.construct_fake Man 3 in
  let t7 = T.construct_fake Suo 1 in
  let t8 = T.construct_fake Suo 2 in
  let t9 = T.construct_fake Suo 3 in
  let t10 = T.construct_fake Dragon 3 in
  let t11 = T.construct_fake Dragon 3 in
  let t12 = T.construct_fake Dragon 3 in
  let t13 = T.construct_fake Dragon 3 in
  let t14 = T.construct_fake Man 9 in
  let t15 = T.construct_fake Man 9 in
  let r1 = P.ron_info_constructor [[t1;t2;t3];[t4;t5;t6];[t7;t8;t9]] [] [[t10;t11;t12;t13]] [t14;t15] in
  assert_equal (P.yi_pei_kou r1) true

let two_pei_kou_test _ = 
  let t1 = T.construct_fake Man 1 in
  let t2 = T.construct_fake Man 2 in
  let t3 = T.construct_fake Man 3 in
  let t4 = T.construct_fake Man 1 in
  let t5 = T.construct_fake Man 2 in
  let t6 = T.construct_fake Man 3 in
  let t7 = T.construct_fake Suo 1 in
  let t8 = T.construct_fake Suo 2 in
  let t9 = T.construct_fake Suo 3 in
  let t10 = T.construct_fake Suo 1 in
  let t11 = T.construct_fake Suo 2 in
  let t12 = T.construct_fake Suo 3 in
  let t14 = T.construct_fake Man 9 in
  let t15 = T.construct_fake Man 9 in
  let r1 = P.ron_info_constructor [[t1;t2;t3];[t4;t5;t6];[t7;t8;t9];[t10;t11;t12]] [] [] [t14;t15] in
  assert_equal (P.two_pei_kou r1) true;
  assert_equal (P.yi_pei_kou r1) true

let yicu_more _ = 
  let t1 = T.construct_fake Man 1 in
  let t2 = T.construct_fake Man 2 in
  let t3 = T.construct_fake Man 3 in
  let t4 = T.construct_fake Man 4 in
  let t5 = T.construct_fake Man 5 in
  let t6 = T.construct_fake Man 6 in
  let t7 = T.construct_fake Man 7 in
  let t8 = T.construct_fake Man 8 in
  let t9 = T.construct_fake Man 9 in
  let t10 = T.construct_fake Suo 1 in
  let t11 = T.construct_fake Suo 2 in
  let t12 = T.construct_fake Suo 3 in
  let t14 = T.construct_fake  Dragon 1 in
  let t15 = T.construct_fake Dragon 1 in
  let r1 = P.ron_info_constructor [[t1;t2;t3];[t4;t5;t6];[t7;t8;t9];[t10;t11;t12]] [] [] [t14;t15] in
  assert_equal (P.yi_cu r1) true

let san_se_more _ = 
  let t1 = T.construct_fake Man 1 in
  let t2 = T.construct_fake Man 2 in
  let t3 = T.construct_fake Man 3 in
  let t4 = T.construct_fake Pin 1 in
  let t5 = T.construct_fake Pin 2 in
  let t6 = T.construct_fake Pin 3 in
  let t7 = T.construct_fake Suo 1 in
  let t8 = T.construct_fake Suo 2 in
  let t9 = T.construct_fake Suo 3 in
  let t10 = T.construct_fake Man 1 in
  let t11 = T.construct_fake Man 2 in
  let t12 = T.construct_fake Man 3 in
  let t14 = T.construct_fake  Dragon 1 in
  let t15 = T.construct_fake Dragon 1 in
  let r1 = P.ron_info_constructor [[t1;t2;t3];[t4;t5;t6];[t7;t8;t9];[t10;t11;t12]] [] [] [t14;t15] in
  assert_equal (P.san_su_tong_shun r1) true

let san_an_ke_test _ = 
  let t1 = T.construct_fake Dragon 1 in
  let t2 = T.construct_fake Dragon 1 in
  let t3 = T.construct_fake Dragon 1 in
  let t4 = T.construct_fake Dragon 2 in
  let t5 = T.construct_fake Dragon 2 in
  let t6 = T.construct_fake Dragon 2 in
  let t7 = T.class_constructor 0 Suo 1 true false false in
  let t8 = T.class_constructor 0 Suo 1 true false false in
  let t9 = T.class_constructor 0 Suo 1 true false false in
  let t10 = T.construct_fake Wind 3 in
  let t11 = T.construct_fake Wind 3 in
  let t12 = T.construct_fake Wind 3 in
  let t14 = T.construct_fake Dragon 3 in
  let t15 = T.construct_fake Dragon 3 in
  let r1 = P.ron_info_constructor [] [[t1;t2;t3];[t4;t5;t6];[t7;t8;t9];[t10;t11;t12]] [] [t14;t15] in
  assert_equal (P.san_an_ke r1) true

let ron_cal_yaku_test_more _ =
  let t1 = T.construct_fake Dragon 1 in
  let t2 = T.construct_fake Dragon 1 in
  let t3 = T.construct_fake Dragon 1 in
  let t4 = T.construct_fake Dragon 2 in
  let t5 = T.construct_fake Dragon 2 in
  let t6 = T.construct_fake Dragon 2 in
  let t7 = T.construct_fake Suo 1 in
  let t8 = T.construct_fake Suo 1 in
  let t9 = T.construct_fake Suo 2 in
  let t10 = T.construct_fake Dragon 3 in
  let t11 = T.construct_fake Dragon 3 in
  let t12 = T.construct_fake Dragon 3 in
  let t13 = T.construct_fake Dragon 3 in
  let t14 = T.construct_fake Man 9 in
  let t15 = T.construct_fake Man 9 in
  let p = P.class_constructor 0 false false [t1;t2;t3;t4;t5;t6;t7;t8;t9;t10;t11;t12;t13;t14;t15] [] in
  let (_,num) = P.ron_and_cal_yaku [t1;t2;t3;t4;t5;t6;t7;t8;t9;t10;t11;t12;t13;t14;t15] p  in
  assert_equal num 0



let riichii_test _ = 
  let t1 = T.construct_fake Dragon 1 in
  let t2 = T.construct_fake Dragon 1 in
  let t3 = T.construct_fake Dragon 1 in
  let t4 = T.construct_fake Dragon 2 in
  let t5 = T.construct_fake Dragon 2 in
  let t6 = T.construct_fake Dragon 2 in
  let t7 = T.class_constructor 0 Suo 1 true false false in
  let t8 = T.class_constructor 0 Suo 1 true false false in
  let t9 = T.class_constructor 0 Suo 1 true false false in
  let t10 = T.construct_fake Wind 3 in
  let t11 = T.construct_fake Wind 3 in
  let t12 = T.construct_fake Wind 3 in
  let t14 = T.construct_fake Dragon 3 in
  let p = P.class_constructor 0 false false [t1;t2;t3;t4;t5;t6;t7;t8;t9;t10;t11;t12;t14] [] in
  let t_list = P.get_riichi p in 
  assert_equal (List.length t_list) 1;
  P.set_riichi p;
  assert_equal (p.richii) true

let update_ron_info_more _ = 
  let t1 = T.construct_fake Dragon 1 in
  let t2 = T.construct_fake Dragon 1 in
  let t3 = T.construct_fake Dragon 1 in
  let t4 = T.construct_fake Dragon 2 in
  let t5 = T.construct_fake Dragon 2 in
  let t6 = T.construct_fake Dragon 2 in
  let t7 = T.construct_fake Dragon 2 in 
  let p = P.class_constructor 0 false false [t1;t2;t3;t4;t5;t6;t7] [] in
  P.update_ron_info p [t1;t2;t3];
  P.update_ron_info p [t4;t5;t6;t7];
  assert_equal (List.length p.r_info.pong_list) 1

let test_san_se_shun_more_more _ = 
  let t1 = T.construct_fake Man 7 in
  let t2 = T.construct_fake Man 8 in
  let t3 = T.construct_fake Man 9 in
  let t4 = T.construct_fake Pin 1 in
  let t5 = T.construct_fake Pin 2 in
  let t6 = T.construct_fake Pin 3 in
  let t7 = T.construct_fake Suo 1 in
  let t8 = T.construct_fake Suo 2 in
  let t9 = T.construct_fake Suo 3 in
  let t10 = T.construct_fake Man 1 in
  let t11 = T.construct_fake Man 2 in
  let t12 = T.construct_fake Man 3 in
  let t14 = T.construct_fake  Dragon 1 in
  let t15 = T.construct_fake Dragon 1 in
  let r1 = P.ron_info_constructor [[t1;t2;t3];[t4;t5;t6];[t7;t8;t9];[t10;t11;t12]] [] [] [t14;t15] in
  let r2 = P.ron_info_constructor [[t4;t5;t6];[t1;t2;t3];[t7;t8;t9];[t10;t11;t12]] [] [] [t14;t15] in
  let r3 = P.ron_info_constructor [[t4;t5;t6];[t7;t8;t9];[t1;t2;t3];[t10;t11;t12]] [] [] [t14;t15] in
  let r4 = P.ron_info_constructor [[t4;t5;t6];[t7;t8;t9];[t10;t11;t12];[t1;t2;t3]] [] [] [t14;t15] in
  assert_equal (P.san_su_tong_shun r1) true;
  assert_equal (P.san_su_tong_shun r2) true;
  assert_equal (P.san_su_tong_shun r3) true;
  assert_equal (P.san_su_tong_shun r4) true

let san_se_tong_ke_test_more _ =
  let t1 = T.construct_fake Man 1 in
  let t2 = T.construct_fake Man 1 in
  let t3 = T.construct_fake Man 1 in
  let t4 = T.construct_fake Pin 1 in
  let t5 = T.construct_fake Pin 1 in
  let t6 = T.construct_fake Pin 1 in
  let t7 = T.construct_fake Suo 1 in
  let t8 = T.construct_fake Suo 1 in
  let t9 = T.construct_fake Suo 1 in
  let t10 = T.construct_fake Wind 3 in
  let t11 = T.construct_fake Wind 3 in
  let t12 = T.construct_fake Wind 3 in
  let t14 = T.construct_fake  Dragon 1 in
  let t15 = T.construct_fake Dragon 1 in
  let r1 = P.ron_info_constructor [] [[t10;t11;t12];[t1;t2;t3];[t4;t5;t6];[t7;t8;t9]] [] [t14;t15] in
  let r2 = P.ron_info_constructor [] [[t1;t2;t3];[t10;t11;t12];[t4;t5;t6];[t7;t8;t9]] [] [t14;t15] in
  let r3 = P.ron_info_constructor [] [[t1;t2;t3];[t4;t5;t6];[t10;t11;t12];[t7;t8;t9]] [] [t14;t15] in
  assert_equal (P.san_su_tong_ke r1) true;
  assert_equal (P.san_su_tong_ke r2) true;
  assert_equal (P.san_su_tong_ke r3) true

let test_xio_san_gan_moto _ =
  let t1 = T.construct_fake Dragon 1 in
  let t2 = T.construct_fake Dragon 1 in
  let t3 = T.construct_fake Dragon 1 in
  let t33 = T.construct_fake Dragon 1 in 
  let t4 = T.construct_fake Dragon 3 in
  let t5 = T.construct_fake Dragon 3 in
  let t6 = T.construct_fake Dragon 3 in
  let t66 = T.construct_fake Dragon 3 in
  let t7 = T.construct_fake Suo 1 in
  let t8 = T.construct_fake Suo 1 in
  let t9 = T.construct_fake Suo 1 in
  let t10 = T.construct_fake Wind 3 in
  let t11 = T.construct_fake Wind 3 in
  let t12 = T.construct_fake Wind 3 in
  let t13 = T.construct_fake Wind 3 in
  let t14 = T.construct_fake Dragon 2 in
  let t15 = T.construct_fake Dragon 2 in
  let r1 = P.ron_info_constructor [] [[t7;t8;t9]] [[t1;t2;t3;t33];[t4;t5;t6;t66];[t10;t11;t12;t13]] [t14;t15] in
  assert_equal (P.xio_san_gan r1) true

let test_ron_and_cal_yaku_more _ = 
  let t1 = T.construct_fake Man 1 in
  let t2 = T.construct_fake Man 1 in
  let t3 = T.construct_fake Man 1 in
  let t4 = T.construct_fake Man 2 in
  let t5 = T.construct_fake Man 2 in
  let t6 = T.construct_fake Man 2 in
  let t7 = T.construct_fake Man 3 in
  let t8 = T.construct_fake Man 3 in
  let t9 = T.construct_fake Man 3 in
  let t10 = T.class_constructor 0 Man 4 true false true in
  let t11 = T.class_constructor 0 Man 4 true false true in
  let t12 = T.class_constructor 0 Man 4 true false true in
  let t14 = T.construct_fake Pin 1 in
  let t15 = T.construct_fake Pin 1 in
  let p = P.class_constructor 0 false false [t1;t2;t3;t4;t5;t6;t7;t8;t9;t10;t11;t12;t14;t15] [] in
  let (_,num) = P.ron_and_cal_yaku [t1;t2;t3;t4;t5;t6;t7;t8;t9;t10;t11;t12;t14;t15] p  in
  assert_equal num 4

let section1_tests = 
  "Section 1" >: test_list [
     "tile constructor">::test_constructor;
     "fulu" >:: test_fulu;
     "non yao" >:: test_check_non_yao;
     "check shun" >:: test_check_shun;
     "check equal" >:: test_check_equal;
     "check sort same kind">:: test_sort_same_kind;
     "check sort hand">:: test_sort_hand;
     "check delete all tile">:: test_delete_all_tile;
     "check delete one tile">:: test_delete_a_tile;
     "check get all shun">:: test_get_all_shun;
     "check get pong">:: test_get_pong;
     "check get kang">:: test_get_kang;
     "check get all action">::test_get_all_ways;
     "get num test">:: get_num_test;
     "generate_game_tiles test">:: generate_game_tiles_test;
     "check tiles test">:: check_tiles_test;
     "contain lao tou list test">:: contain_lao_tou_list_test;
     "same kind across shun teset">:: same_kind_across_shun_test;
     "is wind dragon test">:: is_wind_dragon_test;
     "contain yao list">:: contain_yao_list_and_other_funs_test;
     "test read str">:: test_read_str;
     "init game">:: initialize_game;
     ]

let player_tests = 
  "Player test" >: test_list [
     "check ron">:: test_check_ron;
     "stress check ron">:: stresstest_check_ron;
     "set fulu test">:: set_fulu_test;
     "get all fulu">:: get_all_fulu_test;
     "turn draw tile test">:: turn_draw_tile_test;
     "play tile test">:: play_tile_test;
     "test user chii pong kan">:: test_user_chii_pong_kan;
     "test check shun list ">:: test_check_shun_list;
     "test check pair ">:: test_check_pair;
     "test update ron info">:: test_update_ron_info;
     "test concat two ron info ">:: test_concat_two_ron_info;
     "test tanyao check">:: tanyao_check_test;
     "test hun yisu">:: hun_yisu_check_test;
     "test qing yisu">:: qing_yisu_test;
     "test zi yisu ">:: zi_yisu_test;
     "dragon triplet">:: dragon_triplet_test;
     "check pin fu">:: check_pin_fu;
     "tui tui">:: has_tui_tui_test;
     "dai san gan">:: has_dai_san_gan_test;
     "qing lao tou">:: is_qing_lao_tou_test;
     "xiao su xi">:: is_Xiao_su_xi_test;
     "dai su xi">::is_Dai_su_xi_test;
     "san su tong shun">:: san_su_tong_shun_test;
     "san se tong ke ">:: san_se_tong_ke_test;
     "yi cu">:: yi_cu_test;
     "han chan">:: test_han_chan_dai;
     "han lao tou">:: test_han_lao_tou;
     "xio san gan">:: test_xio_san_gan;
     "chan kan">:: test_chan_kan;
     "green yisu">:: test_green_yisu;
     "yaku test">:: cal_yaku_test;
     "ron test">:: ron_cal_yaku_test;
     "yi pei kou">:: yi_pei_kou_test;
     "yi pei kou more">:: yi_pei_kou_more;
     "two pei kou test">:: two_pei_kou_test;
     "yicu more">:: yicu_more;
     "san se more">::san_se_more;
     "san an ke">:: san_an_ke_test;
     "pin fu false">:: pin_fu_false;
     "dai san gan false">::dai_san_gan_false;
     "dai san gan test more">::has_dai_san_gan_test_more;
     "has tui tui false">:: has_tui_tui_false;
     "xiao su xi more">:: is_Xiao_su_xi_test_more;
     "xiao su xi">:: is_Xiao_su_xi_test_more1;
     "dai su xi more">:: is_Dai_su_xi_test_more;
     "san an ke more">:: san_an_ke_test_more;
     "ron cal yaku test more">:: ron_cal_yaku_test_more;
     "riichi test">:: riichii_test;
     "update ron info more">:: update_ron_info_more;
     "test san se shun more more">:: test_san_se_shun_more_more;
     "test san se tong ke more ">::san_se_tong_ke_test_more;
     "test xio san gan more">:: test_xio_san_gan_moto;
     "test ron and cal yaku">:: test_ron_and_cal_yaku_more;
     ]

let series = 
  "Assignment4 Test">:::[
      section1_tests;
      player_tests;
      ]
let () =
  run_test_tt_main series


