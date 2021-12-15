open Core;;
open OUnit2;;

module T = Tile
module P = Player   


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
  assert_equal (T.check_non_yao tile1) false

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
  let (ron_tf,this) = P.check_ron [tile1;tile2;tile3;tile4;tile5;tile6;tile7;tile8] in
  P.print_ron_info_list this;
  assert_equal ron_tf true;

  let tile81 = T.class_constructor 0 Man 1 false false false in
  let tile82 = T.class_constructor 0 Man 1 false false false in
  let tile83 = T.class_constructor 0 Man 2 false false false in
  let tile84 = T.class_constructor 0 Man 2 false false false in
  let tile85 = T.class_constructor 0 Man 3 false false false in
  let tile86 = T.class_constructor 0 Man 3 false false false in
  let tile87 = T.class_constructor 0 Man 4 false false false in
  let tile88 = T.class_constructor 0 Man 4 false false false in
  let (eron_tf,ethis) = P.check_ron [tile81;tile82;tile83;tile84;tile85;tile86;tile87;tile88] in
  P.print_ron_info_list ethis; 
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
  print_endline( string_of_int (List.length t1));
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
  let t_list2 = [t1;t2] in 
  assert_equal (P.check_pair t_list t_list1) true;
  assert_equal (P.check_pair t_list t_list2) false

  


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
     
     ]

let series = 
  "Assignment4 Test">:::[
      section1_tests;
      player_tests;
      ]
let () =
  run_test_tt_main series


