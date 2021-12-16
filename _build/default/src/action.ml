open Core;;

type action = 
  | Chii of  int
  | Discard of (Tile.kind * int)
  | Skip
  | Quit
  
exception Error
            
[@@@coverage off]
let read_tile (xs:string list) = 
  match xs with 
  | x::xy -> if (List.length xy = 1) then (
               if (String.equal x "Man") then (Tile.Man,int_of_string (List.nth_exn xy 0))
               else if (String.equal x "Pin") then (Tile.Pin,int_of_string (List.nth_exn xy 0))
               else if (String.equal x "Suo") then (Tile.Suo,int_of_string (List.nth_exn xy 0))
               else if (String.equal x "Wind") then (Tile.Wind,int_of_string (List.nth_exn xy 0))
               else if (String.equal x "Dragon") then (Tile.Dragon,int_of_string (List.nth_exn xy 0))
               else raise Error
             ) 
             else raise Error
  | _ -> failwith("how")
[@@@coverage on]
let read_str (s:string) = 
  let split_string = String.split_on_chars s ~on:[' ';'\n';'\t'] in
  let non_empty = List.filter split_string ~f:(fun x -> not (String.equal x "")) in 
  match non_empty with 
  | x::xs -> if (String.equal x "discard") && (List.length xs = 2)
             then Discard (read_tile xs)     
             else if (String.equal x "chii") && (List.length xs = 1) 
             then Chii (int_of_string (List.nth_exn xs 0))
             else if (String.equal x "skip") then Skip
             else if (String.equal x "quit") then Quit
             else raise Error
  | _ -> raise Error
           

