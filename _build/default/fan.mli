


module type fan = sig 
  (*each of the functions in fan takes in a list of tiles, which are represented as strings and to check if the hand satisfies the requirements of this fan. There are many, many of fans but a couple of examples are given *)
  val tan_yao : String list -> bool
  val chin_yisu : String list -> bool
  val hun_yisu : String list -> bool
  val dai_sanyan : String list -> bool
end
