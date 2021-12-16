(lang dune 2.9)

(generate_opam_files true)
(authors "Alex Zhang")
(maintainers "alexzhang802@gmail.com")
(package
 (name two_people_mahjong) 
 (synopsis "A simplified and altered version of riichi mahjong, made for two players.")
 (description "This project includes most of the traditional mahjong features, including riichi, yaku, 
 point system, dora, etc. However, some of the rules are altered in order to make the game go faster. For example,
 you can riichi even when you have fulu.")
 (depends
  (ounit2 (> 2.0)) ; 
  (core (> 0.14.1)) ; 
))
