(*
 * test.ml
 * -----------
 * Copyright : (c) 2018 - 2023, ZAN DoYe <zandoye@gmail.com>
 * Licence   : MIT
 *
 * This file is a part of rangeSet.
 *)


open RangeSet

let%test_module _= (module struct
  module Int=
    struct
      type t= int
      let compare= compare
      let to_string= string_of_int
    end

  module IS = Continuous.Make(Int)

  let s1= IS.of_point (Exc 1) (Exc 7)
  let s2= IS.of_point (Inc 1) (Inc 7)

  let%test "s1 mem 1"= not (IS.mem 0 s1)
  let%test "s1 mem 2"= not (IS.mem 1 s1)
  let%test "s1 mem 3"= not (IS.mem 7 s1)
  let%test "s1 mem 4"= not (IS.mem 8 s1)

  let%test "s2 mem 1"= not (IS.mem 0 s2)
  let%test "s2 mem 2"= IS.mem 1 s2
  let%test "s2 mem 3"= IS.mem 7 s2
  let%test "s2 mem 4"= not (IS.mem 8 s2)

  let s_add= IS.empty
  let s_add= IS.add 1 s_add
  let%expect_test "add 1"= s_add
    |> IS.to_string |> print_endline;
    [%expect "[ (Inc 1, Inc 1) ]"]
  let s_add= IS.add 2 s_add
  let%expect_test "add 2"= s_add
    |> IS.to_string |> print_endline;
    [%expect "[ (Inc 1, Inc 1); (Inc 2, Inc 2) ]"]
  let%expect_test "add 3"= IS.of_point (Inc 1) (Exc 2) |> IS.add 2
    |> IS.to_string |> print_endline;
    [%expect "[ (Inc 1, Inc 2) ]"]
  let%expect_test "add 4"= IS.of_point (Exc 1) (Inc 2) |> IS.add 1
    |> IS.to_string |> print_endline;
    [%expect "[ (Inc 1, Inc 2) ]"]
  let%expect_test "add 5"=
    IS.union
      (IS.of_point (Exc 1) (Exc 2))
      (IS.of_point (Exc 2) (Exc 3))
      |> IS.add 2
      |> IS.to_string |> print_endline;
    [%expect "[ (Exc 1, Exc 3) ]"]

  let s_remove= IS.of_point (Inc 1) (Inc 3)
  let s_remove= IS.remove 1 s_remove
  let%expect_test "remove 1"= 
    s_remove |> IS.to_string |> print_endline;
    [%expect "[ (Exc 1, Inc 3) ]"]
  let s_remove= IS.remove 3 s_remove
  let%expect_test "remove 2"= 
    s_remove |> IS.to_string |> print_endline;
    [%expect "[ (Exc 1, Exc 3) ]"]
  let s_remove= IS.remove 2 s_remove
  let%expect_test "remove 3"= 
    s_remove |> IS.to_string |> print_endline;
    [%expect "[ (Exc 1, Exc 2); (Exc 2, Exc 3) ]"]
  let s_remove= IS.remove 10 s_remove
  let%expect_test "remove 4"= 
    s_remove |> IS.to_string |> print_endline;
    [%expect "[ (Exc 1, Exc 2); (Exc 2, Exc 3) ]"]

  let%expect_test "of_point 1"= IS.of_point (Exc 1) (Inc 1)
    |> IS.to_string |> print_endline;
    [%expect "[  ]"]
  let%expect_test "of_point 2"= IS.of_point (Inc 1) (Exc 1)
    |> IS.to_string |> print_endline;
    [%expect "[  ]"]
  let%expect_test "of_point 3"= IS.of_point (Inc 2) (Inc 1)
    |> IS.to_string |> print_endline;
    [%expect "[  ]"]
  let%expect_test "of_point 4"= IS.of_point (Inc 1) (Inc 1)
    |> IS.to_string |> print_endline;
    [%expect "[ (Inc 1, Inc 1) ]"]
  let%expect_test "of_point 5"= IS.of_point (Inc 1) (Inc 2)
    |> IS.to_string |> print_endline;
    [%expect "[ (Inc 1, Inc 2) ]"]

  let%expect_test "of_range 1"= IS.of_range { start= Exc 1; stop= Inc 1 }
    |> IS.to_string |> print_endline;
    [%expect "[  ]"]
  let%expect_test "of_range 2"= IS.of_range { start= Inc 1; stop= Exc 1 }
    |> IS.to_string |> print_endline;
    [%expect "[  ]"]
  let%expect_test "of_range 3"= IS.of_range { start= Exc 1; stop= Exc 1 }
    |> IS.to_string |> print_endline;
    [%expect "[  ]"]
  let%expect_test "of_range 1"= IS.of_range { start= Inc 1; stop= Inc 1 }
    |> IS.to_string |> print_endline;
    [%expect "[ (Inc 1, Inc 1) ]"]
  let%expect_test "of_range 1"= IS.of_range { start= Inc 1; stop= Inc 2 }
    |> IS.to_string |> print_endline;
    [%expect "[ (Inc 1, Inc 2) ]"]

  let%expect_test "of_ranges 1"= IS.of_ranges [
    { start= Exc 1; stop= Exc 2 };
    { start= Inc 1; stop= Inc 2 }; ]
    |> IS.to_string |> print_endline;
    [%expect "[ (Inc 1, Inc 2) ]"]
  let%expect_test "of_ranges 2"= IS.of_ranges [
    { start= Exc 1; stop= Exc 2 };
    { start= Inc 2; stop= Inc 3 }; ]
    |> IS.to_string |> print_endline;
    [%expect "[ (Exc 1, Inc 3) ]"]
  let%expect_test "of_ranges 3"= IS.of_ranges [
    { start= Exc 2; stop= Exc 3 };
    { start= Inc 1; stop= Inc 2 }; ]
    |> IS.to_string |> print_endline;
    [%expect "[ (Inc 1, Exc 3) ]"]
  let%expect_test "of_ranges 4"= IS.of_ranges [
    { start= Exc 1; stop= Exc 3 };
    { start= Exc 7; stop= Exc 9 };
    { start= Exc 2; stop= Exc 8 }; ]
    |> IS.to_string |> print_endline;
    [%expect "[ (Exc 1, Exc 9) ]"]


  let s3= IS.of_ranges [
    { start= Exc 1; stop= Inc 4 };
    { start= Inc 7; stop= Exc 9 } ;
    { start= Inc 13; stop= Exc 15 };
    { start= Exc 19; stop= Inc 21 }; ]

  let%expect_test "union"=
    let test s1 s2=
      IS.union s1 s2
        |> IS.to_string |> print_endline;
      [%expect "[ (Inc -5, Inc -4); (Exc 1, Inc 4); (Inc 7, Inc 10); (Inc 13, Inc 21) ]"]
    in
    let s4= IS.of_ranges [
      { start= Inc (-5); stop= Inc (-4) };
      { start= Inc 2; stop= Inc 3 };
      { start= Inc 9; stop= Inc 10 };
      { start= Inc 15; stop= Inc 19 }; ]
    in
    test s3 s4;
    test s4 s3

  let%expect_test "unmerge1"=
    IS.unmerge { start= Exc 7; stop= Inc 8 } s3
    |> IS.to_string |> print_endline;
    [%expect "[ (Exc 1, Inc 4); (Inc 7, Inc 7); (Exc 8, Exc 9); (Inc 13, Exc 15); (Exc 19, Inc 21) ]"]
  let%expect_test "unmerge2"=
    IS.unmerge { start= Inc 6; stop= Inc 16 } s3
    |> IS.to_string |> print_endline;
    [%expect "[ (Exc 1, Inc 4); (Exc 19, Inc 21) ]"]
  let%expect_test "unmerge3"=
    IS.unmerge { start= Inc 8; stop= Inc 14 } s3
      |> IS.to_string |> print_endline;
    [%expect "[ (Exc 1, Inc 4); (Inc 7, Exc 8); (Exc 14, Exc 15); (Exc 19, Inc 21) ]"]

  let%expect_test "diff"=
    let s4= IS.of_ranges [
      { start= Inc 0; stop= Inc 4 };
      { start= Exc 7; stop= Inc 8 };
      { start= Inc 9; stop= Exc 10 };
      { start= Inc 12; stop= Exc 14 };
      ]
    in
    IS.diff s3 s4 |> IS.to_string |> print_endline;
    [%expect "[ (Inc 7, Inc 7); (Exc 8, Exc 9); (Inc 14, Exc 15); (Exc 19, Inc 21) ]"]

  let%test "inter1"= IS.inter s1 s2 = s1
  let%test "inter2"= IS.inter s2 s1 = s1
  let%expect_test "inter3"=
    let test s1 s2= IS.inter s1 s2
      |> IS.to_string |> print_endline;
      [%expect "[ (Exc 1, Inc 4); (Exc 7, Inc 8); (Inc 13, Exc 14) ]"]
    in
    let s4= IS.of_ranges [
      { start= Inc 0; stop= Inc 4 };
      { start= Exc 7; stop= Inc 8 };
      { start= Inc 9; stop= Exc 10 };
      { start= Inc 12; stop= Exc 14 };
      ]
    in
    test s3 s4;
    test s4 s3
end)

let%test_module _= (module struct
  module Int=
    struct
      type t= int
      let compare= compare
      let to_string= string_of_int
      let pred n= n - 1
      let succ n= n + 1
    end

  module IS = Discrete.Make(Int)

  let s1= IS.of_point 1 7
  let s2= IS.of_point 2 8

  let%test "s1 mem 1"= not (IS.mem 0 s1)
  let%test "s1 mem 2"= not (IS.mem 8 s1)

  let%test "s2 mem 1"= not (IS.mem 0 s2)
  let%test "s2 mem 2"= not (IS.mem 1 s2)
  let%test "s2 mem 3"= IS.mem 7 s2
  let%test "s2 mem 4"= IS.mem 8 s2

  let s_add= IS.empty
  let s_add= IS.add 1 s_add
  let%expect_test "add 1"= s_add
    |> IS.to_string |> print_endline;
    [%expect "[ (1, 1) ]"]
  let s_add= IS.add 2 s_add
  let%expect_test "add 2"= s_add
    |> IS.to_string |> print_endline;
    [%expect "[ (1, 2) ]"]
  let%expect_test "add 3"= IS.of_point 1 2 |> IS.add 2
    |> IS.to_string |> print_endline;
    [%expect "[ (1, 2) ]"]
  let%expect_test "add 4"= IS.of_point 1 2 |> IS.add 3
    |> IS.to_string |> print_endline;
    [%expect "[ (1, 3) ]"]
  let%expect_test "add 5"= IS.of_point 1 2 |> IS.add 4
    |> IS.to_string |> print_endline;
    [%expect "[ (1, 2); (4, 4) ]"]
  let%expect_test "add 6"=
    IS.union
      (IS.of_point 1 2)
      (IS.of_point 4 5)
      |> IS.add 3
      |> IS.to_string |> print_endline;
    [%expect "[ (1, 5) ]"]

  let s_remove= IS.of_point 1 3
  let s_remove= IS.remove 1 s_remove
  let%expect_test "remove 1"= 
    s_remove |> IS.to_string |> print_endline;
    [%expect "[ (2, 3) ]"]
  let s_remove= IS.remove 3 s_remove
  let%expect_test "remove 2"= 
    s_remove |> IS.to_string |> print_endline;
    [%expect "[ (2, 2) ]"]
  let s_remove= IS.remove 2 s_remove
  let%expect_test "remove 3"= 
    s_remove |> IS.to_string |> print_endline;
    [%expect "[  ]"]
  let s_remove= IS.remove 10 s_remove
  let%expect_test "remove 4"= 
    s_remove |> IS.to_string |> print_endline;
    [%expect "[  ]"]


  let s3= IS.of_ranges [
    { start= 1; stop= 4 };
    { start= 7; stop= 9 } ;
    { start= 13; stop= 15 };
    { start= 19; stop= 21 };
    { start= 25; stop= 35 };
    ]

  let%expect_test "union"=
    let test s1 s2=
      IS.union s1 s2
        |> IS.to_string |> print_endline;
      [%expect "[ (-5, -4); (1, 4); (7, 10); (13, 21); (25, 35) ]"]
    in
    let s4= IS.of_ranges [
      { start= -5; stop= -4 };
      { start= 2; stop= 3 };
      { start= 9; stop= 10 };
      { start= 15; stop= 19 }; ]
    in
    test s3 s4;
    test s4 s3

  let%expect_test "unmerge1"=
    IS.unmerge { start= 7; stop= 8 } s3
    |> IS.to_string |> print_endline;
    [%expect "[ (1, 4); (9, 9); (13, 15); (19, 21); (25, 35) ]"]
  let%expect_test "unmerge2"=
    IS.unmerge { start= 6; stop= 16 } s3
    |> IS.to_string |> print_endline;
    [%expect "[ (1, 4); (19, 21); (25, 35) ]"]
  let%expect_test "unmerge3"=
    IS.unmerge { start= 8; stop= 14 } s3
      |> IS.to_string |> print_endline;
    [%expect "[ (1, 4); (7, 7); (15, 15); (19, 21); (25, 35) ]"]
  let%expect_test "unmerge3"=
    IS.unmerge { start= 29; stop= 31 } s3
      |> IS.to_string |> print_endline;
    [%expect "[ (1, 4); (7, 9); (13, 15); (19, 21); (25, 28); (32, 35) ]"]

  let s3= IS.of_ranges [
    { start= 1; stop= 4 };
    { start= 7; stop= 10 } ;
    { start= 13; stop= 15 };
    { start= 19; stop= 21 };
    { start= 25; stop= 35 };
    ]

  let%expect_test "diff"=
    let s4= IS.of_ranges [
      { start= 0; stop= 2 };
      { start= 9; stop= 11 };
      { start= 12; stop= 23 };
      { start= 28; stop= 32 };
      ]
    in
    IS.diff s3 s4 |> IS.to_string |> print_endline;
    [%expect "[ (3, 4); (7, 8); (25, 27); (33, 35) ]"]

  let%test "inter1"= IS.inter s1 s2 = (IS.of_range {start=2; stop=7} )
  let%test "inter2"= IS.inter s2 s1 = (IS.of_range {start=2; stop=7} )
  let%expect_test "inter3"=
    let test s1 s2= IS.inter s1 s2
      |> IS.to_string |> print_endline;
      [%expect "[ (1, 4); (7, 10); (13, 14) ]"]
    in
    let s4= IS.of_ranges [
      { start= 0; stop= 4 };
      { start= 7; stop= 8 };
      { start= 9; stop= 10 };
      { start= 12; stop= 14 };
      ]
    in
    test s3 s4;
    test s4 s3

  let s5= IS.of_ranges [
    { start= 1; stop= 3 };
    { start= 5; stop= 7 } ;
    { start= 11; stop= 13 };
    ]

  let%expect_test "iter_elt"=
    IS.iter_elt (Printf.printf " %d") s5;
    [%expect "1 2 3 5 6 7 11 12 13"]

  let%expect_test "map_elt"=
    let s6= s5
      |>IS.map_elt (fun v->
        let r= v * 2 in
        match r with
        | 22-> 22
        | 24-> 23
        | 26-> 24
        | _-> r)
    in
    s6 |> IS.iter_elt (Printf.printf " %d");
    [%expect "2 4 6 10 12 14 22 23 24"];
    s6 |> IS.to_string |> print_endline;
    [%expect "[ (2, 2); (4, 4); (6, 6); (10, 10); (12, 12); (14, 14); (22, 24) ]"]

  let%expect_test "elements"= IS.elements s5
    |> List.iter (Printf.printf " %d");
    [%expect "1 2 3 5 6 7 11 12 13"]
end)

