open RangeSet

let%test_module _= (module struct
  module Int=
    struct
      type t= int
      let compare= compare
      let to_string= string_of_int
      let pred n= n - 1
      let succ n= n + 1
    end

  module IS = Make_Discrete(Int)

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

  let s5= IS.of_ranges [
    { start= Exc 1; stop= Exc 4 };
    { start= Exc 5; stop= Inc 7 } ;
    { start= Inc 8; stop= Exc 10 };
    { start= Inc 11; stop= Inc 13 };
    ]

  let%expect_test "iter_elt"=
    IS.iter_elt (Printf.printf " %d") s5;
    [%expect "2 3 6 7 8 9 11 12 13"]

  let%expect_test "map_elt"=
    s5
      |>IS.map_elt (fun v->
        let r= v * 2 in
        if r > 20 then 20
        else r)
      |> IS.iter_elt (Printf.printf " %d");
    [%expect "4 6 12 14 16 18 20"]

  let%expect_test "elements"= IS.elements s5
    |> List.iter (Printf.printf " %d");
    [%expect "2 3 6 7 8 9 11 12 13"]
    
end)

