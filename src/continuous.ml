(*
 * continuous.ml
 * -----------
 * Copyright : (c) 2018 - 2023, ZAN DoYe <zandoye@gmail.com>
 * Licence   : MIT
 *
 * This file is a part of rangeSet.
 *)


module type OrderedType =
  sig
    include Set.OrderedType
    val to_string: t -> string
  end

module type S =
  sig
    type elt
    type t
    type point= Inc of elt | Exc of elt
    type range= { start: point; stop: point }

    val point_to_string : point -> string
    val range_to_string : range -> string
    val to_string : t -> string
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val of_point : point -> point -> t
    val of_range : range -> t
    val of_ranges : range list -> t
    val remove : elt -> t -> t
    val merge : range -> t -> t
    val unmerge : range -> t -> t
    val cover : range -> t -> t
    val union : t -> t -> t
    val diff : t -> t -> t
    val inter : t -> t -> t
  end

module Make(Ord: OrderedType)=
  struct
    type elt= Ord.t
    type point= Inc of elt | Exc of elt
    type range= { start: point; stop: point }

    let point_to_string= function
      | Inc elt-> "Inc " ^ (Ord.to_string elt)
      | Exc elt-> "Exc " ^ (Ord.to_string elt)

    let range_to_string range=
      Printf.sprintf "(%s, %s)"
        (point_to_string range.start)
        (point_to_string range.stop)

    let extract_point= function
      | Inc p-> p
      | Exc p-> p

    let comp_point= function
      | Inc p-> Exc p
      | Exc p-> Inc p

    let compare_point p1 p2=
      (* compare for order *)
      match p1 with
      | Inc p1->
        (match p2 with
        | Inc p2-> Ord.compare p1 p2
        | Exc p2->
          let c= Ord.compare p1 p2 in
          if c = 0 then -1
          else c)
      | Exc p1->
        (match p2 with
        | Inc p2->
          let c= Ord.compare p1 p2 in
          if c = 0 then 1
          else c
        | Exc p2-> Ord.compare p1 p2)

    let compare_point_rev p1 p2=
      match p1 with
      | Inc p1->
        (match p2 with
        | Inc p2-> Ord.compare p1 p2
        | Exc p2->
          let c= Ord.compare p1 p2 in
          if c = 0 then 1
          else c)
      | Exc p1->
        (match p2 with
        | Inc p2->
          let c= Ord.compare p1 p2 in
          if c = 0 then -1
          else c
        | Exc p2-> Ord.compare p1 p2)


    let compare_point_left p1 p2=
      let exc_compare p1 p2=
        let c= Ord.compare p1 p2 in
        if c = 0 then
          -1 (* p1 is on the left of p2 *)
        else c
      in
      match p1, p2 with
      | Inc p1, Inc p2-> compare p1 p2
      | _-> exc_compare (extract_point p1) (extract_point p2)

    let compare_point_right p1 p2=
      let exc_compare p1 p2=
        let c= Ord.compare p1 p2 in
        if c = 0 then
          1 (* p1 is on the right of p2 *)
        else c
      in
      match p1, p2 with
      | Inc p1, Inc p2-> Ord.compare p1 p2
      | _-> exc_compare (extract_point p1) (extract_point p2)

    let point_include_left p1 p2= compare_point p1 p2 <= 0

    let point_include_right p1 p2= compare_point_rev p1 p2 >= 0

    (* if p1 and p2 close a point *)
    let is_closed p1 p2=
      match p1, p2 with
      | Exc _, Exc _-> false
      | _-> (extract_point p1) = (extract_point p2)

    (* if the points are closed *)
    let _are_closed ps=
      let rec check inc value ps=
        match ps with
        | []-> inc
        | Inc p::tail when p = value-> check true value tail
        | Exc p::tail when p = value-> check inc value tail
        | _-> false
      in
      match ps with
      | []-> false
      | Inc p::tail-> check true p tail
      | Exc p::tail-> check false p tail



    module OrdR = struct
      type t= range
      let compare r1 r2=
        let p1= r1.start
        and p2= r2.start in
        compare_point p1 p2
    end
    module S = struct
      include Set.Make(OrdR)

      let max_elt_opt t=
        try Some (max_elt t)
        with Not_found-> None

      let min_elt_opt t=
        try Some (min_elt t)
        with Not_found-> None
    end
    type t= S.t

    let to_string t= t
      |> S.elements
      |> List.map range_to_string
      |> String.concat "; "
      |> Printf.sprintf "[ %s ]"

    let empty= S.empty
    let is_empty= S.is_empty

    let mem elt t=
      let point= Inc elt in
      let dummy_range= { start= point; stop= point; } in
      let l, v, _r= S.split dummy_range t in
      if v then v
      else
        match S.max_elt_opt l with
        | Some range-> point_include_right range.stop point
        | None-> false

    let add elt t=
      let point= Inc elt in
      let dummy_range= { start= point; stop= point; } in
      let l, v, r= S.split dummy_range t in
      if v then t
      else
        let prev= S.max_elt_opt l
        and next= S.min_elt_opt r in
        let merge_prev=
          match prev with
          | Some prev-> is_closed point prev.stop
          | None-> false
        and merge_next=
          match next with
          | Some next-> is_closed point next.start
          | None-> false
        in
        if merge_prev && merge_next then
          let prev= Option.get prev
          and next= Option.get next in
          t |> S.remove prev
            |> S.remove next
            |> S.add { start= prev.start; stop= next.stop }
        else if merge_prev then
          let prev= Option.get prev in
          t |> S.remove prev
            |> S.add { start= prev.start; stop= point }
        else if merge_next then
          let next= Option.get next in
          t |> S.remove next
            |> S.add { start= point; stop= next.stop }
        else
          t |> S.add dummy_range

    let singleton elt=
      let point= Inc elt in
      let range= { start= point; stop= point; } in
      S.singleton range

    let of_point start stop=
      if compare_point_right start stop <= 0 then
        S.singleton { start; stop }
      else
        empty

    let of_range { start; stop }=
      if compare_point_right start stop <= 0 then
        S.singleton { start; stop }
      else
        empty

    let remove elt t=
      let point= Inc elt in
      let dummy_range= { start= point; stop= point; } in
      let l, v, _r= S.split dummy_range t in
      if v then
        let range= S.find dummy_range t in
        if point_include_right point range.stop then
          t |> S.remove range
        else
          let new_range= { range with start= Exc elt } in
          t |> S.remove range
            |> S.add new_range
      else
        match S.max_elt_opt l with
        | Some range->
          let compare_result= compare_point_right point range.stop in
          if compare_result > 0 then
            t
          else if compare_result = 0 then
            let new_range= { range with stop= Exc elt } in
            t |> S.remove range
              |> S.add new_range
          else
            let new_range_left= { range with stop= Exc elt }
            and new_range_right= { range with start= Exc elt } in
            t |> S.remove range
              |> S.add new_range_left
              |> S.add new_range_right
        | None-> t

    let merge range t=
      let merge_range range1 range2=
        let range1, range2=
          if compare_point range1.start range2.start <= 0
          then range1, range2
          else range2, range1
        in
        if compare_point_right range2.start range1.stop <= 0
          || is_closed range2.start range1.stop
        then
          let start= range1.start
          and stop=
            if point_include_right range2.stop range1.stop
            then range2.stop
            else range1.stop
          in
          Some { start; stop }
        else
          None
      in
      let expand_left range t=
        match S.max_elt_opt t with
        | Some max->
          (match merge_range max range with
          | Some new_range->
            let l, _, _= S.split max t in
            Some (S.add new_range l)
          | None-> None)
        | None-> None
      in
      let rec expand_right ?(flag= false) range t=
        match S.min_elt_opt t with
        | Some min->
          (match merge_range range min with
          | Some new_range->
            let _, _, r= S.split min t in
            expand_right ~flag:true new_range r
          | None->
            if flag
            then Some (S.add range t)
            else None)
        | None->
          if flag
          then Some (S.add range t)
          else None
      in
      let l, v, r= S.split range t in
      let r=
        if v then
          let middle= S.find range t in
          S.add middle r
        else r
      in
      let left= expand_left range l
      and right= expand_right range r in
      match left, right with
      | None, None-> S.add range t
      | Some left, Some right->
        let left_max= S.max_elt left
        and right_min= S.min_elt right in
        let middle= { start= left_max.start; stop= right_min.stop } in
        S.union
          (left |> S.remove left_max)
          (right |> S.remove right_min)
          |> S.add middle
      | Some left, None-> S.union left r
      | None, Some right-> S.union l right

    let unmerge range t=
      let diff r1 r2=
        if compare_point_right r2.start r1.stop > 0
          || compare_point_left r2.stop r1.start < 0
        then false, S.singleton r1
        else if compare_point r2.start r1.start <= 0
          && compare_point_rev r2.stop r1.stop >= 0
        then true, S.empty
        else if compare_point r2.start r1.start > 0
          && compare_point_rev r2.stop r1.stop < 0 then
          true, S.union
            (of_point r1.start (comp_point r2.start))
            (of_point (comp_point r2.stop) r1.stop)
        else if compare_point r2.start r1.start > 0 then
          let stop= comp_point r2.start in
          true, S.singleton { start= r1.start; stop }
        else
          let start= comp_point r2.stop in
          true, S.singleton { start; stop= r1.stop }
      in
      let expand_left range t=
        match S.max_elt_opt t with
        | Some max->
          let l, _, _r= S.split max t in
          let _, s= diff max range in S.union l s
        | None-> S.empty
      in
      let rec expand_right range t=
        match S.min_elt_opt t with
        | Some min->
          let change, rest= diff min range in
          if change then
            let _, _, r= S.split min t in
            expand_right range (S.union rest r)
          else t
        | None-> S.empty
      in
      let l, v, r= S.split range t in
      let middle=
        if v then
          let _, s= diff (S.find range t) range in s
        else
          S.empty
      in
      S.union
        (expand_left range l)
        (expand_right range r)
        |> S.union middle

    let cover range t=
      let cover r1 r2=
        if compare_point_right r2.start r1.stop > 0
          || compare_point_left r2.stop r1.start < 0
          (* they are not intersectant *)
        then false, S.empty
        else if point_include_left r2.start r1.start
          && point_include_right r2.stop r1.stop
          (* r2 covers r1 wholly *)
        then true, S.singleton r1
        else if compare_point r2.start r1.start > 0 then
          (* r2 doesn't cover r1's left side *)
          let stop=
            if compare_point_rev r2.stop r1.stop < 0
            then r2.stop
            else r1.stop
          in
          true, S.singleton { start= r2.start; stop }
        else
          (* r2 doesn't cover r1's right side *)
          let start=
            if compare_point r2.start r1.start > 0
            then r2.start
            else r1.start
          in
          true, S.singleton { start; stop= r2.stop }
      in
      let expand_left range t=
        match S.max_elt_opt t with
        | Some max-> let _, s= cover max range in s
        | None-> S.empty
      in
      let rec expand_right range t=
        match S.min_elt_opt t with
        | Some min->
          let change, rest= cover min range in
          if change then
            let _, _, r= S.split min t in
            S.union rest (expand_right range r)
          else S.empty
        | None-> S.empty
      in
      let l, v, r= S.split range t in
      let middle=
        if v then
          let _, s= cover (S.find range t) range in s
        else
          S.empty
      in
      S.union
        (expand_left range l)
        (expand_right range r)
        |> S.union middle

    let union s1 s2= S.fold merge s2 s1

    let diff s1 s2= S.fold unmerge s2 s1

    let inter s1 s2= s2
      |> S.elements
      |> List.map (fun v-> cover v s1)
      |> List.fold_left union S.empty


    let of_ranges rs= rs
      |> List.map of_range
      |> List.fold_left (fun acc s-> union acc s) S.empty
  end

