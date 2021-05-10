let time =
  Crowbar.map
    [ Crowbar.range 2; Crowbar.range 4; Crowbar.list (Crowbar.range 1000) ]
    (fun max_height max_branching randomness ->
       let max_height = 1 + max_height in
       let max_branching = 1 + max_branching in
       Builder.build ~enable_extra_restrictions:false ~min_year:2000
         ~max_year_inc:2002 ~max_height ~max_branching ~randomness)

let timestamp =
  Crowbar.map [ Crowbar.int64; Crowbar.int ] (fun s ns ->
      Timedesc.Span.make ~s ~ns ())

let pos_span =
  Crowbar.map [ Crowbar.int64; Crowbar.int ] (fun s ns ->
      Timedesc.Span.(abs @@ make ~s ~ns ()))

let nz_pos_span =
  Crowbar.map [ Crowbar.int64; Crowbar.int ] (fun s ns ->
      let ns = ns + 1 in
      Timedesc.Span.(abs @@ make ~s ~ns ()))

let pattern =
  Crowbar.map
    [ Crowbar.list (Crowbar.range 5000) ]
    (fun randomness ->
       let min_year = 0000 in
       let max_year_inc = 9999 in
       let rng = Builder.make_rng ~randomness in
       Builder.make_pattern ~rng ~min_year ~max_year_inc)

let points : Points.t Crowbar.gen =
  Crowbar.map
    [ Crowbar.list (Crowbar.range 5000) ]
    (fun randomness ->
       let min_year = 0000 in
       let max_year_inc = 9999 in
       let rng = Builder.make_rng ~randomness in
       Builder.make_points ~rng ~min_year ~max_year_inc ~max_precision:7)

let time_zone : Timedesc.Time_zone.t Crowbar.gen =
  let tz_count = List.length Timedesc.Time_zone.available_time_zones in
  Crowbar.map [ Crowbar.int ] (fun n ->
      let n = max 0 n mod tz_count in
      Timedesc.Time_zone.make_exn
        (List.nth Timedesc.Time_zone.available_time_zones n))

let search_space =
  Crowbar.map
    [
      Crowbar.list
        (Crowbar.map [ timestamp; nz_pos_span ] (fun search_start search_size ->
             let search_start =
               Timedesc.Span.(
                 min
                   (max Timedesc.Timestamp.min_val search_start)
                   Timedesc.Timestamp.max_val)
             in
             let search_end_exc =
               Timedesc.Span.(
                 min Timedesc.Timestamp.max_val (search_start + search_size))
             in
             (search_start, search_end_exc)));
    ]
    (fun l -> CCList.to_seq l |> Time.Intervals.normalize |> CCList.of_seq)

let permute (seed : int) (l : 'a list) : 'a list =
  match l with
  | [] -> []
  | _ ->
    let len = List.length l in
    let seed = max 0 seed mod len in
    let l = ref l in
    OSeq.(0 --^ len)
    |> Seq.map (fun i ->
        let l' = List.mapi (fun i x -> (i, x)) !l in
        let len = List.length l' in
        let pick = max 0 (i * seed) mod len in
        let r = List.assoc pick l' in
        l := List.remove_assoc pick l' |> List.map (fun (_, x) -> x);
        r)
    |> CCList.of_seq
