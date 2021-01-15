open Date_components

let time =
  Crowbar.map
    [ Crowbar.range 2; Crowbar.range 4; Crowbar.list (Crowbar.range 1000) ]
    (fun max_height max_branching randomness ->
       let max_height = 1 + max_height in
       let max_branching = 1 + max_branching in
       Builder.build ~enable_extra_restrictions:false ~min_year:2000
         ~max_year_inc:2002 ~max_height ~max_branching ~randomness)

let time' = Crowbar.map [ time ] Resolver.t_of_ast

let pattern =
  Crowbar.map
    [ Crowbar.list (Crowbar.range 5000) ]
    (fun randomness ->
       let min_year = 0000 in
       let max_year_inc = 9999 in
       let rng = Builder.make_rng ~randomness in
       Builder.make_pattern ~rng ~min_year ~max_year_inc
       )

let points : Points.t Crowbar.gen =
  Crowbar.map
    [ Crowbar.list (Crowbar.range 5000) ]
    (fun randomness ->
       let min_year = 0000 in
       let max_year_inc = 9999 in
       let rng = Builder.make_rng ~randomness in
       Builder.make_points ~rng ~min_year ~max_year_inc
    )

let search_space =
  Crowbar.map
    [
      Crowbar.list
        (Crowbar.map [ Crowbar.int64; Crowbar.int64 ]
           (fun search_start search_size ->
              let search_start =
                min (max Time.min_timestamp search_start) Time.max_timestamp
              in
              let search_size = Int64.abs search_size in
              let search_end_exc =
                min Time.max_timestamp (Int64.add search_start search_size)
              in
              (search_start, search_end_exc)));
    ]
    (fun l -> CCList.to_seq l |> Time.Intervals.normalize |> CCList.of_seq)
