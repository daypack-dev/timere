let time =
  Crowbar.map [
    Crowbar.range ~min:1 3; Crowbar.range ~min:1 5; Crowbar.list (Crowbar.range 1000)
  ]
    (fun height max_branching randomness ->
       Builder.make ~min_year:2000 ~max_year_inc:2002 ~height ~max_branching
         ~randomness)
