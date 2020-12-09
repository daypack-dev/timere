let time =
  Crowbar.map
    [
      Crowbar.range ~min:1 3;
      Crowbar.range ~min:1 5;
      Crowbar.list (Crowbar.range 1000);
    ]
    (fun max_height max_branching randomness ->
       Builder.build ~min_year:2000 ~max_year_inc:2002 ~max_height ~max_branching
         ~randomness)
