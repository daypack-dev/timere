type entry = {
  is_dst : bool;
  offset : int;
}

type table = entry Int64_map.t

type db_utc = table String_map.t

let db_utc : db_utc =
  String_map.empty
  |> String_map.add "Cuba" (
        Int64_map.empty
        |> Int64_map.add (-2524501832L)
           {
             is_dst = false;
             offset = -19776;
           }
        |> Int64_map.add (-1402813824L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (-1311534000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (-1300996800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (-933534000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (-925675200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (-902084400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (-893620800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (-870030000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (-862171200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (-775681200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (-767822400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (-744231600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (-736372800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (-144702000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (-134251200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (-113425200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (-102542400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (-86295600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (-72907200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (-54154800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (-41457600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (-21495600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (-5774400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (9954000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (25675200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (41403600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (57729600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (73458000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (87364800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (104907600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (118900800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (136357200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (150436800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (167806800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (183528000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (199256400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (215582400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (230706000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (247032000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (263365200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (276667200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (290581200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (308721600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (322030800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (340171200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (358318800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (371620800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (389768400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (403070400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (421218000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (434520000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (452667600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (466574400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (484117200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (498024000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (511333200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (529473600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (542782800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (560923200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (574837200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (592372800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (606286800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (623822400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (638946000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (655876800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (671000400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (687330000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (702450000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (718779600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (733899600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (750229200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (765349200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (781678800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (796798800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (813128400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (828853200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (844578000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (860302800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (876632400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (891147600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (909291600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (922597200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (941346000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (954651600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (972795600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (986101200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (1004245200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (1018155600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (1035694800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (1049605200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (1067144400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (1080450000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (1162098000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (1173589200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (1193547600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (1205643600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (1224997200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (1236488400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (1256446800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (1268542800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (1288501200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (1300597200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (1321160400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (1333256400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (1352005200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (1362891600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (1383454800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (1394341200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (1414904400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (1425790800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (1446354000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (1457845200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (1478408400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (1489294800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (1509858000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (1520744400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (1541307600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (1552194000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (1572757200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (1583643600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (1604206800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (1615698000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (1636261200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (1647147600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (1667710800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (1678597200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (1699160400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (1710046800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (1730610000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (1741496400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (1762059600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (1772946000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (1793509200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (1805000400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (1825563600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (1836450000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (1857013200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (1867899600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (1888462800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (1899349200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (1919912400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (1930798800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (1951362000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (1962853200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (1983416400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (1994302800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (2014866000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (2025752400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (2046315600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (2057202000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (2077765200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (2088651600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (2109214800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (2120101200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (2140664400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (2152155600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (2172718800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (2183605200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (2204168400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (2215054800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (2235618000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (2246504400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (2267067600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (2277954000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (2298517200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (2309403600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (2329966800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (2341458000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (2362021200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (2372907600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (2393470800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (2404357200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (2424920400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (2435806800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (2456370000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (2467256400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (2487819600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (2499310800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (2519874000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (2530760400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (2551323600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (2562210000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (2582773200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (2593659600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (2614222800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (2625109200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (2645672400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (2656558800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (2677122000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (2688613200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (2709176400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (2720062800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (2740626000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (2751512400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (2772075600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (2782962000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (2803525200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (2814411600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (2834974800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (2846466000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (2867029200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (2877915600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (2898478800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (2909365200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (2929928400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (2940814800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (2961378000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (2972264400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (2992827600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (3003714000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (3024277200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (3035768400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (3056331600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (3067218000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (3087781200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (3098667600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (3119230800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (3130117200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (3150680400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (3161566800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (3182130000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (3193016400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (3213579600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (3225070800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (3245634000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (3256520400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (3277083600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (3287970000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (3308533200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (3319419600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (3339982800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (3350869200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (3371432400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (3382923600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (3403486800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (3414373200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (3434936400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (3445822800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (3466386000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (3477272400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (3497835600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (3508722000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (3529285200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (3540171600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (3560734800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (3572226000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (3592789200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (3603675600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (3624238800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (3635125200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (3655688400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (3666574800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (3687138000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (3698024400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (3718587600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (3730078800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (3750642000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (3761528400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (3782091600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (3792978000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (3813541200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (3824427600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (3844990800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (3855877200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (3876440400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (3887326800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (3907890000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (3919381200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (3939944400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (3950830800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (3971394000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (3982280400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (4002843600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (4013730000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (4034293200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (4045179600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (4065742800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (4076629200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (4097192400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (4108683600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (4129246800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (4140133200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (4160696400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (4171582800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (4192146000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (4203032400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (4223595600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (4234482000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (4255045200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (4265931600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (4286494800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (4297986000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (4318549200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (4329435600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (4349998800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (4360885200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (4381448400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (4392334800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (4412898000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (4423784400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (4444347600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (4455234000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (4475797200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (4487288400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (4507851600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (4518738000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (4539301200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (4550187600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (4570750800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (4581637200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (4602200400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (4613086800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (4633650000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (4645141200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (4665704400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (4676590800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (4697154000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (4708040400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (4728603600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (4739490000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (4760053200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (4770939600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (4791502800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (4802389200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (4822952400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (4834443600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (4855006800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (4865893200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (4886456400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (4897342800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (4917906000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (4928792400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (4949355600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (4960242000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (4980805200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (4992296400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (5012859600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (5023746000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (5044309200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (5055195600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (5075758800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (5086645200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (5107208400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (5118094800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (5138658000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (5149544400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (5170107600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (5181598800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (5202162000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (5213048400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (5233611600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (5244498000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (5265061200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (5275947600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (5296510800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (5307397200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (5327960400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (5338846800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (5359410000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (5370901200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (5391464400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (5402350800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (5422914000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (5433800400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (5454363600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (5465250000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (5485813200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (5496699600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (5517262800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (5528754000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (5549317200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (5560203600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (5580766800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (5591653200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (5612216400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (5623102800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (5643666000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (5654552400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (5675115600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (5686002000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (5706565200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (5718056400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (5738619600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (5749506000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (5770069200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (5780955600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (5801518800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (5812405200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (5832968400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (5843854800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (5864418000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (5875909200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (5896472400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (5907358800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (5927922000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (5938808400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (5959371600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (5970258000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (5990821200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (6001707600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (6022270800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (6033157200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (6053720400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (6065211600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (6085774800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (6096661200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (6117224400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (6128110800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (6148674000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (6159560400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (6180123600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (6191010000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (6211573200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (6222459600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (6243022800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (6254514000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (6275077200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (6285963600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (6306526800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (6317413200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (6337976400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (6348862800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (6369426000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (6380312400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (6400875600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (6412366800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (6432930000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (6443816400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (6464379600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (6475266000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (6495829200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (6506715600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (6527278800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (6538165200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (6558728400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (6569614800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (6590178000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (6601669200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (6622232400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (6633118800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (6653682000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (6664568400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (6685131600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (6696018000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (6716581200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (6727467600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (6748030800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (6759522000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (6780085200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (6790971600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (6811534800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (6822421200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (6842984400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (6853870800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (6874434000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (6885320400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (6905883600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (6916770000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (6937333200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (6948824400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (6969387600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (6980274000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (7000837200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (7011723600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (7032286800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (7043173200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (7063736400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (7074622800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (7095186000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (7106072400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (7126635600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (7138126800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (7158690000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (7169576400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (7190139600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (7201026000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (7221589200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (7232475600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (7253038800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (7263925200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (7284488400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (7295374800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (7315938000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (7327429200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (7347992400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (7358878800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (7379442000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (7390328400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (7410891600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (7421778000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (7442341200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (7453227600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (7473790800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (7484677200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (7505240400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (7516731600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (7537294800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (7548181200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (7568744400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (7579630800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (7600194000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (7611080400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (7631643600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (7642530000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (7663093200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (7674584400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (7695147600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (7706034000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (7726597200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (7737483600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (7758046800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (7768933200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (7789496400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (7800382800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (7820946000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (7831832400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (7852395600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (7863886800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (7884450000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (7895336400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (7915899600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (7926786000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (7947349200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (7958235600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (7978798800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (7989685200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (8010248400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (8021739600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (8042302800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (8053189200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (8073752400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (8084638800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (8105202000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (8116088400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (8136651600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (8147538000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (8168101200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (8178987600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (8199550800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (8211042000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (8231605200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (8242491600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (8263054800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (8273941200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (8294504400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (8305390800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (8325954000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (8336840400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (8357403600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (8368290000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (8388853200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (8400344400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (8420907600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (8431794000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (8452357200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (8463243600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (8483806800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (8494693200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (8515256400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (8526142800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (8546706000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (8558197200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (8578760400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (8589646800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (8610210000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (8621096400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (8641659600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (8652546000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (8673109200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (8683995600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (8704558800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (8715445200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (8736008400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (8747499600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (8768062800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (8778949200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (8799512400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (8810398800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (8830962000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (8841848400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (8862411600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (8873298000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (8893861200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (8905352400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (8925915600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (8936802000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (8957365200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (8968251600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (8988814800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (8999701200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (9020264400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (9031150800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (9051714000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (9062600400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (9083163600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (9094654800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (9115218000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (9126104400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (9146667600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (9157554000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (9178117200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (9189003600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (9209566800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (9220453200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (9241016400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (9251902800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (9272466000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (9283957200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (9304520400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (9315406800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (9335970000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (9346856400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (9367419600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (9378306000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (9398869200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (9409755600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (9430318800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (9441810000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (9462373200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (9473259600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (9493822800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (9504709200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (9525272400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (9536158800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (9556722000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (9567608400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (9588171600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (9599058000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (9619621200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (9631112400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (9651675600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (9662562000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (9683125200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (9694011600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (9714574800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (9725461200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (9746024400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (9756910800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (9777474000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (9788965200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (9809528400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (9820414800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (9840978000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (9851864400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (9872427600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (9883314000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (9903877200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (9914763600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (9935326800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (9946213200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (9966776400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (9978267600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (9998830800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (10009717200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (10030280400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (10041166800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (10061730000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (10072616400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (10093179600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (10104066000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (10124629200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (10135515600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (10156078800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (10167570000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (10188133200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (10199019600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (10219582800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (10230469200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (10251032400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (10261918800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (10282482000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (10293368400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (10313931600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (10325422800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (10345986000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (10356872400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (10377435600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (10388322000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (10408885200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (10419771600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (10440334800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (10451221200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (10471784400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (10482670800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (10503234000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (10514120400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (10534683600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (10546174800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (10566738000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (10577624400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (10598187600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (10609074000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (10629637200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (10640523600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (10661086800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (10671973200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (10692536400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (10704027600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (10724590800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (10735477200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (10756040400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (10766926800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (10787490000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (10798376400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (10818939600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (10829826000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (10850389200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (10861275600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (10881838800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (10893330000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (10913893200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (10924779600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (10945342800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (10956229200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (10976792400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (10987678800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (11008242000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (11019128400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (11039691600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (11051182800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (11071746000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (11082632400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (11103195600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (11114082000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (11134645200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (11145531600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (11166094800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (11176981200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (11197544400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (11208430800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (11228994000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (11240485200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (11261048400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (11271934800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (11292498000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (11303384400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (11323947600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (11334834000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (11355397200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (11366283600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (11386846800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (11397733200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (11418296400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (11429787600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (11450350800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (11461237200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (11481800400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (11492686800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (11513250000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (11524136400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (11544699600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (11555586000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (11576149200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (11587640400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (11608203600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (11619090000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (11639653200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (11650539600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (11671102800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (11681989200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (11702552400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (11713438800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (11734002000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (11744888400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (11765451600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (11776942800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (11797506000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (11808392400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (11828955600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (11839842000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (11860405200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (11871291600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (11891854800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (11902741200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (11923304400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (11934795600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (11955358800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (11966245200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (11986808400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (11997694800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (12018258000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (12029144400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (12049707600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (12060594000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (12081157200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (12092043600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (12112606800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (12124098000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (12144661200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (12155547600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (12176110800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (12186997200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (12207560400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (12218446800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (12239010000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (12249896400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (12270459600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (12281346000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (12301909200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (12313400400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (12333963600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (12344850000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (12365413200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (12376299600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (12396862800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (12407749200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (12428312400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (12439198800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (12459762000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (12471253200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (12491816400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (12502702800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (12523266000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (12534152400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (12554715600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (12565602000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (12586165200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (12597051600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (12617614800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (12628501200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (12649064400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (12660555600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (12681118800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (12692005200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (12712568400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (12723454800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (12744018000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (12754904400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (12775467600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (12786354000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (12806917200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (12818408400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (12838971600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (12849858000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (12870421200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (12881307600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (12901870800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (12912757200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (12933320400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (12944206800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (12964770000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (12975656400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (12996219600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (13007710800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (13028274000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (13039160400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (13059723600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (13070610000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (13091173200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (13102059600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (13122622800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (13133509200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (13154072400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (13164958800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (13185522000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (13197013200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (13217576400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (13228462800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (13249026000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (13259912400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (13280475600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (13291362000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (13311925200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (13322811600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (13343374800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (13354866000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (13375429200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (13386315600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (13406878800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (13417765200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (13438328400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (13449214800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (13469778000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (13480664400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (13501227600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (13512114000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (13532677200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (13544168400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (13564731600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (13575618000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (13596181200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (13607067600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (13627630800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (13638517200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (13659080400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (13669966800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (13690530000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (13702021200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (13722584400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (13733470800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (13754034000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (13764920400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (13785483600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (13796370000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (13816933200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (13827819600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (13848382800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (13859269200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (13879832400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (13891323600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (13911886800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (13922773200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (13943336400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (13954222800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (13974786000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (13985672400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (14006235600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (14017122000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (14037685200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (14048571600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (14069134800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (14080626000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (14101189200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (14112075600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (14132638800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (14143525200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (14164088400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (14174974800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (14195538000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (14206424400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (14226987600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (14238478800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (14259042000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (14269928400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (14290491600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (14301378000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (14321941200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (14332827600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (14353390800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (14364277200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (14384840400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (14395726800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (14416290000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (14427781200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (14448344400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (14459230800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (14479794000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (14490680400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (14511243600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (14522130000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (14542693200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (14553579600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (14574142800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (14585634000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (14606197200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (14617083600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (14637646800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (14648533200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (14669096400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (14679982800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (14700546000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (14711432400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (14731995600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (14742882000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (14763445200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (14774936400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (14795499600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (14806386000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (14826949200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (14837835600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (14858398800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (14869285200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (14889848400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (14900734800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (14921298000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (14932184400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (14952747600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (14964238800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (14984802000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (14995688400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (15016251600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (15027138000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (15047701200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (15058587600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (15079150800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (15090037200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (15110600400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (15122091600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (15142654800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (15153541200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (15174104400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (15184990800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (15205554000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (15216440400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (15237003600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (15247890000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (15268453200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (15279339600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (15299902800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (15311394000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (15331957200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (15342843600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (15363406800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (15374293200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (15394856400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (15405742800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (15426306000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (15437192400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (15457755600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (15469246800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (15489810000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (15500696400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (15521259600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (15532146000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (15552709200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (15563595600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (15584158800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (15595045200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (15615608400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (15626494800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (15647058000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (15658549200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (15679112400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (15689998800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (15710562000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (15721448400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (15742011600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (15752898000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (15773461200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (15784347600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (15804910800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (15815797200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (15836360400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (15847851600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (15868414800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (15879301200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (15899864400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (15910750800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (15931314000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (15942200400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (15962763600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (15973650000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (15994213200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (16005704400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (16026267600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (16037154000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (16057717200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (16068603600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (16089166800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (16100053200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (16120616400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (16131502800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (16152066000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (16162952400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (16183515600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (16195006800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (16215570000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (16226456400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (16247019600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (16257906000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (16278469200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (16289355600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (16309918800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (16320805200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (16341368400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (16352859600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (16373422800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (16384309200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (16404872400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (16415758800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (16436322000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (16447208400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (16467771600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (16478658000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (16499221200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (16510107600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (16530670800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (16542162000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (16562725200L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (16573611600L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (16594174800L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (16605061200L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (16625624400L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (16636510800L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (16657074000L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (16667960400L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (16688523600L)
           {
             is_dst = false;
             offset = -18000;
           }
        |> Int64_map.add (16699410000L)
           {
             is_dst = true;
             offset = -14400;
           }
        |> Int64_map.add (16719973200L)
           {
             is_dst = false;
             offset = -18000;
           }
     )
