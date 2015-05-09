(*
** This file has been generated, if you wish to
** modify it in a permanent way, please refer
** to the script file : gen/generator_caml.rb
*)

open Api;;

let min_list l ( < ) =
  match l with
    [] -> failwith "empty list"
  | h :: t ->
    List.fold_left (fun u v -> if u < v then u else v) h t
;;

let min_list_key l key =
  min_list l (fun x y -> key x < key y)
;;

let max_list_key l key =
  min_list l (fun x y -> key x > key y)
;;


let max_nth_list_key index l key =
(*min_list l (fun x y -> key x > key y)*)
  List.nth (List.sort (fun x y -> compare (key y) (key x)) l) index
;;

let logged f name =
  (fun x -> (*let t = Unix.time () in*)
    print_string name; print_newline ();
            let r = f x in
            (*        let tt = Unix.time () in*)
            print_string name; print_string "end"; print_newline ();
            (*            print_string name; print_string ": "; print_float (tt -. t); print_newline ();*)
            r
  )
;;
let logged f name = f;; (* disable logging *)

(*
let max_taille_tas = 10000;;
type 'a tas = {mutable size : int; data : 'a array};;
let create_tas a = {size = 0; data = Array.make max_taille_tas a};;
let swap t i j =
  let tmp = t.(i) in
  t.(i) <- t.(j);
  t.(j) <- tmp
;;

let rec sift_up data i ( < ) =
  if i <> 0 then begin
    let j = (i - 1) / 2 in
    if (data.(i)) < (data.(j)) then begin
      swap data i j;
      sift_up data j ( < )
    end
  end
;;
let rec sift_down data size i ( < ) =
  let sg = 2 * i + 1 in
  let sd = sg + 1 in
  if sd > size then ()
  else if sd = size then begin
    if data.(sg) < data.(i) then swap data i sg
  end
  else
    if data.(sd) < data.(i) then
      if data.(sg) < data.(sd) then begin
        swap data sg i; sift_down data size sg ( < )
      end else begin
        swap data sd i; sift_down data size sd ( < )
      end
    else if data.(sg) < data.(i) then begin
      swap data sg i; sift_down data size sg ( < )
    end
;;
let push_tas tas x ( < ) =
  tas.data.(tas.size) <- x;
  sift_up tas.data tas.size ( < );
  tas.size <- tas.size + 1;
;;
let pop_tas tas ( < ) =
  tas.size <- tas.size - 1;
  swap tas.data 0 tas.size;
  sift_down tas.data tas.size 0 ( < );
  tas.data.(tas.size)
;;
let is_empty tas = tas.size = 0;;
*)

let mypos () = position_agent (moi ());;
let advpos () = position_agent (adversaire ());;

let index_pos (x, y) =
  (*assert (0 <= x && x < taille_terrain && 0 <= y && y < taille_terrain);*)
  x * taille_terrain + y
;;

(*
let vers p p' =
  afficher_position p; afficher_position p'; print_newline ();
  if p = p' then [] else begin
    let c = Array.make (taille_terrain * taille_terrain) (-1, -1, p) in
    (* c.(index_pos p) <- (0, 0); *)
    let u (a, b, _) (a', b', _) =
      (a < a') || ((a = a') && (b > b'))
    in
    let cc (_, z) (_, z') = u z z' in
    (*let pp (a, b) (a', b') = (a + a', b + b') in*)
    let tas = create_tas (p, (-1, -1, p)) in
    push_tas tas (p, (0, 0, p)) cc;
    while c.(index_pos p') = (-1, -1, p) do
      let (np, (d, nport, prv)) = pop_tas tas cc in
      if c.(index_pos np) <> (-1, -1, p) then () else begin
      (* afficher_position np;*)
      (*if d < 4 then begin
        afficher_position np; afficher_position p; afficher_position p'; print_newline () end;*)
      c.(index_pos np) <- (d, nport, prv);
      let nnport = nport + (let a = portail_joueur np in if a = (-1) || a = adversaire () then 1 else 0) in
      let (x, y) = np in
      if x > 0 then
        push_tas tas ((x - 1, y), (d + 1, nnport, np)) cc;
      if y > 0 then
        push_tas tas ((x, y - 1), (d + 1, nnport, np)) cc;
      if x < taille_terrain - 1 then
        push_tas tas ((x + 1, y), (d + 1, nnport, np)) cc;
      if y < taille_terrain - 1 then
        push_tas tas ((x, y + 1), (d + 1, nnport, np)) cc;
      end
    done;
    let chemin = ref [] in
    let u = ref p' in
    while !u <> p do
      chemin := !u :: !chemin;
      let (d, _, v) = c.(index_pos !u) in
      (*print_int d; afficher_position !u;*)
      u := v
    done;
    !chemin
  end
  ;;*)

let rec ( <|> ) a b =
  if a >= b then
    []
  else
    a :: ((a + 1) <|> b)
;;
let rec ( >|< ) a b =
  if a <= b then
    []
  else
    a :: ((a - 1) >|< b)
;;

let position_valid (x, y) =
  0 <= x && x < taille_terrain && 0 <= y && y < taille_terrain
;;
let pos_a_dist (x, y) d =
  (*let dxp = min d (max_taille - 1 - x) in
  let dxm = min d x in
  let dyp = min d (max_taille - 1 - y) in
    let dym = min d y in*)
  List.filter position_valid ((List.combine (x <|> x + d) (y + d >|< y)) @
                              (List.combine (x + d >|< x) (y >|< y - d)) @
                              (List.combine (x >|< x - d) (y - d <|> y)) @
                              (List.combine (x - d <|> x) (y <|> y + d)))
;;

let is_portail_not_me p =
  let u = portail_joueur p in
  u = (-1) || u = adversaire ()
;;

let move_towards (x, y) =
  let d = points_deplacement () in
  let (xx, yy) = mypos () in
  let adx = min (abs (x - xx)) d in
  let ady = min (abs (y - yy)) (d - adx) in
  let dx = if xx < x then adx else -adx in
  let dy = if yy < y then ady else -ady in
  ignore (deplacer (xx + dx, yy + dy))
;;

(*let move_towards p =
  ignore (deplacer (List.hd (vers (position_agent (moi ())) p)))
  ;;*)

(* Mise a jour de la liste des liens qui peuvent etre crees
** Complexite 0(nbportails ^ 2 * nbLiensChanges)
**
**
*)
let portails_pos = ref [||];;
let nb_portails = ref 0;;
let portails_id = Array.make (taille_terrain * taille_terrain) (-1);;
let are_linked = ref [||];;
let nb_blocking_links = ref [||];;
(*
** Fonction appelée au début de la partie.
*)
let partie_init () =  (* Pose ton code ici *)
  portails_pos := liste_portails ();
  (*Array.iter afficher_position !portails_pos;*)
  nb_portails := Array.length !portails_pos; 
  Array.iteri (fun i p -> portails_id.(index_pos p) <- i) !portails_pos;
  are_linked := Array.init !nb_portails (fun i ->
    Array.make !nb_portails false);
  nb_blocking_links := Array.init !nb_portails (fun i ->
    Array.make !nb_portails 0);

  flush stderr; flush stdout;; (* Pour que vos sorties s'affichent *)

let link_add p1 p2 =
  Array.iteri (fun i p ->
    Array.iteri (fun j p' ->
      if intersection_segments p1 p2 p p' then
        !nb_blocking_links.(i).(j) <- !nb_blocking_links.(i).(j) + 1
    ) !portails_pos
  ) !portails_pos
;;
let link_remove p1 p2 =
  Array.iteri (fun i p ->
    Array.iteri (fun j p' ->
      if intersection_segments p1 p2 p p' then
        !nb_blocking_links.(i).(j) <- !nb_blocking_links.(i).(j) - 1
    ) !portails_pos
  ) !portails_pos
;;
let update_blocking_links_newturn () =
  (*print_string "ubln";*)
  let liens = liste_liens () in
  let ll = Array.init !nb_portails
    (fun i -> Array.make !nb_portails false) in
  Array.iter (fun link ->
    let u, v = portails_id.(index_pos link.extr1), portails_id.(index_pos link.extr2) in
    (*print_int u; print_string " "; print_int v; print_newline ();*)
    ll.(u).(v) <- true; ll.(v).(u) <- true) liens;
  for i = 0 to !nb_portails - 1 do
    for j = 0 to !nb_portails - 1 do
      if i <= j && !are_linked.(i).(j) <> ll.(i).(j) then begin
        if ll.(i).(j) then link_add !portails_pos.(i) !portails_pos.(j)
        else link_remove !portails_pos.(i) !portails_pos.(j)
      end;
      !are_linked.(i).(j) <- ll.(i).(j)
    done
  done
(*;print_string "ubln end"*)
;;
let update_blocking_links_newturn = logged update_blocking_links_newturn "ubln";;

let portails_joueur joueur =
  List.filter (fun pos -> portail_joueur pos = joueur) (Array.to_list (liste_portails ()))
;;

let score_champ ch = score_triangle ch.som1 ch.som2 ch.som3;;
let _valeur_portail_now p =
  List.fold_left (+) 0 (List.map score_champ (Array.to_list (champs_incidents_portail p)))
;;
let vpn_cache = Array.make (taille_terrain * taille_terrain) None;;
let vpn_cache_clear = ref true;;
let valeur_portail_now p =
  match vpn_cache.(index_pos p) with
    None -> let r = _valeur_portail_now p in
            vpn_cache.(index_pos p) <- Some r;
            vpn_cache_clear := false;
            r
  | Some r -> r
;;
let valeur_portail_now = _valeur_portail_now;;
let clear_vpn_cache () =
  if not (!vpn_cache_clear) then begin
    for i = 0 to (Array.length vpn_cache) - 1 do
      vpn_cache.(i) <- None
    done;
    vpn_cache_clear := true
  end
;;

let rec list_product l1 l2 =
  match l1 with
    [] -> []
  | x :: t -> (List.map (fun y -> (x, y)) l2) @ (list_product t l2)
;;

(*let link_cache = Array.make (taille_terrain * taille_terrain) true;;
let make_link_cache p =
  Array.iter (fun p2 -> link_cache.(index_pos p) <- Array.length (liens_bloquants p p2) > 0)
    (liste_portails ());
;;
  let link_blocked p2 = link_cache.(index_pos p2);;*)
let link_blocked p1 p2 =
  let u, v = portails_id.(index_pos p1), portails_id.(index_pos p2) in
  !nb_blocking_links.(u).(v) > 0
;;
let _valeur_portail_build p player =
  if portail_joueur p = (-2) then
    0
  else if Array.length (case_champs p) > 0 then
    points_capture
  else begin
    (*make_link_cache p;*)
    (List.fold_left (+) 0
       (List.map
          (fun lien ->
            let (p1, p2) = (lien.extr1, lien.extr2) in
            if (p = p1 || p = p2 || lien.joueur_l <> player ||
                ((lien_existe p p1) && (lien_existe p p2)) ||
                  (link_blocked p p1) ||
                  (link_blocked p p2)) then
              0
            else
              score_triangle p p1 p2)
          (Array.to_list (liste_liens ())))) + points_capture
  end
;;
let vpb_cache = Array.make (2 * taille_terrain * taille_terrain) None;;
let vpb_cache_clear = ref true;;
let valeur_portail_build p player =
  match vpb_cache.(player - 1 + 2 * (index_pos p)) with
    None -> let r = _valeur_portail_build p player in
            vpb_cache.(player - 1 + 2 * (index_pos p)) <- Some r;
            vpb_cache_clear := false;
            r
  | Some r -> r
;;
let valeur_portail_build = _valeur_portail_build;;
let clear_vpb_cache () =
  if not (!vpb_cache_clear) then begin
    for i = 0 to (Array.length vpb_cache) - 1 do
      vpb_cache.(i) <- None
    done;
    vpb_cache_clear := true
  end
;;
let clear_caches () =
  clear_vpn_cache ();
  clear_vpb_cache ()
;;

let valeur_portail_build = logged valeur_portail_build "vpb";;

let valeur_portail p =
  let u = portail_joueur p in
  if u = (-2) then
    0
  else if u = (-1) then
    valeur_portail_build p (moi ())
  else if u = moi () then
    0
  else
    (valeur_portail_build p (moi ())) + (valeur_portail_now p)
;;


let valeur_portail_any p =
  let u = portail_joueur p in
  if u = (-2) then
    0
  else if u = (-1) then
    (valeur_portail_build p (moi ())) + (valeur_portail_build p (adversaire ()))
  else if u = moi () then
    (valeur_portail_now p) + (valeur_portail_build p (adversaire ()))
  else
    (valeur_portail_now p) + (valeur_portail_build p (moi ()))
;;


let newlink p1 p2 =
  (*print_string "newlink";*)
  let u, v = portails_id.(index_pos p1), portails_id.(index_pos p2) in
  !are_linked.(u).(v) <- true; !are_linked.(v).(u) <- true;
  link_add p1 p2
;;
let dellink p1 p2 =
  (*print_string "dellink";*)
  let u, v = portails_id.(index_pos p1), portails_id.(index_pos p2) in
  !are_linked.(u).(v) <- false; !are_linked.(v).(u) <- false;
  link_remove p1 p2
;;
let neutral () =
  let p = position_agent (moi ()) in
  let liens = liens_incidents_portail p in
  match neutraliser () with
    Ok -> Array.iter (fun lien -> dellink lien.extr1 lien.extr2) liens; clear_caches (); Ok
  | r -> r
;;
let neutraliser = "This should not be called anymore; use neutral instead";;
let make_link p =
  let p1 = position_agent (moi ()) in
  match (lier p) with
    Ok -> newlink p p1; clear_caches (); Ok
  | r -> r
;;
let lier = "This should not be called anymore; use make_link instead";;
let capture () =
  match capturer () with
    Ok -> clear_caches (); Ok
  | r -> r
;;
let capturer = "This should not be called anymore; use capture instead";;

let err f x =
  match (f x) with
    Ok -> ()
  | _ -> failwith ""
;;

let neutraliserf = err neutral;;
let capturerf = err capture;;
let lierf = err make_link;;

let vers p =
  let cp = position_agent (moi ()) in
  let dt = Array.make (taille_terrain * taille_terrain) (-1, []) in
  dt.(index_pos cp) <- (0, []);
  let d = distance p cp in
  for i = 1 to d do
    let ps = pos_a_dist cp i in
    List.iter (fun (x, y) ->
      let mm, u =
        List.fold_left (fun (a, b) (c, d) -> if a < c then (c, d) else (a, b)) (-1, [])
          (List.map (fun pp -> dt.(index_pos pp))
             (List.filter (fun pp -> position_valid pp && dt.(index_pos pp) <> (-1, []))
                [(x - 1, y); (x, y + 1); (x + 1, y); (x, y - 1)])) in
      let mp = mm + valeur_portail p in
      dt.(index_pos (x, y)) <- (mp, (x, y) :: u)) ps
  done;
  snd (dt.(index_pos p))
;;


let nbtours dist = (dist + points_deplacement () - 1) / nb_points_deplacement;;


let valeur_portail2 p =
  let u = portail_joueur p in
  let d = distance p (position_agent (moi ())) in
  let n = d + 3 * (nbtours d) in
  (*let n = nbtours d in*)
  (*let n = d in*)
  let nn = float_of_int n in
  let da = distance p (position_agent (adversaire ())) in
  let tours_restants = nb_tours - tour_actuel () - n in
  if u = (-2) then
    min_int
  else if u = (-1) then
  (*(float_of_int (valeur_portail_me p)) /. ((nn +. 1.) *. (nn +. 1.))*)
    -40 * n - 20 * da + (1 * (valeur_portail_build p (moi ())))
  else if u = moi () then
    min_int
  else
(*(float_of_int ((valeur_portail_me p) + (valeur_portail_adv p))) /. ((nn +. 1.) *. (nn +. 1.))*)
    -40 * n - 20 * da + (1 * (valeur_portail_build p (moi ())) + (valeur_portail_now p))
;;

let _make_links () =
  let build_links = (points_action ()) / cout_lien in
  let p = position_agent (moi ()) in
  (*print_int (portail_joueur p);*)
  let cn = (List.filter (fun p' -> p <> p' && not (link_blocked p p')) (portails_joueur (moi ()))) in
  (*print_string "abc";*)
  let connectables = Array.of_list cn in
  let n = Array.length connectables in
  (*print_int n;*)
  let best_score = ref 0 in
  let best_config = Array.make n false in
  let best_nactive = ref 0 in
  let current_config = Array.make n false in
  let nactive = ref 0 in
  for e = 1 to (1 lsl n) - 1 do
    let u = ref 0 in
    (*print_int !u; print_string " "; print_int !nactive; print_int e; print_newline ();*)
    while current_config.(!u) do
      current_config.(!u) <- false; incr u
    done;
    current_config.(!u) <- true;
    nactive := !nactive + 1 - !u;
    if (!nactive <= build_links) then begin
      (* Check configuration *)
      let total_value = ref 0 in
      for i = 0 to n - 1 do
        if current_config.(i) then
          for j = i + 1 to n - 1 do
            if current_config.(j) then begin
              let p1, p2 = connectables.(i), connectables.(j) in
              if intersection_segments p p1 p p2 then
                total_value := min_int (* Incorrect configuration *)
              ;
              if lien_existe p1 p2 then
                total_value := !total_value + score_triangle p p1 p2
              ;
            end
          done
      done;
      if (!total_value > !best_score) || (!total_value = !best_score && !nactive > !best_nactive) then begin
        best_score := !total_value;
        for i = 0 to n - 1 do best_config.(i) <- current_config.(i) done;
        best_nactive := !nactive
      end
    end
  done;
  (*print_int (Array.length connectables); print_int (Array.length best_config);*)
  Array.iteri (fun i b -> if b then ignore (make_link connectables.(i))) best_config
;;
let make_links () =
  if portail_joueur (position_agent (moi ())) = moi () then
    _make_links ()
;;
let make_links = logged make_links "make_links";;

let shield_values = [|10; 50; 150; 1000; 2000; 4000; max_int|];;
let _make_shields () =
  let p = position_agent (moi ()) in
  let value = valeur_portail_now p + valeur_portail_build p (adversaire ()) in
  let i = ref 0 in
  while value >= shield_values.(!i) do
    incr i
  done;
  let nb_shields = portail_boucliers p in
  for u = 1 to max 0 (!i - nb_shields) do
    ignore (ajouter_bouclier ())
  done;
  let portails_pas_a_moi =
    (List.filter (fun pos -> portail_joueur pos <> moi ())
       (Array.to_list (liste_portails ()))) in
  if portails_pas_a_moi = [] then () else begin
    let d_closest = min_list (List.map (distance (position_agent (moi ()))) portails_pas_a_moi) (<) in
    if (((points_deplacement ()) + ((points_action ()) / cout_turbo)) < d_closest) &&
        (((points_action ()) mod cout_turbo) > (cout_bouclier + portail_boucliers p)) then
      ignore (ajouter_bouclier ())
  end
;;
let make_shields () =
  if portail_joueur (position_agent (moi ())) = moi () then
    _make_shields ()
;;

let should_take p =
(*(valeur_portail_adv p) >= 10 || (valeur_portail_me p) >= 50*)
  true
;;

let score_tour player =
  List.fold_left (+) 0 (List.map score_champ
      (List.filter (fun ch -> ch.joueur_c = player) (Array.to_list (liste_champs ()))))
;;

let captures = ref [];;
let objective = ref None;;
let rec step no_repeat_index =
  let mypos = position_agent (moi ()) in
  if portail_joueur mypos = moi () then begin
    make_links ();
    make_shields ()
  end;
  if (points_deplacement () > 0) && (points_action () >= cout_lien) then
    objective := None; 
  if (no_repeat_index > 0) && (Some (position_agent (moi ())) = !objective) then (* Assume we are trying to neutralize a 6-shield player *)
    objective := None;
  if not ((!objective <> None) && (points_deplacement () = 0 || points_action () < cout_lien)) then begin
  (*if !objective = None then begin*)
    let portails_pas_a_moi =
      (List.filter (fun pos -> portail_joueur pos <> moi ())
         (Array.to_list (liste_portails ()))) in
    if portails_pas_a_moi = [] then () else begin
      let closest = max_nth_list_key no_repeat_index portails_pas_a_moi valeur_portail2 in
      let v = valeur_portail2 closest in
    (*afficher_position closest; print_int v;*)
      match !objective with
        None -> objective := Some closest
      | Some p -> if (valeur_portail2 p) * 2 < v then
          objective := Some closest
    end
  (*end*);
    match !objective with
      None -> ()
    | Some p -> (
    (*move_towards p;*)
      let chemin = vers p in
      List.iter (fun pp ->
      (*afficher_position pp; *)move_towards pp; ignore (neutral ()); ignore (capture ()); make_links (); make_shields ()) chemin; 
      let player = portail_joueur (position_agent (moi ())) in
      if (player <> adversaire () && player <> (-1)) then begin
        if (points_deplacement ()) > 0 then step no_repeat_index
        else if (utiliser_turbo ()) = Ok then step no_repeat_index
      end else begin
        try
          if (player = adversaire ()) then begin neutraliserf () end;
          capturerf ();
          step no_repeat_index
        with
          _ -> ()
      end)
  end
and haunt () =
      (*captures := !captures @ (Array.to_list (hist_portails_captures ()));*)
      (*if score (moi ()) <= score (adversaire ()) || (score_tour (moi ()) < (score_tour (adversaire ()))) then*)
      step 0;
  (*begin
  try
    while (!captures <> []) && ((points_action () >= cout_turbo) || (points_deplacement () > 0)) do
      let p = List.hd !captures in
      if should_take p then begin
        move_towards p;
        ignore (neutraliser ());
        ignore (capturer ());
        make_links ();
        if (points_deplacement () = 0) then
          if (points_action () < cout_turbo) then
            ignore (ajouter_bouclier ())
          else 
            ignore (utiliser_turbo ())
        else if (portail_joueur p) = moi () && points_action () > cout_lien then
          captures := List.tl !captures
        else
          failwith ""
      end else
        captures := List.tl !captures
    done
  with
    _ -> ()
  end
  ;
  if (!captures = []) then (* back to normal way *)
    step ()*)
    
(*move_towards (position_agent (adversaire ()))*)
;;


let step1 () =
  let pp = position_agent (moi ()) in
  let portails = Array.to_list (liste_portails ()) in
  let closest = min_list_key portails (distance pp) in
  let d = distance closest pp in
  while (points_action ()) > cout_turbo && 2 * d > (points_deplacement ()) do
    ignore (utiliser_turbo ())
  done;
  if (points_deplacement ()) >= 2 * d then begin
    move_towards closest;
    ignore (capture ());
    for i = 1 to 6 do
      ignore (ajouter_bouclier ())
    done
  end;
  move_towards pp
;;

let magic1 = (1 lsl 31) - 1;;
let magic2 = 1000003;;
let t4 = taille_terrain * taille_terrain * taille_terrain * taille_terrain;;
let link_id link =
  let p1, p2 = (min link.extr1 link.extr2), (max link.extr1 link.extr2) in
  link.joueur_l + 2 * (index_pos p1 + t4 * (index_pos p2))
;;
let hash_situation () =
  let l = List.map link_id (Array.to_list (liste_liens ())) in
  let a1 = index_pos (position_agent (moi ())) in
  let a2 = index_pos (position_agent (adversaire ())) in
  List.fold_left (fun u v -> (magic2 * u + v) mod magic1) 0 (a1 :: a2 :: (List.sort compare l))
;;

let positions_seen = ref [];;
let newturn_detect_loop () =
  let hs = hash_situation () in
  try
    let (tour, score_me, score_adv, num_tries) = List.assoc hs !positions_seen in
    let nl = List.remove_assoc hs !positions_seen in
    let nb_tours_cycle = tour_actuel () - tour in
    let new_score_me = score (moi ()) in
    let new_score_adv = score (adversaire ()) in
    let score_me_increase = new_score_me - score_me in
    let score_adv_increase = new_score_adv - score_adv in
    let score_end_me_predicted_min = new_score_me +
      score_me_increase * ((nb_tours - tour_actuel ()) / nb_tours_cycle) in
    let adv_rem_tours = nb_tours - tour_actuel () - (if moi () = 2 then 1 else 0) in
    let score_end_adv_predicted_max = new_score_adv +
      score_adv_increase * ((adv_rem_tours + nb_tours_cycle - 1) / nb_tours_cycle) in
    print_string "Repeat detected: turn "; print_int tour; print_string " to turn "; print_int (tour_actuel ()); print_newline (); print_string "My score increase: "; print_int score_me_increase; print_newline (); print_string "Adv score increase: "; print_int score_adv_increase; print_newline ();
     print_string "Min score predicted for me: "; print_int score_end_me_predicted_min; print_newline (); print_string "Max score predicted for adv: "; print_int score_end_adv_predicted_max; print_newline ();
    if score_end_me_predicted_min >= score_end_adv_predicted_max then begin
      positions_seen := (hs, (tour_actuel (), new_score_me, new_score_adv, num_tries)) :: nl;
      num_tries
    end else begin
      positions_seen := (hs, (tour_actuel (), new_score_me, new_score_adv, num_tries + 1)) :: nl;
      num_tries + 1
    end
  with
    Not_found -> begin
      positions_seen :=
        (hs, (tour_actuel (), score (moi ()), score (adversaire ()), 0)) :: !positions_seen;
      0
    end
;;

(*
** Fonction appelée à chaque tour.
*)
let jouer_tour () =  (* Pose ton code ici *)
  print_string "Turn "; print_int (tour_actuel ());
  update_blocking_links_newturn ();
  clear_caches ();
  let ll = newturn_detect_loop () in
  (*print_string "abc"; print_int (moi ()); print_int (tour_actuel ());*)
  (if moi () = 1 && tour_actuel () = 1 then
    step1 ()
  else
  (*haunt ();*)
      step ll
  );
(*  (*print_int 0;
    step ();*)
    haunt ();*)
  flush stderr; flush stdout;; (* Pour que vos sorties s'affichent *)

(*
** Fonction appelée à la fin de la partie.
*)
let partie_fin () =  (* Pose ton code ici *)
  flush stderr; flush stdout;; (* Pour que vos sorties s'affichent *)

(* /!\ Ne touche pas a ce qui suit /!\ *)
Callback.register "ml_partie_init" partie_init;;Callback.register "ml_jouer_tour" jouer_tour;;Callback.register "ml_partie_fin" partie_fin;;
