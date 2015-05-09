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

let index_pos (x, y) =
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
      let (mp, v) = if is_portail_not_me (x, y) then (mm + 1, (x, y) :: u) else (mm, u) in
      dt.(index_pos (x, y)) <- (mp, v)) ps
  done;
  snd (dt.(index_pos p))
;;

let move_towards (x, y) =
  let d = points_deplacement () in
  let (xx, yy) = position_agent (moi ()) in
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
let portails_pos = liste_portails ();;
let nb_portails = Array.length portails_pos;;
let portails_id = Array.make (taille_terrain * taille_terrain) (-1);;
Array.iteri (fun i p -> portails_id.(index_pos p) <- i);;
let nb_blocking_links = Array.init nb_portails (fun i ->
  Array.make (Array.length portails_pos) 0);;
let link_add p1 p2 =
  Array.iteri (fun i p ->
    Array.iteri (fun j p' ->
      if intersection_segments p1 p2 p p' then
        nb_blocking_links.(i).(j) <- nb_blocking_links.(i).(j) + 1
    ) portails_pos
  ) portails_pos
;;
let link_remove p1 p2 =
  Array.iteri (fun i p ->
    Array.iteri (fun j p' ->
      if intersection_segments p1 p2 p p' then
        nb_blocking_links.(i).(j) <- nb_blocking_links.(i).(j) - 1
    ) portails_pos
  ) portails_pos
;;
let are_linked = Array.init nb_portails (fun i ->
  Array.make nb_portails false);;
let update_blocking_links_newturn () =
  let liens = liste_liens () in
  let ll = Array.init nb_portails
    (fun i -> Array.make nb_portails false) in
  Array.iter (fun link ->
    let u, v = portails_id.(index_pos link.extr1), portails_id.(index_pos link.extr2) in
    ll.(u).(v) <- true; ll.(v).(u) <- true) liens;
  for i = 0 to nb_portails - 1 do
    for j = 0 to nb_portails - 1 do
      if i <= j && are_linked.(i).(j) <> ll.(i).(j) then
        if ll.(i).(j) then link_add portails_pos.(i) portails_pos.(j)
        else link_remove portails_pos.(i) portails_pos.(j)
      ;
      are_linked.(i).(j) <- ll.(i).(j)
    done
  done
;;
let newlink p1 p2 =
  let u, v = portails_id.(index_pos p1), portails_id.(index_pos p2) in
  are_linked.(u).(v) <- true; are_linked.(v).(u) <- true;
  link_add p1 p2
;;
let dellink p1 p2 =
  let u, v = portails_id.(index_pos p1), portails_id.(index_pos p2) in
  are_linked.(u).(v) <- false; are_linked.(v).(u) <- false;
  link_remove p1 p2
;;
let neutral () =
  let p = position_agent (moi ()) in
  let liens = liens_incidents_portail p in
  match neutraliser () with
    Ok -> Array.iter (fun lien -> dellink lien.extr1 lien.extr2) liens; Ok
  | r -> r
;;
let neutraliser = "This should not be called anymore; use neutral instead";;
let make_link p =
  let p1 = position_agent (moi ()) in
  match (lier p) with
    Ok -> newline p p1; Ok
  | r -> r
;;
let lier = "This should not be called anymore; use make_link instead";;

let err f x =
  match (f x) with
    Ok -> ()
  | _ -> failwith ""
;;

let neutraliserf = err neutral;;
let capturerf = err capturer;;
let lierf = err make_link;;


let portails_joueur joueur =
  List.filter (fun pos -> portail_joueur pos = joueur) (Array.to_list (liste_portails ()))
;;

let score_champ ch = score_triangle ch.som1 ch.som2 ch.som3;;
let valeur_portail_now p =
  List.fold_left (+) 0 (List.map score_champ (Array.to_list (champs_incidents_portail p)))
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
  nb_blocking_links.(u).(v) > 0
;;
let valeur_portail_build p player =
  if Array.length (case_champs p) > 0 then
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

let valeur_portail p =
  let u = portail_joueur p in
  if u = (-1) then
    valeur_portail_build p (moi ())
  else if u = moi () then
    0
  else
    (valeur_portail_build p (moi ())) + (valeur_portail_now p)
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
  if u = (-1) then
  (*(float_of_int (valeur_portail_me p)) /. ((nn +. 1.) *. (nn +. 1.))*)
    -40 * n - 20 * da + (1 * (valeur_portail_build p (moi ())))
  else if u = moi () then
    min_int
  else
(*(float_of_int ((valeur_portail_me p) + (valeur_portail_adv p))) /. ((nn +. 1.) *. (nn +. 1.))*)
    -40 * n - 20 * da + (1 * (valeur_portail_build p (moi ())) + (valeur_portail_now p))
;;

let make_links () =
  List.iter (fun p -> ignore (make_link p)) (portails_joueur (moi ()))
;;

let shield_values = [|10; 50; 150; 1000; 2000; 4000; max_int|];;
let make_shields () =
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
let rec step () =
  make_links ();
  make_shields ();
  if (points_deplacement () > 0) && (points_action () >= cout_lien) then
    objective := None;
    (*if !objective = None then begin*)
  let portails_pas_a_moi =
    (List.filter (fun pos -> portail_joueur pos <> moi ())
       (Array.to_list (liste_portails ()))) in
  if portails_pas_a_moi = [] then () else begin
    let closest = max_list_key portails_pas_a_moi valeur_portail2 in
    print_string "Turn "; print_int (tour_actuel ());
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
      (*afficher_position pp; *)move_towards pp; ignore (neutral ()); ignore (capturer ()); make_links (); make_shields ()) chemin; 
    let player = portail_joueur (position_agent (moi ())) in
    if (player <> adversaire () && player <> (-1)) then begin
      if (points_deplacement ()) > 0 then step ()
      else if (utiliser_turbo ()) = Ok then step ()
    end else begin
      try
        if (player = adversaire ()) then begin neutraliserf () end;
        capturerf ();
        step ()
      with
        _ -> ()
    end)
and haunt () =
      (*captures := !captures @ (Array.to_list (hist_portails_captures ()));*)
      (*if score (moi ()) <= score (adversaire ()) || (score_tour (moi ()) < (score_tour (adversaire ()))) then*)
      step ();
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

(*
** Fonction appelée au début de la partie.
*)
let partie_init () =  (* Pose ton code ici *)
  flush stderr; flush stdout;; (* Pour que vos sorties s'affichent *)

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
    ignore (capturer ());
    for i = 1 to 6 do
      ignore (ajouter_bouclier ())
    done
  end;
  move_towards pp
;;

(*
** Fonction appelée à chaque tour.
*)
let jouer_tour () =  (* Pose ton code ici *)
  (*print_string "abc"; print_int (moi ()); print_int (tour_actuel ());*)
  (if moi () = 1 && tour_actuel () = 1 then
    step1 ()
  else
    haunt ());
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
