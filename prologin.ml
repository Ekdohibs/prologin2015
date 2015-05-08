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


let move_towards (x, y) =
  let d = points_deplacement () in
  let (xx, yy) = position_agent (moi ()) in
  let adx = min (abs (x - xx)) d in
  let ady = min (abs (y - yy)) (d - adx) in
  let dx = if xx < x then adx else -adx in
  let dy = if yy < y then ady else -ady in
  ignore (deplacer (xx + dx, yy + dy))
;;

let err f x =
  match (f x) with
    Ok -> ()
  | _ -> failwith ""
;;

let neutraliserf = err neutraliser;;
let capturerf = err capturer;;
let lierf = err lier;;


let portails_joueur joueur =
  List.filter (fun pos -> portail_joueur pos = joueur) (Array.to_list (liste_portails ()))
;;

let score_champ ch = score_triangle ch.som1 ch.som2 ch.som3;;
let valeur_portail_adv p =
  List.fold_left (+) 0 (List.map score_champ (Array.to_list (champs_incidents_portail p)))
;;
let valeur_portail_me p =
  (List.fold_left (+) 0
    (List.map
       (fun pp -> if (p = pp || lien_existe p pp) then 0 else
           List.fold_left (+) 0 (List.map score_champ (Array.to_list (champs_incidents_segment p pp))))
    (portails_joueur (moi ())))) + points_capture

let valeur_portail p =
  let u = portail_joueur p in
  if u = (-1) then
    valeur_portail_me p
  else if u = moi () then
    0
  else
    (valeur_portail_me p) + (valeur_portail_adv p)
;;

let nbtours dist = (dist + nb_points_deplacement - 1) / nb_points_deplacement;;


let valeur_portail2 p =
  let u = portail_joueur p in
  let d = distance p (position_agent (moi ())) in
  (*let n = nbtours d in*)
  let n = d in
  let nn = float_of_int n in
  let da = distance p (position_agent (adversaire ())) in
  let tours_restants = nb_tours - tour_actuel () - n in
  if u = (-1) then
    (float_of_int (valeur_portail_me p)) /. ((nn +. 1.) *. (nn +. 1.))
  else if u = moi () then
    0.
  else
    (float_of_int ((valeur_portail_me p) + (valeur_portail_adv p))) /. ((nn +. 1.) *. (nn +. 1.))
;;

let make_links () =
  List.iter (fun p -> ignore (lier p)) (portails_joueur (moi ()))
;;
        

let rec step () =
  make_links ();
  let portails_pas_a_moi =
    (List.filter (fun pos -> portail_joueur pos <> moi ())
       (Array.to_list (liste_portails ()))) in
  if portails_pas_a_moi = [] then () else begin
    (*let closest = min_list_key portails_pas_a_moi
      (fun distance (position_agent (moi ()))) in*)
    let mypos = position_agent (moi ()) in
    let advpos = position_agent (adversaire ()) in
    (*let closest = max_list_key portails_pas_a_moi
      (fun p -> 0.001 *. (float_of_int (distance p advpos)) +.
      (float_of_int (valeur_portail p)) /. (float_of_int (nbtours (distance mypos p)))) in *)
    (*let closest = max_list_key portails_pas_a_moi valeur_portail2 in*)
    let close = min_list_key portails_pas_a_moi (distance mypos) in
    let dd = (distance close mypos) in
    let closest = max_list_key (List.filter (fun p -> ((distance mypos p)) <= dd) portails_pas_a_moi) valeur_portail2 in
    move_towards closest;
    let pp = portail_joueur closest in
    if (pp <> adversaire () && pp <> (-1)) then begin
      if (utiliser_turbo ()) = Ok then step ()
    end else begin
      try
        if (pp = adversaire ()) then begin neutraliserf () end;
        capturerf ();
        step ()
      with
        _ -> ()
    end
  end
;;

let captures = ref [];;
let haunt () =
  captures := !captures @ (Array.to_list (hist_portails_captures ()));
  begin
  try
    while (!captures <> []) && ((points_action () >= cout_turbo) || (points_deplacement () > 0)) do
      let p = List.hd !captures in
      move_towards p;
      ignore (neutraliser ());
      ignore (capturer ());
      make_links ();
      if (points_deplacement () = 0) then
        ignore (utiliser_turbo ())
      else if (portail_joueur p) = moi () && points_action () > cout_lien then
        captures := List.tl !captures
      else
        failwith ""
    done
  with
    _ -> ()
  end
  ;
  if (!captures = []) then (* back to normal way *)
    step ()
    
(*move_towards (position_agent (adversaire ()))*)
;;

(*
** Fonction appelée au début de la partie.
*)
let partie_init () =  (* Pose ton code ici *)
  flush stderr; flush stdout;; (* Pour que vos sorties s'affichent *)

(*
** Fonction appelée à chaque tour.
*)
let jouer_tour () =  (* Pose ton code ici *)
  (*print_int 0;
    step ();*)
  haunt ();
  flush stderr; flush stdout;; (* Pour que vos sorties s'affichent *)

(*
** Fonction appelée à la fin de la partie.
*)
let partie_fin () =  (* Pose ton code ici *)
  flush stderr; flush stdout;; (* Pour que vos sorties s'affichent *)

(* /!\ Ne touche pas a ce qui suit /!\ *)
Callback.register "ml_partie_init" partie_init;;Callback.register "ml_jouer_tour" jouer_tour;;Callback.register "ml_partie_fin" partie_fin;;
