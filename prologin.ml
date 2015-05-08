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
let valeur_portail_now p =
  List.fold_left (+) 0 (List.map score_champ (Array.to_list (champs_incidents_portail p)))
;;
let rec list_product l1 l2 =
  match l1 with
    [] -> []
  | x :: t -> (List.map (fun y -> (x, y)) l2) @ (list_product t l2)
;;
let valeur_portail_build p player =
  if Array.length (case_champs p) > 0 then
    points_capture
  else
    let pp = list_product (portails_joueur player) (portails_joueur player) in
    (List.fold_left (+) 0
       (List.map
          (fun (p1, p2) ->
            if (p = p1 || p = p2 || p1 <= p2 ||
                  (not (lien_existe p1 p2)) ||
                  ((lien_existe p p1) && (lien_existe p p2)) ||
                  (Array.length (liens_bloquants p p1) > 0) ||
                  (Array.length (liens_bloquants p p2) > 0)) then
              0
            else
              score_triangle p p1 p2)
          pp)) + points_capture
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
    -40 * n + (1 * (valeur_portail_build p (moi ())))
  else if u = moi () then
    min_int
  else
(*(float_of_int ((valeur_portail_me p) + (valeur_portail_adv p))) /. ((nn +. 1.) *. (nn +. 1.))*)
    -40 * n + (1 * (valeur_portail_build p (moi ())) + (valeur_portail_now p))
;;

let make_links () =
  List.iter (fun p -> ignore (lier p)) (portails_joueur (moi ()))
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
  done
;;


let objective = ref None;;
let rec step () =
  make_links ();
  make_shields ();
  if (points_deplacement () > 0) && (points_action () >= cout_lien) then
    objective := None;
  if !objective = None then begin
    let portails_pas_a_moi =
      (List.filter (fun pos -> portail_joueur pos <> moi ())
         (Array.to_list (liste_portails ()))) in
    if portails_pas_a_moi = [] then () else begin
      let closest = max_list_key portails_pas_a_moi valeur_portail2 in
      print_string "Turn "; print_int (tour_actuel ());
      afficher_position closest; print_int (valeur_portail2 closest);
      objective := Some closest
    end
  end;
  match !objective with
    None -> ()
  | Some p ->
    move_towards p;
    let player = portail_joueur (position_agent (moi ())) in
    if (player <> adversaire () && player <> (-1)) then begin
      if (utiliser_turbo ()) = Ok then step ()
    end else begin
      try
        if (player = adversaire ()) then begin neutraliserf () end;
        capturerf ();
        step ()
      with
        _ -> ()
    end
;;

let should_take p =
(*(valeur_portail_adv p) >= 10 || (valeur_portail_me p) >= 50*)
  true
;;

let captures = ref [];;
let haunt () =
  step ();
  captures := !captures @ (Array.to_list (hist_portails_captures ()));
  begin
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
