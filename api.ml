(*
** This file has been generated, if you wish to
** modify it in a permanent way, please refer
** to the script file : gen/generator_caml.rb
*)

(*
** Taille du terrain (longueur et largeur).
*)
let taille_terrain = 30

(*
** Nombre de tours à jouer avant la fin de la partie.
*)
let nb_tours = 100

(*
** Nombre de points de déplacement par tour (avant utilisation du turbo).
*)
let nb_points_deplacement = 6

(*
** Nombre de points d'action par tour.
*)
let nb_points_action = 42

(*
** Nombre de points d'action que coûte la capture d'un portail.
*)
let cout_capture = 3

(*
** Nombre de points d'action que coûte la création d'un lien.
*)
let cout_lien = 2

(*
** Nombre de points d'action que coûte la neutralisation d'un portail de base.
*)
let cout_neutralisation = 10

(*
** Nombre de points d'action supplémentaires que coûte la neutralisation pour chaque bouclier présent.
*)
let cout_neutralisation_bouclier = 5

(*
** Nombre de points d'action que coûte la mise en place d'un bouclier sur un portail sans bouclier.
*)
let cout_bouclier = 3

(*
** Nombre de points d'action que coûte l'utilisation d'un turbo.
*)
let cout_turbo = 6

(*
** Nombre maximum de boucliers sur un même portail.
*)
let max_boucliers = 6

(*
** Nombre de points que rapporte la capture d'un portail.
*)
let points_capture = 10

(*
** Constante de proportionnalité reliant l'aire d'un champ au nombre de points qu'il rapporte par tour.
*)
let points_champ = 2

(*
** Erreurs possibles
*)
type erreur =
| Ok (* <- L'action a été exécutée avec succès *)
| Pa_insuffisants (* <- Vous ne possédez pas assez de points d'action pour cette action. *)
| Aucun_portail (* <- La position spécifiée n'est pas un portail. *)
| Position_invalide (* <- La position spécifiée est hors de la carte. *)
| Position_eloignee (* <- La destination est trop éloignée. *)
| Portail_ami (* <- Le portail vous appartient. *)
| Portail_neutre (* <- Le portail est neutre. *)
| Portail_ennemi (* <- Le portail appartient à votre adversaire. *)
| Lien_intersection (* <- Le lien croise un lien existant. *)
| Lien_champ (* <- Le lien se trouve dans un champ existant. *)
| Lien_degenere (* <- Les deux extrémités du lien coïncident. *)
| Limite_boucliers (* <- Ce portail est équipé du nombre maximal de boucliers. *)


(*
** Position sur la carte, donnée par deux coordonnées.
*)
type position = (int * int)

(*
** Représente un lien existant.
*)
type lien = {
  extr1 : position ; (* <- Première extrémité du lien. *)
  extr2 : position ; (* <- Seconde extrémité du lien. *)
  joueur_l : int ; (* <- Joueur possédant ce lien. *)
}

(*
** Représente un champ de contrôle existant.
*)
type champ = {
  som1 : position ; (* <- Premier sommet du champ. *)
  som2 : position ; (* <- Deuxième sommet du champ. *)
  som3 : position ; (* <- Troisième sommet du champ. *)
  joueur_c : int ; (* <- Joueur possédant ce champ. *)
}

(*
** Déplace votre agent sur la case passée en argument.
*)
external deplacer : position -> erreur = "ml_deplacer"
(*
** Utilise un turbo.
*)
external utiliser_turbo : unit -> erreur = "ml_utiliser_turbo"
(*
** Capture le portail où est positionné votre agent.
*)
external capturer : unit -> erreur = "ml_capturer"
(*
** Crée un lien entre le portail où se trouve votre agent et le portail de destination donné en argument.
*)
external lier : position -> erreur = "ml_lier"
(*
** Neutralise le portail où se trouve votre agent.
*)
external neutraliser : unit -> erreur = "ml_neutraliser"
(*
** Ajoute un bouclier au portail sur lequel se trouve votre agent.
*)
external ajouter_bouclier : unit -> erreur = "ml_ajouter_bouclier"
(*
** Renvoie la liste de tous les liens présents.
*)
external liste_liens : unit -> lien array = "ml_liste_liens"
(*
** Renvoie la liste de tous les champs de contrôle.
*)
external liste_champs : unit -> champ array = "ml_liste_champs"
(*
** Renvoie la liste de tous les portails de la carte.
*)
external liste_portails : unit -> position array = "ml_liste_portails"
(*
** Renvoie la liste de tous les liens existants qui croisent un segment, entravant la création d'un lien.
*)
external liens_bloquants : position -> position -> lien array = "ml_liens_bloquants"
(*
** Prend les positions de deux portails, et renvoie un booléen indiquant s'ils sont reliés. Le résultat est `false` lorsque l'une des deux positions ne repère pas un portail.
*)
external lien_existe : position -> position -> bool = "ml_lien_existe"
(*
** Renvoie un booléen indiquant si les 3 positions repèrent bien 3 portails tous reliés entre eux.
*)
external champ_existe : position -> position -> position -> bool = "ml_champ_existe"
(*
** Renvoie un booléen indiquant si la case ``pos`` se trouve dans un champ.
*)
external case_dans_champ : position -> bool = "ml_case_dans_champ"
(*
** Renvoie la liste des champs à l'intérieur desquels ``pos`` se trouve. Si la case est un portail, le résultat de ``case_champs`` sera disjoint de celui de ``champs_incidents_portail``.
*)
external case_champs : position -> champ array = "ml_case_champs"
(*
** Renvoie le numéro du joueur correspondant au portail donné, -1 si le portail est neutre, -2 si la case n'est pas un portail. Vous pouvez utiliser cette fonction pour vérifier qu'une case donnée est bien un portail.
*)
external portail_joueur : position -> int = "ml_portail_joueur"
(*
** Renvoie le nombre de boucliers présents sur un portail (-2 si la case n'est pas un portail).
*)
external portail_boucliers : position -> int = "ml_portail_boucliers"
(*
** Renvoie la liste de tous les liens dont le portail donné est une extrémité.
*)
external liens_incidents_portail : position -> lien array = "ml_liens_incidents_portail"
(*
** Renvoie la liste de tous les champs dont le portail donné est un sommet.
*)
external champs_incidents_portail : position -> champ array = "ml_champs_incidents_portail"
(*
** Renvoie la liste de tous les champs dont le lien donné est un côté. Si le segment n'est pas un lien présent, renvoie la liste de tous les champs que la création du lien ferait apparaître.
*)
external champs_incidents_segment : position -> position -> champ array = "ml_champs_incidents_segment"
(*
** Renvoie la liste des portails capturés par votre adversaire au dernier tour.
*)
external hist_portails_captures : unit -> position array = "ml_hist_portails_captures"
(*
** Renvoie la liste des portails neutralisés par votre adversaire au dernier tour.
*)
external hist_portails_neutralises : unit -> position array = "ml_hist_portails_neutralises"
(*
** Renvoie la liste des liens créés par votre adversaire au dernier tour.
*)
external hist_liens_crees : unit -> lien array = "ml_hist_liens_crees"
(*
** Renvoie la liste des champs créés par votre adversaire au dernier tour.
*)
external hist_champs_crees : unit -> champ array = "ml_hist_champs_crees"
(*
** Renvoie la liste des positions où votre adversaire a ajouté des boucliers au dernier tour.
*)
external hist_boucliers_ajoutes : unit -> position array = "ml_hist_boucliers_ajoutes"
(*
** Renvoie la distance de Manhattan entre deux positions.
*)
external distance : position -> position -> int = "ml_distance"
(*
** Renvoie le nombre de points que rapporte(rait) chaque tour un champ existant ou hypothétique.
*)
external score_triangle : position -> position -> position -> int = "ml_score_triangle"
(*
** Indique si deux segments se croisent. Cette fonction correspond exactement à la condition d'interférence entre liens, c'est-à-dire qu'elle renvoie ``false`` si l'intersection est une extrémité des deux segments.
*)
external intersection_segments : position -> position -> position -> position -> bool = "ml_intersection_segments"
(*
** Indique si un point se trouve à l'intérieur d'un triangle. Le critère coïncide avec celui de ``case_champs``.
*)
external point_dans_triangle : position -> position -> position -> position -> bool = "ml_point_dans_triangle"
(*
** Renvoie votre numéro de joueur.
*)
external moi : unit -> int = "ml_moi"
(*
** Renvoie le numéro de votre adversaire.
*)
external adversaire : unit -> int = "ml_adversaire"
(*
** Indique la position de l'agent du joueur désigné par le numéro ``id_joueur``.
*)
external position_agent : int -> position = "ml_position_agent"
(*
** Indique votre nombre de points d'actions restants pour ce tour-ci.
*)
external points_action : unit -> int = "ml_points_action"
(*
** Indique votre nombre de points de déplacement restants pour ce tour-ci.
*)
external points_deplacement : unit -> int = "ml_points_deplacement"
(*
** Renvoie le score du joueur désigné par le numéro ``id_joueur``.
*)
external score : int -> int = "ml_score"
(*
** Renvoie le numéro du tour actuel.
*)
external tour_actuel : unit -> int = "ml_tour_actuel"
(*
** Annule la dernière action. Renvoie ``false`` quand il n'y a pas d'action à annuler ce tour-ci.
*)
external annuler : unit -> bool = "ml_annuler"
(*
** Affiche le contenu d'une valeur de type erreur
*)
external afficher_erreur : erreur -> unit = "ml_afficher_erreur"
(*
** Affiche le contenu d'une valeur de type position
*)
external afficher_position : position -> unit = "ml_afficher_position"
(*
** Affiche le contenu d'une valeur de type lien
*)
external afficher_lien : lien -> unit = "ml_afficher_lien"
(*
** Affiche le contenu d'une valeur de type champ
*)
external afficher_champ : champ -> unit = "ml_afficher_champ"
