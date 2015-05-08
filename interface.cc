///
// This file has been generated, if you wish to
// modify it in a permanent way, please refer
// to the script file : gen/generator_caml.rb
//

extern "C" {
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/alloc.h>
#include <caml/memory.h>
}
#include "interface.hh"

template <typename Lang, typename Cxx>
Lang cxx2lang(Cxx in)
{
  return in.__if_that_triggers_an_error_there_is_a_problem;
}

template <>
value cxx2lang<value, int>(int in)
{
  CAMLparam0();
  CAMLreturn(Val_int(in));
}

template<>
value cxx2lang<value, std::string>(std::string in)
{
  CAMLparam0();
  size_t l = in.length();
  char * out = (char *) malloc(l + 1);
  for (int i = 0; i < l; i++) out[i] = in[i];
  out[l] = 0;
  CAMLreturn(caml_copy_string(out));
}

template <>
value cxx2lang<value, bool>(bool in)
{
  CAMLparam0();
  CAMLreturn(Val_int(in));
}

template <typename Cxx>
value cxx2lang_array(const std::vector<Cxx>& in)
{
  CAMLparam0();
  CAMLlocal1(v);

  size_t size = in.size();
  if (size == 0)
    CAMLreturn(Atom(0));

  v = caml_alloc(size, 0);
  for (int i = 0; i < size; ++i)
    caml_initialize(&Field(v, i), cxx2lang<value, Cxx>(in[i]));

  CAMLreturn(v);
}

template <typename Lang, typename Cxx>
Cxx lang2cxx(Lang in)
{
  return in.__if_that_triggers_an_error_there_is_a_problem;
}

template<>
std::string lang2cxx<value, std::string>(value in)
{
  CAMLparam1(in);
  CAMLreturnT(std::string, String_val(in));
}

template <>
int lang2cxx<value, int>(value in)
{
  CAMLparam1(in);
  CAMLreturnT(int, Int_val(in));
}

template <>
bool lang2cxx<value, bool>(value in)
{
  CAMLparam1(in);
  CAMLreturnT(bool, Int_val(in));
}

template <typename Cxx>
std::vector<Cxx> lang2cxx_array(value in)
{
  CAMLparam1(in);
  std::vector<Cxx> out;
  mlsize_t size = Wosize_val(in);

  for (int i = 0; i < size; ++i)
    out.push_back(lang2cxx<value, Cxx>(Field(in, i)));

  CAMLreturnT(std::vector<Cxx>, out);
}
///
// Erreurs possibles
//
template <>
value cxx2lang<value, erreur>(erreur in)
{
  CAMLparam0();
  CAMLreturn(Val_int(in));
}

template <>
erreur lang2cxx<value, erreur>(value in)
{
  CAMLparam1(in);
  CAMLreturnT(erreur, (erreur)Int_val(in));
}

///
// Position sur la carte, donnée par deux coordonnées.
//
template <>
value cxx2lang<value, position>(position in)
{
  CAMLparam0();
  CAMLlocal1(out);
  out = caml_alloc(2, 0);
  caml_initialize(&Field(out, 0), cxx2lang<value, int>(in.x));
  caml_initialize(&Field(out, 1), cxx2lang<value, int>(in.y));
  CAMLreturn(out);
}

template <>
position lang2cxx<value, position>(value in)
{
  CAMLparam1(in);
  position out;
  out.x = lang2cxx<value, int>(Field(in, 0));
  out.y = lang2cxx<value, int>(Field(in, 1));
  CAMLreturnT(position, out);
}

///
// Représente un lien existant.
//
template <>
value cxx2lang<value, lien>(lien in)
{
  CAMLparam0();
  CAMLlocal1(out);
  out = caml_alloc(3, 0);
  caml_initialize(&Field(out, 0), cxx2lang<value, position>(in.extr1));
  caml_initialize(&Field(out, 1), cxx2lang<value, position>(in.extr2));
  caml_initialize(&Field(out, 2), cxx2lang<value, int>(in.joueur_l));
  CAMLreturn(out);
}

template <>
lien lang2cxx<value, lien>(value in)
{
  CAMLparam1(in);
  lien out;
  out.extr1 = lang2cxx<value, position>(Field(in, 0));
  out.extr2 = lang2cxx<value, position>(Field(in, 1));
  out.joueur_l = lang2cxx<value, int>(Field(in, 2));
  CAMLreturnT(lien, out);
}

///
// Représente un champ de contrôle existant.
//
template <>
value cxx2lang<value, champ>(champ in)
{
  CAMLparam0();
  CAMLlocal1(out);
  out = caml_alloc(4, 0);
  caml_initialize(&Field(out, 0), cxx2lang<value, position>(in.som1));
  caml_initialize(&Field(out, 1), cxx2lang<value, position>(in.som2));
  caml_initialize(&Field(out, 2), cxx2lang<value, position>(in.som3));
  caml_initialize(&Field(out, 3), cxx2lang<value, int>(in.joueur_c));
  CAMLreturn(out);
}

template <>
champ lang2cxx<value, champ>(value in)
{
  CAMLparam1(in);
  champ out;
  out.som1 = lang2cxx<value, position>(Field(in, 0));
  out.som2 = lang2cxx<value, position>(Field(in, 1));
  out.som3 = lang2cxx<value, position>(Field(in, 2));
  out.joueur_c = lang2cxx<value, int>(Field(in, 3));
  CAMLreturnT(champ, out);
}

/*
** Inititialize caml
*/
static inline void _init_caml()
{
    static bool is_initialized = false;

    if (!is_initialized)
    {
        is_initialized = true;

        const char* argv[2] = {"./caml", NULL};
        caml_startup(const_cast<char**>(argv));
    }
}

///
// Déplace votre agent sur la case passée en argument.
//
extern "C" value ml_deplacer(value dest)
{
  CAMLparam0();
  CAMLxparam1(dest);
  CAMLreturn((cxx2lang<value, erreur>(api_deplacer(lang2cxx<value, position>(dest)))));
}

///
// Utilise un turbo.
//
extern "C" value ml_utiliser_turbo(value unit)
{
  CAMLparam0();
  CAMLxparam1(unit);
  CAMLreturn((cxx2lang<value, erreur>(api_utiliser_turbo())));
}

///
// Capture le portail où est positionné votre agent.
//
extern "C" value ml_capturer(value unit)
{
  CAMLparam0();
  CAMLxparam1(unit);
  CAMLreturn((cxx2lang<value, erreur>(api_capturer())));
}

///
// Crée un lien entre le portail où se trouve votre agent et le portail de destination donné en argument.
//
extern "C" value ml_lier(value portail)
{
  CAMLparam0();
  CAMLxparam1(portail);
  CAMLreturn((cxx2lang<value, erreur>(api_lier(lang2cxx<value, position>(portail)))));
}

///
// Neutralise le portail où se trouve votre agent.
//
extern "C" value ml_neutraliser(value unit)
{
  CAMLparam0();
  CAMLxparam1(unit);
  CAMLreturn((cxx2lang<value, erreur>(api_neutraliser())));
}

///
// Ajoute un bouclier au portail sur lequel se trouve votre agent.
//
extern "C" value ml_ajouter_bouclier(value unit)
{
  CAMLparam0();
  CAMLxparam1(unit);
  CAMLreturn((cxx2lang<value, erreur>(api_ajouter_bouclier())));
}

///
// Renvoie la liste de tous les liens présents.
//
extern "C" value ml_liste_liens(value unit)
{
  CAMLparam0();
  CAMLxparam1(unit);
  CAMLreturn((cxx2lang_array<lien>(api_liste_liens())));
}

///
// Renvoie la liste de tous les champs de contrôle.
//
extern "C" value ml_liste_champs(value unit)
{
  CAMLparam0();
  CAMLxparam1(unit);
  CAMLreturn((cxx2lang_array<champ>(api_liste_champs())));
}

///
// Renvoie la liste de tous les portails de la carte.
//
extern "C" value ml_liste_portails(value unit)
{
  CAMLparam0();
  CAMLxparam1(unit);
  CAMLreturn((cxx2lang_array<position>(api_liste_portails())));
}

///
// Renvoie la liste de tous les liens existants qui croisent un segment, entravant la création d'un lien.
//
extern "C" value ml_liens_bloquants(value ext1, value ext2)
{
  CAMLparam0();
  CAMLxparam2(ext1, ext2);
  CAMLreturn((cxx2lang_array<lien>(api_liens_bloquants(lang2cxx<value, position>(ext1), lang2cxx<value, position>(ext2)))));
}

///
// Prend les positions de deux portails, et renvoie un booléen indiquant s'ils sont reliés. Le résultat est `false` lorsque l'une des deux positions ne repère pas un portail.
//
extern "C" value ml_lien_existe(value ext1, value ext2)
{
  CAMLparam0();
  CAMLxparam2(ext1, ext2);
  CAMLreturn((cxx2lang<value, bool>(api_lien_existe(lang2cxx<value, position>(ext1), lang2cxx<value, position>(ext2)))));
}

///
// Renvoie un booléen indiquant si les 3 positions repèrent bien 3 portails tous reliés entre eux.
//
extern "C" value ml_champ_existe(value som1, value som2, value som3)
{
  CAMLparam0();
  CAMLxparam3(som1, som2, som3);
  CAMLreturn((cxx2lang<value, bool>(api_champ_existe(lang2cxx<value, position>(som1), lang2cxx<value, position>(som2), lang2cxx<value, position>(som3)))));
}

///
// Renvoie un booléen indiquant si la case ``pos`` se trouve dans un champ.
//
extern "C" value ml_case_dans_champ(value pos)
{
  CAMLparam0();
  CAMLxparam1(pos);
  CAMLreturn((cxx2lang<value, bool>(api_case_dans_champ(lang2cxx<value, position>(pos)))));
}

///
// Renvoie la liste des champs à l'intérieur desquels ``pos`` se trouve. Si la case est un portail, le résultat de ``case_champs`` sera disjoint de celui de ``champs_incidents_portail``.
//
extern "C" value ml_case_champs(value pos)
{
  CAMLparam0();
  CAMLxparam1(pos);
  CAMLreturn((cxx2lang_array<champ>(api_case_champs(lang2cxx<value, position>(pos)))));
}

///
// Renvoie le numéro du joueur correspondant au portail donné, -1 si le portail est neutre, -2 si la case n'est pas un portail. Vous pouvez utiliser cette fonction pour vérifier qu'une case donnée est bien un portail.
//
extern "C" value ml_portail_joueur(value portail)
{
  CAMLparam0();
  CAMLxparam1(portail);
  CAMLreturn((cxx2lang<value, int>(api_portail_joueur(lang2cxx<value, position>(portail)))));
}

///
// Renvoie le nombre de boucliers présents sur un portail (-2 si la case n'est pas un portail).
//
extern "C" value ml_portail_boucliers(value portail)
{
  CAMLparam0();
  CAMLxparam1(portail);
  CAMLreturn((cxx2lang<value, int>(api_portail_boucliers(lang2cxx<value, position>(portail)))));
}

///
// Renvoie la liste de tous les liens dont le portail donné est une extrémité.
//
extern "C" value ml_liens_incidents_portail(value portail)
{
  CAMLparam0();
  CAMLxparam1(portail);
  CAMLreturn((cxx2lang_array<lien>(api_liens_incidents_portail(lang2cxx<value, position>(portail)))));
}

///
// Renvoie la liste de tous les champs dont le portail donné est un sommet.
//
extern "C" value ml_champs_incidents_portail(value portail)
{
  CAMLparam0();
  CAMLxparam1(portail);
  CAMLreturn((cxx2lang_array<champ>(api_champs_incidents_portail(lang2cxx<value, position>(portail)))));
}

///
// Renvoie la liste de tous les champs dont le lien donné est un côté. Si le segment n'est pas un lien présent, renvoie la liste de tous les champs que la création du lien ferait apparaître.
//
extern "C" value ml_champs_incidents_segment(value ext1, value ext2)
{
  CAMLparam0();
  CAMLxparam2(ext1, ext2);
  CAMLreturn((cxx2lang_array<champ>(api_champs_incidents_segment(lang2cxx<value, position>(ext1), lang2cxx<value, position>(ext2)))));
}

///
// Renvoie la liste des portails capturés par votre adversaire au dernier tour.
//
extern "C" value ml_hist_portails_captures(value unit)
{
  CAMLparam0();
  CAMLxparam1(unit);
  CAMLreturn((cxx2lang_array<position>(api_hist_portails_captures())));
}

///
// Renvoie la liste des portails neutralisés par votre adversaire au dernier tour.
//
extern "C" value ml_hist_portails_neutralises(value unit)
{
  CAMLparam0();
  CAMLxparam1(unit);
  CAMLreturn((cxx2lang_array<position>(api_hist_portails_neutralises())));
}

///
// Renvoie la liste des liens créés par votre adversaire au dernier tour.
//
extern "C" value ml_hist_liens_crees(value unit)
{
  CAMLparam0();
  CAMLxparam1(unit);
  CAMLreturn((cxx2lang_array<lien>(api_hist_liens_crees())));
}

///
// Renvoie la liste des champs créés par votre adversaire au dernier tour.
//
extern "C" value ml_hist_champs_crees(value unit)
{
  CAMLparam0();
  CAMLxparam1(unit);
  CAMLreturn((cxx2lang_array<champ>(api_hist_champs_crees())));
}

///
// Renvoie la liste des positions où votre adversaire a ajouté des boucliers au dernier tour.
//
extern "C" value ml_hist_boucliers_ajoutes(value unit)
{
  CAMLparam0();
  CAMLxparam1(unit);
  CAMLreturn((cxx2lang_array<position>(api_hist_boucliers_ajoutes())));
}

///
// Renvoie la distance de Manhattan entre deux positions.
//
extern "C" value ml_distance(value pos1, value pos2)
{
  CAMLparam0();
  CAMLxparam2(pos1, pos2);
  CAMLreturn((cxx2lang<value, int>(api_distance(lang2cxx<value, position>(pos1), lang2cxx<value, position>(pos2)))));
}

///
// Renvoie le nombre de points que rapporte(rait) chaque tour un champ existant ou hypothétique.
//
extern "C" value ml_score_triangle(value som1, value som2, value som3)
{
  CAMLparam0();
  CAMLxparam3(som1, som2, som3);
  CAMLreturn((cxx2lang<value, int>(api_score_triangle(lang2cxx<value, position>(som1), lang2cxx<value, position>(som2), lang2cxx<value, position>(som3)))));
}

///
// Indique si deux segments se croisent. Cette fonction correspond exactement à la condition d'interférence entre liens, c'est-à-dire qu'elle renvoie ``false`` si l'intersection est une extrémité des deux segments.
//
extern "C" value ml_intersection_segments(value a1, value a2, value b1, value b2)
{
  CAMLparam0();
  CAMLxparam4(a1, a2, b1, b2);
  CAMLreturn((cxx2lang<value, bool>(api_intersection_segments(lang2cxx<value, position>(a1), lang2cxx<value, position>(a2), lang2cxx<value, position>(b1), lang2cxx<value, position>(b2)))));
}

///
// Indique si un point se trouve à l'intérieur d'un triangle. Le critère coïncide avec celui de ``case_champs``.
//
extern "C" value ml_point_dans_triangle(value p, value som1, value som2, value som3)
{
  CAMLparam0();
  CAMLxparam4(p, som1, som2, som3);
  CAMLreturn((cxx2lang<value, bool>(api_point_dans_triangle(lang2cxx<value, position>(p), lang2cxx<value, position>(som1), lang2cxx<value, position>(som2), lang2cxx<value, position>(som3)))));
}

///
// Renvoie votre numéro de joueur.
//
extern "C" value ml_moi(value unit)
{
  CAMLparam0();
  CAMLxparam1(unit);
  CAMLreturn((cxx2lang<value, int>(api_moi())));
}

///
// Renvoie le numéro de votre adversaire.
//
extern "C" value ml_adversaire(value unit)
{
  CAMLparam0();
  CAMLxparam1(unit);
  CAMLreturn((cxx2lang<value, int>(api_adversaire())));
}

///
// Indique la position de l'agent du joueur désigné par le numéro ``id_joueur``.
//
extern "C" value ml_position_agent(value id_joueur)
{
  CAMLparam0();
  CAMLxparam1(id_joueur);
  CAMLreturn((cxx2lang<value, position>(api_position_agent(lang2cxx<value, int>(id_joueur)))));
}

///
// Indique votre nombre de points d'actions restants pour ce tour-ci.
//
extern "C" value ml_points_action(value unit)
{
  CAMLparam0();
  CAMLxparam1(unit);
  CAMLreturn((cxx2lang<value, int>(api_points_action())));
}

///
// Indique votre nombre de points de déplacement restants pour ce tour-ci.
//
extern "C" value ml_points_deplacement(value unit)
{
  CAMLparam0();
  CAMLxparam1(unit);
  CAMLreturn((cxx2lang<value, int>(api_points_deplacement())));
}

///
// Renvoie le score du joueur désigné par le numéro ``id_joueur``.
//
extern "C" value ml_score(value id_joueur)
{
  CAMLparam0();
  CAMLxparam1(id_joueur);
  CAMLreturn((cxx2lang<value, int>(api_score(lang2cxx<value, int>(id_joueur)))));
}

///
// Renvoie le numéro du tour actuel.
//
extern "C" value ml_tour_actuel(value unit)
{
  CAMLparam0();
  CAMLxparam1(unit);
  CAMLreturn((cxx2lang<value, int>(api_tour_actuel())));
}

///
// Annule la dernière action. Renvoie ``false`` quand il n'y a pas d'action à annuler ce tour-ci.
//
extern "C" value ml_annuler(value unit)
{
  CAMLparam0();
  CAMLxparam1(unit);
  CAMLreturn((cxx2lang<value, bool>(api_annuler())));
}

///
// Affiche le contenu d'une valeur de type erreur
//
extern "C" value ml_afficher_erreur(value v)
{
  CAMLparam0();
  CAMLxparam1(v);
  api_afficher_erreur(lang2cxx<value, erreur>(v));
  CAMLreturn(Val_unit);
}

///
// Affiche le contenu d'une valeur de type position
//
extern "C" value ml_afficher_position(value v)
{
  CAMLparam0();
  CAMLxparam1(v);
  api_afficher_position(lang2cxx<value, position>(v));
  CAMLreturn(Val_unit);
}

///
// Affiche le contenu d'une valeur de type lien
//
extern "C" value ml_afficher_lien(value v)
{
  CAMLparam0();
  CAMLxparam1(v);
  api_afficher_lien(lang2cxx<value, lien>(v));
  CAMLreturn(Val_unit);
}

///
// Affiche le contenu d'une valeur de type champ
//
extern "C" value ml_afficher_champ(value v)
{
  CAMLparam0();
  CAMLxparam1(v);
  api_afficher_champ(lang2cxx<value, champ>(v));
  CAMLreturn(Val_unit);
}

///
// Fonction appelée au début de la partie.
//
void partie_init()
{
  _init_caml();
  CAMLparam0();
  CAMLlocal1(_ret);
  static value *closure = NULL;
  if (closure == NULL)
    closure = caml_named_value("ml_partie_init");
  _ret = callback(*closure, Val_unit);
  CAMLreturn0;
}


///
// Fonction appelée à chaque tour.
//
void jouer_tour()
{
  _init_caml();
  CAMLparam0();
  CAMLlocal1(_ret);
  static value *closure = NULL;
  if (closure == NULL)
    closure = caml_named_value("ml_jouer_tour");
  _ret = callback(*closure, Val_unit);
  CAMLreturn0;
}


///
// Fonction appelée à la fin de la partie.
//
void partie_fin()
{
  _init_caml();
  CAMLparam0();
  CAMLlocal1(_ret);
  static value *closure = NULL;
  if (closure == NULL)
    closure = caml_named_value("ml_partie_fin");
  _ret = callback(*closure, Val_unit);
  CAMLreturn0;
}


