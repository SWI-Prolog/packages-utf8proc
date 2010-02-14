#include <SWI-Prolog.h>
#include "utf8proc.h"

static atom_t ATOM_stable;
static atom_t ATOM_compose;
static atom_t ATOM_decompose;
static atom_t ATOM_ignore;
static atom_t ATOM_rejectna;
static atom_t ATOM_nlf2ls;
static atom_t ATOM_nlf2ps;
static atom_t ATOM_nlf2lf;
static atom_t ATOM_stripcc;
static atom_t ATOM_casefold;
static atom_t ATOM_charbound;
static atom_t ATOM_lump;
static atom_t ATOM_stripmark;


static int
type_error(const char *expected, term_t actual)
{ term_t ex;

  if ( (ex = PL_new_term_ref()) &&
       PL_unify_term(ex,
                     PL_FUNCTOR, FUNCTOR_error2,
                       PL_FUNCTOR, FUNCTOR_type_error2,
                         PL_CHARS, expected,
                         PL_TERM, actual,
                       PL_VARIABLE) )
    return PL_raise_exception(ex);

  return FALSE;
}


static int
domain_error(const char *domain, term_t actual)
{ term_t ex;

  if ( (ex = PL_new_term_ref()) &&
       PL_unify_term(ex,
                     PL_FUNCTOR, FUNCTOR_error2,
                       PL_FUNCTOR, FUNCTOR_domain_error2,
                         PL_CHARS, domain,
                         PL_TERM, actual,
                       PL_VARIABLE) )
  return PL_raise_exception(ex);

  return FALSE;
}


/** unicode_property(?Code, ?property)

*/

		 /*******************************
		 *	      MAPPING		*
		 *******************************/

static int
get_map_mask(term_t t, int *mask)
{ int m = 0;
  term_t tail = PL_copy_term_ref(t);
  term_t head = PL_new_term_ref();

  while ( PL_get_list(tail, head, tail) )
  { atom_t a;

    if ( !PL_get_atom(head, &a) )
      return type_error("atom", head);
    if ( a == ATOM_stable )
      m |= STABLE;
    else if ( a == ATOM_compose )
      m |= COMPOSE;
    else if ( a == ATOM_decompose )
      m |= DECOMPOSE;
    else if ( a == ATOM_ignore )
      m |= IGNORE;
    else if ( a == ATOM_rejectna )
      m |= REJECTNA;
    else if ( a == ATOM_nlf2ls )
      m |= NLF2LS;
    else if ( a == ATOM_nlf2ps )
      m |= NLF2PS;
    else if ( a == ATOM_nlf2lf )
      m |= NLF2LF;
    else if ( a == ATOM_stripcc )
      m |= STRIPCC;
    else if ( a == ATOM_casefold )
      m |= CASEFOLD;
    else if ( a == ATOM_charbound )
      m |= CHARBOUND;
    else if ( a == ATOM_lump )
      m |= LUMP;
    else if ( a == ATOM_stripmark )
      m |= STRIPMARK;
    else
      return domain_error("unicode_mapping", head);
  }

  if ( !PL_get_nil(tail) )
    return type_error("list", tail);

  *mask = m;

  return TRUE;
}



/** unicode_map(+In, -Out, +Options)
*/

static foreign_t
unicode_map(term_t in, term_t out, term_t options)
{ int mask;
  size_t len_in;
  char *utf8_in;
  char *utf8_out;

  if ( !get_map_mask(options, &mask) )
    return FALSE;
  if ( !PL_get_chars(in, &len_in, &utf8_in,
		     CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION|
		     REP_UTF8) )
    return FALSE;

}


/** '$unicode_map'(+In, -Out, +Mask)
*/

/** '$unicode_mask'(+Options, -Mask)
*/


#define MKATOM(n) \
        ATOM_ ## n = PL_new_atom(#n)
#define MKFUNCTOR(n,a) \
        FUNCTOR_ ## n ## a = PL_new_functor(PL_new_atom(#n), a)

install_t
install_unicode4pl
{ MKATOM(stable);
  MKATOM(compose);
  MKATOM(decompose);
  MKATOM(ignore);
  MKATOM(rejectna);
  MKATOM(nlf2ls);
  MKATOM(nlf2ps);
  MKATOM(nlf2lf);
  MKATOM(stripcc);
  MKATOM(casefold);
  MKATOM(charbound);
  MKATOM(lump);
  MKATOM(stripmark);
}
