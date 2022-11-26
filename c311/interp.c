#include <setjmp.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include "interp.h"

void *ktr_emptyr__m__k (void *dismountr__m__trampoline)
{
  kt *_data = (kt *) malloc (sizeof (kt));

  if (!_data) {
    fprintf (stderr, "Out of memory\n");
    exit (1);
  }
  _data->tag = _emptyr__m__k_kt;
  _data->u._emptyr__m__k._dismountr__m__trampoline = dismountr__m__trampoline;
  return (void *) _data;
}

void *ktr_r__t__r__m__multr__m__innerr__m__k (void *vr__ex__, void *k)
{
  kt *_data = (kt *) malloc (sizeof (kt));

  if (!_data) {
    fprintf (stderr, "Out of memory\n");
    exit (1);
  }
  _data->tag = _r__t__r__m__multr__m__innerr__m__k_kt;
  _data->u._r__t__r__m__multr__m__innerr__m__k._vr__ex__ = vr__ex__;
  _data->u._r__t__r__m__multr__m__innerr__m__k._k = k;
  return (void *) _data;
}

void *ktr_r__t__r__m__multr__m__outerr__m__k (void *xr2, void *env, void *k)
{
  kt *_data = (kt *) malloc (sizeof (kt));

  if (!_data) {
    fprintf (stderr, "Out of memory\n");
    exit (1);
  }
  _data->tag = _r__t__r__m__multr__m__outerr__m__k_kt;
  _data->u._r__t__r__m__multr__m__outerr__m__k._xr2 = xr2;
  _data->u._r__t__r__m__multr__m__outerr__m__k._env = env;
  _data->u._r__t__r__m__multr__m__outerr__m__k._k = k;
  return (void *) _data;
}

void *ktr_r__t__r__m__subr1r__m__k (void *k)
{
  kt *_data = (kt *) malloc (sizeof (kt));

  if (!_data) {
    fprintf (stderr, "Out of memory\n");
    exit (1);
  }
  _data->tag = _r__t__r__m__subr1r__m__k_kt;
  _data->u._r__t__r__m__subr1r__m__k._k = k;
  return (void *) _data;
}

void *ktr_r__t__r__m__zeror__m__k (void *k)
{
  kt *_data = (kt *) malloc (sizeof (kt));

  if (!_data) {
    fprintf (stderr, "Out of memory\n");
    exit (1);
  }
  _data->tag = _r__t__r__m__zeror__m__k_kt;
  _data->u._r__t__r__m__zeror__m__k._k = k;
  return (void *) _data;
}

void *ktr_r__t__r__m__ifr__m__k (void *conseq, void *alt, void *env, void *k)
{
  kt *_data = (kt *) malloc (sizeof (kt));

  if (!_data) {
    fprintf (stderr, "Out of memory\n");
    exit (1);
  }
  _data->tag = _r__t__r__m__ifr__m__k_kt;
  _data->u._r__t__r__m__ifr__m__k._conseq = conseq;
  _data->u._r__t__r__m__ifr__m__k._alt = alt;
  _data->u._r__t__r__m__ifr__m__k._env = env;
  _data->u._r__t__r__m__ifr__m__k._k = k;
  return (void *) _data;
}

void *ktr_r__t__r__m__letr__m__k (void *body, void *env, void *k)
{
  kt *_data = (kt *) malloc (sizeof (kt));

  if (!_data) {
    fprintf (stderr, "Out of memory\n");
    exit (1);
  }
  _data->tag = _r__t__r__m__letr__m__k_kt;
  _data->u._r__t__r__m__letr__m__k._body = body;
  _data->u._r__t__r__m__letr__m__k._env = env;
  _data->u._r__t__r__m__letr__m__k._k = k;
  return (void *) _data;
}

void *ktr_r__t__r__m__throwr__m__k (void *vr__m__exp, void *env)
{
  kt *_data = (kt *) malloc (sizeof (kt));

  if (!_data) {
    fprintf (stderr, "Out of memory\n");
    exit (1);
  }
  _data->tag = _r__t__r__m__throwr__m__k_kt;
  _data->u._r__t__r__m__throwr__m__k._vr__m__exp = vr__m__exp;
  _data->u._r__t__r__m__throwr__m__k._env = env;
  return (void *) _data;
}

void *ktr_r__t__r__m__randr__m__k (void *r, void *k)
{
  kt *_data = (kt *) malloc (sizeof (kt));

  if (!_data) {
    fprintf (stderr, "Out of memory\n");
    exit (1);
  }
  _data->tag = _r__t__r__m__randr__m__k_kt;
  _data->u._r__t__r__m__randr__m__k._r = r;
  _data->u._r__t__r__m__randr__m__k._k = k;
  return (void *) _data;
}

void *ktr_r__t__r__m__ratorr__m__k (void *rand, void *env, void *k)
{
  kt *_data = (kt *) malloc (sizeof (kt));

  if (!_data) {
    fprintf (stderr, "Out of memory\n");
    exit (1);
  }
  _data->tag = _r__t__r__m__ratorr__m__k_kt;
  _data->u._r__t__r__m__ratorr__m__k._rand = rand;
  _data->u._r__t__r__m__ratorr__m__k._env = env;
  _data->u._r__t__r__m__ratorr__m__k._k = k;
  return (void *) _data;
}

void *closr_closure (void *body, void *env)
{
  clos *_data = (clos *) malloc (sizeof (clos));

  if (!_data) {
    fprintf (stderr, "Out of memory\n");
    exit (1);
  }
  _data->tag = _closure_clos;
  _data->u._closure._body = body;
  _data->u._closure._env = env;
  return (void *) _data;
}

void *envrr_emptyr__m__env ()
{
  envr *_data = (envr *) malloc (sizeof (envr));

  if (!_data) {
    fprintf (stderr, "Out of memory\n");
    exit (1);
  }
  _data->tag = _emptyr__m__env_envr;
  return (void *) _data;
}

void *envrr_extendr__m__env (void *a, void *env)
{
  envr *_data = (envr *) malloc (sizeof (envr));

  if (!_data) {
    fprintf (stderr, "Out of memory\n");
    exit (1);
  }
  _data->tag = _extendr__m__env_envr;
  _data->u._extendr__m__env._a = a;
  _data->u._extendr__m__env._env = env;
  return (void *) _data;
}

void *exprr_const (void *n)
{
  expr *_data = (expr *) malloc (sizeof (expr));

  if (!_data) {
    fprintf (stderr, "Out of memory\n");
    exit (1);
  }
  _data->tag = _const_expr;
  _data->u._const._n = n;
  return (void *) _data;
}

void *exprr_var (void *n)
{
  expr *_data = (expr *) malloc (sizeof (expr));

  if (!_data) {
    fprintf (stderr, "Out of memory\n");
    exit (1);
  }
  _data->tag = _var_expr;
  _data->u._var._n = n;
  return (void *) _data;
}

void *exprr_if (void *test, void *conseq, void *alt)
{
  expr *_data = (expr *) malloc (sizeof (expr));

  if (!_data) {
    fprintf (stderr, "Out of memory\n");
    exit (1);
  }
  _data->tag = _if_expr;
  _data->u._if._test = test;
  _data->u._if._conseq = conseq;
  _data->u._if._alt = alt;
  return (void *) _data;
}

void *exprr_mult (void *expr1, void *expr2)
{
  expr *_data = (expr *) malloc (sizeof (expr));

  if (!_data) {
    fprintf (stderr, "Out of memory\n");
    exit (1);
  }
  _data->tag = _mult_expr;
  _data->u._mult._expr1 = expr1;
  _data->u._mult._expr2 = expr2;
  return (void *) _data;
}

void *exprr_subr1 (void *exp)
{
  expr *_data = (expr *) malloc (sizeof (expr));

  if (!_data) {
    fprintf (stderr, "Out of memory\n");
    exit (1);
  }
  _data->tag = _subr1_expr;
  _data->u._subr1._exp = exp;
  return (void *) _data;
}

void *exprr_zero (void *exp)
{
  expr *_data = (expr *) malloc (sizeof (expr));

  if (!_data) {
    fprintf (stderr, "Out of memory\n");
    exit (1);
  }
  _data->tag = _zero_expr;
  _data->u._zero._exp = exp;
  return (void *) _data;
}

void *exprr_letcc (void *body)
{
  expr *_data = (expr *) malloc (sizeof (expr));

  if (!_data) {
    fprintf (stderr, "Out of memory\n");
    exit (1);
  }
  _data->tag = _letcc_expr;
  _data->u._letcc._body = body;
  return (void *) _data;
}

void *exprr_throw (void *kr__m__exp, void *vr__m__exp)
{
  expr *_data = (expr *) malloc (sizeof (expr));

  if (!_data) {
    fprintf (stderr, "Out of memory\n");
    exit (1);
  }
  _data->tag = _throw_expr;
  _data->u._throw._kr__m__exp = kr__m__exp;
  _data->u._throw._vr__m__exp = vr__m__exp;
  return (void *) _data;
}

void *exprr_let (void *exp, void *body)
{
  expr *_data = (expr *) malloc (sizeof (expr));

  if (!_data) {
    fprintf (stderr, "Out of memory\n");
    exit (1);
  }
  _data->tag = _let_expr;
  _data->u._let._exp = exp;
  _data->u._let._body = body;
  return (void *) _data;
}

void *exprr_lambda (void *body)
{
  expr *_data = (expr *) malloc (sizeof (expr));

  if (!_data) {
    fprintf (stderr, "Out of memory\n");
    exit (1);
  }
  _data->tag = _lambda_expr;
  _data->u._lambda._body = body;
  return (void *) _data;
}

void *exprr_app (void *rator, void *rand)
{
  expr *_data = (expr *) malloc (sizeof (expr));

  if (!_data) {
    fprintf (stderr, "Out of memory\n");
    exit (1);
  }
  _data->tag = _app_expr;
  _data->u._app._rator = rator;
  _data->u._app._rand = rand;
  return (void *) _data;
}

int main ()
{
  r__t__er__t__ =
    (void *)
    exprr_let (exprr_lambda
	       (exprr_lambda
		(exprr_if
		 (exprr_zero (exprr_var ((void *) 0)),
		  exprr_const ((void *) 1),
		  exprr_mult (exprr_var ((void *) 0),
			      exprr_app (exprr_app
					 (exprr_var ((void *) 1),
					  exprr_var ((void *) 1)),
					 exprr_subr1 (exprr_var
						      ((void *) 0))))))),
	       exprr_mult (exprr_letcc
			   (exprr_app
			    (exprr_app
			     (exprr_var ((void *) 1), exprr_var ((void *) 1)),
			     exprr_throw (exprr_var ((void *) 0),
					  exprr_app (exprr_app
						     (exprr_var ((void *) 1),
						      exprr_var ((void *) 1)),
						     exprr_const ((void *)
								  4))))),
			   exprr_const ((void *) 5)));
  r__t__envr__t__ = (void *) envrr_emptyr__m__env ();
  pc = &valuer__m__ofr__m__cps;
  mount_tram ();
  printf ("Fact 5: %d\n", (int) r__t__vr__t__);
}

void appr__m__k ()
{
  kt *_c = (kt *) r__t__kr__t__;

  switch (_c->tag) {
  case _emptyr__m__k_kt:{
      void *dismountr__m__trampoline =
	_c->u._emptyr__m__k._dismountr__m__trampoline;
      _trstr *trstr = (_trstr *) r__t__vr__t__;

      longjmp (*trstr->jmpbuf, 1);
      break;
    }
  case _r__t__r__m__multr__m__innerr__m__k_kt:{
      void *vr__ex__ = _c->u._r__t__r__m__multr__m__innerr__m__k._vr__ex__;
      void *k = _c->u._r__t__r__m__multr__m__innerr__m__k._k;

      r__t__kr__t__ = (void *) k;
      r__t__vr__t__ =
	(void *) (void *) ((int) vr__ex__ * (int) r__t__vr__t__);
      pc = &appr__m__k;
      break;
    }
  case _r__t__r__m__multr__m__outerr__m__k_kt:{
      void *xr2 = _c->u._r__t__r__m__multr__m__outerr__m__k._xr2;
      void *env = _c->u._r__t__r__m__multr__m__outerr__m__k._env;
      void *k = _c->u._r__t__r__m__multr__m__outerr__m__k._k;

      r__t__er__t__ = (void *) xr2;
      r__t__envr__t__ = (void *) env;
      r__t__kr__t__ =
	(void *) ktr_r__t__r__m__multr__m__innerr__m__k (r__t__vr__t__, k);
      pc = &valuer__m__ofr__m__cps;
      break;
    }
  case _r__t__r__m__subr1r__m__k_kt:{
      void *k = _c->u._r__t__r__m__subr1r__m__k._k;

      r__t__kr__t__ = (void *) k;
      r__t__vr__t__ = (void *) (void *) ((int) r__t__vr__t__ - 1);
      pc = &appr__m__k;
      break;
    }
  case _r__t__r__m__zeror__m__k_kt:{
      void *k = _c->u._r__t__r__m__zeror__m__k._k;

      r__t__kr__t__ = (void *) k;
      r__t__vr__t__ = (void *) (r__t__vr__t__ == 0);
      pc = &appr__m__k;
      break;
    }
  case _r__t__r__m__ifr__m__k_kt:{
      void *conseq = _c->u._r__t__r__m__ifr__m__k._conseq;
      void *alt = _c->u._r__t__r__m__ifr__m__k._alt;
      void *env = _c->u._r__t__r__m__ifr__m__k._env;
      void *k = _c->u._r__t__r__m__ifr__m__k._k;

      if (r__t__vr__t__) {
	r__t__er__t__ = (void *) conseq;
	r__t__envr__t__ = (void *) env;
	r__t__kr__t__ = (void *) k;
	pc = &valuer__m__ofr__m__cps;

      } else {
	r__t__er__t__ = (void *) alt;
	r__t__envr__t__ = (void *) env;
	r__t__kr__t__ = (void *) k;
	pc = &valuer__m__ofr__m__cps;

      }
      break;
    }
  case _r__t__r__m__letr__m__k_kt:{
      void *body = _c->u._r__t__r__m__letr__m__k._body;
      void *env = _c->u._r__t__r__m__letr__m__k._env;
      void *k = _c->u._r__t__r__m__letr__m__k._k;

      r__t__er__t__ = (void *) body;
      r__t__envr__t__ = (void *) envrr_extendr__m__env (r__t__vr__t__, env);
      r__t__kr__t__ = (void *) k;
      pc = &valuer__m__ofr__m__cps;
      break;
    }
  case _r__t__r__m__throwr__m__k_kt:{
      void *vr__m__exp = _c->u._r__t__r__m__throwr__m__k._vr__m__exp;
      void *env = _c->u._r__t__r__m__throwr__m__k._env;

      r__t__er__t__ = (void *) vr__m__exp;
      r__t__envr__t__ = (void *) env;
      r__t__kr__t__ = (void *) r__t__vr__t__;
      pc = &valuer__m__ofr__m__cps;
      break;
    }
  case _r__t__r__m__randr__m__k_kt:{
      void *r = _c->u._r__t__r__m__randr__m__k._r;
      void *k = _c->u._r__t__r__m__randr__m__k._k;

      r__t__pr__t__ = (void *) r;
      r__t__kr__t__ = (void *) k;
      pc = &applyr__m__closure;
      break;
    }
  case _r__t__r__m__ratorr__m__k_kt:{
      void *rand = _c->u._r__t__r__m__ratorr__m__k._rand;
      void *env = _c->u._r__t__r__m__ratorr__m__k._env;
      void *k = _c->u._r__t__r__m__ratorr__m__k._k;

      r__t__er__t__ = (void *) rand;
      r__t__envr__t__ = (void *) env;
      r__t__kr__t__ = (void *) ktr_r__t__r__m__randr__m__k (r__t__vr__t__, k);
      pc = &valuer__m__ofr__m__cps;
      break;
    }
  }
}

void applyr__m__env ()
{
  envr *_c = (envr *) r__t__envr__t__;

  switch (_c->tag) {
  case _emptyr__m__env_envr:{
      fprintf (stderr, "unbound identifier");
      exit (1);
      break;
    }
  case _extendr__m__env_envr:{
      void *a = _c->u._extendr__m__env._a;
      void *env = _c->u._extendr__m__env._env;

      if ((r__t__nr__t__ == 0)) {
	r__t__vr__t__ = (void *) a;
	pc = &appr__m__k;

      } else {
	r__t__nr__t__ = (void *) (void *) ((int) r__t__nr__t__ - 1);
	r__t__envr__t__ = (void *) env;
	pc = &applyr__m__env;

      }
      break;
    }
  }
}

void applyr__m__closure ()
{
  clos *_c = (clos *) r__t__pr__t__;

  switch (_c->tag) {
  case _closure_clos:{
      void *body = _c->u._closure._body;
      void *env = _c->u._closure._env;

      r__t__er__t__ = (void *) body;
      r__t__envr__t__ = (void *) envrr_extendr__m__env (r__t__vr__t__, env);
      pc = &valuer__m__ofr__m__cps;
      break;
    }
  }
}

void valuer__m__ofr__m__cps ()
{
  expr *_c = (expr *) r__t__er__t__;

  switch (_c->tag) {
  case _const_expr:{
      void *n = _c->u._const._n;

      r__t__vr__t__ = (void *) n;
      pc = &appr__m__k;
      break;
    }
  case _mult_expr:{
      void *expr1 = _c->u._mult._expr1;
      void *expr2 = _c->u._mult._expr2;

      r__t__er__t__ = (void *) expr1;
      r__t__kr__t__ =
	(void *) ktr_r__t__r__m__multr__m__outerr__m__k (expr2,
							 r__t__envr__t__,
							 r__t__kr__t__);
      pc = &valuer__m__ofr__m__cps;
      break;
    }
  case _subr1_expr:{
      void *exp = _c->u._subr1._exp;

      r__t__er__t__ = (void *) exp;
      r__t__kr__t__ = (void *) ktr_r__t__r__m__subr1r__m__k (r__t__kr__t__);
      pc = &valuer__m__ofr__m__cps;
      break;
    }
  case _zero_expr:{
      void *exp = _c->u._zero._exp;

      r__t__er__t__ = (void *) exp;
      r__t__kr__t__ = (void *) ktr_r__t__r__m__zeror__m__k (r__t__kr__t__);
      pc = &valuer__m__ofr__m__cps;
      break;
    }
  case _if_expr:{
      void *test = _c->u._if._test;
      void *conseq = _c->u._if._conseq;
      void *alt = _c->u._if._alt;

      r__t__er__t__ = (void *) test;
      r__t__kr__t__ =
	(void *) ktr_r__t__r__m__ifr__m__k (conseq, alt, r__t__envr__t__,
					    r__t__kr__t__);
      pc = &valuer__m__ofr__m__cps;
      break;
    }
  case _let_expr:{
      void *e = _c->u._let._exp;
      void *body = _c->u._let._body;

      r__t__er__t__ = (void *) e;
      r__t__kr__t__ =
	(void *) ktr_r__t__r__m__letr__m__k (body, r__t__envr__t__,
					     r__t__kr__t__);
      pc = &valuer__m__ofr__m__cps;
      break;
    }
  case _letcc_expr:{
      void *body = _c->u._letcc._body;

      r__t__er__t__ = (void *) body;
      r__t__envr__t__ =
	(void *) envrr_extendr__m__env (r__t__kr__t__, r__t__envr__t__);
      pc = &valuer__m__ofr__m__cps;
      break;
    }
  case _throw_expr:{
      void *kr__m__exp = _c->u._throw._kr__m__exp;
      void *vr__m__exp = _c->u._throw._vr__m__exp;

      r__t__er__t__ = (void *) kr__m__exp;
      r__t__kr__t__ =
	(void *) ktr_r__t__r__m__throwr__m__k (vr__m__exp, r__t__envr__t__);
      pc = &valuer__m__ofr__m__cps;
      break;
    }
  case _var_expr:{
      void *y = _c->u._var._n;

      r__t__nr__t__ = (void *) y;
      pc = &applyr__m__env;
      break;
    }
  case _lambda_expr:{
      void *body = _c->u._lambda._body;

      r__t__vr__t__ = (void *) closr_closure (body, r__t__envr__t__);
      pc = &appr__m__k;
      break;
    }
  case _app_expr:{
      void *rator = _c->u._app._rator;
      void *rand = _c->u._app._rand;

      r__t__er__t__ = (void *) rator;
      r__t__kr__t__ =
	(void *) ktr_r__t__r__m__ratorr__m__k (rand, r__t__envr__t__,
					       r__t__kr__t__);
      pc = &valuer__m__ofr__m__cps;
      break;
    }
  }
}

int mount_tram ()
{
  srand (time (NULL));
  jmp_buf jb;
  _trstr trstr;
  void *dismount;
  int _status = setjmp (jb);

  trstr.jmpbuf = &jb;
  dismount = &trstr;
  if (!_status) {
    r__t__kr__t__ = (void *) ktr_emptyr__m__k (dismount);
    for (;;) {
      pc ();
    }
  }
  return 0;
}
