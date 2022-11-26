struct expr;
typedef struct expr expr;
struct expr
{
  enum
  {
    _const_expr,
    _var_expr,
    _if_expr,
    _mult_expr,
    _subr1_expr,
    _zero_expr,
    _letcc_expr,
    _throw_expr,
    _let_expr,
    _lambda_expr,
    _app_expr
  } tag;
  union
  {
    struct
    {
      void *_n;
    } _const;
    struct
    {
      void *_n;
    } _var;
    struct
    {
      void *_test;
      void *_conseq;
      void *_alt;
    } _if;
    struct
    {
      void *_expr1;
      void *_expr2;
    } _mult;
    struct
    {
      void *_exp;
    } _subr1;
    struct
    {
      void *_exp;
    } _zero;
    struct
    {
      void *_body;
    } _letcc;
    struct
    {
      void *_kr__m__exp;
      void *_vr__m__exp;
    } _throw;
    struct
    {
      void *_exp;
      void *_body;
    } _let;
    struct
    {
      void *_body;
    } _lambda;
    struct
    {
      void *_rator;
      void *_rand;
    } _app;
  } u;
};

void *exprr_const (void *n);
void *exprr_var (void *n);
void *exprr_if (void *test, void *conseq, void *alt);
void *exprr_mult (void *expr1, void *expr2);
void *exprr_subr1 (void *exp);
void *exprr_zero (void *exp);
void *exprr_letcc (void *body);
void *exprr_throw (void *kr__m__exp, void *vr__m__exp);
void *exprr_let (void *exp, void *body);
void *exprr_lambda (void *body);
void *exprr_app (void *rator, void *rand);

void (*pc) ();

void *r__t__er__t__, *r__t__envr__t__, *r__t__kr__t__, *r__t__vr__t__,
  *r__t__pr__t__, *r__t__nr__t__;

void valuer__m__ofr__m__cps ();
struct envr;
typedef struct envr envr;
struct envr
{
  enum
  {
    _emptyr__m__env_envr,
    _extendr__m__env_envr
  } tag;
  union
  {
    struct
    {
      char dummy;
    } _emptyr__m__env;
    struct
    {
      void *_a;
      void *_env;
    } _extendr__m__env;
  } u;
};

void *envrr_emptyr__m__env ();
void *envrr_extendr__m__env (void *a, void *env);

struct clos;
typedef struct clos clos;
struct clos
{
  enum
  {
    _closure_clos
  } tag;
  union
  {
    struct
    {
      void *_body;
      void *_env;
    } _closure;
  } u;
};

void *closr_closure (void *body, void *env);

void applyr__m__closure ();
void applyr__m__env ();
struct kt;
typedef struct kt kt;
struct kt
{
  enum
  {
    _emptyr__m__k_kt,
    _r__t__r__m__multr__m__innerr__m__k_kt,
    _r__t__r__m__multr__m__outerr__m__k_kt,
    _r__t__r__m__subr1r__m__k_kt,
    _r__t__r__m__zeror__m__k_kt,
    _r__t__r__m__ifr__m__k_kt,
    _r__t__r__m__letr__m__k_kt,
    _r__t__r__m__throwr__m__k_kt,
    _r__t__r__m__randr__m__k_kt,
    _r__t__r__m__ratorr__m__k_kt
  } tag;
  union
  {
    struct
    {
      void *_dismountr__m__trampoline;
    } _emptyr__m__k;
    struct
    {
      void *_vr__ex__;
      void *_k;
    } _r__t__r__m__multr__m__innerr__m__k;
    struct
    {
      void *_xr2;
      void *_env;
      void *_k;
    } _r__t__r__m__multr__m__outerr__m__k;
    struct
    {
      void *_k;
    } _r__t__r__m__subr1r__m__k;
    struct
    {
      void *_k;
    } _r__t__r__m__zeror__m__k;
    struct
    {
      void *_conseq;
      void *_alt;
      void *_env;
      void *_k;
    } _r__t__r__m__ifr__m__k;
    struct
    {
      void *_body;
      void *_env;
      void *_k;
    } _r__t__r__m__letr__m__k;
    struct
    {
      void *_vr__m__exp;
      void *_env;
    } _r__t__r__m__throwr__m__k;
    struct
    {
      void *_r;
      void *_k;
    } _r__t__r__m__randr__m__k;
    struct
    {
      void *_rand;
      void *_env;
      void *_k;
    } _r__t__r__m__ratorr__m__k;
  } u;
};

void *ktr_emptyr__m__k (void *dismountr__m__trampoline);
void *ktr_r__t__r__m__multr__m__innerr__m__k (void *vr__ex__, void *k);
void *ktr_r__t__r__m__multr__m__outerr__m__k (void *xr2, void *env, void *k);
void *ktr_r__t__r__m__subr1r__m__k (void *k);
void *ktr_r__t__r__m__zeror__m__k (void *k);
void *ktr_r__t__r__m__ifr__m__k (void *conseq, void *alt, void *env, void *k);
void *ktr_r__t__r__m__letr__m__k (void *body, void *env, void *k);
void *ktr_r__t__r__m__throwr__m__k (void *vr__m__exp, void *env);
void *ktr_r__t__r__m__randr__m__k (void *r, void *k);
void *ktr_r__t__r__m__ratorr__m__k (void *rand, void *env, void *k);

void appr__m__k ();
int main ();
int mount_tram ();

struct _trstr;
typedef struct _trstr _trstr;
struct _trstr
{
  jmp_buf *jmpbuf;
  int value;
};
