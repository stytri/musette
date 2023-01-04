/*
Copyright (c) 2022 Tristan Styles

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>

#define inline  static inline

static void errorf(char const *fmt, ...) {
	va_list vl;
	va_start(vl, fmt);
	vfprintf(stderr, fmt, vl);
	fputc('\n', stderr);
	fflush(stderr);
	va_end(vl);
}

static void fail(char const *file, int line) {
	errorf("%s:%i: %s", file, line, strerror(errno));
	exit(EXIT_FAILURE);
}

inline void *fail_if_null(void *p, char const *file, int line) {
	if(p) return p;
	fail(file, line);
}

inline void *xmalloc(size_t z, char const *file, int line) {
	return fail_if_null(malloc(z), file, line);
}
#define malloc(malloc__z)  xmalloc(malloc__z,__FILE__,__LINE__)

inline void *xcalloc(size_t n, size_t z, char const *file, int line) {
	return fail_if_null(calloc(n, z), file, line);
}
#define calloc(calloc__n,calloc__z)  xcalloc(calloc__n,calloc__z,__FILE__,__LINE__)

inline void *xrealloc(void *p, size_t z, char const *file, int line) {
	return fail_if_null(realloc(p, z), file, line);
}
#define realloc(realloc__p,realloc__z)  xrealloc(realloc__p,realloc__z,__FILE__,__LINE__)

inline char *xstrndup(char const *cs, size_t n, char const *file, int line) {
	char *s = memcpy(xmalloc(n+1, file, line), cs, n);
	s[n] = '\0';
	return s;
}
#define strndup(strndup__cs,strndup__n) (xstrndup)(strndup__cs,strndup__n,__FILE__,__LINE__)

inline void xfree(void const *p) {
	free((void *)p);
}
#define free(free__p)  xfree(free__p)

static char *mfgets(FILE *in, int end) {
	size_t z   = BUFSIZ-1, n = 0;
	char  *buf = malloc(z+1);
	for(int c;
		(buf[n] = '\0') || (((c = fgetc(in)) != EOF) && (c != end));
		buf[n++] = (char)c
	) {
		if(n == z) buf = realloc(buf, (z+=BUFSIZ)+1);
	}
	return buf;
}

static int strntoi(char const *cs, size_t n, char **end, int b) {
	char t[(sizeof(int)*CHAR_BIT)+4];
	if(n >= (sizeof(t)-1)) n = (sizeof(t)-1);
	memcpy(t, cs, n), t[n] = '\0';
	int v = (int)strtoll(t, end, b);
	if(end) *end = (char *)(cs + (*end - t));
	return v;
}

static void fprints(FILE *out, size_t n, char const *s) {
	for(int c; n > 0; fputc(c, out), n--) {
		if((c = *s++) == '\\') switch((c = *s++)) {
			char *t;
		case 't': n-- , c = '\t';        break;
		case 'n': n-- , c = '\n';        break;
		case 'r': n-- , c = '\r';        break;
		case '^': n-=2, c = *s++ & 0x1f; break;
		case 'x': case 'X':
			c  = strntoi(s, (n <= 2) ? n - 1 : 2, &t, 16);
			n -= (t-s) + 1;
			s  = t;
			break;
		default :
			if((c < '0') && (c > '7')) break;
			c  = strntoi(s-1, (n <= 3) ? n - 1 : 3, &t, 8);
			n -= (t-s) + 1;
			s  = t;
		}
	}
}

inline void prints(size_t n, char const *s) { fprints(stdout, n, s); }

typedef struct {
	size_t z;
	size_t n;
	size_t m;
	size_t x;
	void **p;
}
	array;
#define ARRAY(ARRAY__Type,...)  { sizeof(ARRAY__Type), 0, 0, 0, NULL }

static void array_clear(array *a) {
	for(a->n = a->m = 0; a->x > 0; ) {
		a->x--;
		free(a->p[a->x]);
	}
	free(a->p);
	a->p = NULL;
}

static void *array_next(array *a) {
	if(a->n == a->m) {
		size_t x   = a->x + 1, m = a->m ? a->m : 1;
		a->p       = realloc(a->p, x * sizeof(*a->p));
		a->p[a->x] = calloc(m, a->z);
		a->m      += m;
		a->x       = x;
	}
	size_t x = a->x - 1, i = a->n++ - (a->m / 2);
	return &((char *)a->p[x])[i * a->z];
}

inline void *array_copy (array *a, void const *p) {
	return memcpy(array_next(a), p, a->z);
}

static void *array_at(array *a, size_t i) {
	if(i < a->n) {
		for(size_t x = a->x / 2, h = (size_t)1 << x, l = h >> 1;;)
			if(i < l) l >>= 1, h >>= 1, x--;
			else if(i >= h) l <<= 1, h <<= 1, x++;
			else return &((char *)a->p[x])[(i-l) * a->z];
	}
	return NULL;
}

#define foreach(foreach__type,foreach__ptr,foreach__array,foreach__action)  \
do if((foreach__array)->n > 0) { \
	size_t         foreach__rem = (foreach__array)->n - ((foreach__array)->m / 2); \
	size_t const   foreach__end = (foreach__array)->x - (foreach__rem > 0); \
	foreach__type *foreach__ptr; \
	for(size_t foreach__i = 0; foreach__i < foreach__end; foreach__i++) { \
		size_t foreach__n = (size_t)1 << (foreach__i - (foreach__i > 0)); \
		for(foreach__ptr = (foreach__array)->p[foreach__i]; foreach__n--; foreach__ptr++) { \
			foreach__action; \
		} \
	} \
	if(foreach__rem > 0) { \
		for(foreach__ptr = (foreach__array)->p[foreach__end]; foreach__rem--; foreach__ptr++) { \
			foreach__action; \
		} \
	} \
} while(0)

#define RETURN_CODES \
	ENUM(FAIL, = -1) ENUM(OK) ENUM(BREAK) ENUM(CONTINUE) ENUM(RETURN)
#define ENUM(ENUM__name,...)  ENUM__name __VA_ARGS__,
enum { RETURN_CODES };
#undef ENUM
#define ENUM(ENUM__name,...)  #ENUM__name,
static char const        *const return_code_name__actual[] = { RETURN_CODES };
#undef ENUM
static char const *const *const return_code_name = &return_code_name__actual[1];

typedef struct env {
	array       y;
	array       e;
	array       t;
	struct env *p;
}
	env;
#define ENV(...)  { \
	.y = ARRAY(symbol), \
	.e = ARRAY(expr), \
	.t = ARRAY(token), \
	.p = __VA_ARGS__+0, \
}

typedef struct {
	int         type;
	size_t      len;
	char const *cs;
	size_t      ln;
}
	token;

inline bool eq(token const *cs, token const *ct) {
	return (cs->len == ct->len) && !memcmp(cs->cs, ct->cs, ct->len);
}

typedef struct expr {
	int        (*eval)(env *v, struct expr const *e, struct expr *p);
	union {
	struct expr *l;
	token       *t;
	int          i;
	char const  *s;
	};
	union {
	struct expr *r;
	size_t       n;
	void        *q;
	};
	size_t       ln;
}
	expr;

typedef struct {
	token t;
	expr  e;
}
	symbol;

static symbol *lookup(env *v, token *t) {
	do {
		foreach(symbol, y, &v->y,
			if(eq(&y->t, t)) return y
		);
	} while((v = v->p));
	return NULL;
}

static symbol *insert(env *v, token *t) {
	symbol *y = array_next(&v->y);
	y->t.type = t->type;
	y->t.cs   = strndup(t->cs, t->len);
	y->t.len  = t->len;
	y->t.ln   = t->ln;
	return y;
}

inline int eval(env *v, expr const *e, expr *p) {
	return e ? e->eval(v, e, p) : FAIL;
}

static expr *clone_expr(env *v, expr *e);

static void env_clear(env *v);

typedef struct parser {
	char const *next;
	size_t      ln;
	array       t;
	array       e;
	size_t      errcnt;
}
	parser;
#define PARSER(PARSER__source)  { \
	.next   = PARSER__source, \
	.ln     = 1, \
	.t      = ARRAY(token), \
	.e      = ARRAY(expr), \
	.errcnt = 0, \
}

enum {
	F_Constant    = 0x0010,
	F_Integer     = 0x0001,
	F_String      = 0x0004,
	F_Identifier  = 0x0020,
	F_Operator    = 0x0040,
	F_Primary     = 0x0080,
	F_Expression  = 0x0100,
	F_Statement   = 0x0200,
	F_List        = 0x0400,
	F_Delimeter   = 0x1000,
	F_Begin       = 0x2000,
	F_End         = 0x4000,
};

enum {
	T_ListDelimeter       = F_Delimeter | F_List,
	T_StatementDelimeter  = F_Delimeter | F_Statement,
	T_Operator            = F_Operator  | F_Expression,
	T_OperatorIdentifier  = F_Operator  | F_Expression | F_Identifier,
	T_Zen                 = F_Primary   | F_Constant,
	T_Integer             = F_Primary   | F_Constant   | F_Integer,
	T_String              = F_Primary   | F_Constant   | F_String,
	T_Identifier          = F_Primary   | F_Expression | F_Identifier,
	T_BeginExpression     = F_Primary   | F_Expression | F_Begin,
	T_EndExpression       =               F_Expression | F_End,
	T_BeginScope          = F_Primary   | F_Statement  | F_Begin,
	T_EndScope            =               F_Statement  | F_End,
	T_BeginList           = F_Primary   | F_List       | F_Begin,
	T_EndList             =               F_List       | F_End,
	T_Eof                 =                              F_End,
};

inline int isidentifier1(int c) { return isalpha(c) || (c == '_'); }
inline int isidentifierN(int c) { return isalnum(c) || (c == '_'); }
inline int isoperator   (int c) {
	switch(c) {
	default : return ispunct(c);
	case '#': case '"': case '`': case ';': case ',': case '_':
	case '(': case ')': case '{': case '}': case '[': case ']':
		return 0;
	}
}

static parser *tokenize(parser *g) {
#define TOKEN(TOKEN__size,TOKEN__type)  do { \
		t       = array_next(&g->t); \
		t->ln   = g->ln; \
		t->cs   = g->next, g->next += (TOKEN__size); \
		t->len  = (TOKEN__size); \
		t->type = (TOKEN__type); \
	} while(0)
	for(int u = 0, c;;) {
		token *t;
		for(; (c = *g->next) && !isgraph(c); g->next++) {
			if(c == '\n') g->ln++;
		}
		switch(c) {
		case  0 : TOKEN(0,T_Eof);                return g;
		case '(': TOKEN(1,T_BeginExpression);    continue;
		case ')': TOKEN(1,T_EndExpression);      continue;
		case '{': TOKEN(1,T_BeginScope);         continue;
		case '}': TOKEN(1,T_EndScope);           continue;
		case '[': TOKEN(1,T_BeginList);          continue;
		case ']': TOKEN(1,T_EndList);            continue;
		case ';': TOKEN(1,T_StatementDelimeter); continue;
		case ',': TOKEN(1,T_ListDelimeter);      continue;
		default :
			if(isoperator(c)) {
				t = array_next(&g->t);
				for(t->cs = g->next++; isoperator(c = *g->next); g->next++);
				t->ln   = g->ln;
				t->len  = (size_t)(g->next - t->cs);
				t->type = T_Operator;
				continue;
			}
			if(isidentifier1(c)) {
				t = array_next(&g->t);
				for(t->cs = g->next++; isidentifierN(c = *g->next); g->next++);
				t->ln   = g->ln;
				t->len  = (size_t)(g->next - t->cs);
				t->type = ((t->len == 1) && *t->cs == '_') ? T_Zen : T_Identifier;
				continue;
			}
			if(isdigit(c)) {
				t     = array_next(&g->t);
				t->cs = g->next;
				t->ln = g->ln;
				(void)strtoll(g->next, (char **)&g->next, 0);
				t->len  = (size_t)(g->next - t->cs);
				t->type = T_Integer;
				continue;
			}
			if(u++ == 0) errorf("%zu: unexpected character: '%c'", g->ln, c);
			g->errcnt++;
			g->next++;
			continue;
		case '`':
			t = array_next(&g->t);
			g->next++;
			for(t->cs = g->next; (c = *g->next) && (c != '`'); g->next++);
			t->ln   = g->ln;
			t->len  = (size_t)(g->next - t->cs);
			t->type = T_OperatorIdentifier;
			g->next += (c == '`');
			continue;
		case '"':
			t = array_next(&g->t);
			g->next++;
			t->cs = g->next;
			for(bool q = false; (c = *g->next) && ((c != '"') || q); g->next++) {
				q = q ? false : (c == '\\');
			}
			t->ln   = g->ln;
			t->len  = (size_t)(g->next - t->cs);
			t->type = T_String;
			g->next += (c == '"');
			continue;
		case '#':
			for(g->next++; (c = *g->next) && (c != '\n'); g->next++);
		}
	}
#undef TOKEN
}

static int eval__zen(env *v, struct expr const *e, struct expr *p) {
	*p = *e;
	return OK;
}

static expr zen = { .eval = eval__zen, .i = 0 };

static int eval__integer(env *v, struct expr const *e, struct expr *p) {
	*p = *e;
	return OK;
}

static int eval__string(env *v, struct expr const *e, struct expr *p) {
	*p = *e;
	return OK;
}

static int eval__identifier(env *v, struct expr const *e, struct expr *p) {
	symbol *y = lookup(v, e->t);
	if(y) {
		*p = y->e;
		return OK;
	}
	errorf("%zu: %.*s undefined", e->t->ln, (int)e->t->len, e->t->cs);
	return FAIL;
}

#define ENUMERATE_BINARY_OPERATORS(...) \
	ENUM(mul,*) ENUM(div,/) ENUM(add,+) ENUM(sub,-) \
	ENUM(eq,==) ENUM(neq,!=) ENUM(lt,<) ENUM(lte,<=) ENUM(gt,>) ENUM(gte,>=)

#define ENUM(ENUM__name,ENUM__operator)  \
static int eval__##ENUM__name(env *v, struct expr const *e, struct expr *p) { \
	expr x; \
	int           rc = eval(v, e->l, p); \
	if(rc == OK)  rc = eval(v, e->r, &x); \
	if(rc == OK) *p  = (expr){ eval__integer, .i = p->i ENUM__operator x.i }; \
	return rc; \
}
ENUMERATE_BINARY_OPERATORS()
#undef ENUM

static int eval__list(env *v, struct expr const *e, struct expr *p) {
	return eval(v, e->l, p);
}

static int eval__scope(env *v, struct expr const *e, struct expr *p) {
	env w = ENV(v);
	int rc = eval(&w, e->l, p);
	env_clear(&w);
	return rc;
}

static int eval__alternate(env *v, struct expr const *e, struct expr *p) {
	int rc;
	if((rc = eval(v, e->l, p)) == OK) return eval(v, e->r, p);
	return rc;
}

static int eval__statement(env *v, struct expr const *e, struct expr *p) {
	int rc;
	if((rc = eval(v, e->l, p)) == OK) return eval(v, e->r, p);
	return rc;
}

static int eval__sequence(env *v, struct expr const *e, struct expr *p) {
	int rc;
	if((rc = eval(v, e->l, p)) == OK) return eval(v, e->r, p);
	return rc;
}

static int eval__if(env *v, struct expr const *e, struct expr *p) {
	int  rc = OK;
	env  w = ENV(v);
	expr q, *c = e->l, *b = e->r;
	if(c->eval == eval__statement) {
		rc = eval(&w, c->l, &q);
		c = c->r;
	}
	if((rc == OK) && ((rc = eval(&w, c, &q)) == OK)) {
		if(!q.i) {
			rc = (b->eval == eval__alternate) ? eval(&w, b->r, p) : OK;
		} else {
			if(b->eval == eval__alternate) b = b->l;
			if(b->eval == eval__scope) b = b->l;
			rc = eval(&w, b, p);
		}
	}
	env_clear(&w);
	return rc;
}

static int eval__while(env *v, struct expr const *e, struct expr *p) {
	int  rc = OK;
	env  w = ENV(v);
	expr q, *c = e->l, *b = e->r;
	if(c->eval == eval__statement) {
		rc = eval(&w, c->l, &q);
		c = c->r;
	}
	if((rc == OK) && ((rc = eval(&w, c, &q)) == OK)) {
		if(!q.i) {
			rc = (b->eval == eval__alternate) ? eval(&w, b->r, p) : OK;
		} else {
			if(b->eval == eval__alternate) b = b->l;
			if(b->eval == eval__scope) b = b->l;
			while((((rc = eval(&w, b, p)) == OK) || (rc == CONTINUE))
				&& (((rc = eval(&w, c, &q)) == OK) || (rc == CONTINUE))
				&& q.i
			);
			if(rc == BREAK) rc = OK;
		}
	}
	env_clear(&w);
	return rc;
}

static int eval__break(env *v, struct expr const *e, struct expr *p) {
	int  rc;
	expr q;
	if((rc = eval(v, e->l, &q)) == OK) {
		if((q.eval == eval__integer) && !q.i) return OK;
		if((e->r->eval == eval__zen) || ((rc = eval(v, e->r, p)) == OK)) return BREAK;
	}
	return rc;
}

static int eval__continue(env *v, struct expr const *e, struct expr *p) {
	int  rc;
	expr q;
	if((rc = eval(v, e->l, &q)) == OK) {
		if((q.eval == eval__integer) && !q.i) return OK;
		if((e->r->eval == eval__zen) || ((rc = eval(v, e->r, p)) == OK)) return CONTINUE;
	}
	return rc;
}

static int eval__lambda(env *v, struct expr const *e, struct expr *p) {
	*p = *e;
	return OK;
}

static int eval__apply_lambda(env *v, expr const *y, struct expr const *e, struct expr *p) {
	if(y->l->eval == eval__zen) return eval(v, y->r, p);
	int   rc;
	env   w = ENV(v);
	expr *l = y->l, x;
	if(l->eval == eval__sequence) {
		for(symbol *s; ; ) {
			if(e->eval == eval__sequence) {
				if((rc = eval(v, e->l, &x)) != OK) break;
				e = e->r;
			} else {
				if((rc = eval(v, e, &x)) != OK) break;
				e = &zen;
			}
			if(l->eval == eval__sequence) {
				s = insert(&w, l->l->t);
				if((rc = eval(v, &x, &s->e)) != OK) break;
				l = l->r;
			} else {
				s = insert(&w, l->t);
				rc = eval(v, &x, &s->e);
				break;
			}
		}
	} else {
		symbol *s = insert(&w, l->t);
		rc = eval(v, e, &s->e);
	}
	if(rc == OK) rc = eval(&w, (y->r->eval == eval__scope) ? y->r->l : y->r, p);
	env_clear(&w);
	return (rc == RETURN) ? OK : rc;
}

static int eval__apply(env *v, struct expr const *e, struct expr *p) {
	if(e->l->eval == eval__lambda) return eval__apply_lambda(v, e->l, e->r, p);
	if(e->l->eval == eval__identifier) {
		token  *t = e->l->t;
		symbol *y = lookup(v, t);
		if(!y) {
			errorf("%zu: %.*s undefined", t->ln, (int)t->len, t->cs);
			return FAIL;
		}
		if(y->e.eval == eval__lambda) return eval__apply_lambda(v, &y->e, e->r, p);
	}
	expr x = { eval__mul, .l = e->l, .r = e->r };
	return eval(v, &x, p);
}

static int eval__operator_function(env *v, struct expr const *e, struct expr *p) {
	expr r = { .eval = eval__sequence, .l = e->l->l, .r = e->r };
	expr x = { .eval = eval__apply   , .l = e->l->r, .r = &r };
	return eval__apply(v, &x, p);
}

static int eval__set(env *v, struct expr const *e, struct expr *p) {
	int rc = FAIL;
	if(e->l->eval == eval__identifier) {
		symbol *y = lookup(v, e->l->t);
		if(!y)  y = insert(v, e->l->t);
		if((rc = eval(v, e->r, p)) == OK) {
			if(y->e.eval == eval__string) free(y->e.s);
			if(p->eval == eval__string) {
				y->e.eval = eval__string;
				y->e.s    = strndup(p->s, p->n);
				y->e.n    = p->n;
			} else if(p->eval == eval__lambda) {
				y->e.eval = eval__lambda;
				y->e.l    = clone_expr(v, p->l);
				y->e.r    = clone_expr(v, p->r);
			} else {
				y->e      = *p;
			}
		}
	} else if(e->l->eval == eval__zen) {
		if((rc = eval(v, e->r, p)) == OK) rc = RETURN;
	} else errorf("%zu: invalid assignee", e->ln);
	return rc;
}

static int eval__get(env *v, struct expr const *e, struct expr *p) {
	int rc;
	if((rc = eval(v, e->l, p)) == OK) {
		char  *buf = mfgets(stdin, '\n');
		size_t i   = 0;
		parser g   = PARSER(buf);
		tokenize(&g);
		for(expr x, y, *d = (expr *)(e = e->r); e; i++) {
			if(e->eval == eval__apply) {
				d = (expr *)(e->l);
				e = e->r;
			} else {
				d = (expr *)(e);
				e = NULL;
			}
			for(token *t; (t = array_at(&g.t, i++))->type != T_Eof; y = zen)
				if(t->type == T_Integer) {
					y = (expr){ eval__integer, .i = strntoi(t->cs, t->len, NULL, 0) };
					break;
				} else if((t->type == T_String) || (t->type == T_Identifier)) {
					y = (expr){ eval__string, .s = t->cs, .n = t->len };
					break;
				}
			x = (expr){ eval__set, .l = d, .r = &y };
			rc = eval__set(v, &x, p);
		}
		array_clear(&g.t);
		free(buf);
	}
	return rc;
}

static int eval__put(env *v, struct expr const *e, struct expr *p) {
	int rc;
	if((rc = eval(v, e->l, p)) == OK) {
		FILE *out = p->i ? stderr : stdout;
		for(e = e->r; e;) {
			if(e->eval == eval__apply) {
				if((rc = eval(v, e->l, p)) != OK) break;
				e = e->r;
			} else {
				if((rc = eval(v, e, p)) != OK) break;
				e = NULL;
			}
			if(p->eval == eval__integer) fprintf(out, "%i", p->i);
			else if(p->eval == eval__string) fprints(out, p->n, p->s);
			else {
				errorf("%zu: invalid argument", p->ln);
				rc = FAIL;
				break;
			}
		}
		fputc('\n', out);
	}
	return rc;
}

static struct {
	size_t      n;
	char const *lexeme;
	int       (*eval)(env *v, struct expr const *, struct expr *);
}
	optab[] = {
#	define ENUM(ENUM__name,ENUM__operator)  \
		{ sizeof(#ENUM__operator)-1, #ENUM__operator, eval__##ENUM__name },
	ENUMERATE_BINARY_OPERATORS()
#	undef ENUM
	{ 1, "[" , eval__list },
	{ 1, "{" , eval__scope },
	{ 1, ":" , eval__alternate },
	{ 1, ";" , eval__statement },
	{ 1, "," , eval__sequence },
	{ 1, "?" , eval__if },
	{ 2, "?*", eval__while },
	{ 2, "?/", eval__break },
	{ 2, "?.", eval__continue},
	{ 2, "=>", eval__lambda },
	{ 1, "." , eval__apply },
	{ 1, "=" , eval__set },
	{ 2, ":>", eval__get },
	{ 2, "<:", eval__put },
};

static expr *expr_alloc(array *a, token *t) {
	expr *e = array_next(a);
	e->ln = t->ln;
	return e;
}

static expr *expr_integer(parser *g, token *t) {
	expr *e = expr_alloc(&g->e, t);
	e->eval = eval__integer;
	e->i    = strntoi(t->cs, t->len, NULL, 0);
	return e;
}

static expr *expr_string(parser *g, token *t) {
	expr *e = expr_alloc(&g->e, t);
	e->eval = eval__string;
	e->s    = t->cs;
	e->n    = t->len;
	return e;
}

static expr *expr_identifier(parser *g, token *t) {
	expr *e = expr_alloc(&g->e, t);
	e->eval = eval__identifier;
	e->t = t;
	return e;
}

static expr *expr_apply(parser *g, token *t, expr *l, expr *r) {
	expr *e = expr_alloc(&g->e, t);
	e->eval = eval__apply;
	e->l    = l;
	e->r    = r;
	return e;
}

static expr *expr_operator(parser *g, token *t, expr *l, expr *r) {
	for(size_t i = 0; i < (sizeof(optab)/sizeof(optab[0])); i++) {
		if(t->type & F_Identifier) {
			expr *e = expr_alloc(&g->e, t);
			expr *x = expr_alloc(&g->e, t);
			e->eval = eval__operator_function;
			x->r    = expr_identifier(g, t);
			x->l    = l;
			e->l    = x;
			e->r    = r;
			return e;
		} else if((optab[i].n == t->len) && (memcmp(optab[i].lexeme, t->cs, t->len) == 0)) {
			expr *e = expr_alloc(&g->e, t);
			e->eval = optab[i].eval;
			e->l    = l;
			e->r    = r;
			if((e->eval == eval__lambda) && (l->eval != eval__zen)) {
				if(l->eval == eval__sequence) {
					for(r = l->r, l = l->l; ; l = r->l, r = r->r) {
						if(l->eval != eval__identifier) break;
						if(r->eval != eval__sequence) {
							l = r;
							break;
						}
					}
				}
				if(l && (l->eval != eval__identifier)) {
					errorf("%zu: invalid parameter", t->ln);
					g->errcnt++;
					e = &zen;
				}
			}
			return e;
		}
	}
	errorf("%zu: unknown operator %.*s", t->ln, (int)t->len, t->cs);
	g->errcnt++;
	return &zen;
}

static expr *parse_sequence(parser *g, size_t *i);

static expr *parse_primary(parser *g, size_t *i) {
	expr  *e;
	token *t = array_at(&g->t, *i), *q;
	switch(t->type) {
	default :
		break;
	case T_Zen:
		++*i;
	case T_EndExpression: case T_EndScope: case T_EndList:
		return &zen;
	case T_Integer:
		++*i;
		return expr_integer(g, t);
	case T_String:
		++*i;
		return expr_string(g, t);
	case T_Identifier:
		++*i;
		return expr_identifier(g, t);
	case T_BeginExpression:
		++*i;
		e = parse_sequence(g, i);
		t = array_at(&g->t, *i);
		if(t && (t->type == T_EndExpression)) {
			++*i;
			return e;
		}
		break;
	case T_BeginScope:
		++*i;
		e = parse_sequence(g, i);
		q = array_at(&g->t, *i);
		if(q && (q->type == T_EndScope)) {
			++*i;
			return expr_operator(g, t, e, NULL);
		}
		t = q;
		break;
	case T_BeginList:
		++*i;
		e = parse_sequence(g, i);
		q = array_at(&g->t, *i);
		if(q && (q->type == T_EndList)) {
			++*i;
			return expr_operator(g, t, e, NULL);
		}
		t = q;
		break;
	case T_Operator:
		++*i;
		e = parse_primary(g, i);
		return expr_operator(g, t, &zen, e);
	case T_Eof:
		return NULL;
	}
	if(!t) errorf("unexpected end of input");
	else {
		++*i;
		errorf("%zu: invalid token %.*s", t->ln, (int)t->len, t->cs);
	}
	g->errcnt++;
	return &zen;
}

static expr *parse_binding(parser *g, size_t *i) {
	expr *e = parse_primary(g, i), **lp = &e, *r;
	if(e) for(token *t;
		(t = array_at(&g->t, *i)) && (t->type & F_Primary);
		lp = &(*lp)->r
	) {
		if(!(r = parse_primary(g, i))) break;
		*lp = expr_apply(g, t, *lp, r);
	}
	return e;
}

static expr *parse_operation(parser *g, size_t *i) {
	expr *e = parse_binding(g, i), **lp = &e, *r;
	if(e) for(token *t;
		(t = array_at(&g->t, *i)) && (t->type & F_Operator);
		lp = &(*lp)->r
	) {
		++*i;
		if(!(r = parse_binding(g, i))) break;
		*lp = expr_operator(g, t, *lp, r);
	}
	return e;
}

static expr *parse_sequence(parser *g, size_t *i) {
	expr *e = parse_operation(g, i), **lp = &e, *r;
	if(e) for(token *t;
		(t = array_at(&g->t, *i)) && (t->type & F_Delimeter);
		lp = &(*lp)->r
	) {
		++*i;
		if(!(r = parse_operation(g, i))) break;
		*lp = expr_operator(g, t, *lp, r);
	}
	return e;
}

static expr *parse(parser *g) {
	size_t i = 0;
	return parse_sequence(g, &i);
}

static expr *clone_expr(env *v, expr *e) {
	if(e) {
		e = array_copy(&v->e, e);
		if(e->eval == eval__identifier) {
			token *t = array_copy(&v->t, e->t);
			t->cs = strndup(t->cs, t->len);
			e->t = t;
		} else if(e->eval == eval__string) {
			e->s = strndup(e->s, e->n);
		} else if((e->eval != eval__zen) && (e->eval != eval__integer)) {
			e->l = clone_expr(v, e->l);
			e->r = clone_expr(v, e->r);
		}
	}
	return e;
}

static void env_clear(env *v) {
	if(v->y.n > 0) {
		foreach(symbol, y, &v->y,
			if(y->e.eval == eval__string) free(y->e.s);
			free(y->t.cs);
		);
		array_clear(&v->y);
	}
	if(v->e.n > 0) {
		foreach(expr, e, &v->e,
			if(e->eval == eval__string) free(e->s);
		);
		array_clear(&v->e);
	}
	if(v->t.n > 0) {
		foreach(token, t, &v->t,
			free(t->cs);
		);
		array_clear(&v->t);
	}
	v->p = NULL;
}

static int evaluate(env *v, char const *s, expr *p) {
	parser g = PARSER(s);
	expr  *e = parse(tokenize(&g));
	int    r = g.errcnt ? FAIL : eval(v, e, p);
	array_clear(&g.e);
	array_clear(&g.t);
	return r;
}

static char const *predef[] = {
	"VERSION=230104", NULL
};

static bool isopt(char const *ct, char const *cs, char const *cl) {
	return (cs && (strcmp(ct, cs) == 0)) || (cl && (strcmp(ct, cl) == 0));
}

static FILE *mufopen(char const *cs) {
	cs = strcat(strcpy(malloc(strlen(cs)+4), cs), ".mu");
	FILE *in = fopen(cs, "r");
	free(cs);
	return in;
}

int main(int argc, char **argv) {
	env   v = ENV();
	expr  result;
	char *buf;
	for(; (argc > 1) && (*argv[1] == '-'); argc--, argv++) {
		if(isopt(argv[1], "-h", "--help")) puts("musette - a small oboe");
	}
	for(size_t i = 0; predef[i] != NULL; i++) {
		int rc = evaluate(&v, predef[i], &result);
		if(rc != OK) errorf(return_code_name[rc]);
	}
	for(; argc == 1; free(buf)) {
		printf("musette> "), fflush(stdout);
		buf = mfgets(stdin, '\n');
		if(isopt(buf, ".q", ".quit")) break;
		if(isopt(buf, ".c", ".clear")) {
			env_clear(&v);
			continue;
		}
		int rc = evaluate(&v, buf, &result);
		printf("%s: ", return_code_name[rc]);
		if(result.eval == eval__string) prints(result.n, result.s);
		else if(result.eval == eval__integer) printf("%i", result.i);
		putchar('\n');
	}
	for(FILE *in; (argc > 1) && (in = mufopen(argv[1])); free(buf), argc--, argv++) {
		buf = mfgets(in, EOF), fclose(in);
		int rc = evaluate(&v, buf, &result);
		if(rc != OK) errorf(return_code_name[rc]);
	}
	env_clear(&v);
	if(argc < 2) return EXIT_SUCCESS;
	perror(argv[1]);
	return EXIT_FAILURE;
}
