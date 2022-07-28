#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

enum expr_tag {
  VAR, VAL, BINOP, FUN
};

enum binop {
  ADD, SUB, MUL, DIV
};

enum fun {
  SIN, COS
};

struct expr_binop {
  enum binop op;
  struct expr *lhs;
  struct expr *rhs;
};

struct expr_fun {
  enum fun op;
  struct expr *arg;
};

struct expr {
  enum expr_tag tag;
  union {
    char *var;
    float val;
    struct expr_binop binop;
    struct expr_fun fun;
  } payload;
};

void free_expr(struct expr *e) {
  if (e == NULL) {
    return;
  }

  switch (e->tag) {
  case VAR:
    free(e->payload.var);
    break;
  case VAL:
    break;
  case BINOP:
    free_expr(e->payload.binop.lhs);
    free_expr(e->payload.binop.rhs);
    break;
  case FUN:
    free_expr(e->payload.fun.arg);
    break;
  }
  free(e);
}

void print_expr(const struct expr *e) {
  switch (e->tag) {
  case VAR:
    printf("%s", e->payload.var);
    break;
  case VAL:
    printf("%f", e->payload.val);
    break;
  case BINOP:
    printf("(");
    print_expr(e->payload.binop.lhs);
    switch (e->payload.binop.op) {
    case ADD:
      printf("+");
      break;
    case SUB:
      printf("-");
      break;
    case MUL:
      printf("*");
      break;
    case DIV:
      printf("/");
      break;
    }
    print_expr(e->payload.binop.rhs);
    printf(")");
    break;
  case FUN:
    switch (e->payload.fun.op) {
    case SIN:
      printf("sin");
      break;
    case COS:
      printf("cos");
      break;
    }
    printf("(");
    print_expr(e->payload.fun.arg);
    printf(")");
    break;
  }
}

// Grammar:
//
// E -> var
// E -> const
// E -> (E)
// E -> FUN(E)
// E -> E * E
// E -> E / E
// E -> E + E
// E -> E - E
//
// Resolving ambiguities:
//
// E0 -> var
// E0 -> const
// E0 -> (E)
// E0 -> FUN(E)
// E1 -> E1 * E0
// E1 -> E1 / E0
// E2 -> E2 + E1
// E2 -> E2 - E1
// E -> E2
//
// Left-factorising:
//
// E0 -> var
// E0 -> const
// E0 -> (E)
// E0 -> FUN(E)
// E1 -> E0 E1'
// E1' ->
// E1' -> * E0 E1'
// E1' -> / E0 E1'
// E2 -> E1 E2'
// E2' ->
// E2' -> + E1 E2'
// E2' -> - E1 E2'
// E -> E2

void skipspaces(const char **s) {
  while (isspace(**s)) {
    (*s)++;
  }
}

struct expr* parse_e2(const char **s);

typedef struct expr* (*parser)(const char **);

struct expr* parse_val(const char **s) {
  double x = 0;
  int digits = 0;
  while (isdigit(**s)) {
    x = x * 10 + (**s - '0');
    (*s)++;
    digits++;
  }
  if (digits == 0) {
    return NULL;
  }
  skipspaces(s);
  struct expr* e = malloc(sizeof(struct expr));
  e->tag = VAL;
  e->payload.val = x;
  return e;
}

struct expr* parse_var(const char **s) {
  int i = 0;
  int capacity = 50;
  char *v = malloc(capacity);
  while (isalpha(**s) && i < capacity-1) {
    v[i++] = **s;
    (*s)++;
  }
  if (i == 0) {
    free(v);
    return NULL;
  }
  v[i] = 0;
  skipspaces(s);
  struct expr* e = malloc(sizeof(struct expr));
  e->tag = VAR;
  e->payload.var = v;
  return e;
}

struct expr* parse_parens(const char **s) {
  if (**s != '(') {
    return NULL;
  }
  (*s)++;
  skipspaces(s);
  struct expr* e = parse_e2(s);
  if (e == NULL) {
    return NULL;
  }
  if (**s != ')') {
    free_expr(e);
    return NULL;
  }
  (*s)++;
  skipspaces(s);
  return e;
}

struct expr* parse_this_fun(const char **s, const char *fun_s, enum fun fun) {
  int n = strlen(fun_s);
  if (strncmp(fun_s,*s,n) == 0) {
    *s += n;
    skipspaces(s);

    struct expr* arg = parse_parens(s);

    if (arg == NULL) {
      return NULL;
    }

    struct expr *e = malloc(sizeof(struct expr));
    e->tag = FUN;;
    e->payload.fun.op = fun;
    e->payload.fun.arg = arg;
    return e;
  } else {
    return NULL;
  }
}

struct expr* parse_fun(const char **s) {
  struct expr *e = NULL;

  if (e == NULL) {
    e = parse_this_fun(s, "sin", SIN);
  }
  if (e == NULL) {
    e = parse_this_fun(s, "cos", COS);
  }
  return e;
}

struct expr* parse_e0(const char **s) {
  struct expr* e = NULL;
  if (e == NULL) {
    e = parse_val(s);
  }
  if (e == NULL) {
    e = parse_fun(s);
  }
  if (e == NULL) {
    e = parse_var(s);
  }
  if (e == NULL) {
    e = parse_parens(s);
  }
  return e;
}

struct expr* mk_binop(struct expr* lhs, enum binop op, struct expr* rhs) {
  struct expr *e = malloc(sizeof(struct expr));
  e->tag = BINOP;
  e->payload.binop.op = op;
  e->payload.binop.lhs = lhs;
  e->payload.binop.rhs = rhs;
  return e;
}

struct expr* parse_e1(const char **s) {
  struct expr* lhs = parse_e0(s);
  skipspaces(s);

  while (1) {
    switch (**s) {
    case '*': {
      (*s)++;
      skipspaces(s);
      struct expr* rhs = parse_e0(s);
      if (rhs) {
        lhs = mk_binop(lhs, MUL, rhs);
      } else {
        free_expr(lhs);
        return NULL;
      }
    }
      break;
    case '/': {
      (*s)++;
      skipspaces(s);
      struct expr* rhs = parse_e0(s);
      if (rhs) {
        lhs = mk_binop(lhs, DIV, rhs);
      } else {
        free_expr(lhs);
        return NULL;
      }
    }
      break;
    default:
      return lhs;
    }
  }
}

struct expr* parse_e2(const char **s) {
  struct expr* lhs = parse_e1(s);
  skipspaces(s);

  while (1) {
    switch (**s) {
    case '+': {
      (*s)++;
      skipspaces(s);
      struct expr* rhs = parse_e1(s);
      if (rhs) {
        lhs = mk_binop(lhs, ADD, rhs);
      } else {
        free_expr(lhs);
        return NULL;
      }
    }
      break;
    case '-': {
      (*s)++;
      skipspaces(s);
      struct expr* rhs = parse_e1(s);
      if (rhs) {
        lhs = mk_binop(lhs, SUB, rhs);
      } else {
        free_expr(lhs);
        return NULL;
      }
    }
      break;
    default:
      return lhs;
    }
  }
}

struct expr* parse_expr(const char *input) {
  const char * const orig = input;
  const char **s = &input;
  struct expr* e = parse_e2(s);
  if (e == NULL || **s != 0) {
    fputs("Parse error here:\n", stderr);
    fputs(orig, stderr);
    fputs("\n", stderr);
    for (int i = 0; i < *s - orig; i++) {
      fputc(' ', stderr);
    }
    fputs("^\n", stderr);
    if (e != NULL) {
      free_expr(e);
    }
    return NULL;;
  } else {
    return e;
  }
}

static int encode_expr_worker(const struct expr* e, uint64_t *words, int* w) {
  switch (e->tag) {
  case VAR: {
    uint64_t v;
    if (strcmp(e->payload.var, "v") == 0) {
      v = 2;
    } else if (strcmp(e->payload.var, "u") == 0) {
      v = 1;
    } else if (strcmp(e->payload.var, "t") == 0) {
      v = 0;
    } else {
      fprintf(stderr, "Unknown variable: %s\n", e->payload.var);
      return 1;
    }
    words[(*w)++] = (v<<32) | 1;
  }
    break;
  case VAL:
    words[(*w)++] = (*(uint64_t*)(&e->payload.val)) << 32;
    break;
  case BINOP:
    if (encode_expr_worker(e->payload.binop.lhs, words, w) != 0) {
      return 1;
    }
    if (encode_expr_worker(e->payload.binop.rhs, words, w) != 0) {
      return 1;
    }
    switch (e->payload.binop.op) {
    case ADD:
      words[(*w)++] = 4;
      break;
    case SUB:
      words[(*w)++] = 5;
      break;
    case MUL:
      words[(*w)++] = 6;
      break;
    case DIV:
      words[(*w)++] = 7;
      break;
    }
    break;
  case FUN:
    if (encode_expr_worker(e->payload.fun.arg, words, w) != 0) {
      return 1;
    }
    switch (e->payload.fun.op) {
    case COS:
      words[(*w)++] = 8;
      break;
    case SIN:
      words[(*w)++] = 9;
      break;
    }
    break;
  }
  return 0;
}

uint64_t* encode_expr(const struct expr* e, int* len) {
  uint64_t *words = calloc(200, sizeof(uint32_t));
  *len = 0;
  int x = encode_expr_worker(e, words, len);
  if (x == 0) {
    return words;
  } else {
    free(words);
    return NULL;
  }
}
