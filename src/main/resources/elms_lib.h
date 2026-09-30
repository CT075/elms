/* Definitions the ELMS C backend generates references to.
 *
 * Everything here is defined unconditionally. `CCodegen.Options` decides what
 * the generator reaches for; this file does not know and does not care. */
#ifndef ELMS_LIB_H
#define ELMS_LIB_H

/* `ELMS_ARR_DECL` allocates, so the header that needs `malloc` asks for it
 * rather than leaning on whatever includes this. */
#include <stdlib.h>

/* A half-open interval that outlived the `foreach` it was written for. */
typedef struct { int start; int end; } elms_range;

static inline elms_range elms_range_mk(int start, int end) {
  elms_range r = { start, end };
  return r;
}

/* An array that carries its length, declared once per element type because C
 * has no generics. The generated file writes one `ELMS_ARR_DECL` per element
 * type it uses, since a vendored header cannot know what a program allocates.
 *
 * The struct does not own `data`. Nothing generated ever calls `free`, which is
 * what a bare `malloc`ed pointer already did, but wrapping it in a struct makes
 * it look more owned than it is. */
#define ELMS_ARR_DECL(NAME, T)                                    \
  typedef struct { T * data; int len; } NAME;                     \
  static inline NAME NAME##_new(int n) {                          \
    NAME a;                                                       \
    a.data = (T *)malloc(sizeof(T) * n);                          \
    a.len = n;                                                    \
    return a;                                                     \
  }                                                               \
  static inline NAME NAME##_zero(void) {                          \
    NAME a;                                                       \
    a.data = NULL;                                                \
    a.len = 0;                                                    \
    return a;                                                     \
  }

#endif /* ELMS_LIB_H */
