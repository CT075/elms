/* Definitions the ELMS C backend generates references to.
 *
 * Everything here is defined unconditionally. `CCodegen.Options` decides what
 * the generator reaches for; this file does not know and does not care. */
#ifndef ELMS_LIB_H
#define ELMS_LIB_H

/* A half-open interval that outlived the `foreach` it was written for. */
typedef struct { int start; int end; } elms_range;

static inline elms_range elms_range_mk(int start, int end) {
  elms_range r = { start, end };
  return r;
}

#endif /* ELMS_LIB_H */
