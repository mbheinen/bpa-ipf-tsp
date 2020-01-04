#include <setjmp.h>
#include <stdio.h>

extern struct jump_err {
  jmp_buf saved_context;
#ifdef UNDERSCORE
  } jump_err_;
#else
  } jump_err;
#endif

#ifdef UNDERSCORE
void set_exit_ (unsigned long *a)
#else
void set_exit (unsigned long *a)
#endif
{
  int b = 1;
  if (a != NULL && *a > '\0') b = *a;
#ifdef UNDERSCORE
  longjmp (jump_err_.saved_context, b);
#else
  longjmp (jump_err.saved_context, b);
#endif
}
