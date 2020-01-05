#include <setjmp.h>
#include <stdio.h>

extern struct jump_err {
  jmp_buf saved_context;
#if defined UNDERSCORE
  } jump_err_;
#elif defined DUNDERSCORE
  } jump_err_;
#else
  } jump_err;
#endif

#if defined UNDERSCORE
void set_exit_ (unsigned long *a)
#elif defined DUNDERSCORE
void set_exit__ (unsigned long *a)
#else
void set_exit (unsigned long *a)
#endif
{
  int b = 1;
  if (a != NULL && *a > '\0') b = *a;
#if defined UNDERSCORE || DUNDERSCORE
  longjmp (jump_err_.saved_context, b);
#else
  longjmp (jump_err.saved_context, b);
#endif
}
