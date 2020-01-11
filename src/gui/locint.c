#ifdef UNDERSCORE
int locint_ (int *var)
#elif DUNDERSCORE
int locint_ (int *var)
#else
int locint (int *var)
#endif
{
  return (int ) var;
}
