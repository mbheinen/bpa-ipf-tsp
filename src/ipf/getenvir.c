#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined __WATCOMC__                                         /* WATCOM */
#pragma aux getenvir "^";
typedef struct descriptor {
  char     *addr;
  unsigned len;
} descriptor;

void getenvir( descriptor *WATCOM_text, descriptor *WATCOM_var)
{

  char *text = WATCOM_text->addr;
  char *var = WATCOM_var->addr;
  unsigned size1 = WATCOM_text->len;
  unsigned size2 = WATCOM_var->len;
  char *p;
                                      
/*
  fprintf (stderr, " __WATCOMC__ is defined \n");
  fprintf (stderr, " *WATCOM_text = %x \n", WATCOM_text);
  fprintf (stderr, " *WATCOM_var  = %x \n", WATCOM_var);
*/

#elif defined VMS                                               /* VMS */
#include <descrip.h>
void getenvir ( struct dsc$descriptor_s *VMS_text, 
                struct dsc$descriptor_s *VMS_var )
{
  char *text = VMS_text->dsc$a_pointer;
  char *var = VMS_var->dsc$a_pointer;
  int size1 = VMS_text->dsc$w_length;
  int size2 = VMS_var->dsc$w_length;
  char *p;

/*
  fprintf (stderr, " VMS is defined \n");
  fprintf (stderr, " *VMS_text = %x \n", WATCOM_text);
  fprintf (stderr, " *VMS_var  = %x \n", WATCOM_var);
*/

#elif defined DBOS                                              /* DBOS */
void "FORTRAN" getenvir ( char *text, char *var, long size1, long size2) 
  "GETENVIR"
{
  char *p;

#elif defined UNDERSCORE
void getenvir_(char * text, char * var, int size1, int size2)
{
  char *p;

#elif DUNDERSCORE
void getenvir_(char * text, char * var, int size1, int size2)
{
  char *p;

#else
void getenvir(char * text, char * var, int size1, int size2)
{
  char *p;
#endif

  /* Convert the blank filled Fortran string "text" to a NULL-terminated
  ** C string   
  */

  for (p = text; p < &text[size1]; p++) {
    if (*p == ' ') *p = '\x00';
  }
  p = getenv(text);
  if (p != 0 && strlen(p) < size2) 
    strcpy (var, p);
  else if (p != 0) 
    strncpy (var, p, size2);
  else  
    *var = '\x00';

  /* Convert the NULL-terminated C-strings "text" and "var" to blank filled
     Fortran strings  */


  for (p = text; p < &text[size1] && *p > 0; p++);
  for (; p < &text[size1]; p++) *p = ' ';

  for (p = var; p < &var[size2] && *p > 0; p++);
  for (; p < &var[size2]; p++) *p = ' ';
}
