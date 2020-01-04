#include <stdlib.h>
#include <string.h>
#include <stdio.h>
main()
{
  char home_dir[80] = "";
  static char xgui_file_name[120];
  char *p;
  FILE *xgui_ptr;

  p = getenv ("SYS$LOGIN");
  strcpy(&home_dir[0], p);                         /* Pointer to string value */
  strcpy( xgui_file_name, &home_dir[0] );
                                  
  strcat( xgui_file_name, "XGUI.DAT" );  

  xgui_ptr = fopen( xgui_file_name, "r+");

  if (xgui_ptr == NULL) 
    fprintf (stderr, "cannot open file %s \n", xgui_file_name );
  else
    fprintf (stderr, "can open file %s \n", xgui_file_name );
}
