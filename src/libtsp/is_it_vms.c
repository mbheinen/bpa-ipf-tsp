/*** is_it_vms  routine ***/

#ifdef UNDERSCORE
int is_it_vms_()
#else
int is_it_vms()
#endif
{
#ifdef VMS
   return 1;
#else
   return 0;
#endif
}

