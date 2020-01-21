/*** is_it_vms  routine ***/

#ifdef WIN32
#pragma aux is_it_vms "^";
#endif

#ifdef UNDERSCORE
int is_it_vms_()
#elif DUNDERSCORE
int is_it_vms__()
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

