/* 
 * The following is an example of a COPE program, re-written
 * in the "C" programming language using the CFLOW library.
 * It does a Shunt Reactive Summary report on the currently
 * loaded case, for a user-entered zone. Output to gui T/W.
 */

#include <stdio.h>
#include <string.h>
/* use this form if "cflowlib.h" is in a "user
library" include area. Your compile procedure
should use the "-I or /I" option that specifies
the path to "cflowlib.h" */
#include "cflowlib.h"

int readln( char *s, int lim ) /* Function to read input from the T/W. */
{
    int i;
    char c;
    for ( i=0; i < lim - 1 && ( c = getchar() ) != EOF && c != '\n'; ++i )
s[i] = c;
s[i] = '\0';
return i;
}
main( int argc, char *argv[] ) /* Main Program */
{
pf_rec r; /* CFLOW structure */
char zn[3];
int error, status ;
float q_avail_react_tot, q_avail_cap_tot,
q_used_react_tot, q_used_cap_tot,
q_unused_react_tot, q_unused_cap_tot,
q_unsched_react_tot, q_unsched_cap_tot;
pf_cflow_init( argc, argv ); /* IPC connection function, required. */
/* Ask user for zone to report */
printf("Enter Zone to report Shunt Reactive Summary > ");
readln( zn, sizeof(zn) );
zn[sizeof(zn)] = '\0';
printf("\n\n Shunt Reactive Summary for Zone %s \n\n",zn);
printf(" Avail_caps Avail_reac Used_caps Used_reac Unus_caps Unus_rx Unsch_caps
Unsch_rx\n\n");
q_avail_react_tot = q_avail_cap_tot =
q_used_react_tot = q_used_cap_tot =
q_unused_react_tot = q_unused_cap_tot =
q_unsched_react_tot = q_unsched_cap_tot = 0.0;
/* Compute zone quantities */
error = pf_rec_bus( &r, "F" ); /* get first bus in case */
status = pf_rec_bus( &r, "O" ); /* get solution data for first bus */
while ( !error && !status ) { /* Loop through all buses in case */
if ( strcmp(r.i.ACbus.zone, zn )==0)
{ /* If bus is in the zone */
q_avail_react_tot += r.s.ACbus.Bshunt_sch_rx;
q_used_react_tot += r.s.ACbus.Bshunt_used_rx;
q_avail_cap_tot += r.s.ACbus.Bshunt_sch_cap;
q_used_cap_tot += r.s.ACbus.Bshunt_used_cap;
if( r.s.ACbus.Qunsch < 0 ) {
q_unsched_react_tot -= r.s.ACbus.Qunsch;
}
else
{
q_unsched_cap_tot += r.s.ACbus.Qunsch;
}
}
error = pf_rec_bus( &r, "N" ); /* get next bus in case */
status = pf_rec_bus( &r, "O" ); /* get solution data for next bus */
}
q_unused_react_tot = q_avail_react_tot - q_used_react_tot;
q_unused_cap_tot = q_avail_cap_tot - q_used_cap_tot;
/* Print zone summary */
printf(" %6.1f %6.1f %6.1f %6.1f %6.1f %6.1f %6.1f %6.1f \n\n",
q_avail_react_tot, q_avail_cap_tot,
q_used_react_tot, q_used_cap_tot,
q_unused_react_tot, q_unused_cap_tot,
q_unsched_react_tot, q_unsched_cap_tot );
pf_cflow_exit(); /* Drop IPC connection */