pf_rec c, a, bus, branch; 
int error, status;
char net_data[80]; 
FILE *out;

printf("Enter branch identifying data: "); 
gets(net_data); 
pf_rec_a2b(net_data, &b, "I");

pf_rec_bus(&bus, "F");
pf_rec_b2a(net_data, &bus, "I");
printf("%s\n", net_data);

pf_cflow_init( argc, argv );
out = fopen ("ipf_report.txt", "w");

/* Obtain case comments */ 
error = pf_rec_comments (&c, "G");
fprintf (out, "Current case is: %s Description: %s\n\n", c.case_name, c.case_descrip);

fprintf (out, "%s\n", c.h[0]); 
fprintf (out, "%s\n", c.h[1]);
fprintf (out, "%s\n\n", c.h[2]);
fprintf (out, "%s\n", c.c[0]);
fprintf (out, "%s\n", c.c[1]);
fprintf (out, "%s\n\n", c.c[2]);

pf_rec_branch(&branch, "O");

/* Area data */
fprintf (out, "\n******** AREA DATA ********\n\n");
cf_debug = 1;
error = pf_rec_area( &a, “F” ); /* get first area */
cf_debug = 0;

status = pf_rec_area( &a, “O” ); /* get first area output*/

while ( !error && !status )
{
    fprintf (out, "Type Area Name  Slack Bus    NZn  Export    Pgen Pload   Ploss Pexport  Vmax  Vmin\n");
    fprintf (out, " %s  %s %s%5.1f %d %7.2f %7.1f %7.1f %7.1f %7.1f %6.4f %6.4f\n", 
      a.i.area.type, a.i.area.name, a.i.area.sbus_name, a.i.area.sbus_kv, a.i.area.num_zones, a.i.area.sched_export, a.s.area.Pgen, a.s.area.Pload, a.s.area.Ploss, a.s.area.Pexport, a.i.area.max_Vpu,a.i.area.min_Vpu);
    
    fprintf (out, "Zones in Area:  %s  %s  %s  \n\n", a.i.area.zones[0], a.i.area.zones[1], a.i.area.zones[2]);
    
    error = pf_rec_area( &a, “N” ); /* get next area */
    status = pf_rec_area( &a, “O” ); /* get next area output*/
}

pf_init_itie("I ", areaname1, areaname2);
status = pf_rec_itie (&itie, "G");

for (error = pf_rec_branch(&branch,"f1"); error == 0; error = pf_rec_branch(&branch,"n1"))
{
    printf("      %s, kv= %6.1f,name2= %s, kv2= %6.1f\n", 
         branch.i.branch.bus1_name, branch.i.branch.bus1_kv, branch.i.branch.bus2_name, branch.i.branch.bus2_kv);
}

for (error = pf_rec_bus(&bus, "f"); error == 0; error = pf_rec_bus(&bus, "n"))
{
    pf_rec_bus(&r,"o");
    printf("name= %s, kv= %6.1f, vmag=%6.1f, vdeg=%6.1f\n", 
        bus.i.ACbus.name, bus.i.ACbus.kv, bus.s.ACbus.Vmag, bus.s.ACbus.Vdeg);
}

/* CBUS DATA */
fprintf (out,”\n******** CBUS DATA ********\n\n”);      error = pf_rec_bus( &r, “F” );           /* get first bus in case */     status = pf_rec_cbus( &r, “F1” );  /* is there a cbus record?  */
while (!error) {
    do {
        error = pf_rec_bus( &r, “N” ); /* get next bus in case */
        status = pf_rec_cbus( &r, “F1” ); /* is there a cbus record? */
    } while (status);

    while ( !status )  /* loop on bus with cbus record(s) */ {
        status = pf_rec_cbus( &r, “O” );
        fprintf (out, "Type Own  Bus Name         Pload  Qload  Gshunt Bshunt    Pgen  Qgn-mx    Qmin  \n");
        fprintf (out, " %s %s  %s%5.1f %7.1f %7.1f %7.1f %7.1f %7.1f %7.1f %7.1f\n\n", 
          r.i.cbus.type, r.i.cbus.owner,r.i.cbus.name,r.i.cbus.kv, r.s.cbus.Pload,r.s.cbus.Qload,r.s.cbus.Gshunt,r.s.cbus.Bshunt, r.i.cbus.Pgen ,r.i.cbus.Qgen_Qmax,r.i.cbus.Qmin);
        status = pf_rec_cbus( &r, “N1” );
    }
}


close(out);