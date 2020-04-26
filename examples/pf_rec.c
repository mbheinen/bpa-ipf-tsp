pf_rec a, bus, branch; 
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
out = fopen ("misc.rpt", "w");

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

