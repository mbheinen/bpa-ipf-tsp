pf_rec r;

printf("Initialize area record == \n"); 
pf_init_area(&r, "A", "NORTHWEST");
printf("Type = %s  Area Name = %s  Scheduled export = %7.1f\n\n", r.i.area.type, r.i.area.name, r.i.area.sched_export);


printf("Initialize branch record == \n");

pf_init_branch(&r, "L", "WESTMESA", 345.0, "FOURCORN", 345.0, "1", 0);

printf("Bus1 Name = %s%5.1f Bus2 Name = %s%5.1f R = %7.1f X = %7.1f\n\n", 
   r.i.branch.bus1_name, r.i.branch.bus1_kv, r.i.branch.bus2_name, r.i.branch.bus2_kv, r.i.branch.r, r.i.branch.x);


int error;
pf_rec *b; 

/* This function is normally used to find data for a specific bus. 
pf_init_bus stores the ID fields in the structure. */
pf_init_bus (&b, "B", "SJUAN G1", 22.0); 

/* Then pf_rec_bus retrieves the data. */
error = pf_rec_bus (&b, “G”);

/* Gets rest of bus data (input). */
if (!error)
    error = pf_rec_bus (&b, "O"); /* Gets output bus data. */

fprintf (out, "Initialize cbus record == \n");
pf_init_cbus (&r,"+","PNM", "SAN JUAN", 345.0, " ");
fprintf (out, "Bus Name = %s%5.1f Owner = %s  Load = %7.1f\n\n", r.i.cbus.name, r.i.cbus.kv, r.i.cbus.owner, r.i.cbus.Pload);

fprintf (out, "Initialize itie record == \n");
pf_init_itie (&r, "I", "NORTHWEST", "BC=HYDRO");
fprintf (out, "Area 1 = %s  Area 2 = %s  Scheduled flow = %7.1f\n\n", 
  r.i.itie.area1_name, r.i.itie.area2_name, r.i.itie.sched_export);

fprintf (out, "Initialize Q=curve record == \n");
pf_init_bus (&r, "QP", "SJUAN G1", 22.0);
fprintf (out, "Bus Name = %s%5.1f Status code = %s\n\n",
           r.i.qcurve.bus_name, r.i.qcurve.bus_kv, r.i.qcurve.active);

pf_init_rec(r, AREA);
pf_init_rec(r, L_LINE);
pf_init_rec(r, AC_BUS);
pf_init_rec(r, CBUS);
pf_init_rec(r, ITIE);
pf_init_rec(r, QCURVE);