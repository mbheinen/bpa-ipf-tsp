pf_rec r;

printf("Initialize branch record == \n");

pf_init_branch(&r, "L", "WESTMESA", 345.0, "FOURCORN", 345.0, "1", 0);

printf("Bus1 Name = %s%5.1f Bus2 Name = %s%5.1f R = %7.1f X = %7.1f\n\n", 
   r.i.branch.bus1_name, r.i.branch.bus1_kv, r.i.branch.bus2_name, r.i.branch.bus2_kv, r.i.branch.r, r.i.branch.x);

