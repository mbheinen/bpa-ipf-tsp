pf_rec r;

printf(”Initialize area record == \n”); 
pf_init_area(&r, "A", "NORTHWEST");
printf("Type = %s  Area Name = %s  Scheduled export = %7.1f\n\n", r.i.area.type, r.i.area.name, r.i.area.sched_export);