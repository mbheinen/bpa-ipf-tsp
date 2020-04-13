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