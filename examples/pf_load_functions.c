int error;
pf_rec br;

error = pf_load_oldbase ("43bus.bse");
printf("Loaded old base 43bus.bse, status = %d\n\n", error);

if (!error) {
    error = pf_load_changes("43bus.chg");
    printf("Loaded change file 43bus.chg, status = %d\n\n", error);
}

error = pf_load_oldbase ("j98cy94.bse, rebuild = ON");

pf_load_refbase("J98CY94.BSE"); 
pf_select_base('R'); 
pf_rec_branch(&br, "F")