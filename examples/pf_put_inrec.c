int error;
char record [130];

/* record needs to contain valid change, add, or delete input data */
error = pf_put_inrec ( record );
if (!error)
    printf ("Successfully changed, added, or deleted input record.\n");
else
    printf ("Invalid record.\n"); }
