
        parameter (MXORDR = MAXYE+4*MAXTIE+2*MAXTBX) ! reordering elements
        common /renum/ kownt (2,MAXBUS), loc1 (MAXBUS), kolum1 (MXORDR),
     1                 kordr1 (MXORDR), loc2 (MAXBUS), kolum2 (MXORDR),
     2                 kordr2 (MXORDR)
 
