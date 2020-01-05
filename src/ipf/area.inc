
        common /area/ tie(10,MAXTIE), area(8,MAXCAR), jarzn(MAXBUS), 
     &                kaloc(4*MAXTIE)

	double precision tie
	integer karea(8,MAXCAR)
        real area
        integer jarzn, kaloc

        equivalence (karea,area)
 
