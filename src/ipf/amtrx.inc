
        parameter ( MAXMTX = 8.0 * MAXYE ) ! Factored Jacobian elements
c
        common /amtrx/ dpt(2,MAXBUS+2*MAXLTC), amtrx(MAXMTX)
	double precision dpt, amtrx
c
