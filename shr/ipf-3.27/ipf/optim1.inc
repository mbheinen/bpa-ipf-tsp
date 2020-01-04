c
        parameter   ( MXDISP = 100 )   ! buses w/econic dispatch
        parameter   ( MXQPEN = 100 )   ! buses w/react penalties
        parameter   ( MXOPCH = 1000)   ! temporary optimal changes
        parameter   ( MAXBUSTIE = 120) ! temporary ideal bus ties

        common /optim1/ txtie(10,MAXBUSTIE), tbxtr(14,MAXLTC), 
     &                  buspen(MAXBUS), knvert(4,MXOPCH), 
     &                  qpent(10,MXQPEN), econ(10,MXDISP), sen(3,50),
     &                  knvtx(MAXLTC), dkdp(MAXCAR)
        dimension kqpent(10,MXQPEN), kecon(10,MXDISP), ksen(3,50)
	double precision txtie, tbxtr
	real buspen, qpent, econ, sen, dkdp
	integer knvert, knvtx, kqpent, kecon, ksen
c
        equivalence (qpent,kqpent), (econ,kecon), (sen,ksen)

