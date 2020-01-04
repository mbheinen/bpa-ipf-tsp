C    @(#)svs.inc	4.3 9/21/93

c      double precision bs7, bs89, bs10, bs1112, bs1314

       real     as1,    as2,    as3,    as4,    as5,    as6,    as7, 
     &          as8,    as9,    as10,   as11,   as12,   as13,   as14, 
     &          brpmax, cksvs,  bpmax,  vemax,  vemin,  vmax,   vmin,
     &          dv,     bmax,   bmin,   bsv1,   bsv2,   bsv3,   svref, 
     &          cksd,   bsv4,   bsv1o,  bsv2o,  dvlo,   dvhi,   etermo, 
     &          svsetn, svserr, cks1,   cks2,   cks3,   vscsmx, 
     &          sigo1,  sigo2,  svgio,  bpmin, 
     &          svbio,  svgij,  svbij,  svango, vscs

       integer ksvtot, isvno, iremte, iptr, ispcde, isvsfz, irmote,
     &         jrmote, isupt, isupb, irbus, igenno

c      MAXSVS is max # of SVS's allowed
c      DTSVS is the sub time step on which SVS's are calculated (in cyc.)

       integer     MAXSVS
       parameter ( MAXSVS = 25) 
       real        DTSVS 
       parameter ( DTSVS = 0.1)

       common /svs/		ksvtot,			
     & isvno  (MAXSVS),		iremte (MAXSVS),	iptr   (MAXSVS),
     & ispcde (MAXSVS),		as1    (MAXSVS),	as2    (MAXSVS),
     & as3    (MAXSVS),		as4    (MAXSVS),	as5    (MAXSVS),
     & vemax  (MAXSVS),		brp    (MAXSVS),	cksvs  (MAXSVS),
     & cksd   (MAXSVS),		bpmax  (MAXSVS),	dv     (MAXSVS),
     & as6    (MAXSVS),		bmax   (MAXSVS),	bmin   (MAXSVS),
     & bsv1   (MAXSVS),		bsv2   (MAXSVS),	bsv3   (MAXSVS),
     & svref  (MAXSVS),	 	bsv4   (MAXSVS),	vemin  (MAXSVS),
     & bsv2o  (MAXSVS),		dvlo   (MAXSVS),	vmax   (MAXSVS),
     & dvhi   (MAXSVS),		etermo (MAXSVS),	vmin   (MAXSVS),
     & bsv4o  (MAXSVS),         svsetn (MAXSVS),        bpmin  (MAXSVS),
     & svserr (MAXSVS),         isvsfz (MAXSVS)

       common /svsup/ 		irmote (MAXSVS),	jrmote (MAXSVS),
     & isupt  (MAXSVS), 	isupb  (MAXSVS),	irbus  (MAXSVS),
     & igenno (MAXSVS), 	cks1   (MAXSVS),	as7    (MAXSVS),
     & as8    (MAXSVS),		as9    (MAXSVS),	cks2   (MAXSVS),
     & as10   (MAXSVS),		as11   (MAXSVS),	as12   (MAXSVS),
     & as13   (MAXSVS),		as14   (MAXSVS),	cks3   (MAXSVS),
     & vscsmx (MAXSVS), 	bs7    (MAXSVS),	bs89   (MAXSVS),
     & bs10   (MAXSVS),		bs1112 (MAXSVS),	bs1314 (MAXSVS),
     & sigo1  (MAXSVS),	
     & sigo2  (MAXSVS),		svgio  (MAXSVS),	svbio  (MAXSVS),
     & svgij  (MAXSVS), 	svbij  (MAXSVS),	svango (MAXSVS),
     & vscs   (MAXSVS)


       character*1 iparsv
       common/svs1/iparsv(MAXSVS)

       common/svsout/		bsvs   (MAXSVS)

       integer svstyp
       real svsbias, dv2, bshunt, tdelay, etermoo, sigoo1, sigoo2, 
     &      timeover
       common / svs2 /
     & svsbias(MAXSVS),	        dv2    (MAXSVS),       bshunt  (MAXSVS),
     & tdelay (MAXSVS),         svstyp (MAXSVS),       etermoo (MAXSVS),
     & sigoo1 (MAXSVS),         sigoo2 (MAXSVS),       timeover(MAXSVS)

