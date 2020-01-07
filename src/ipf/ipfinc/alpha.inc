
      common /alpha/  gkmu(MAXYE), bkmu(MAXYE),			 ! R*8
     +                gkku(MAXBUS), bkku(MAXBUS),                ! R*8
     +                pnetu(MAXBUS),qnetu(MAXBUS),      	 ! R*8
     +                inetr(MAXBUS),ineti(MAXBUS),               ! R*8
     +                ploadu(MAXBUS), qloadu(MAXBUS),            ! R*8
     +                vlimn(MAXBUS),vlimx(MAXBUS),               ! R*4
     +		      km(MAXBUS+1), kmlen(MAXBUS), ikmu(MAXYE),  ! I*4
     +                ntypu(MAXBUS),nspar(MAXBUS), yptr          ! I*4
c
c     The following were converted from r*4 to r*8. 
c	      
      double precision gkmu, bkmu, gkku, bkku, pnetu, qnetu,
     &                 inetr, ineti, ploadu, qloadu
      real vlimn, vlimx
      integer km, kmlen, ikmu, ntypu, nspar, yptr
c
