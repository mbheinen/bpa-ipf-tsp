C    @(#)movnew.f	20.3 2/13/96
c
c	Subroutine movnew.  Re-orders one and two dimensional arrays.
c	31-Oct-94 Added three new entry points to handle double precision
c	arrays: mvnew1d, movod1d and movod2d.  Also added an entry point
c	to handle a real array movol1r.
c
        subroutine movnew (inar1, inar2, inarc, inar1d, inar2d, inar1r,
     &                     order)
        dimension inar2(2,*), inar2d(2,*), inar1(*)
        dimension inar1d(*), order(*), inarc(*), inar1r(*)
c
	double precision inar1d, inar2d
        real inar1r
        character inarc*(*)
        integer order
C
        include 'ipfinc/parametr.inc'
c
        dimension kscr2(2,MAXBUS),kscr1(MAXBUS), scr1(MAXBUS)
        dimension kscr1d(MAXBUS), kscr2d(2,MAXBUS)
c
        double precision kscr1d, kscr2d
c
        real scr1
c
        equivalence (kscr1,kscr2,kscr1d,kscr2d,scr1)
c
        character kscrc(MAXBUS)*8
        save
C
C       ENTRY "MVNEW2" REORDERS 2-DIMENSIONAL ARRAYS
C              new = ORDER (old)
C
        entry mvnew2  (inar2,order,ntot)
        do 100 i = 1,ntot
           j = order(i)
           kscr2(1,j) = inar2(1,i)
           kscr2(2,j) = inar2(2,i)
  100   continue
        do 110 i = 1,ntot
           inar2(1,i) = kscr2(1,i)
           inar2(2,i) = kscr2(2,i)
  110   continue
        return
C
C       ENTRY "MOVOL2" REORDERS 2-DIMENSIONAL ARRAYS
C              old = ORDER (new)
C
        entry movol2  (inar2,order,ntot)
        do 112 i = 1,ntot
           j = order(i)
           kscr2(1,i) = inar2(1,j)
           kscr2(2,i) = inar2(2,j)
  112   continue
        do 114 i = 1,ntot
           inar2(1,i) = kscr2(1,i)
           inar2(2,i) = kscr2(2,i)
  114   continue
        return
C
C       ENTRY "MVNEW1" REORDERS 1-DIMENSIONAL ARRAYS
C              new = ORDER (old)
C
        entry mvnew1  (inar1,order,ntot)
        do 120 i = 1,ntot
        j = order(i)
  120   kscr1(j) = inar1(i)
        do 130 i = 1,ntot
  130   inar1(i) = kscr1(i)
        return
C
C       ENTRY "MOVOL1" REORDERS 1-DIMENSIONAL ARRAY
C              old = ORDER (new)
C
        entry movol1  (inar1,order,ntot)
        do 132 i = 1,ntot
           j = order(i)
           kscr1(i) = inar1(j)
  132   continue
        do 134 i = 1,ntot
           inar1(i) = kscr1(i)
  134   continue
        return
C
C       ENTRY "MVNEWC" READERS CHARACTER ARRAYS
C              new = ORDER (old)
C
        entry mvnewc (inarc,order,ntot)
        do 140 i=1,ntot
           j=order(i)
  140   kscrc(j)=inarc(i)
        do 150 i=1,ntot
  150   inarc(i)=kscrc(i)
        return
C
C       ENTRY "MOVOLC" REORDERS CHARACTER ARRAY
C              old = ORDER (new)
C
        entry movolc  (inarc,order,ntot)
        do 160 i = 1,ntot
           j = order(i)
           kscrc(i) = inarc(j)
  160   continue
        do 170 i = 1,ntot
           inarc(i) = kscrc(i)
  170   continue
        return
C
C       ENTRY "MVNEW1D" REORDERS Double Precision ARRAYS
C              new = ORDER (old)
C
        entry mvnew1d (inar1d,order,ntot)
        do 180 i=1,ntot
           j=order(i)
  180   kscr1d(j)=inar1d(i)
        do 190 i=1,ntot
  190   inar1d(i)=kscr1d(i)
        return
C
C       ENTRY "MOVOL1D" REORDERS Double Precision ARRAYS
C              old = ORDER (new)
C
        entry movol1d  (inar1d,order,ntot)
        do 200 i = 1,ntot
           j = order(i)
           kscr1d(i) = inar1d(j)
  200   continue
        do 210 i = 1,ntot
           inar1d(i) = kscr1d(i)
  210   continue
        return
C
C       ENTRY "MOVOL2D" REORDERS 2-DIMENSIONAL DOUBLE PRECISION ARRAYS
C              old = ORDER (new)
C
        entry movol2d  (inar2d,order,ntot)
        do 220 i = 1,ntot
           j = order(i)
           kscr2d(1,i) = inar2d(1,j)
           kscr2d(2,i) = inar2d(2,j)
  220   continue
        do 230 i = 1,ntot
           inar2d(1,i) = kscr2d(1,i)
           inar2d(2,i) = kscr2d(2,i)
  230   continue
        return
C
C       ENTRY "MVNEW2D" REORDERS 2-DIMENSIONAL DOUBLE PRECISION ARRAYS
C              new = ORDER (old)
C
        entry mvnew2d  (inar2d,order,ntot)
        do 240 i = 1,ntot
           j = order(i)
           kscr2d(1,j) = inar2d(1,i)
           kscr2d(2,j) = inar2d(2,i)
  240   continue
        do 250 i = 1,ntot
           inar2d(1,i) = kscr2d(1,i)
           inar2d(2,i) = kscr2d(2,i)
  250   continue
        return
C
C       ENTRY "MOVOL1R" REORDERS 1-DIMENSIONAL REAL ARRAY
C              old = ORDER (new)
C
        entry movol1r  (inar1r,order,ntot)
        do 251 i = 1,ntot
           j = order(i)
           scr1(i) = inar1r(j)
  251   continue
        do 252 i = 1,ntot
           inar1r(i) = scr1(i)
  252   continue
        return
        end
