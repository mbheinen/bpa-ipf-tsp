C     Revisions:

C     Apr/24/92 - DEM - Added some char vars to PLOTLC for new format
C                 of aux file listings.

      common /plotl/nplot, isym
      common /plotlc/plott, itemnm, auxfrm, doplot, plprpg
      character plott*8, itemnm*40, auxfrm*1, doplot*1, plprpg*1

      common /ecsadr/igecs1, kwork, kigent, ibecs, ksub13, ksb13c,
     &       knewt, busmax(4), busmin(4), ibuscl(4), brnmax(3), 
     &       brnmin(3), ibrncl(3), ksb13a, ksb13b, ksb13d, ksb13e, 
     &       ksb13f, ksb13g, ksb13h, ksb13i, ladecs, ildatt, ktbus, 
     &       ktbus1, ktnol1, ktnol2, ktzpp, ktnol4, ktnol5, ktnol6, 
     &       ktnol7, ktnol8, jtot, ilinsw
      common /opt/igrup(100), genmax(15), genmin(15), igencl(15)
      common /par/icount, icoun2, nogrp, t(maxstp), ktcom, nsym, ntim,
     &       ipltkt, tmax, edt, ihdsw
      common /parc/clf(120)
      character*10 clf, alf

      common /genmnd/ksub(15), msub(26), msindx(15), xmax, xmin, rmax,
     &       rmin, actr, actx, ksub10, maxu(7)
      common /max/kecmax

      integer nplot, isym

      integer igecs1, kwork, kigent, ibecs, ksub13, ksb13c,
     &       knewt, ibuscl,
     &       ibrncl, ksb13a, ksb13b, ksb13d, ksb13e, 
     &       ksb13f, ksb13g, ksb13h, ksb13i, ladecs, ildatt, ktbus, 
     &       ktbus1, ktnol1, ktnol2, ktzpp, ktnol4, ktnol5, ktnol6, 
     &       ktnol7, ktnol8, jtot, ilinsw
      real   busmax, busmin, brnmax, brnmin
      integer igrup, igencl
      real genmax, genmin
      integer icount, icoun2, nogrp, ktcom, nsym, ntim,
     &       ipltkt, ihdsw
      real t, edt, tmax

      integer ksub, msub, msindx, ksub10, maxu
      real xmax, xmin, rmax, rmin, actr, actx
      integer kecmax
