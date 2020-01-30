C    @(#)brchek.f	20.3 2/13/96
      subroutine brchek (jtot)
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/branch.inc'
c	Global variables used:
c		kx, ky, brsect, brtype, brnch, brid
      include 'ipfinc/bus.inc'
c	Global variables used:
c		base, bus
      include 'ipfinc/prt.inc'
c	Global variables used:
c		outbuf
 
C
C     BLKDTA-BRCHEK
C
      common /ran001/ range(9,10)
      character type*2
C
      character id*1, brntyp*2
      external brntyp
C
      k1 = kx(jtot)
      k2 = ky(jtot)
      id = brid(jtot)
      isect = brsect(jtot)
      ltype = brtype(jtot)
      type = brntyp(ltype)
C
      go to (210,210,100,210,100,100,210,100,210) ltype
C
  100 if (brnch(6,jtot) .lt. 0.0) go to 210
      do 110 k = 1,10
         if (base(k1) - range(1,k)) 120,130,110
  110 continue
      k = 10
      go to 130
C
  120 k = k - 1
  130 ratio = 0.0
      if (brnch(6,jtot) .ne. 0.0) ratio = brnch(5,jtot)/brnch(6,jtot)
      if (ratio .gt. 0.0) then
         pct = dim(ratio,range(3,k)) - dim(range(2,k),ratio)
         if (pct .gt. 0.0) then
            pct = 100.0*pct/ratio
            write (outbuf,140) type, bus(k1), base(k1), bus(k2),
     1        base(k2),id,isect,ratio,pct
  140       format(' Branch data error -- R/X for branch (',a2,2(2x,a8,
     1             f7.1),a3,i3,') is ',f6.3,' violation is ',f7.2,' %')
            call prtout(1)
         endif
      endif
  150 if (ltype .eq. 3) then
         amiles = brnch(9,jtot)
         if (amiles .gt. 0.0) then
            alines = amax1(1.0,brnch(1,jtot))
            ratio = brnch(5,jtot)/amiles*base(k1)**2/100.0/alines
            if (ratio .gt. 0.0) then
               pct = dim(ratio,range(5,k)) - dim(range(4,k),ratio)
               if (pct .gt. 0.0) then
                  pct = 100.0*pct/ratio
                  write (outbuf,160) type, bus(k1), base(k1), bus(k2),
     &               base(k2), id, isect, ratio, pct
  160             format(' Branch data error -- R/mile for branch (',
     1               a2,2(2x,a8,f7.1),a3,i3,' is ',f6.3,
     2               ' violation is ',f7.2,' %')
                  call prtout(1)
               endif
            endif
            ratio = brnch(6,jtot)/amiles*base(k1)**2/100.0/alines
            if (ratio .gt. 0.0) then
               pct = dim(ratio,range(7,k)) - dim(range(6,k),ratio)
               if (pct .gt. 0.0) then
                  pct = 100.0*pct/ratio
                  write (outbuf,180) type, bus(k1), base(k1), bus(k2),
     &               base(k2), id, isect, ratio, pct
  180             format(' Branch data error -- X/MILE for branch (',
     1              a2,2(2x,a8,f7.1),a3,i3,') IS ',f6.3,' Violation is '
     2              ,f7.2,' %')
                  call prtout(1)
               endif
            endif
            ratio = brnch(8,jtot)/amiles/base(k1)**2*10.0**8*alines
            if (ratio .gt. 0.0) then
               pct = dim(ratio,range(9,k)) - dim(range(8,k),ratio)
               if (pct .gt. 0.0) then
                  pct = 100.0*pct/ratio
                  write (outbuf,200) type, bus(k1), base(k1), bus(k2),
     &               base(k2), id, isect, ratio, pct
  200             format(' BRANCH DATA ERROR --',
     &                   'MICRO-B/MILE FOR BRANCH (',
     &                    a2,2(2x,a8,f7.1),a3,i3,') IS ',f6.1,
     &                    ' VIOLATION IS ',f7.2,' %')
                  call prtout(1)
               endif
            endif
         endif
      endif
  210 continue
      return
      end
