C    @(#)znsum.f	20.3 2/13/96
      subroutine znsum(zn)
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/anlys.inc'
c	Global variables used:
c		None
      include 'ipfinc/arcntl.inc'
c	Global variables used:
c		acznum, acznam
      include 'ipfinc/blank.inc'
c	Global variables used:
c		nztot
      include 'ipfinc/bus.inc'
c	Global variables used:
c		base
      include 'ipfinc/busanl.inc'
c	Global variables used:
c		zsum
      include 'ipfinc/lodtyp.inc'
c	Global variables used:
c		lodtyp, tottyp
      include 'ipfinc/outpt2.inc'
c	Global variables used:
c		ploss(r*8), qloss(r*8), pvlv(r*8), qvlv(r*8)
c		qgenmw(r*8), ploadm(r*8), qloadm(r*8), gequiv(r*8), 
c		bequiv(r*8), skond(r*8), pgenmw(r*8), pmax(r*8), 
c		nb, qmax(r*8), qmin(r*8) 
      include 'ipfinc/prt.inc'
c	Global variables used:
c		outbuf
 
      common /neggen/ neggen
      real neggen
C
      dimension z(8)
      character zn*2
 
      save ll
 
      write (outbuf,190) zn
  190 format('0',7x,'SUMMARY OF SYSTEM QUANTITIES FOR ZONE ',a2 )
      call prtout(1)
 
      write (outbuf,200)
  200 format('0',7x,'GENERATION',21x,'LOAD',23x,'LOSSES',22x,'SHUNT')
      call prtout(1)
 
      write (outbuf,210)
  210 format(6x,'MW',9x,'MVAR',13x,'MW',9x,'MVAR',13x,'MW',9x,'MVAR',
     113x,'MW',9x,'MVAR')
      call prtout(1)
 
      write (outbuf,220) (zsum(i,ll),i=1,8)
  220 format('0',f10.1,f11.1,f17.1,f11.1,f17.1,f11.1,f17.1,f11.1)
      call prtout(1)
      call space(1)
 
      return
C                     * * * * * * * * * * * * *
      entry zonint
 
      do 100 i = 1,nztot
      do 100 j=1,26
  100 zsum(j,i)=0.0
      neggen = 0.0
      return
 
C                     * * * * * * * * * * * * *
 
      entry znbsum(zn)
 
      l1 = 1
      l2 = nztot
  110 ll = (l1 + l2)/2
      if (kompr(zn,acznam(ll),junk)) 120,160,130
  120 l2 = ll - 1
      go to 140
 
  130 l1 = ll + 1
  140 if (l1.le.l2) go to 110
 
      write (errbuf(1),150) zn
  150 format('0 ANALYSIS ERROR -- ZONE ',a2,' IS NOT IN SYSTEM.')
      call prterx ('I',1)
      go to 170
 
  160 continue
 
  170 return
 
C                     * * * * * * * * * * * * *
 
      entry zlsum(zn)
 
      zsum(5,ll) = zsum(5,ll) + ploss
      zsum(6,ll) = zsum(6,ll) + qloss
      return
 
C                     * * * * * * * * * * * * *
 
      entry znbsuf
 
      zsum(2,ll) = zsum(2,ll) + qgenmw
      zsum(3,ll) = zsum(3,ll) + ploadm
      zsum(4,ll) = zsum(4,ll) + qloadm
      zsum(5,ll) = zsum(5,ll) + pvlv + gequiv
      zsum(6,ll) = zsum(6,ll) + qvlv + bequiv
      zsum(7,ll) = zsum(7,ll) + skcond
      zsum(8,ll) = zsum(8,ll) + qcap+qreak
      if (pgenmw .gt. 0.0) then
         zsum(1,ll) = zsum(1,ll) + pgenmw
         zsum(9,ll) = zsum(9,ll) + dmax1(pgenmw,pmax)
         zsum(10,ll) = zsum(10,ll) + pgenmw
      else
         zsum(3,ll) = zsum(3,ll) - pgenmw
         neggen = neggen + pgenmw
      endif
 
      if(pgenmw.gt.0.0.and.base(nb).lt.30.0) then
         zsum(11,ll) = zsum(11,ll) + qmax
         zsum(12,ll) = zsum(12,ll) + dmax1(qgenmw,0.0d0)
         zsum(13,ll) = zsum(13,ll) + qmin
         zsum(14,ll) = zsum(14,ll) + dmin1(qgenmw,0.0d0)
      endif
 
      zsum(15,ll) = zsum(15,ll) + skcap
      zsum(16,ll) = zsum(16,ll) + qcap
      zsum(17,ll) = zsum(17,ll) + dmax1(qunsk,0.0d0)
      zsum(18,ll) = zsum(18,ll) + skreak
      zsum(19,ll) = zsum(19,ll) + qreak
      zsum(20,ll) = zsum(20,ll) + dmin1(qunsk,0.0d0)
      zsum(21,ll) = zsum(21,ll) + tottyp(1)
      zsum(22,ll) = zsum(22,ll) + tottyp(2)
      zsum(23,ll) = zsum(23,ll) + tottyp(3)
      zsum(24,ll) = zsum(24,ll) + tottyp(4)
      zsum(25,ll) = zsum(25,ll) + tottyp(5)
      zsum(26,ll) = zsum(26,ll) + tottyp(6)
      return
C                     * * * * * * * * * * * * *
      entry arsum (iarea)
 
      do 230 i = 1,8
  230 z(i) = 0.0
 
      do 250 i = 1,nztot
      if (acznum(i) .ne. iarea) go to 250
 
      do 240 j = 1,6
  240 z(j) = z(j) + zsum(j,i)
 
  250 continue
 
      write (outbuf,260) arcnam(iarea)
  260 format('0',7x,'SUMMARY OF SYSTEM QUANTITIES FOR AREA ',a10 )
      call prtout(1)
 
      write (outbuf,200)
      call prtout(1)
 
      write (outbuf,210)
      call prtout(1)
 
      write (outbuf,220) (z(i),i=1,8)
      call prtout(1)
      call space(1)
 
      return
 
      end
