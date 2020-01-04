C    @(#)pqparl.f	20.3 2/13/96
      subroutine pqparl (ptr, nxtybr)
      integer ptr
c
c     Calculate PIN, QIN, POUT, QOUT, PLOSS, QLOSS quantites
c     and accumulate for the analysis reports.
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/anlys.inc'
c	Global variables used:
c		ratbtl, rateeff, rattag
      include 'ipfinc/blank.inc'
c	Global variables used:
c		bmva, nztot
      include 'ipfinc/branch.inc'
c	Global variables used:
c		None
      include 'ipfinc/bus.inc'
c	Global variables used:
c		base, inp2alf, bus, e(r*8), f(r*8)
      include 'ipfinc/dc2t.inc'
c	Global variables used:
c		dct2(r*8)
      include 'ipfinc/dcmt.inc'
c	Global variables used:
c		dcmtbs(r*8), dcmtln(r*8)
      include 'ipfinc/ecvar.inc'
c	Global variables used:
c		None
      include 'ipfinc/mwm.inc'
c	Global variables used:
c		pav
      include 'ipfinc/outpt2.inc'
c	Global variables used:
c		intor, pin, pout, k1, k2, nbsec, a(cmplx*16), 
c		v(cmplx*16), s(cmplx*16), y(cmplx*16), 
c		qin, qout, ploss, qloss, pintot, qintot, poutot, qoutot,
c		aout, ain, ltype, idc, nb, jdc, amag, vamag, rating,
c		base1, base2, tang, arate, ovld, pctol, pwrfk
      include 'ipfinc/prt.inc'
c	Global variables used:
c		errbuf
      include 'ipfinc/lfiles.inc'
c	Global variables used:
c		None

c     set up record type code parameters....

      include 'ipfinc/brtype.inc'
c
      double precision a1, a2, s1, s2, ar1, ar2
c
      complex v2

      character mown*4,nown*4
c
      integer gtmetpnt
c
      save kt, mt, ksw, lan
 
      kt = inp2opt(k1)
      mt = inp2opt(k2)

      if (ltype.ne.BRTYP_LM .and. ltype.ne.BRTYP_LD) go to 150
C
C     COMPUTE DC LINE FLOWS
C
      if (ltype.eq.BRTYP_LM) then
         if (nb.eq.dcmtln(1,jdc)) then
            l1 = dcmtln(8,jdc)
            l2 = dcmtln(9,jdc)
         else
            l1 = dcmtln(9,jdc)
            l2 = dcmtln(8,jdc)
         endif
         vt1 = dcmtbs(20,l1)
         vt2 = dcmtbs(20,l2)
         ain = (vt1 - vt2)/dcmtln(4,jdc)
      else
         if (nb .eq. dc2t(1,idc)) then
            vt1 = dc2t(40,idc)
            vt2 = dc2t(41,idc)
            ain = 0.001 * dc2t(39,idc)
         else
            vt1 = dc2t(41,idc)
            vt2 = dc2t(40,idc)
            ain = -0.001*dc2t(39,idc)
         endif
      endif
      pin = vt1 * ain
      pout = -vt2 * ain
      ain = 1000.0 * ain
      aout = -ain
      qin = 0
      qout = 0
      go to 180
C
C     COMPUTE AC LINE FLOWS
C
  150 continue
      v(1)=dcmplx(e(kt),f(kt))
      v(2)=dcmplx(e(mt),f(mt))
      do 170 i=1,2
         a(i)=dcmplx(0.0d0,0.0d0)
         do 160 j=1,2
            a(i)=a(i)+y(i,j)*v(j)
  160    continue
         s(i)=v(i)*dconjg(a(i))
  170 continue
      pin= real(dreal (s(1)))*bmva
      qin= real(dimag (s(1)))*bmva
      pout= real(dreal (s(2)))*bmva
      qout= real(dimag (s(2)))*bmva

  180 continue
      ploss = pin + pout
      qloss = qin + qout
      pintot = pintot + pin
      qintot = qintot + qin
      poutot = poutot + pout
      qoutot = qoutot + qout
      pltot = pltot + ploss
      qltot = qltot + qloss
      ksw = 2
      go to 220
C
C     COMPUTE SECTION VOLTAGES AND LINE FLOWS
C
      entry pqsect(ptr, nxtybr)
      if (nbsec.ne.1) then
         v(1)=v(2)
         a(1)=-a(2)
         s(1)=-s(2)
      endif
      v(2)=(a(1)-y(1,1)*v(1))/y(1,2)
      a(2)=y(2,1)*v(1)+y(2,2)*v(2)
      s(2)=v(2)*dconjg(a(2))
      pin=real(dreal(s(1)))*bmva
      qin=real(dimag(s(1)))*bmva
      pout=real(dreal(s(2)))*bmva
      qout=real(dimag(s(2)))*bmva
      ploss = pin + pout
      qloss = qin + qout
      if (nxtybr .ne. 3) then
         mt = inp2opt(k2)
         v2=cmplx(e(mt),f(mt))
         dv=cdabs(v2-v(2))
         if (dv .gt. 0.001) then
            write (errbuf(1),200) bus(k1),base(k1),bus(k2),base(k2),id
  200       format('0 Error in section voltages : branch ',a8,f6.1,2x,
     &         a8,f6.1,2x,a1)
            write (errbuf(2),202) v2,v(2)
  202       format (t31, 'Actual V ',2f7.4,' Calculated ',2f7.4)
            call prterx ('W',2)
         endif
      endif
C
C     COMPUTE OVERLOADS,ETC.
C
      ksw = 1

  220 if (ltype .ne. BRTYP_T .and. ltype .ne. BRTYP_TP) go to 240
      a1 = cdabs(a(1))*bmva   ! This is current converted into equivalent
C                            ! MVA.
      a2 = cdabs(a(2))*bmva
      s1 = cdabs(s(1))*bmva
      s2 = cdabs(s(2))*bmva
C
C     Adjust transformer Primary/secondary loading to be larger of
C       (1) Primary MVA / Primary Voltage (p.u.) or
C       (2) Secondary MVA / Secondary Voltage (p.u.)
C     Store in variable AMAG
C
      ar1 = s1 / cdabs (v(1))
      ar2 = s2 / cdabs (v(2))
      amag = sngl(dmax1 (ar1, ar2))
C
C     Evaluate MVA loading at receiving sending end
C
      if (pin .lt. pout .and. 
     &    dmax1 (dabs(pin), dabs(pout)) .gt. 0.1d0) then
         vamag = sngl(dmax1(s1,1.0d-6))
         pwrfk = pin/vamag
      else if ( dmax1(dabs(pin), dabs(pout)) .gt. 0.1) then 
         vamag = sngl(dmax1(s2,1.0d-6))
         pwrfk = pout/vamag
      else if ( qin .lt. qout) then
         vamag =  sngl(dmax1(s1, 1.0d-6))
         pwrfk =  pin/vamag
      else 
         vamag = sngl(dmax1(s2,1.0d-6))
         pwrfk = pout/vamag
      endif
      go to 280

  240 if (ltype .ne. BRTYP_LM .and. ltype .ne. BRTYP_LD) go to 250
      a1 = cdabs(a(1))
      a2 = a1
      amag = sngl(a1)
      vamag = dmax1(cdabs(s(1)),1.0d-6)*bmva
      pwrfk = pin/vamag
      go to 280

  250 a1 = 577.3502692*bmva*cdabs(a(1))/base1
      a2 = 577.3502692*bmva*cdabs(a(2))/base2
      if (a1 .gt. a2) then
         amag = sngl(a1)
         vamag = dmax1(cdabs(s(1)),1.0d-6)*bmva
         pwrfk = pin/vamag
      else
         amag = sngl(a2)
         vamag = dmax1(cdabs(s(2)),1.0d-6)*bmva
         pwrfk = pout/vamag
      endif
  280 continue
C
C     ANALYSIS
C
      if (ksw .eq. 2 .or. nbsec .eq. 1) then
         if (ltype .ne. BRTYP_PEQ) then
            lan = 1
            if (inp2alf(k1) .lt. inp2alf(k2)) then
               lan = 2
               pav = 0.5*abs(pin-pout)
               call lnanal
            endif
         endif
      else if (lan .eq. 2) then
         pav = 0.5*abs(pin-pout)
         call lnanal
      endif
C
C     OVERLOAD ANALYSIS
C
      pctol = 0.0
      if (ltype .eq. BRTYP_PEQ) then
         xmag = 0.0
         pctlc = 0.0
         go to 550
      endif

      if (ltype .eq. BRTYP_T .or. ltype .eq. BRTYP_TP) go to 400
      if (rating.gt.0.0) pctol = 100.0*dim(amag,rating)/rating
      ovld = ' '
      xmag = amag
       if (pctol .ne. 0.0) then
          if (amag.lt.100000.0) then
             iamag=amag
             write (ovld,362) iamag
  362        format(i5,'A  ')
          else
             ovld='*****A  '
          endif
       endif
      if (lan.eq.1) go to 440
      tang = 0.0
      if (ksw.eq.1) go to 430
C
C     Analyze parallels
C
      if (ltype.ne.BRTYP_LD .and. ltype.ne.BRTYP_LM) then
         tang = datan2(f(kt),e(kt)) - datan2(f(mt),e(mt))
      else
         tang = 0.0
      endif
      tang = 57.2957795*abs(tang)
      if (rating .gt. 0.0 .and.  
     &   (amag .gt. 0.01*ratln*rating .or. tang.ge. 30.0)) then
         call olanal
      endif
      if (rating .gt. 0.0 .and. amag. gt. 0.01*rateff*rating) then
         call effanl
      endif
      go to 440

  400 if (rating .gt. 0.0) then
         pctol = 100.0 * dim(vamag,rating) / rating
      else
         pctol = 0.0
      endif
      if (ratbtl .gt. 0.0) then
         pctol1 = 100.0 * dim(amag,ratbtl) / ratbtl
      else
         pctol1 = 0.0
      endif
C
C     Redefine minimum rating if bottleneck rating is the solitary
C     violation.
C
      if (pctol .eq. 0.0) then
         if (pctol1 .gt. 0.0) then
            pctol = pctol1
            rattag = 'B'
            rating = ratbtl
         endif
      endif
      xmag = vamag
      ovld = '     '
      if (pctol .ne. 0.0) then
         if (vamag .lt. 100000.0) then
            iamag = vamag
            write (ovld,408)iamag
  408       format(i5,'MVA')
         else
            ovld='*****MVA'
         endif
      endif
      basex = amax1(base1,base2)
      arate = 577.3502694*rating/basex

      if (lan .ne. 1) then
         if (rating.gt.0.and.vamag.gt.0.01*rattx*rating) then
            call olanal
         endif
         if ((rating.gt.0).and.(ltype.eq.BRTYP_T)) call txefan
      endif
      go to 440

  430 if (lan .ne. 1) then
         tang = datan2(f(kt),e(kt)) - datan2(f(mt),e(mt))
         tang = 57.2957795*abs(tang)
         if (ltype .ne. BRTYP_T .and. ltype .ne. BRTYP_TP) then
            if (rating .gt. 0.0 .and.
     &         (amag .gt. 0.01*ratln*rating .or. tang .ge. 30.0)) then
               call olanal
            endif
            if (rating .gt. 0.0 .and. amag .gt. 0.01*rateff*rating) then
               call effanl
            endif
         else
            if (rating .gt. 0.0 .and. vamag .gt. 0.01*rattx*rating) then
               call olanal
            endif
            if (rating.gt.0) call txefan
         endif
      endif
C
C     Analyze sections
C
  440 if (ltype.eq.BRTYP_T) call txanal
      if (rating .eq. 0.0) then
         pctld = 0.0
      else
         pctld = xmag/rating
      endif
C
C     LOSS ANALYSIS
C
      if (ltype .eq. BRTYP_PEQ) go to 550

      if (intor .eq. 0) intor = gtmetpnt(ptr)

      if (intor .eq. 2 .and. nztot .gt. 0) then
         call zlsum(zname)
      endif
      if (ltype .eq. BRTYP_LM .or. ltype .eq. BRTYP_LD) then
         call dclnal
      endif
  550 return
      end
