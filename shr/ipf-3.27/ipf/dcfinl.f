C    @(#)dcfinl.f	20.3 2/13/96
        subroutine dcfinl
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
c	Global variables used:
c		mtdcln, ntotcs
      include 'ipfinc/bus.inc'
c	Global variables used:
c		opt2inp, bus, base
      include 'ipfinc/com009.inc'
c	Global variables used:
c		jdcx
      include 'ipfinc/dc.inc'
c	Global variables used:
c		dcbus(r*8)
      include 'ipfinc/dc2t.inc'
c	Global variables used:
c		dc2t(r*8)
      include 'ipfinc/dcmt.inc'
c	Global variables used:
c		dcmtbs(r*8), dcmtln(r*8)
      include 'ipfinc/ecvar.inc'
c	Global variables used:
c		ntotdc, idckt, nckt
      include 'ipfinc/intbus.inc'
c	Global variables used:
c		None
      include 'ipfinc/prt.inc'
c	Global variables used:
c		errbuf
 
        dimension mdc(50),ndc(50),ldc(50)
 
        character dc_code*1
c 
C     RE-ENTRY AFTER SOLUTION TO IMPLEMENT SOLUTION VALUES
C       INTO DC ARRAYS
 
      if (ntotdc.eq.0) go to 260
 
      do 100 i = 1,ntotdc
        ldc(i) = 0
        mdc(i) = 0
        ndc(i) = 0
  100 continue
 
      jsw = 0
 
C     ESTABLISH CROSS-INDEX "DCBUS --> KMTDCB"
 
      do 120 jckt = 1,idckt
         nec = nckt(jckt) - 1
         ntdc = nckt(jckt+1) - nckt(jckt)
         do 110 i = 1,ntdc
            j=i+nec
            if (dcbus(22,j).gt.0) then
               k = dcbus(22,j)
               mdc(j) = k
               ndc(j) = jckt
               ldc(k) = j
            endif
  110    continue
 
  120 continue
 
C     UPDATE 2-TERMINAL AND N-TERMINAL DC ARRAYS
 
      do 220 i = 1,ntotdc
      id = dcbus(22,i)
      if (id.gt.0) go to 160
 
C     UPDATE "DC2T"
 
      k1 = dcbus(1,i)
      k2 = dcbus(3,i)
      k1 = opt2inp(k1)
      k2 = opt2inp(k2)
      id = -id
      if (k1 .eq. dc2t(1,id) .and. k2 .eq. dc2t(33,id)) then
         ksw = 1
      else if (k1 .eq. dc2t(3,id) .and. k2 .eq. dc2t(34,id)) then
         ksw = 2
      else
         call erexit
      endif
 
      do 150 j = 1,28
      k = jdcx(j,ksw)
      if (k.eq.0) go to 150
      dc2t(k,id) = dcbus(j,i)
 
  150 continue
 
      if (ksw.eq.1) dc2t(39,id) = 1000.0*dcbus(19,i)/dcbus(20,i)
 
      dc2t(20,id) = 10000*ifix(sngl(dcbus(27,i)))
     &            + 1000*ifix(sngl(dcbus(28,i)))
     &            + 100*ifix(sngl(dcbus(31,i)))
     &            + 10*ifix(sngl(dcbus(32,i)))
     &            + ifix(sngl(dcbus(33,i)))
      go to 220
 
C     UPDATE "DCMTBS"
 
  160 do 170 j = 13,36
      dcmtbs(j,id) = dcbus(j,i)
  170 continue
 
      dcmtbs(21,id) = ndc(i)
      dcmtbs(22,id) = ldc(id)
      dcmtbs(30,id) = 0
      call getchr_8 (1, dc_code, dcmtbs(4,id))
      if (dcmtbs(3,id) .eq. 0) then
 
         if (dc_code .ne. ' ' ) then
            k1 = dcmtbs(1,id)
            write (errbuf(1),210) bus(k1),base(k1),dc_code,
     &         dcmtbs(19,id)
  210       format('0 DC CONVERTER ',a8,f7.1,' SCHEDULED MODE (',a1,
     1             ') IS INCOMPATIBLE WITH ACTUAL MODE ',f10.3,' MW ')
            call prterx ('W',1)
            jsw = 1
         endif
 
      else if ((dc_code .eq. 'R') .and. (dcmtbs(19,id) .gt. 0)) then
 
      else if ((dc_code .eq. 'I' .or. dc_code .eq. 'M') .and.
     1         (dcmtbs(19,id) .lt. 0)) then
      else
            k1 = dcmtbs(1,id)
            write (errbuf(1),210) bus(k1),base(k1),dc_code,
     &         dcmtbs(19,id)
            call prterx ('W',1)
            jsw = 1
      endif
 
      if (dc_code .eq. 'I' .or. dc_code .eq. 'M') then
 
         if (dcmtbs(12,id) .ne. dcmtbs(13,id)) then
 
            k1 = dcmtbs(1,id)
            write (errbuf(1),212) bus(k1),base(k1),dc_code,
     &         dcmtbs(12,id),dcmtbs(13,id)
  212       format('0 DC CONVERTER ',a8,f7.1,' MODE (',a1,') GAMMA (',
     1              f6.4,') .NE. GAMMA_nominal (',f6.4,').')
            call prterx ('W',1)
 
         endif
 
      endif
 
  220 continue
 
      if (mtdcln.eq.0) go to 240
 
C     UPDATE "DCLINE"
 
      do 230 i = 1,mtdcln
        k1 = dcline(1,i)
        i1 = dcline(8,i)
        i2 = dcline(9,i)
        dcmtln(8,i) = mdc(i1)
        dcmtln(9,i) = mdc(i2)
        dcmtln(7,i) = ndc(i1)
        if (ndc(i1).ne.ndc(i2)) call erexit
  230 continue
 
  240 continue
      if (jsw.eq.0) go to 260
 
      write (errbuf(1),250)
  250 format('0 CAUTION -- ABOVE DC ERRORS ARE UNACCEPTABLE FOR ',
     1       ' SUBSEQUENT TRANSIENT STABILITY STUDIES .')
      call prterx ('W',1)
 
      ntotcs = 1
  260 continue
      return
      end
