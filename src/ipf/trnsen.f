C    @(#)trnsen.f	20.3 2/13/96
      subroutine trnsen
 
C        This subroutine computes the transfer T which will alleviate
C        any overload in a monitored line F which is caused by a
C        forced outage L.
C
C        L --> F <--> T
C
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha2.inc'
c	Global variables used:
c		jflag, iflag
      include 'ipfinc/amtrx.inc'
c	Global variables used:
c		dpt(r*8)
      include 'ipfinc/arcntl.inc'
c	Global variables used:
c		arcnam
      include 'ipfinc/area.inc'
c	Global variables used:
c		tie(r*8)
      include 'ipfinc/blank.inc'
c	Global variables used:
c		nbslck, jtie, ntot, bmva
      include 'ipfinc/branch.inc'
c	Global variables used:
c		None
      include 'ipfinc/bus.inc'
c	Global variables used:
c		e(r*8), f(r*8)
      include 'ipfinc/ecvar.inc'
c	Global variables used:
c		None
      include 'ipfinc/intbus.inc'
c	Global variables used:
c		intbus, intbas
      include 'ipfinc/lfiles.inc'
c	Global variables used:
c		None
      include 'ipfinc/prt.inc'
c	Global variables used:
c		outbuf
      include 'ipfinc/transf.inc'
c	Global variables used:
c		tdata, numt, numf, fdata, ldata
c
      double precision sensttd(MAXBUS)
c
      character idl * 1, idf * 1, label * 3
c
      real ibase, senstt(MAXBUS)
c
      integer pctovl
C
C     Phase I: Check monitored branches for base overload and the
C              attendant transfer which is necessary to alleviate it.
C
      call forbtm
      write (outbuf,310)
  310 format('Base Line Flows of Monitored Branches')
      call rpnlod
 
      write (outbuf,320)
  320 format (t2,'/------- Monitored Branch ------/',
     1    t35,'/---- Base ---/',
     2    t51,'Overload',
     3    t61,'/-- Transfer --/',
     4    t80,'/----- Intertie -----/',
     5   t105,'Base    Transfer  Transfer')
      call shdlod (1)
      write (outbuf,330)
  330 format (
     1    t35,'/   Loading   /',
     2    t51,'   (%)   ',
     3    t61,'/   Loading    /',
     4   t105,'Flow      (MW)      Flow ')
      call shdlod (2)
      write (outbuf,340)
  340 format (
     1    t35,'  (MW)  (amps)',
     2    t61,'  (MW)  (amps)',
     3   t105,'(MW)                (MW) ')
      call shdlod (3)
      outbuf = ' '
      call shdlod (4)
      call shdlod (5)
      call fortop
C                                 Begin base overload loop.
      do 342 i = 1,ntot
         sensttd(i) = 0.0d0        !r*8 used for call to tieflo.
         senstt(i) = 0.0
         dpt(1,i) = 0.0
         dpt(2,i) = 0.0
  342 continue
 
      do 400 lt = 1, numf
         k1 = kfdata(1,lt)
         k2 = kfdata(2,lt)
         idf = char(kfdata(3,lt))
         ksectf = kfdata(4,lt)
C                                   Check for base overloads
         pij = fdata(6,lt) * bmva
         qij = fdata(7,lt) * bmva
 
         sij = sqrt (pij**2 + qij**2)
         ibase = fdata(10,lt)
         vk = e(k1)
         aij = sij * ibase / (bmva * vk)
         rate = fdata(8,lt)
         if (rate .gt. 0.0) then
            ratcur = rate / bmva * ibase
            ratmva = rate
            ratepu = ratmva / bmva * pij / sij
            label = 'MVA'
            pctovl = 100.0 * dim (sij,ratmva) / ratmva
         else
            rate = -rate
            ratcur = rate
            ratmva = rate / ibase * bmva
            ratepu = ratmva / bmva * pij / sij
            label = 'amp'
            pctovl = 100.0 * dim (aij,ratcur) / ratcur
         endif
         call space (1)
         write (outbuf,350) intbus(k1), intbas(k1), intbus(k2),
     1      intbas(k2), idf, ksectf, pij, aij, pctovl
  350    format (t2,a8,f6.1,1x,a8,f6.1,1x,a1,i2,
     1          t35,f8.1,1x,f8.1,
     2          t51,i5,'%')
 
         if (pctovl .eq. 0) then
            call prtout (1)
         else
 
            dpij = pij/bmva - sign (ratepu, pij)
 
            do 380 nt = 1, numt
              read (11,rec=nt) (senstt(i),i=1,ntot)
              sensttd(i) = senstt(i)
              ka1 = ktdata(1,nt)
              ka2 = ktdata(2,nt)
 
              call gettrf (0,lt,nt,dpt,senstt,0.0,dpij,comp1,tx1)
 
              txmva = tx1 * bmva
C
C        Compute transfer-perturbed flow in intertie
C
              tienew = 0.0
 
              do 352 j = 1,jtie
 
              if (ka1.eq.tie(2,j).and.ka2.eq.tie(8,j)) then
                 factor = 1.0
              else if (ka1.eq.tie(8,j).and.ka2.eq.tie(2,j))
     1        then
                 factor = -1.0
              else
                 go to 352
              endif
 
              tiexxx = tieflo (j, 0.0, dpt, tx1, sensttd)
              tienew = tienew + factor * tiexxx
 
  352         continue
C
C        Compute transfer-perturbed flow in monitored branch
C
         call linflo (lt,ain,pin,qin,0.0,dpt,tx1,senstt)
 
         if (nt .eq. 1) then
            write (outbuf(59:),360) pin, ain, arcnam(ka1),
     1         arcnam(ka2), tdata(7,nt), txmva, tienew
  360       format (f8.1,1x,f8.1,t22,a10,1x,a10,1x,f8.1,1x,f8.1,1x,
     1         f8.1)
            call prtout (1)
         else
            write (outbuf,370) pin, ain, arcnam(ka1), arcnam(ka2),
     1         tdata(7,nt), txmva, tienew
  370       format (t59,f8.1,1x,f8.1,t80,a10,1x,a10,1x,f8.1,1x,
     1         f8.1,1x,f8.1)
            call prtout (1)
         endif
  380    continue
      endif
 
  400 continue
C
C     Phase II: Loop through forced outages, check monitored branches
C               for overload; and compute any transfer necessary to
C               alleviate that overload.
C
      call forbtm
      write (outbuf,410)
  410 format('Contingency Line Flows of Monitored Branches')
      call rpnlod
 
      write (outbuf,420)
  420 format (t2,'/----- Contingency Branch ------/')
      call shdlod (1)
      write (outbuf,430)
  430 format (t2,'/------- Monitored Branch ------/',
     1    t35,'/ Contingency /',
     2    t51,'Overload',
     3    t61,'/-- Transfer --/',
     4    t80,'/----- Intertie -----/',
     5   t105,'Contingency Xfr   Transfer')
      call shdlod (2)
      write (outbuf,440)
  440 format (
     1    t36,'/   Loading  /',
     2    t51,'   (%)   ',
     3    t61,'/   Loading    /',
     4   t105,'Flow      (MW)      Flow')
      call shdlod (3)
      write (outbuf,450)
  450 format (
     1    t36,'  (MW)  (amps)',
     2    t61,'  (MW)  (amps)',
     3   t105,'(MW)                (MW) ')
      call shdlod (4)
      outbuf = ' '
      call shdlod (5)
      call fortop
C
C     Begin outage loop.
C
      do 530 jt = 1, numl
 
      k1 = kldata(1,jt)
      k2 = kldata(2,jt)
      idl = char(kldata(3,jt))
      ksectl = kldata(4,jt)
C
C     Compute the Sensitivity G(x)**-1 * G(u)
C
      do 460 i = 1,ntot
  460 dpt(1,i) = 0.0
 
      pout = ldata(6,jt)
 
      if (k1 .le. nbslck) then
      else
         do 462 i = iflag(k1),iflag(k1+1)-1
         if (jflag(1,i).eq.3) then
            go to 464
         endif
  462    continue
         dpt(1,k1) = -1.0
  464    continue
      endif
 
      if (k2 .le. nbslck) then
      else
         do 466 i = iflag(k2),iflag(k2+1)-1
         if (jflag(1,i).eq.3) then
            go to 468
         endif
  466    continue
         dpt(1,k2) = 1.0
  468    continue
      endif
C
C     Compute the Sensitivity
C
      call baktrn (0)
C
C     Compute the compensation L.
C
      call gettrf (jt, 0, 0, dpt, senstt, pout, 0.0, comp1, tx1)
      if (pout .eq. 0.0) then
         call space (1)
         write (outbuf,470) intbus(k1), intbas(k1), intbus(k2),
     1         intbas(k2), idl, ksectl
  470    format (t2,a8,f6.1,1x,a8,f6.1,1x,a1,i2,
     &           ' outage is trival (zero flow) and is ignored.')
         call prtout (1)
         go to 530
      else if (abs (comp1/pout) .gt. 1000.0) then
         call space (1)
         write (outbuf,472) intbus(k1), intbas(k1), intbus(k2),
     1         intbas(k2), idl, ksectl
  472    format (t2,a8,f6.1,1x,a8,f6.1,1x,a1,i2,
     &           ' outage separates the network.')
         call prtout (1)
         go to 530
      else
         call space (1)
         write (outbuf,474) intbus(k1), intbas(k1), intbus(k2),
     1         intbas(k2), idl, ksectl
  474    format (t2,a8,f6.1,1x,a8,f6.1,1x,a1,i2,' outage ')
         call prtout (1)
C
C        Update Tieflow with outage flows
C
         do 478 nt = 1, numt
 
         ka1 = ktdata(1,nt)
         ka2 = ktdata(2,nt)
         tdata(7,nt) = 0.0
 
         do 476 j = 1,jtie
 
         if (ka1 .eq. tie(2,j) .and. ka2 .eq. tie(8,j)) then
            factor = 1.0
         else if (ka1 .eq. tie(8,j) .and. ka2 .eq. tie(2,j))
     1      then
            factor = -1.0
         else
            go to 476
         endif
 
         pij = tieflo (j, comp1, dpt, 0.0, sensttd)
         tdata(7,nt) = tdata(7,nt) + factor * pij
 
  476    continue
  478    continue
C
C        Loop through monitored branches.
C
         do 520 lt = 1, numf
 
         k3 = kfdata(1,lt)
         k4 = kfdata(2,lt)
         idf = char(kfdata(3,lt))
         ksectf = kfdata(4,lt)
         if (min0 (k3,k4) .eq. min0 (k1,k2) .and.
     1       max0 (k3,k4) .eq. max0 (k1,k2) .and.
     2       idl .eq. idf) go to 520
C
C        Check for contingent overloads.
C
         pijold  = fdata(6,lt)
C
C        Compute contingent-perturbed flow in monitored branch
C
         tx2 = 0.0
         call linflo (lt,aij,pij,qij,comp1,dpt,tx2,senstt)
 
         sij = sqrt (pij**2 + qij**2)
         ibase = fdata(10,lt)
         vk = e(k1)
         rate = fdata(8,lt)
         if (rate .gt. 0.0) then
            ratcur = rate / bmva * ibase
            ratmva = rate
            ratepu = ratmva / bmva * pij / sij
            label = 'MVA'
            pctovl = 100.0 * dim (sij,ratmva) / ratmva
         else
            rate = -rate
            ratcur = rate
            ratmva = rate / ibase * bmva
            ratepu = ratmva / bmva * pij / sij
            label = 'amp'
            pctovl = 100.0 * dim (aij,ratcur) / ratcur
         endif
         dpij = pijold - sign (ratepu, pijold)
         write (outbuf,480) intbus(k3), intbas(k3), intbus(k4),
     1      intbas(k4), idf, ksectf, pij, aij, pctovl
  480    format (t2,a8,f6.1,1x,a8,f6.1,1x,a1,i2,
     1          t35,f8.1,1x,f8.1,
     2          t51,i5,'%')
 
         if (pctovl .eq. 0) then
            call prtout (1)
         else
            do 510 nt = 1, numt
            read (11,rec=nt) (senstt(i),i=1,ntot)
            sensttd(i) = senstt(i)
            ka1 = ktdata(1,nt)
            ka2 = ktdata(2,nt)
            call gettrf (jt, lt, nt, dpt, senstt, pout, dpij, comp2,
     1                   tx2)
            txmva = tx2 * bmva
 
            tienew = 0.0
 
            do 482 j = 1,jtie
 
            if (ka1 .eq. tie(2,j) .and. ka2 .eq. tie(8,j)) then
               factor = 1.0
            else if (ka1 .eq. tie(8,j) .and. ka2 .eq. tie(2,j))
     1         then
               factor = -1.0
            else
               go to 482
            endif
 
            tiexxx = tieflo (j, comp2, dpt, tx2, sensttd)
            tienew = tienew + factor * tiexxx
 
  482       continue
C
C           Compute transfer-perturbed flow in monitored branch
C
            call linflo (lt,ain,pin,qin,comp2,dpt,tx2,senstt)
 
            if (nt .eq. 1) then
               write (outbuf(59:),490) pin, ain, arcnam(ka1),
     1            arcnam(ka2), tdata(7,nt), txmva, tienew
  490          format (f8.1,1x,f8.1,t22,a10,1x,a10,1x,f8.1,1x,f8.1,1x,
     1            f8.1)
               call prtout (1)
            else
               write (outbuf,500) pin, ain, arcnam(ka1), arcnam(ka2),
     1            tdata(7,nt), txmva, tienew
  500          format (t59,f8.1,1x,f8.1,t80,a10,1x,a10,1x,f8.1,1x,
     1            f8.1,1x,f8.1)
               call prtout (1)
            endif
  510       continue
         endif
  520    continue
      endif
  530 continue
 
      outbuf = ' '
      do 540 i = 1,5
      call shdlod (i)
  540 continue
 
C
C     Restore rectangular form of voltages
C
      do 550 kt=1,ntot
      vk = e(kt)
      ak = f(kt)
      e(kt) = vk * cos(ak)
  550 f(kt) = vk * sin(ak)
 
      return
      end
