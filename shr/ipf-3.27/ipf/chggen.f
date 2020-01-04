C    @(#)chggen.f	20.1 11/12/98
      subroutine chggen (nwrd, word, pctp, pctq, pltoto, qltoto,
     1                   pltotn, qltotn, typout, error)
 
      integer nwrd, error
      character word (*)*(*), typout * 1
      real pctp, pltotn, pctq, qltotn, pltoto, qltoto
 
C     process %GEN_CHANGE within /CHANGE_PARAMETERS
C  
C     Input: NWRD - counter of WORD().  
C            WORD - character array from SCAN of BUF.   
C       
C     Output: NWRD - updated counter.   
C             WORD - stripped character array.  
C             PCTP - percent P specified.   
C             PCTQ - percent Q specified.   
C             PLTOTO - total quantity in MW before percentage change
C                     applied.
C             QLTOTO - total quantity in MVAR before percentage change
C                     applied.
C             PLTOTN - total quantity in MW afterpercentage change
C                      applied.
C             QLTOTN - total quantity in MVAR after percentage change
C                      applied.
C             TYPOUT - 'P' or 'Q', the disired plotted output quantity.
C             ERROR - return status (0=normal/1=error)
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/alpha2.inc'
      include 'ipfinc/arcntl.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/cbus.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/ordsta.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/tbx.inc'
 
      common /amtrx/ ikk(5,MAXBUS), ownlis(100)
      character ownlis*3

      common /chgbas/ chgbas, baslod
      logical chgbas, baslod

      common /is_batch / is_batch
      
      double precision  dpltot, dqltot

      dimension pctchg(4), ipctch(2)
      character cbown * 3, cbtyp * 1, cbkyr * 2, owntmp * 3,  own * 3   
      logical found 

      chgbas = .false.
      error = 0 
      pctp = 0.0
      pctq = 0.0
      pltotn = 0.0  
      qltotn = 0.0  
      pltoto = 0.0  
      qltoto = 0.0  
      typout = ' '  
        
C     > CHANGE_PARAMETERS, BUS = <name> <base>, QY=? -  
C                         %GEN_CHANGE, %PX = <##>, %QY = <##>, -   
C                                               AREAS = NORTHWEST, -
C                                               ZONES = NB, -
C                                               OWNERS = PSP, -
C                                               TYPES = N <
C                                                              
C     Initialize IKK array:                                    
C                                                              
C     (1,*) = 0 :  normal.                                     
C             1 :  Bus has its load modified.
C     (2,*) = 0 :  bus is not eligible for systematic % changes.
C             1 :  bus is eligible for systematic % changes.    
C     (3,*) = I    (cross index to TBX array)                   
C     (4,*) = 0    (not used)                                   
C     (5,*) = 0    (not used)                                   
C                                                               
      do nb = 1, ntot
         ikk(1,nb) = 0
         ikk(2,nb) = 0
         ikk(3,nb) = 0
         ikk(4,nb) = 0
         ikk(5,nb) = 0
      enddo
 
      do i = 1, ntotb
         ltyp = tbx(1,i)
         if (ltyp .lt. 10) then
            nb = tbx(2,i)
            if (ordtbx .eq. 2) nb = opt2inp(nb)
            ikk(3,nb) = i
         endif
      enddo
C                                            Search for %LOAD.
      do i = 1, nwrd
         if (word(i)(1:1) .eq. '%') then
            if (word(i)(1:2) .eq. '%P') then
               next = i + 1
               last = index(word(next), '%') - 1
               if (last .le. 0) last = lastch (word(next))
               pctp = ftn_atof (word(next)(1:last))
               if (typout .eq. ' ') typout = 'P'
               word(next) = ' ' 
            else if (word(i)(1:2) .eq. '%Q') then   
               next = i + 1 
               last = index(word(next), '%') - 1
               if (last .le. 0) last = lastch (word(next))
               pctq = ftn_atof (word(next)(1:last))
               if (typout .eq. ' ') typout = 'Q'
               word(next) = ' ' 
            endif   
            word(i) = ' '   
 
         endif
 
      enddo
 
      if (pctp .eq. 0.0 .and. pctq .eq. 0.0) then
 
         write (errbuf(1),190)
  190    format ('No %P or %Q percentages specified.')
         call prterx ('W', 1)
         error = 1
         go to 900
 
      endif

C     Search for OWNERS = <owner_1,...>

      kntown = 0
        
      do i = 1, nwrd
         if (word(i)(1:4) .eq. 'OWNE') then 
            word(i) = ' '   
            last = i
            do j = i+1, nwrd
               if (word(j)(1:4) .eq. 'AREA') then   
                  go to 220 
               else if (word(j)(1:4) .eq. 'ZONE') then  
                  go to 220 
               else 
                  kntown = kntown + 1   
                  ownlis(kntown) = word(j)  
                  word(j) = ' ' 
                  last = j  
               endif
            enddo
         endif
  210    continue
      enddo
  220 continue
C                                                   
C     Search for AREAS = <area_1,...> or
C                ZONES = <zone_1,...>               
C                                                   
      do i = 1, nwrd
         if (word(i)(1:4) .eq. 'AREA') then 
            word(i) = ' '   
            do nb = 1, ntot 
               ikk(2,nb) = 0   
            enddo
            do j = i+1, nwrd
               do k = 1, ntotc  
                  if (arcnam(k) .eq. word(j)) then  
                     do nb = 1, ntot
                        if (jarzn(nb) .eq. k) ikk(2,nb) = 1 
                     enddo
                     go to 270  
                  endif 
               enddo
               last = lastch (word(j))  
               write (errbuf(1),260) word(j)(1:last)
  260          format ('Interchange area (', a, 
     1                 ') is not in system') 
               call prterx ('W', 1) 

  270          word(j) = ' '   
            enddo
            go to 340   
         else if (word(i)(1:4) .eq. 'ZONE') then
            word(i) = ' '   
            do nb = 1, ntot 
               ikk(2,nb) = 0   
            enddo
            do j = i+1, nwrd
               found = .false.  
               do nb = 1, ntot  
                  if (zone(nb) .eq. word(j)) then   
                     found = .true. 
                     ikk(2,nb) = 1  
                  endif 
               enddo
               if (.not.found) then 
                  last = lastch (word(j))   
                  write (errbuf(1), 300) word(j)(1:last)
  300             format ('Zone (', a, ') is not in system')
                  call prterx ('W', 1)  
               endif
  310          word(j) = ' '   
            enddo
            go to 340   
         endif  
      enddo
C       
C     Neither ZONES nor AREAS have been specified.  The entire  
C     system is eligible.   
C       
      do i = 1, ntot
         ikk(2,i) = 1   
      enddo
        
  340 continue  
C                                    Sort OWNLIS   
      if (kntown .eq. 0) then   
         kntown = 1 
         ownlis(kntown) = '###' 
      else  
         do i = 1, kntown-1 
            do j = i+1, kntown  
               komp = kompr(ownlis(i), ownlis(i), komp) 
               if (komp .gt. 0) then
                  owntmp = ownlis(i)
                  ownlis(j) = ownlis(i) 
                  ownlis(i) = owntmp
               endif
            enddo
         enddo
 
      endif
C                                         Print out results
      pctppu = 0.01 * pctp
      pctqpu = 0.01 * pctq

      write (outbuf, 370) pctp, pctq
  370 format ('0 %GEN_CHANGE, PG = ', f6.1, '%, QG = ', f6.1, '%') 
      call prtout (1)   

      if (idswb .ne. 0) then
         write (outbuf, 380)
  380    format ('0            BUS             CARD ZONE OWNER   ---- ',
     &    'ORIGINAL QUANTITIES -----  -------- FINAL QUANTITIES ----')
         call prtout (1)
         write (outbuf, 390)
  390    format ('                                               ---- ',
     1   'LOAD ---  - GENERATION --   ---- LOAD ----  - GENERATION -')
         call prtout (1)
         write (outbuf, 400)
  400    format ('                                                ',
     &   '(MW) (MVAR)   (MW)   (MVAR)    (MW)   (MVAR)   (MW)   (MVAR)')
         call prtout (1)
      endif
 
      ipctch(1) = 0
      ipctch(2) = 0
      do i = 1, 4
         pctchg(i) = 0.0
      enddo
      pltoto = 0.0
      qltoto = 0.0
      pltotn = 0.0
      qltotn = 0.0
 
      do nb = 1, ntot
         if (ikk(2,nb) .eq. 0) go to 520
         dpltot = 0.0   
         dqltot = 0.0   
         ntyp = kbsdta(1,nb)
         if (ntyp .eq. 5 .or. ntyp .eq. 12) go to  520  
         own = owner(nb)
         do l = 1, kntown
            if (own .eq. ownlis(l) .or. ownlis(l) .eq. '###') go to 430
         enddo
         go to  450
 
  430    continue
         plo = busdta(8,nb)
         qlo = busdta(9,nb)
         pctchg(1) = pctchg(1) + plo
         pctchg(2) = pctchg(2) + qlo
         dp = plo * pctppu  
         dq = qlo * pctqpu  
         if (dp .ne. 0.0 .or. dq .ne. 0.0) then 
            dpltot = dpltot + dp / bmva 
            dqltot = dqltot + dq / bmva 
        
            pltoto = pltoto + plo   
            qltoto = qltoto + qlo   
            pltotn = pltotn + plo + dp  
            qltotn = qltotn + qlo + dq  
        
            busdta(8,nb) = plo + dp 
            busdta(9,nb) = qlo + dq 
            ipctch(1) = ipctch(1) + 1   
            if (idswb .ne. 0) then  
               write (outbuf, 440) '  ', '  ', bus(nb), base(nb),   
     1            zone(nb), owner(nb), plo, qlo, busdta(8,nb),  
     2            busdta(9,nb)  
  440          format (3x, 2x, a2, 2x, a3, 1x, a8, f6.1, 2x,
     1            'B ', 3x, a2, 3x, a3, 2f9.2, 17x, 2f9.2)
               call prtout(1)
            endif
         endif

C        Perform %changes on customer buses.

  450    icb = kbsdta(15,nb)
         do while (icb .gt. 0)
C                                                    
C           Exclude type "A" code year "  " and "00" continuation bus
C           data (equivalent or cut system data)
C       
            call getchr(1,cbtyp,kbctbl(8,icb))  
            call getchr(2,cbkyr,kbctbl(9,icb))  
            call getchr(3,cbown,kbctbl(10,icb)) 
            if ((cbtyp .eq. 'A') .and.  
     1          (cbkyr .ne. '01' .and. cbkyr .ne. '02')) go to 500  
            do l = 1, kntown
               if (cbown .eq. ownlis(l) .or. ownlis(l) .eq. '###')  
     1            go to  480
            enddo
            go to  500
C                                                    
C           Found customer bus match
C                                                    
  480       plo = bctbl(6,icb)
            qlo = bctbl(11,icb)
 
            pctchg(1) = pctchg(1) + plo
            pctchg(2) = pctchg(2) + qlo
            dp = plo * pctppu
            dq = qlo * pctqpu
 
            if (dp .ne. 0.0 .or. dq .ne. 0.0) then
               dpltot = dpltot + dp / bmva
               dqltot = dqltot + dq / bmva
               pltoto = pltoto + plo
               qltoto = qltoto + qlo
               pltotn = pltotn + plo + dp   
               qltotn = qltotn + qlo + dq   
        
               bctbl(6,icb) = plo + dp  
               bctbl(11,icb) = qlo + dq  
               ipctch(2) = ipctch(2) + 1
               k = kbctbl(1,icb)
               call getchr(3,cbown,kbctbl(10,icb))  
               if (idswb .ne. 0) then   
                  write (outbuf, 490) bus(k), base(k), cbtyp, cbown,
     1               plo, qlo, bctbl(6,icb), bctbl(11,icb)   
  490             format (13x, a8, f6.1, 2x, '+', a1, 8x, a3,   
     1               2f9.2, 17x, 2f9.2) 
                  call prtout (1)   
               endif
            endif   
  500       icb = bctbl_nxt(icb)
         enddo
  510    continue   
         if (dpltot .ne. 0.0 .or. dqltot .ne. 0.0) then 
            kt = inp2opt(nb)  
            pnetu(kt) = pnetu(kt) + dpltot
            qnetu(kt) = qnetu(kt) + dqltot
            ntbx = ikk(3,nb)
            if (ntbx .gt. 0) then   
               ltyp = tbx(1,ntbx)  
               if (ltyp .eq. 2 .or. ltyp .eq. 3 .or. ltyp .eq. 5) then  
                  tbx(3,ntbx) = tbx(3,ntbx) + dqltot
                  tbx(4,ntbx) = tbx(4,ntbx) + dqltot
               endif
            endif   
         endif  
        
  520    continue  
      enddo
        
      write (outbuf, 530) pltoto, qltoto, pltotn, qltotn
  530 format ('0', 2x, 'TOTAL', t28, 'ORIGINAL GEN', 2f9.2,
     1   7x, 'FINAL GEN', 2f9.2)   
      call prtout (1)   
      write (outbuf, 540) ipctch(1), ipctch(2)  
  540 format ('0', 2x, i5, 'BUSES', i5, '+ BUSES AFFECTED') 
      call prtout (1)   
C       
C     Squeeze out blanked out words from WORD().
C       
  900 jwrd = nwrd   
      nwrd = 0  
      do i = 1, jwrd
         if (word(i) .ne. ' ') then 
            nwrd = nwrd + 1 
            word(nwrd) = word(i)
         endif
      enddo
C                                                    
C     Check proper control card upon program exit    
C                                                    
      if (index ('[(/HSC', card) .eq. 0) then
C                                                    
C        Meaningless control record                  
C                                                    
         write (errbuf(1), 920) buf(1:80)
  920    format ('Unrecognized command: (', a80, ')')
         if (is_batch .eq. 0) then
            call prterx ('E',1)
         else
            call prterx ('F',1)
         endif
         error = 1  
         return 
      endif 
        
      outbuf = '0End of %GEN_CHANGE '  
      call prtout(1)
      call forbtm   
        
      return
      end
