C    @(#)ovextx.f	20.6 7/18/96
      subroutine ovextx
C
C     This routine produces the overexcited transformer report.
C     At this time, the over-voltage threshold is always 5.0%
C     over tfrmr tap.
C
C     Pick up variables in common.
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
c	Global variables used:
c		kspare, ntota, bmva, ntot
      include 'ipfinc/branch.inc'
c	Global variables used:
c		brid, kbrnch, brnch_nxt, brnch, brsect, kx, ky,
c		brnch_ptr, brtype
      include 'ipfinc/bus.inc'
c	Global variables used:
c		zone, bus, inp2opt, kbsdta, base, e(r*8), f(r*8)
      include 'ipfinc/outpt2.inc'
c	Global variables used:
c		k1, k2, ltype, nb, rating, jtx
      include 'ipfinc/overex.inc'
c	Global variables used:
c		itxrptr, txmvaflo, txvration, bus1pu, txtap1, itxbdptr,
c		jsort, iotxptr, txfichel, txpaperl, txmvaflo, bus1pu
      include 'ipfinc/prt.inc'
c	Global variables used:
c		outbuf, lprtsw, fichsw, errbuf
      include 'ipfinc/tran.inc'
c	Global variables used:
c		ltran(i*4)


c     set up record type code parameters...
      include 'ipfinc/brtype.inc'
C
C     Local declarations
C
      character paprfl*1, fichfl*1, tapty1*1, tapty2*1, 
     &          txownr*3, zoneb1*2, zoneb2*2, nameb1*8, 
     &          nameb2*8, ckttid*1, nextckttid*1, chsect*1, 
     &          cktt*1,   sortyp*5, tap1*6,   tap2*6,   bus1*6

      complex * 16 temp_yy(2,2)
      real mvaflo
      integer ptr, qptr, regptr, firstsec
      complex aa(2), ss(2), vv(2), yy(2,2)
      logical found
      external kmtxbs, swtxbs, pofrpt
c
c     Set counters to zero
c
      novxtx = 0          ! counter of num of overexcited txs
      npapr = 0           ! counter of num of txs written to paper rpt.
      nfich = 0           ! counter of num of txs written to fiche rpt. 

      do i=1,2
        do j=1,2
          temp_yy(i,j) = cmplx (0.0, 0.0)
          yy(i,j) = cmplx(0.0, 0.0)
        enddo
      enddo
c
c     Loop through each bus and Loop ptr for all branches connected
c     to it.
c
      do nb = 1, ntot
         regptr = 0              ! pointer for a "R" regulator brnch
         ptr = kbsdta(16,nb)     ! pointer to first branch connected 
         do while (ptr .gt. 0)
            ltype = brtype(ptr)
c
            if (ltype .eq. BRTYP_R) then
c              Branch is a regulator:   save its location for following
c                                       xmfrs.
               regptr = ptr

            else if (ltype .eq. BRTYP_T) then
c
c              Branch is a transformer: Test for output listings and 
c                                       overvoltage criteria
c
               qptr = brnch_ptr(ptr)
               nbr  = iabs(qptr)
               k1   = kx(ptr)
               k2   = ky(ptr)
               zoneb1 = zone(k1)                   ! bus1_zone
               call getchr(3,txownr,kbrnch(3,nbr)) ! tfrmr_owner
c
c                           Test eligibility for paper and fiche reports
c
               call pofrpt(paprfl,fichfl,zoneb1,txownr)
c
c                          If tfrmr eligible for output to anywhere,
c                             then do the over-excitation voltage check;
c                             else skip to next transformer
c
               if (paprfl .eq. 'Y' .or. fichfl .eq. 'Y') then
                  intrn1 = inp2opt(k1)
                  buspu1 = sqrt(e(intrn1)*e(intrn1) + 
     &                          f(intrn1)*f(intrn1))
                  baskv1 = base(k1)
                  buskv1 = buspu1 * baskv1
                  if (qptr .gt. 0) then
                     tapkv1 = brnch(9,nbr)
                  else
                     tapkv1 = brnch(10,nbr)
                  endif
                  vratio = buskv1 / tapkv1

                  if ( vratio .ge. 1.05) then
c
c                    Side_1's bus voltage => side_1's tap setting + 5%
c                    Save this overloaded tx data in OVEREX arrays.
c
c                    Check if tables are full, and if so, 
c                    issue message and quit looking for more 
c                    eligible transformers.
c
                     if (novxtx .ge. 2*MAXOLT) then
                        write (errbuf(1),140) 2*MAXOLT
  140                   format ('0  OVEREXCITED TRANSFORMER Table ',
     1                          'FULL, Truncated to',
     2                          i4,' Transformers.')
                        call prterx('w',1)
                        novxtx = 2*MAXOLT
                        goto 180
                     endif
c
c                                 Calculate MVA flows at both ends and 
c                                 save larger of two for report.
c
                     intrn2 = inp2opt(k2)
                     do 142 i=1,2
                         do 141 j=1,2
                           temp_yy(i,j) = yy(i,j)
  141			 continue
  142                continue
                     call pieqiv(ptr,temp_yy,kerr)
                     do 144 i=1,2
                         do 143 j=1,2
                           yy(i,j) = temp_yy(i,j)
  143			 continue
  144                continue
                     call aspar (aa,ss,vv,yy,e,f,intrn1,intrn2)
                     mvaflo = himva (ss, bmva)
                     novxtx = novxtx + 1
                     iotxptr(novxtx)   = novxtx
                     itxbdptr(novxtx)  = ptr
                     txtap1(novxtx)    = tapkv1
                     bus1pu(novxtx)    = buspu1
                     txvratio(novxtx)  = vratio
                     txmvaflo(novxtx)  = mvaflo
                     itxrptr(novxtx)   = regptr
                     txpaperl(novxtx)  = paprfl
                     txfichel(novxtx)  = fichfl
                  endif
               endif
c
            else if (ltype .eq. BRTYP_PEQ) then
c
c              Begining of series string.  One of the sections
c              may be a transformer and must be checked for 
c              output listing options and overexcitation 
c              by calculating down the string.
c
               found = .false.
               lastptr = ptr
               nextptr = brnch_nxt(ptr)
               do while (nextptr .gt. 0 .and. .not. found .and.
     &                  (ky(nextptr) .eq. ky(ptr) .and. 
     &                   brid(nextptr) .eq. brid(ptr)))
                  if (brtype(nextptr) .eq. BRTYP_T) found = .true.
                  lastptr = nextptr
                  nextptr = brnch_nxt(nextptr)
               enddo
               if (.not. found) then
                  ptr = lastptr
                  go to 150
               endif
               firstsec = 0
               qptr = brnch_ptr(ptr)
               nbr  = iabs(qptr)
               k1   = kx(ptr)
               k2   = ky(ptr)
               intrn1 = inp2opt(k1)
               intrn2 = inp2opt(k2)
               ckttid = brid(ptr)
               regptr = 0

c              calculate end one values of the equivalent PI (sect = 0) 
c
                     do 146 i=1,2
                         do 145 j=1,2
                           temp_yy(i,j) = yy(i,j)
  145			 continue
  146                continue
                     call pieqiv(ptr,temp_yy,kerr)
                     do 148 i=1,2
                         do 147 j=1,2
                           yy(i,j) = temp_yy(i,j)
  147			 continue
  148                continue
               call aspar (aa,ss,vv,yy,e,f,intrn1,intrn2)
c
c              For each section up to the desired transformer, calculate
c              far end voltages and project power into the next section.
c              Next section must be branch types R,L,T,or E.
c
               lastptr = ptr
               nextptr = brnch_nxt(ptr)

               do while ( nextptr .gt. 0 )
                  nextk2  = ky(nextptr)
                  nextckttid = brid(nextptr)
                  if ( nextk2.eq.k2 .and. nextckttid.eq.ckttid) then
c
c                    this is another (first) section in the same ckt.
c
                     nextltype  = brtype(nextptr)
                     if (nextltype .eq. BRTYP_L .or.
     &                   nextltype .eq. BRTYP_E) then
                        if (firstsec .gt. 0) then
c
c                           precess receving end of previous section
c                           to sending end of this section.
c
                           call hndoff (aa,ss,vv)
                        else
                           firstsec = 1
                        endif
                     do 152 i=1,2
                         do 151 j=1,2
                           temp_yy(i,j) = yy(i,j)
  151			 continue
  152                continue
                     call pieqiv(nextptr,temp_yy,kerr)
                     do 154 i=1,2
                         do 153 j=1,2
                           yy(i,j) = temp_yy(i,j)
  153			 continue
  154                continue
                        call asvsec (aa,ss,vv,yy)
                        regptr = 0
                        lastptr = nextptr
                        nextptr = brnch_nxt(nextptr)
                     else if (nextltype .eq. BRTYP_R) then
                        regptr = nextptr
                        lastptr = nextptr
                        nextptr = brnch_nxt(nextptr)
                     else if (nextltype .eq. BRTYP_T) then
c
c                       Branch section is a transformer: Test for
c                       output listings and overvoltage criteria
c
                        qptr = brnch_ptr(nextptr)
                        nbr  = iabs(qptr)
                        zoneb1 = zone(k1)          ! bus1_zone
                        call getchr(3,txownr,kbrnch(3,nbr)) !tfrmr_owner
c
c                           Test eligibility for paper and fiche reports
c
                        call pofrpt(paprfl,fichfl,zoneb1,txownr)
c
c                          If tfrmr eligible for output to anywhere,
c                             then do the over-excitation voltage check;
c                             else skip to next transformer
c
                        if (paprfl .eq. 'Y' .or. fichfl .eq. 'Y') then
                           if (firstsec .gt. 0) then
c
c                              precess receving end of previous section
c                              to sending end of this section.
c
                              call hndoff (aa,ss,vv)
                           else
                              firstsec = 1
                           endif
                           do 156 i=1,2
                              do 155 j=1,2
                                 temp_yy(i,j) = yy(i,j)
  155		              continue
  156                      continue
                           call pieqiv(nextptr,temp_yy,kerr)
                           do 158 i=1,2
                             do 157 j=1,2
                                yy(i,j) = temp_yy(i,j)
  157		             continue
  158                      continue
                           call asvsec (aa,ss,vv,yy)
c
c                          Calculate ratio of bus voltage to tfrmr tap
c
                           baskv1 = base(k1)
                           buspu1 = cabs (vv(1))
                           buskv1 = buspu1 * baskv1
                           if (qptr .gt. 0) then
                              tapkv1 = brnch(9,nbr)
                           else
                              tapkv1 = brnch(10,nbr)
                           endif
                           vratio = buskv1 / tapkv1

                           if ( vratio .ge. 1.05) then
c
c                             Side_1's bus voltage => side_1's tap 
c                             setting + 5%. Save this overloaded tx 
c                             data in OVEREX arrays.
c
c                             Check if tables are full, and if so, 
c                             issue message and quit looking for more 
c                             eligible transformers.
c
                              if (novxtx .ge. 2*MAXOLT) then
                                 write (errbuf(1),140) 2*MAXOLT
                                   call prterx('w',1)
                                 novxtx = 2*MAXOLT
                                 goto 180
                              endif
c
c                                 Calculate MVA flows at both ends and 
c                                 save larger of two for report.
c
                              mvaflo = himva (ss, bmva)
                              novxtx = novxtx + 1
                              iotxptr(novxtx)   = novxtx
                              itxbdptr(novxtx)  = ptr
                              txtap1(novxtx)    = tapkv1
                              bus1pu(novxtx)    = buspu1
                              txvratio(novxtx)  = vratio
                              txmvaflo(novxtx)  = mvaflo
                              itxrptr(novxtx)   = regptr
                              txpaperl(novxtx)  = paprfl
                              txfichel(novxtx)  = fichfl
                           endif
                        endif          ! end of tx section 
                        lastptr = nextptr
                        nextptr = brnch_nxt(nextptr)
                     else
                        lastptr = nextptr
                        nextptr = brnch_nxt(nextptr)
                     endif
                  else
                     nextptr = 0
                  endif

               enddo    !  end of passive string
               ptr = lastptr
  150          continue
            else
c              skip all other branch types
               regptr = 0
            endif        ! END of branch type test
c
            ptr = brnch_nxt(ptr)    ! get next branch pointer
c
         enddo             ! END OF Branch LOOP
      enddo             ! END OF BUS LOOP


c
c     Done with NBR loop for each branch
c
  180 continue         ! escape to here if ovextx tables overflow..
c
c     Set JSORT according to type of sort desired; owner, zone or area
c     - 2 means by name only
c     - 1 means by owner
c     - 3 means by zone
c     - 4 means by area
c
      jsort = kspare(21) / 10
      if (jsort .eq. 0) then
         if ( kspare(11) .eq. 0  .or.  kspare(11) .eq. 3 ) then
            jsort = 1
         elseif ( kspare(11) .eq. 1 ) then
            jsort = 3
         elseif ( kspare(11) .eq. 2 ) then
            jsort = 4
         else
            continue
         endif
      endif
c
c     Set character sorting type indicator for report
c
      if (jsort .eq. 1) then
      sortyp = 'Owner'
      elseif (jsort .eq. 3) then
         sortyp = 'Zone'
      elseif (jsort .eq. 4) then
         sortyp = 'Area'
      else
         jsort = 2
         sortyp = 'bUS'
      endif
c
c     Now QIKSORT the OVEREX arrays by ar-zn-own, then tfrmr user_id
c     - using functions KMTXBS and SWTXBS.
c     The table pointers in ITXPTR will be rearranged rather than
c     - the transformer data items themselves.

      call qiksrt(1,novxtx,kmtxbs,swtxbs)
c
c     Build report header
c
      call forbtm
      write (outbuf,200) sortyp
  200 format (t46, 'Transformers Excited Above 5.0% over Tap ',
     1             '(Sorted by ', a, ')')
      call shdlod(1)
      write (outbuf,220)
  220 format ('0', t6, 'Own', t11, 'Zn  Bus.1', t25, 'Base', t31,
     1        'Bus.2', t40, 'Base', t46, 'Ckt Sect', t57,
     2        'Tap.1 / Tap.2', t76, 'Bus.1', t84, 'Ratio', t94,
     3        '<  Excitation >', t112, 'Nomin', t121, 'MVA')
      call shdlod(2)
      write (outbuf,240)
  240 format (t76, 'Volts', t84, 'Bus.1', t94, '<  Violation  >',
     1        t112,'MVA', t121, 'Flow')
      call shdlod(3)
      write (outbuf,260)
  260 format (t76, '(kV)', t84, '/Tap.1', t96, 'p.u.', t105, 'kV',
     1       t112, 'Rating')
      call shdlod(4)
      outbuf = '0 '
      call shdlod(5)
      call fortop
c
c     Save master output paper and fiche switches
c
      ificsv = fichsw
      ipapsv = lprtsw
c
c     Start loop LTX for each transformer
c
      do 380 ltx = 1, novxtx
c
c        JTX is pointer to proper transformer in sort although data
c        - is in an unsorted
c
         jtx = iotxptr(ltx)
c
c        Determine which media will display it
c
         if (txpaperl(jtx) .eq. 'Y') then
            lprtsw = min0(1, ipapsv)
         else
            lprtsw = 0
         endif
         if (txfichel(jtx) .eq. 'Y') then
            fichsw = min0(1, ificsv)
         else
            fichsw = 0
         endif
c
c        Keep count of number of tfrmrs written to paper and to fiche.
c
         nfich = nfich + fichsw
         npapr = npapr + fichsw
c
c        Gather known parameters for report.
c        JTX is entry on overexcited tfrmr table, and NBR is entry in
c        - main branch table.
c
         ptr    = itxbdptr(jtx)
         qptr   = brnch_ptr(ptr)
         nbr    = iabs(qptr)
         tapkv1 = txtap1(jtx)
         buspu1 = bus1pu(jtx)
         vratio = txvratio(jtx)
         mvaflo = txmvaflo(jtx)
         regptr = itxrptr(jtx)
c
c        Calculate missing KV or PU numbers for report
c
         idbus1 = kx(ptr)
         idbus2 = ky(ptr)
         call getchr(3,txownr,kbrnch(3,nbr))
         zoneb1 = zone(idbus1)
         nameb1 = bus(idbus1)
         basb1 = base(idbus1)
         zoneb2 = zone(idbus2)
         nameb2 = bus(idbus2)
         basb2 = base(idbus2)
         cktt = brid(ptr)
         write (chsect,280) brsect(ptr)
  280    format (i1)
         if (chsect .eq. '0') chsect = ' '
         if (qptr .lt. 0) then
            tapkv2 = brnch(9,nbr)
         else
            tapkv2 = brnch(10,nbr)
         endif
         buskv1 = buspu1 * basb1
         violpu = vratio - 1.05
         violkv = violpu * tapkv1
         rating = brnch(4,nbr)
         tapty1 = 'F'
         tapty2 = 'F'
c
c        Determine if this tfrmr is an LTC and if so, find which tap
c        is the adjustable one.
c
         if (regptr .gt. 0) then
c
c                    Tfrmr is indeed an LTC. Now hunt through 
c
            call putchr(1,' ',irtype)
c
            do 300 llt = 1, ntota
               jvarbs = ltran(9,llt)
               jfixbs = ltran(1,llt)
               if (jvarbs.eq.idbus1 .and. jfixbs.eq.idbus2) then
                  tapty1 = 'A'
                  goto 340
               endif
               if (jvarbs.eq.idbus2 .and. jfixbs.eq.idbus1) then
                  tapty2 = 'A'
                  goto 340
               endif
  300       continue
         endif
C
  340    continue
C
C        Build output record buffer and output it
C
         if (tapkv1 .ge. 1000.0) then
            write (tap1,350) tapkv1
         else
            write (tap1,355) tapkv1
         endif
         if (tapkv2 .ge. 1000.0) then
            write (tap2,350) tapkv2
         else
            write (tap2,355) tapkv2
         endif
         if (buskv1 .ge. 1000.0) then
            write (bus1,350) buskv1
         else
            write (bus1,355) buskv1
         endif
  350    format(f6.1)
  355    format(f6.2)
C
         write (outbuf,360 ) txownr, zoneb1, nameb1, basb1,
     1      nameb2, basb2,    cktt,   chsect, tap1, tapty1, tap2,
     2      tapty2, bus1,  vratio,   violpu,   violkv,
     3      rating,    mvaflo
  360    format (            t6,a3, t11,a2, t15,a8, f7.1,
     1      t31,a8, f7.1, t47,a1, t52,a1, t56,a6, a1, '/', a6,
     2      a1, t75,a6, t84,f6.4, t94,f6.4, t102,f7.3,
     3      t112,f6.1, t120,f6.1)
         call prtout(1)
  380 continue
C
C     Done with loop for each overexcited tfrmr
C
C     If tfrmr_printed count is zero, then write -none found-
C
      outbuf = '0          NO OVEREXCITED TRANSFORMERS FOUND '
      lprtsw = 0
      fichsw = 0
      if (npapr .eq. 0) lprtsw = ipapsv
      if (nfich .eq. 0) fichsw = ificsv
      call prtout(1)
C
C     Restore master paper_flag and print_flag
C
      fichsw = ificsv
      lprtsw = ipapsv
C
C     Clear report headers
C
      outbuf = ' '
      do 400 la = 1, 5
         call shdlod(la)
  400 continue
C
C     End of OVEXTX
C
      end
