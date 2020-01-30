C    %W% %G%
      subroutine mdcinp
C    
C     THIS SUBROUTINE READS THE MULTITERMINAL DC TABLES FROM THE
C     POWER FLOW HISTORY FILE, READS THE MULTITERMINAL DC DATA
C     CARDS FROM THE SWING INPUT FILE, AND FORMS DATA TABLES
C     IT CALL MODINP AND GAMINP AND IS CALLED BY DCINP.

      include 'tspinc/params.inc'
      include 'tspinc/blkcom1.inc'
      include 'tspinc/cntrl2.inc'
      include 'tspinc/param.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/ecsind.inc'
      include 'tspinc/ectba.inc'
      include 'tspinc/mdctbl.inc'
      include 'tspinc/ecstbb.inc'
      include 'tspinc/pointr.inc'
      include 'tspinc/comn34.inc'
      include 'tspinc/namec.inc'
      include 'tspinc/toler.inc'
      include 'tspinc/dcpul.inc'
      include 'tspinc/dcinfo.inc'
      include 'tspinc/busnum.inc'
      include 'tspinc/busvolt.inc'
      include 'tspinc/dcard.inc'
      include 'tspinc/nsavdc.inc'
      include 'tspinc/rddtai_mv.inc'

      common /ndxktn/ndxktn(2, MAXLS)

      character*8 dcatc(200), tabc(200), mc, busc, namec, i1c, j1c, 
     &            id1, id2, iconc
      integer knvt(80), kckt(80), ktblec(10), ibrnhn(3, 10), kdca(100), 
     &        kdcb(3300)
      equivalence (dca, kdca), (dcb, kdcb)
c dlc      dimension bsbr(64), dcline(10, 50), kdclin(10, 50), dcat(maxgen),
c dlc    &          kdcat(maxgen), dcx(36, 40), kdcx(36, 40), kdcold(10),
c dlc    &          kdcnew(10)
c dlc     equivalence (dcline, kdclin), (dcat, kdcat)
      dimension bsbr(64),
     &          dcat(MAXGEN), kdcat(MAXGEN), kdcold(10), kdcnew(10)
      equivalence (dcat, kdcat)
      character*8 kdxtac(45, 10)
c dlc      dimension kdxtan(45, 10), dcdtan(45, 10)
c dlc      equivalence (kdxtan, dcdtan)
      character*1 subtyp, iblksw
c dlc
      equivalence (dcx, kdcx)
      equivalence (bsbr(1), pnet), (bsbr(2), qnet)

      pi = 3.14159265
      pisq = pi*pi
C    
C     READ DC BUS DATA KDCX AND DC LINE DATA DCLINE FROM POWER FLO
C    
c dlc      read (l3) ((kdcx(j, i), j = 1, 36), i = 1, kmdcbs)
c dlc      read (l3) ((dcline(j, i), j = 1, 10), i = 1, kmdcbr)
 
C     INITALIZE MTDC CROSS REFERENCE INDEX
 
      do i = 1, 80
        knvt(i) = 0
        kckt(i) = 0
      enddo
      do i = 1, kmdcbs
C      
C       KDCX(22,I) IS THE OPTIMAL BUS ORDER
C       KDCX(21,I) IS THE CIRCUIT ID
C      
        k = kdcx(22, i)
        knvt(k) = i
        kckt(k) = kdcx(21, i)
      enddo
C    
C     ELIMINATE ENTERIES ASSOCIATED WITH TWO TERMINAL DC BUSES
C    
      jcount = 0
      do i = 1, 80
        do while (knvt(i) .eq. 0)
          do j = i + 1, 80
            knvt(j-1) = knvt(j)
            kckt(j-1) = kckt(j)
          enddo
          knvt(80) = 0
          jcount = jcount + 1
          kckt(80) = 0
          if (jcount .ge. 80) goto 100
        enddo
  100   jcount = jcount + 1
        if (jcount .gt. 80) goto 110
      enddo
 
C     RENUMBER MTDC BUS DATA IN OPTIMAL ORDER FOR DC NETWORK REDUCTION
 
  110 ickt = 0
      ickl = 0
      do i = 1, kmdcbs
        if (kckt(i) .ne. ickl) then
          ickt = ickt + 1
          ickl = kckt(i)
        endif
        j = knvt(i)
        kdcx(21, j) = ickt
        kckt(i) = ickt
      enddo
      do i = 1, kmdcbs
        k = knvt(i)
        do j = 1, 36
          kdcbus(j, i) = kdcx(j, k)
        enddo
      enddo
 
C     NOW RENUMBER DC CIRCUIT TABLE IN OPTIMAL ORDER
 
      do i = 1, kmdcbr
        do j = 1, kmdcbs
          if (kdcbus(1, j) .eq. kdclin(1, i)) then
            kdclin(7, i) = kdcbus(21, j)
            kdclin(8, i) = j
          elseif (kdcbus(1, j) .eq. kdclin(2, i)) then
            kdclin(9, i) = j
          endif
        enddo
      enddo
C    
C     FOR DEBUG ONLY
C    
      if (keybrd(22) .ne. 0) then
        call skipln(2)
        write (outbuf, 10000)
        call prtout(1)
10000   format (' AT S3172-KDCBUS TABLE')
        do k = 1, kmdcbs
          write (outbuf, 10010) kdcbus(1, k), dcbus(2, k), (kdcbus(j,
     &     k), j = 3, 4), (dcbus(j, k), j = 5, 11)
          call prtout(1)
          write (outbuf, 10020) (dcbus(j, k), j = 12, 20), kdcbus(21,
     &     k), kdcbus(22, k)
          call prtout(1)
          write (outbuf, 10030) (dcbus(j, k), j = 23, 26), (kdcbus(j,
     &     k), j = 27, 28), dcbus(29, k), (kdcbus(j, k), j = 31, 34)
          call prtout(1)
        enddo
10010   format ('0', i10, f10.2, i10, a1, 7f10.2)
10020   format (1x, 9(f10.2, 1x), a1, 1x, i6, 1x, i6)
10030   format (1x, 4f10.2, 2i6, f10.2, 4i6)
        do k = 1, kmdcbr
          write (outbuf, 10040) (kdclin(j, k), j = 1, 2), (dcline(j, k)
     &     , j = 3, 6), (kdclin(j, k), j = 7, 9)
          call prtout(1)
        enddo
10040   format ('0', 2i10, f10.2, 3f10.2, 3i10)
      endif
      if (jdc .ne. 0) then
        if (kmdcbs .ne. 0) then
C        
C         RETAIN TOTAL COUNT OF 2 TERM CARDS
C        
C         IKTT = IKT
          ilast = 14*jdc
          if (kmdcbs .ne. 0) then
            if (kmdcbr .ne. 0) then
C            
C             SET FLAG MFLT6 = 1 IS TYPE 6, MULTITERMINAL DC LINE
C             CARDS EXIST
C            
              mflt6 = 0
              do i = 1, ifcd
                if (iabs(mflt(i)) .eq. 6) mflt6 = 1
              enddo
              dts = dt/dcitol
              imt = 0
              kckts = 0
              ir = 0
 
C             *** LOOP OVER ALL THE DC NODES FOR MULTI-
C             TERMINAL DC LOGIC ***
              do i = 1, kmdcbs
                if (kdcbus(1, i) .ge. 0) then
                  if (kdcbus(21, i) .ne. 0) then
C                   *   IBRNT WILL BE THE TOTAL OF DC BRANCHES ENCOUNTER
C                   *   BRANCH IS ENCOUNTERED TWICE, IBRNT WILL BE TWICE
C                   *   DC BRANCHES IN A MULTI DC CIRCUIT.  IBRNT WILL B
C                   *   DEFINE NDIMH
                    ibrnt = 0
                    ickt = kdcbus(21, i)
                    kckts = kckts + 1
                    ldc1 = kckts
                    if (ldc1 .ne. ickt) then
                      write (errbuf(1), 10050)
10050                 format ('0 THE CIRCUIT IDENTIFICATION ON POWER FLO
     &W DATAO FILE IS NOT IN CONSECUTIVE INCREASING ORDER.')
                      call prterr('E', 1)
                      iabort = 1
                    endif
                    kdcbus(21, i) = 0
                    ll = i + 1
                    ir = ir + 1
                    jtemp(ir) = i
 
C                   DETERMINE ALL BUSES WITH THE SAME CIRCUIT IDENTIFICA
 
                    do l = ll, kmdcbs
                      if (kdcbus(1, l) .ge. 0) then
                        if (kdcbus(21, l) .ne. 0) then
                          if (ickt .eq. kdcbus(21, l)) then
                            ir = ir + 1
                            jtemp(ir) = l
                            kdcbus(21, l) = 0
                          endif
                        endif
                      endif
                    enddo
                    if (ir .gt. 10) goto 370
                    irknt = ir
 
C                   *   CHECK FOR NUMBER OF CONVERTERS WITH CURRENT MARG
C                   *   IS NONE OR MORE THAN ONE FLAG AS ERROR AND MOVE
 
                    imarsw = 0
                    l = 0
                    il = irknt
                    do while (.true.)
                      l = l + 1
                      ko = jtemp(l)
                      iconc = char(kdcbus(4, ko))
                      if (iconc .eq. 'M') imarsw = imarsw + 1
                      if (l .ge. il) goto 120
                    enddo
  120               if (keybrd(22) .ne. 0) then
                      do jjj = 1, irknt, 10
                        kkk = min0(jjj+9, irknt)
                        write (outbuf, 10060) (jtemp(il), il = jjj,
     &                   kkk)
                        call prtout(1)
                      enddo
                    endif
10060               format ('0AT S4224 JTEMP', 10i7)
                    if (imarsw .ne. 1) then
                      if (imarsw .eq. 0) then
                        write (errbuf(1), 10070) ickt
10070                   format ('0INVERTER WITH CURRENT MARGIN WAS NOT S
     &UPPLIED IN CIRCUIT',  i2)
                        write (errbuf(2), 10071) 
10071                   format (5x, 'THE POWER FLOW MUST BE RERUN WITH O
     &NE INVERTER HAVING A TYPE M IN COLUMN 63 OF THE BD CARDS')
                        call prterr('E', 2)
                      endif
                      if (imarsw .gt. 1) then
                        write (errbuf(1), 10080) imarsw, ickt
10080                   format ('0', i3, 'INVERTERS WITH CURRENT MARGIN 
     &WERE SUPPLIED IN CIRCUIT', i2)
                        write (errbuf(2), 10081)
10081                   format (5x, 'THE POWER FLOW MUST BE RERUN WITH O
     &NLY ONE INVERTER HAVING A TYPE M IN COLUMN 63 OF THE BD CARDS.')
                        call prterr('E', 2)
                      endif
                      iabort = 1
                    endif
                    ir = 0
                    do l = 1, irknt
                      ko = jtemp(l)
                      kdcold(l) = kdcbus(1, ko)
                    enddo
                    if (kckts .eq. 1) kecs = kdc1
                    if (kckts .gt. 5) goto 380
                    if (keybrd(22) .ne. 0) then
                      do jjj = 1, irknt, 10
                        kkk = min0(jjj+9, irknt)
                        write (outbuf, 10090) (kdcold(il), il = jjj,
     &                   kkk)
                        call prtout(1)
                      enddo
                    endif
10090               format ('0AT S4296   KDCOLD TABLE', 10i6)
                    nbus = 0
                    nterm = 0
                    do k = 1, 250
                      dcb(k) = 0.0
                    enddo
                    ipas = 0
C                  
C                   LOOP OVER ALL BUSES ON THIS LINE (IRKNT) AND C
C                   NUMBER OF PASSIVE NODES (IPAS).
C                  
                    do k = 1, irknt
                      ko = jtemp(k)
                      iconc = char(kdcbus(4, ko))
                      if (iconc .eq. ' ') ipas = ipas + 1
                    enddo
                    ipnt = lbt + (irknt-ipas) + 1
                    imodsw = 0
C                  
C                   LOOP OVER ALL BUSES ON THIS LINE AND INITIALIZ
C                   DCA FOR EACH BUS AND DCB FOR EACH BRANCH
C                  
                    do ik = 1, irknt
                      kolumb = jtemp(ik)
C                    
C                     ICONC = ' ' IS A PASSIVE NODE
C                     ICONC = 'R' IS A RECTIFIER
C                     ICONC = 'I' IS AN INVERTER
C                     ICONC = 'M' IS AN INVERTER WITH CURRENT MARG
C                    
                      iconc = char(kdcbus(4, kolumb))
                      do k = 1, msizea
                        dca(k) = 0.0
                      enddo
                      k1 = kdcbus(1, kolumb)
                      k2 = kdcbus(3, kolumb)
                      if (k1 .ge. 0) then
C                      
C                       NBUS COUNTS THE NUMBER OF BUSES ON THE LIN
C                      
                        nbus = nbus + 1
C                      
C                       NO DCA TABLE IS CONSTRUCTED FOR A PASSIVE
C                      
                        if (iconc .eq. ' ') then
 
C                         ***  PASSIVE NODES ***
 
                          do in = 1, kpdc
                            if (k1 .eq. kpsdcn(1, in)) goto 130
                          enddo
                          write (errbuf(1), 10100)
10100                     format ('0 THE MULTI-DC AND KK( ) TABLES DO NO
     &T AGREE ')
                          call prterr('E', 1)
                          goto 210
  130                     namec = kpsdcc(2, in)
                          kb = kpsdcn(2, in)
                          base = basekv(kb)
                          moderi = 0
                        else
                          k1new = indx2n(k1)
                          namec = exnamc(k1new)
                          kb = ixnamn(k1new)
                          base = basekv(kb)
C                        
C                         NTERM COUNTS THE NUMBER ACTIVE TERMINALS
C                        
                          nterm = nterm + 1
                          if (iconc .eq. 'R') then
C                          
C                           RECTIFIER TERMINAL
C                          
                            moderi = 1
                            csign = 1.0
                          elseif (iconc .eq. 'I') then
C                          
C                           INVERTER TERMINAL
C                          
                            moderi = 3
                            csign =  - 1.0
                          elseif (iconc .eq. 'M') then
C                          
C                           *   INVERTER WITH CURRENT MARGIN
C                          
                            moderi = 2
                            csign =  - 1.0
                          else
                            goto 390
                          endif
C                        
C                         FIND THE INDEX IN THE SWING DATA CARD TA
C                        
                          jn = 0
                          do j = 1, jdc
                            j1 = k1save(j)
                            jn = jn + 1
                            if (j1 .eq. k1) goto 140
                          enddo
                          write (errbuf(1), 10110) namec, base, ickt
10110                     format ('0 THE DC BUS ( ', a8, f6.1,
     & ' ) ON CIRCUIT ( ', i10, ')FROM POWER FLOW DOESNT HAVE AN ASSOCIA
     &TED INPUT STABILITY CARD')
                          call prterr('E', 1)
                          goto 210
C                        
C                         DECODE MULTITERMINAL DC DATA CARD FROM S
C                        
  140                     knt = idcecn(1, jn)
                          read (dccd80(knt), 10120) subtyp, busc, base,
     &                     (dcat(itrr), itrr = 1, 6), dcatc(7), (dcat
     &                     (itrr), itrr = 8, 9), dcat(13), dcat(14),
     &                     iblksw
10120                     format (bz, 1x, a1, 1x, a8, f4.0, 5f5.4,
     &                     f5.2, a1, f4.3, f6.1, 5x, f5.4, 5x, f3.3,
     &                     4x, a1)
                          kdcat(16) = 1
                          if (subtyp .eq. 'A') kdcat(16) = 2
                          if (dcatc(7) .eq. 'P') modepc = 1
                          if (dcatc(7) .eq. 'I') modepc = 2
                          idca(94) = 0
                          if (iblksw .ne. ' ') idca(94) = 1
                          if (keybrd(22) .ne. 0) then
                            jnt = idcecn(2, jn)
                            lnt = idcecn(3, jn)
                            write (outbuf, 10130) knt, jnt, lnt
10130                       format (2x, 'AT S 3101 KNT JNT LNT ', 3i10)
                            call prtout(1)
                            write (outbuf, 10140) dccd80(knt)
10140                       format (2x, ' DCCD80(KNT) ', a80)
                            call prtout(1)
                            if (jnt .gt. 0) then
                              write (outbuf, 10150) dccd80(jnt)
C                             WRITE(OUTUBF,3103) DCCD80(JNT)
10150                         format (2x, ' DCCD80(JNT) ', a80)
                              call prtout(1)
                            endif
                            if (lnt .gt. 0) then
                              write (outbuf, 10160) dccd80(lnt)
10160                         format (2x, ' DCCD80(LNT) ', a80)
                              call prtout(1)
                            endif
                          endif
C                        
C                         SET COMMUTATING VOLTAGE TIME DELAY TO DE
C                        
                          if (dcat(13) .le. 0.0) dcat(13) = 0.01
C                        
C                         SET VOLTAGE FOR CURRENT LIMITING TO DEFA
C                        
                          if (dcat(14) .le. 0.0) dcat(14) = .25
                          kdcat(7) = modepc
C                        
C                         DECODE 'DC' CARD FOR DETAILED VDCL CHARA
C                        
                          idca(80) = 0
                          int = idcecn(4, jn)
                          if (int .ge. 1) then
                            read (dccd80(int), 10170) (dca(iv), iv =
     &                       83, 90), dca(96)
10170                       format (bz, 15x, 4f5.4, 4f5.1, f5.4)
                            dca(87) = dca(87)*frqbse
                            dca(88) = dca(88)*frqbse
                            if (dca(96) .le. 0.0) dca(96) = 0.05
                            dca(96) = dca(96)*frqbse
                            dca(95) = dca(90)
 
                            if (dca(83) .gt. dca(84)) then
                              write (errbuf(1), 10180) dccd80(int)
10180                         format (1x, a80)
                              write (errbuf(2), 10190)
10190                         format (1x, 'Y0 IS GREATER THAN Y1')
                              call prterr('E', 2)
                              iabort = 1
                            endif
 
                            if (dca(85) .gt. dca(86) .or. (dca(85) .eq.
     &                       dca(86) .and. dca(85) .ne. 0.0)) then
                              write (errbuf(1), 10200) dccd80(int)
10200                         format (1x, a80)
                              write (errbuf(2), 10210)
10210                         format (1x,
     &                         ' V1 IS GREATER THAN OR EQUAL TO V2')
                              iabort = 1
                              call prterr('E', 2)
                            endif
 
                            if (dca(89) .eq. 0.0) dca(95) = 0.0
                            if (dca(89) .ne. 0.0 .and. dca(95) .le.
     &                       0.0) dca(95) = 0.85
                            if (dca(89) .gt. dca(95)) then
                              write (errbuf(1), 10220) dccd80(int)
10220                         format (1x, a80)
                              write (errbuf(2), 10230)
10230                         format (1x,
     &                         'VC1 MUST BE LESS THAN OR EQUAL TO VC2')
                              iabort = 1
                              call prterr('E', 2)
                            endif
                            if (dca(89) .gt. 1.0) then
                              write (errbuf(1), 10240) dccd80(int)
10240                         format (1x, a80)
                              write (errbuf(2), 10250)
10250                         format (1x,
     &                         'THE SET DC VOLTAGE FOR MODE CONVERSION F
     &ROM '
     &                         ,
     &                         'CONSTANT P TO CONSTANT I IS GREATER THAN
     & 1 PU '
     &                         )
                              iabort = 1
                              call prterr('E', 2)
                            endif
 
                            if (dca(83) .lt. dca(84) .and. dca(85) .lt.
     &                       dca(86)) idca(80) = 1
                            if (dca(89) .le. 0.0 .or. dca(89) .ge. 1.0)
     &                       then
                              idca(81) = 0
                            else
                              idca(81) = 1
                            endif
                          endif
C                        
C                         * READ DATA FOR DC VOLTAGE CONTROL DCA(91) = V
C                        
                          nnt = idcecn(5, jn)
                          if (nnt .gt. 0) then
                            idv = idv + 1
                            read (dccd80(nnt), 10260) dca(91), dca(92)
10260                       format (bz, 43x, f5.4, f4.4)
                            idca(82) = 1
                          endif
C                        
C                         READ DATA FOR DC PULSE CARD IDCP = 1 GAM
C                         IDCP = 2 FOR ALPHA PULSE.  KCKTS COUNTS
C                         CIRCUITS.
C                        
                          nnt = idcecn(6, jn)
                          if (nnt .gt. 0) then
                            read (dccd80(nnt), 10270) idcp(1, kckts),
     &                       dcp(1, kckts), dcp(2, kckts), dcp(3,
     &                       kckts)
10270                       format (bz, 16x, i1, 1x, f6.2, 1x, f6.2,
     &                       1x, f4.1)
                            dcp(3, kckts) = dcp(3, kckts)*0.01745
                          endif
C                        
C                         DC MODULATION LOGIC
C                        
                          jnt = idcecn(2, jn)
                          if (jnt .gt. 0) then
                            ii2 = idcinn(2, jn)
                            ii3 = idcinn(1, jn)
                            if (moderi .eq. 1) then
                              jntsav = jnt
C                            
C                             RECTIFIER MODULATION
C                            
                              if (imodsw .eq. 0) then
                                imodsw = 1
                                imodkt = imodkt + 1
                                call modinp(imodkt, jnt, 1)
                              else
                                imodsw = imodsw + 1
                                if (imodsw .gt. 2) then
                                  write (errbuf(1), 10280)
10280                             format ('0', 5x,
     &                             'TOO MANY MODULATION CARDS FOR ONE LI
     &NE'
     &                             )
                                  write (errbuf(2), 10290) (dccard
     &                             (i4120, jnt), i4120 = 1, 8)
10290                             format (1x, 8a10)
                                  call prterr('E', 2)
                                  iabort = 1
                                  goto 150
                                else
                                  call modinp(imodkt, jnt, 2)
                                endif
                              endif
                              if (modcod .ne. 0) then
                                idca(78) = modcod
                                idca(79) = imodkt
                              endif
                            else
C                            
C                             INVERTER MODULATION
C                            
                              read (dccd80(jnt), 10300) lohi
10300                         format (bz, 53x, i1)
C                            
C                             GAMMA MODULATION
C                            
                              if (lohi .eq. 5) then
                                igam = igam + 1
                                call gaminp(igam, jnt)
                                if (modcod .ne. 0) then
                                  idca(79) = igam
                                  idca(78) = modcod
                                endif
                              elseif (ii2 .eq. ii3 .and. ii2 .gt. 0)
     &                         then
                                kntsav = jnt
                                if (imodsw .eq. 0) then
                                  imodsw = 1
                                  imodkt = imodkt + 1
                                  call modinp(imodkt, jnt, 1)
                                else
                                  imodsw = imodsw + 1
                                  if (imodsw .gt. 2) then
                                    write (errbuf(1), 10280)
                                    write (errbuf(2), 10290) (dccard
     &                               (i4120, jnt), i4120 = 1, 8)
                                    call prterr('E', 2)
                                    iabort = 1
                                    goto 150
                                  else
                                    call modinp(imodkt, jnt, 2)
                                  endif
                                endif
                                if (modcod .ne. 0) then
                                  idca(78) = modcod
                                  idca(79) = imodkt
                                endif
                              else
                                iabort = 1
                                write (errbuf(1), 10310)
                                call prterr('E', 1)
10310                           format ('0', 5x,
     &                           ' THIS MODULATION CARD NOT PERMITTED WI
     &TH INVERTER'
     &                           )
                                write (errbuf(1), 10320) (dccard(i4180,
     &                           jnt), i4180 = 1, 8)
10320                           format (1x, 8a10)
                                call prterr('E', 1)
                              endif
                            endif
                          endif
  150                     idcinn(1, jn) = 0
                          idcinn(2, jn) = 0
                          idcinn(3, jn) = 0
 
C                         FILLING DCA( ) TABLE -- LENGTH EQUAL IDCA
 
                          do k = 1, 9
                            dca(k) = dcat(k)
                          enddo
                          dca(10) = cos(dcbus(11, kolumb))
                          dca(11) = dcbus(18, kolumb)*dcbus(7, kolumb)
                          dca(12) = dcbus(17, kolumb)*dcbus(7, kolumb)
                          dca(14) = dcbus(2, kolumb)
C                        
C                         *   CHECK CURRENT MARGIN APPROPRIATENESS OF IN
C                        
                          if (moderi .ne. 1) then
                            if (moderi .eq. 3) then
C                            
C                             *   CHECK IF DATA FOR INVERTER W/O CURR MA
C                            
                              if (abs(dca(9)) .ge. .0000001) then
                                write (errbuf(1), 10330) ickt, namec,
     &                           base
                                call prterr('W', 1)
10330                           format ('0', ' in mult dc ckt no.', i5,
     &                           ' ,inverter at bus ', a8, 2x, f5.1,
     &                           '  should have zero curr margin')
                              endif
                              dca(9) = 0.0
C                            
C                             *   CHECK IF DATA FOR INV WITH CURR MARG I
C                            
                            elseif (dca(9) .le. 0.) then
                              write (errbuf(1), 10340) ickt, namec,
     &                         base
                              call prterr('W', 1)
10340                         format ('0', ' in mult dc ckt no.', i5,
     &                         ' ,inverter at bus ', a8, 2x, f5.1,
     &                         '  should have positive curr margin')
                              iabort = 1
                            endif
                          endif
                          kdca(15) = moderi
                          call redecs(bsbr, kkp(1, k1new)+kecst, kkp(2,
     &                     k1new))
                          ee1 = eyr(k1new)
                          ff1 = eyi(k1new)
                          emgs = ee1*ee1 + ff1*ff1
                          dca(16) =  - pnet/emgs
                          dca(17) = qnet/emgs
C                         CC ****
C                         CSTORE DC CONVERTER BUS NO. TO OBTAIN PROPER L
                          nsavdc(ndc) = k1new
                          ndc = ndc + 1
                          dca(18) = dcbus(16, kolumb)*dcbus(2, kolumb)
     &                     *dcbus(7, kolumb)
                          dca(19) = 1.0/(bmva)
                          dca(22) = dcbus(14, kolumb)*0.001*60.0
                          if (csign .eq. 1.0) dca(23) = dcbus(8,
     &                     kolumb)*(-2.0)
                          if (csign .eq. -1.0) dca(23) = dcbus(8,
     &                     kolumb)*(2.0)
                          kdca(24) = kdcbus(7, kolumb)
                          dca(25) = dcbus(20, kolumb)
                          dca(26) = dcbus(19, kolumb)
                          dca(27) = dcbus(19, kolumb)/dcbus(20, kolumb)
                          dca(29) = dcbus(9, kolumb)
                          dca(30) = dcbus(20, kolumb)
                          dca(31) = cos(dcbus(10, kolumb))
                          gamma1 = dcbus(12, kolumb)
                          gammao = dcbus(29, kolumb)
                          if (moderi .eq. 2 .and. gammao .ne. dcbus(13,
     &                     kolumb)) then
                            gammao = dcbus(13, kolumb)
                            write (errbuf(1), 10350) ickt
                            call prterr('W', 1)
                          endif
10350                     format (
     &                     '0  WARNING : INVERTER WITH CURRENT MARGIN',
     &                     ' IN CIRCUIT', i3,
     &                     ' NOT AT MINIMUM EXTINCTION ANGLE',
     &                     ' MINIMUM ANGLE REDEFINED')
                          dca(32) = cos(gammao)
                          dca(33) = cos(dcbus(13, kolumb))
                          dca(34) = (18./pisq)*dca(12)
                          dca(35) = (3./pi)*dca(11)
                          dca(36) = dca(35) + dca(34)
                          dca(44) = csign
C                        
C                         COMMUTATING VOLTAGE TIME DELAY
C                        
                          iloc = 0
                          dca(76) = dcat(iloc+13)*frqbse
C                        
C                         ICCDE =1 CONSTANT BETA CONTROL ICCDE = 2
C                        
                          idca(93) = kdcat(iloc+16)
C                        
C                         TERMINAL VOLTAGE CURRENT LIMITING LEVEL
C                        
                          dca(77) = dcat(14)
                          kdca(45) = indx2n(k2)
                          kdca(46) = indx2n(k1)
                          do ig = 1, irknt
                            if (k1 .eq. kdcold(ig)) kdca(47) = ig
                          enddo
                          dca(48) = (1./dca(22))
                          if (csign .eq. 1.0) dca(49) = (-1.0)*dcbus(8,
     &                     kolumb)*dca(48)
                          if (csign .eq. -1.0) dca(49) = (1.0)*dcbus(8,
     &                     kolumb)*dca(48)
                          dca(51) = 1.0/(dca(36)+2.0*dca(22)/dts)
                          dca(52) = dca(36) - 2.0*dca(22)/dts
                          kdcb(lbt+nterm) = kecs
                          call ritecs(dca(1), kecs, msizea)
                          kecs = kecs + msizea
                        endif
C                       TABLE DCA(73) HAS BEEN FORMED
C                      
C                       *   DCVOLT IS ACTUAL DC BUS VOLTS  ... BNAME IS
C                      
                        dcvolt = dcbus(20, kolumb)
                        dcd(ik+irknt) = dcvolt
                        dcd(ik) = dcvolt
                        dcbusv(ik) = dcvolt
                        dcec(ik, kckts) = namec
                        mdcen(ik) = kb
C                      
C                       DETERMINE ALL BRANCHES CONNECTED TO NEW DC BUS
C                      
                        ibrn = 0
                        do ib = 1, kmdcbr
                          j1 = kdclin(8, ib)
                          j2 = kdclin(9, ib)
                          if (kolumb .eq. j1) then
                            ibrn = ibrn + 1
                            ibrnhn(1, ibrn) = j1
                            ibrnhn(2, ibrn) = j2
                            ibrnhn(3, ibrn) = ib
                          elseif (kolumb .eq. j2) then
                            ibrn = ibrn + 1
                            ibrnhn(1, ibrn) = j2
                            ibrnhn(2, ibrn) = j1
                            ibrnhn(3, ibrn) = ib
                          endif
                        enddo
                        ibrnt = ibrnt + ibrn
C                      
C                       END OF MULTITERMINAL DC CIRCUIT LOOP ***
C                      
 
                        if (ibrn .eq. 0) then
 
                          write (errbuf(1), 10360) namec, base
                          call prterr('E', 1)
10360                     format ('0 THE DC BUS ', a10, 2x, f5.1,
     &                     2x, ' HAS NO BRANCHES.')
                        elseif (ibrn .gt. 10) then
                          write (errbuf(1), 10370) ibrn
                          call prterr('E', 1)
10370                     format ('0 YOU HAVE ', i10,
     &                     ' BRANCHES. ONLY 10 ARE ALLOWED.')
                        else
 
C                         BUBBLE UP SORT
 
                          max = ibrn
                          do while (.true.)
                            low1 = ibrnhn(1, 1)
                            low2 = ibrnhn(2, 1)
                            low3 = ibrnhn(3, 1)
                            if (max .eq. 1) goto 200
                            ksw = 1
                            do l = 2, max
                              if (low1 .ge. ibrnhn(1, l)) then
                                if (low1 .le. ibrnhn(1, l)) then
                                  if (low2 .lt. ibrnhn(2, l)) goto 160
                                  if (low2 .le. ibrnhn(2, l)) goto 180
                                endif
                                ibrnhn(1, l-1) = ibrnhn(1, l)
                                ibrnhn(2, l-1) = ibrnhn(2, l)
                                ibrnhn(3, l-1) = ibrnhn(3, l)
                                ibrnhn(1, l) = low1
                                ibrnhn(2, l) = low2
                                ibrnhn(3, l) = low3
                                ksw = 2
                                goto 170
                              endif
  160                         continue
                              low1 = ibrnhn(1, l)
                              low2 = ibrnhn(2, l)
                              low3 = ibrnhn(3, l)
  170                         continue
                            enddo
                            if (ksw .eq. 1) goto 200
                            goto 190
  180                       write (errbuf(1), 10380)
                            call prterr('E', 1)
10380                       format ('0',
     &                       'DUPLICATE MULTI-TERMINAL DC BRANCHES.')
                            iabort = 1
  190                       max = max - 1
                          enddo
C                         FILL IN TABLE DCB(  )
  200                     continue
                          do il = 1, ibrn
                            j1 = ibrnhn(1, il)
                            j2 = ibrnhn(2, il)
                            ib = ibrnhn(3, il)
                            if (il .eq. 1) then
 
                              ktmp = kdcbus(1, j1)
                              do ig = 1, irknt
                                if (ktmp .eq. kdcold(ig)) ktmp =  - ig
                              enddo
                              if (iconc .ne. ' ') ktmp = ktmp - 10
                              kdcb(ipnt) = ktmp
                              ipnt = ipnt + 1
                            endif
                            ktmp = kdcbus(1, j2)
                            do ig = 1, irknt
                              if (ktmp .eq. kdcold(ig)) kdcb(ipnt) = ig
                            enddo
                            dcb(ipnt+1) = dcline(4, ib)
                            dcb(ipnt+2) = dcline(5, ib)
                            k1l = kdclin(1, ib)
                            k1j1 = kdcbus(1, j1)
                            k1j2 = kdcbus(1, j2)
                            if (k1l .eq. k1j1 .or. k1l .eq. k1j2) then
                              dcb(ipnt+5) = (dcbus(20, j1)-dcbus(20,
     &                         j2))/dcline(4, ib)
                              dcb(ipnt+6) = dcb(ipnt+5)
                              ipnt = ipnt + 8
                            else
                              write (errbuf(1), 10390)
                              call prterr('E', 1)
10390                         format ('0',
     &                         'A MIS MATCH BETWEEN DCBUS( ) AND DCLINE(
     & ) TABLES.'
     &                         )
                              iabort = 1
                            endif
                          enddo
                          goto 220
                        endif
  210                   iabort = 1
                      endif
  220                 continue
                    enddo
                    imt = imt + 1
                    kdcm(imt) = kecs
                    nbussq = nbus*nbus
                    ndimc = ipnt - 1
                    if (ndimc .gt. msizeb) msizeb = ndimc
                    ndimx = nbussq
                    nbus2 = nbus*2
                    ndimh = nterm + ibrnt
                    kflt = 0
                    idcnt = 1
                    idcnt1 = 1
                    lsflg = 1
                    irecl = 1
                    n100 = 100
                    necsy = kecs + ndimc
                    necsv = necsy + n100
                    necse = necsv + 2*nbus
                    necsf = necse + nbus
                    necsg = necsf + 2*nterm
                    necsh = necsg + n100
                    necsj = necsh + ndimh
                    necsk = necsj + 3*(ndimh-nterm)/2 + 9*nterm
                    necsl = necsk + n100 + nbus
                    necsm = necsl + n100 + nbus
                    kecs = necsm + 10
                    do ik = 1, nterm
                      idcf(ik) = 1
                      idcf1(ik) = 1
                    enddo
                    do ik = 1, 10
                      do ikk = 1, 2
                        ibrtbl(ikk, ik) = 0
                      enddo
                    enddo
                    call ritecs(idcf(1), necsf, nterm)
                    call ritecs(idcf1(1), necsf+nterm, nterm)
                    call wecn(ibrtbl, 2, necsm, 10)
                    call ritecs(dcb(1), kdcm(imt), ndimc)
                    call ritecs(dcd(1), necsv, 2*nbus)
                    call ritecs(mdcen, necse, nbus)
                    kdcmlt = kecs
C                   *
C                   *   CHECK IF MULTI DC SWITCH BRANCHES ARE VALID DC B
C                   *   WE ARE IN THE LOOP OF PROCESSING MULTI DC CIRCUI
C                   *   BELOW IS SKIPPED IF NO MULT DC SWITCH CARDS WERE
C                   *   BRANCHES PREVIOUSLY FOUND TO BE VALID/INVALID AR
C                   *   SUCH THAT THEY ARE NOT PROCESSED AGAIN
C                   *
                    if (mflt6 .ne. 0) then
                      do if = 1, ifcd
                        if (mflt(if) .eq. 6) then
                          i1n = iftabn(if)
                          i1c = iftabc(if)
                          j1n = jftabn(if)
                          j1c = jftabc(if)
C                         *   CHECK IF THIS SWITCHING CARD WAS PREVIOUSL
                          if (i1c .ne. ' ') then
                            idsw1 = 0
                            idsw2 = 0
                            do idce = 1, nbus
C                             *   ARRAY MDCE IS EQUIVALENCED TO ARRAY DC
                              if (i1c .eq. dcec(idce, kckts) .and. i1n
     &                         .eq. mdcen(idce)) goto 230
C                             *   FIRST BUS ON THIS SWITCH CARD WAS NOT
                            enddo
                            goto 360
  230                       idsw1 = idce
C                           *   FIRST BUS MATCHED. TRY FOR SECOND IF IOP
                            ioptm = iabs(ipcdtn(if))
                            if (ioptm .ne. 2 .and. ioptm .ne. 5 .and.
     &                       ioptm .ne. 8) then
C                             *   BRANCH IS MATCHED.  UNPACK THE SIGNS
C                             *   CHECK IF BUS NAME DESIGNATION IS OK FO
                              if (ioptm .eq. 4) then
                                do lk = 1, nterm
                                  iecdca = idcb(lbt+lk)
                                  call redecs(ibus, iecdca+46, 1)
                                  if (ibus .eq. idsw1) goto 240
                                  if (lk .eq. nterm) goto 250
                                enddo
                                goto 260
  240                           call redecs(moderi, iecdca+14, 1)
                                if (moderi .eq. 1) goto 340
  250                           continue
                                base1 = basekv(i1n)
                                write (errbuf(1), 10400) i1c, base1
                                write (errbuf(2), 10410)
                                call prterr('E', 2)
10400                           format ('0',
     &                           ' error..inital2..s14874..multdc switch
     & card..option 4..bus name ( '
     &                           , a8, 2x, f5.1, ' )')
10410                           format (
     &                           ' the above bus must be a rectifier for
     & blocking/unblocking purposes.'
     &                           )
                                iabort = 1
                                goto 290
                              endif
  260                         if (ioptm .lt. 5) goto 350
                              do lk = 1, nterm
                                iecdca = idcb(lbt+lk)
                                call redecs(ibus, iecdca+46, 1)
                                if (ibus .eq. idsw1) goto 270
                                if (lk .eq. nterm) goto 280
                              enddo
                              goto 290
  270                         call redecs(moderi, iecdca+14, 1)
                              if (moderi .ne. 2) then
                                call redecs(modepc, iecdca+6, 1)
                                if (.not. (modepc .eq. 1 .and. ioptm
     &                           .eq. 7)) then
                                  if (.not. (modepc .eq. 2 .and. ioptm
     &                             .eq. 6)) then
C                                   *   SWITCH CARD MODE DOES NOT MATCH
                                    iabort = 1
                                    write (errbuf(1), 10420) i1c, base1
                                    write (errbuf(2), 10430)
                                    call prterr('E', 2)
10420                               format ('0',
     &                               'ERROR..INITAL2..S24889..MULTIDC SW
     &ITCH CARD..OPTION'
     &                               , ' 6/7..BUS (', a8, 2x, f5.1,
     &                               ' )')
10430                               format (
     &                               '0Control mode for the above valve
     &must match with the'
     &                               , ' switch card option')
                                    goto 290
                                  endif
                                endif
C                               *   FOR OPTION 4,6,OR 7 WE WILL STORE TE
                                jftabn(if) = lk
                                goto 350
                              endif
  280                         continue
                              base1 = basekv(i1n)
                              write (errbuf(1), 10440) i1c, base1
10440                         format ('0 Error..inital2..s14884..multdc 
     &switch card..option 6/7..bus ( ', a8, 2x, f5.1, ' )')
                              write (errbuf(2), 10450)
10450                         format (' The above bus must be a rectifie
     &r or an inverter w/o curr. marg. control for curr/pwr order change
     &')
                              call prterr('E', 2)
                              iabort = 1
  290                         iftabn(if) = 0
                              jftabn(if) = 0
                              iftabc(if) = ' '
                              jftabc(if) = ' '
                              goto 360
                            else
                              do idce = 1, nbus
                                if (j1c .eq. dcec(idce, kckts) .and.
     &                           j1n .eq. mdcen(idce)) goto 300
C                               *   FIRST BUS MATCHED BUT NOT THE SECOND
                              enddo
                              goto 330
  300                         idsw2 = idce
C                             *   BOTH BUSSES MATCHED.  NOW TRY FOR THE
                              ib1 = idsw1
                              jb1 = idsw2
C                             *   ARRANGE BUS NUMBERS IN INCREASING ORDE
                              if (ib1 .ge. jb1) then
                                idum1 = ib1
                                ib1 = jb1
                                jb1 = idum1
                              endif
C                             *   IPNT IS A POINTER FOR DCB TABLE. HERE
                              ipnt = lbt + nterm + 1
                              do while (.true.)
                                ib =  - idcb(ipnt)
                                if (ib .gt. 10) ib = ib - 10
C                               *   ONE BUS ON SWITCH CARD IS MATCHED WI
                                if (ib1 .eq. ib) goto 320
                                if (ib1 .le. ib) goto 330
                                ipnt = ipnt + 1
C                               *   LOOP THRU ALL NEW BUSSES UNTIL A NEW
                                do while (.true.)
                                  ipnt = ipnt + 8
                                  if (ipnt .gt. ndimc) goto 330
                                  if (idcb(ipnt) .lt. 0) goto 310
                                enddo
  310                           continue
                              enddo
  320                         ipnt = ipnt + 1
C                             *   ONE BUS MATCHED WITH A NEW BUS. NOW MA
C                             *   ADJACENT BUS
                              do while (.true.)
                                jb = idcb(ipnt)
C                               *   ADJACENT BUS MATCHED TO MAKE A BRANC
                                if (jb1 .eq. jb) goto 340
C                               *   THIS ADJACENT BUS DID NOT MATCH.  TR
C                               *   SKIP 8 PLACES TO GET THE NEXT ADJACE
                                ipnt = ipnt + 8
                                if (ipnt .gt. ndimc) goto 330
                                if (idcb(ipnt) .lt. 0) goto 330
                              enddo
C                             *   ERROR CONDITION OUTPUT
  330                         continue
                              base1 = basekv(i1n)
                              base2 = basekv(j1n)
                              write (errbuf(1), 10460) i1c, base1, j1c,
     &                         base2
                              call prterr('E', 1)
10460                         format ('0', ' dc switching branch ( ',
     &                         a8, 1x, f5.1, 5x, a8, 1x, f5.1,
     &                         ' ) is not found in any multiple dc circu
     &it....job aborted'
     &                         )
                              iabort = 1
C                             *   WHEN ERROR CONDITION, SET IFTAB,JFTAB
                              iftabn(if) = 0
                              jftabn(if) = 0
                              iftabc(if) = ' '
                              jftabc(if) = ' '
                              goto 360
                            endif
  340                       continue
                            m1 = ndxktn(1, if)
                            m2 = ndxktn(2, if)
C                           *   IF THE SIGNS ARE NEGATIVE, STORE NEGATIV
                            if (m1 .eq. -1) idsw1 =  - idsw1
                            if (m2 .eq. -1) idsw2 =  - idsw2
                            jftabn(if) = idsw2
  350                       iftabn(if) = idsw1
C                           *   STORE MULT DC CIRCUIT NUMBER
                            ndcckt(if) = ldc1
                            iftabc(if) = ' '
                            if (keybrd(22) .ne. 0) then
                              base1 = basekv(i1n)
                              base2 = 0.0
                              if (j1n .ne. 0) base2 = basekv(j1n)
                              write (outbuf, 10470) i1c, base1, j1c,
     &                         base2, ldc1
                              call prtout(1)
10470                         format ('0', ' dc switching branch ( ',
     &                         a8, 1x, f5.1, 5x, a8, 1x, f5.1,
     &                         ' ) is in multiple dc circuit no. ', i3)
                              write (outbuf, 10480)
                              call prtout(1)
                              write (outbuf, 10490) iftabn(if), jftabn
     &                         (if), mflt(if), cyc(if), angblk(if),
     &                         dpiord(if), perct(if), ndcckt(if)
                              call prtout(1)
10480                         format ('0',
     &                         ' IFTAB,JFTAB,MFLT,CYC,ANGBLK,DPORD,PERCT
     &,NDCCKT'
     &                         )
10490                         format (1x, 3i5, 4e15.5, i5)
                            endif
                          endif
                        endif
  360                   continue
                      enddo
                    endif
                  endif
                endif
              enddo
              goto 410
  370         write (errbuf(1), 10500) ickt
              call prterr('E', 1)
10500         format ('0', 'MORE THAN 10 DC BUSSES FOR CIRCUIT ', i10)
              iabort = 1
              ir = 0
              goto 420
  380         write (errbuf(1), 10510)
              call prterr('E', 1)
10510         format ('0 MORE THAN 5 DC MULTI-TERMINAL CIRCUITS, JOB WIL
     &L ABORT ')
              goto 400
  390         write (errbuf(1), 10520) iconc
              call prterr('E', 1)
10520         format ('0 THE CONVERTER CODE MUST BE BLANK, R, I, OR M.  
     &THIS CODE (', a1, ') IS NOT ALLOWED.')
  400         iabort = 1
              goto 420
            endif
          endif
  410     if (ldc1 .le. 0) then
            if (mflt6 .ne. 0) then
C             *   SET ABORT FLAG IF MULT DC SWITCH CARDS BUT NO CIRCUITS
              write (errbuf(1), 10530)
              call prterr('E', 1)
10530         format ('0',
     &         ' mult dc switching cards used but no mult dc circuits ex
     &ist  .....  job will be aborted'
     &         )
              iabort = 1
            endif
          endif
C         *
C         *   CHECK FOR LEFTOVER MULT DC SWITCH CARDS NOT ENCOUNTERED AB
C         *   AND FLAG THEM AS 'NOT FOUND'
C         *
          if (mflt6 .ne. 0) then
            do i = 1, ifcd
              if (mflt(i) .eq. 6) then
                i1n = iftabn(i)
                i1c = iftabc(i)
                j1c = jftabc(i)
                j1n = jftabn(i)
                if (i1c .ne. ' ') then
                  base1 = basekv(i1n)
                  base2 = basekv(j1n)
                  write (outbuf, 10460) i1c, base1, j1c, base2
                  call prtout(1)
                  iabort = 1
                endif
              endif
            enddo
          endif
          if (ldc1 .gt. 0) then
            if (keybrd(22) .ne. 0) then
 
C             *** DEBUG PRINTOUT ***
 
              do i = 1, ldc1
                call redecs(dcb(1), kdcm(i), msizeb)
                il = kdcb(6)
                write (outbuf, 10540) i
                call prtout(1)
10540           format ('0   AT S5100 DCB TABLE FOR CKT', i2)
                kki = lbt + kdcb(2)
                do k = 1, kki
                  write (outbuf, 10550) k, kdcb(k)
10550             format (10x, i4, 1x, i10)
                  call prtout(1)
                enddo
                do while (kki .lt. il)
                  k1 = kki + 1
                  k2 = k1
                  if (kdcb(k1) .lt. 0) k2 = k1 + 1
                  k3 = k2 + 1
                  k4 = k3 + 6
                  do k = k1, k2
                    write (outbuf, 10550) k, kdcb(k)
                    call prtout(1)
                  enddo
                  do k = k3, k4
                    write (outbuf, 10560) k, dcb(k)
10560               format (10x, i4, 1x, e10.3)
                    call prtout(1)
                  enddo
                  kki = k4
                enddo
C               811103NBS CALL REDECS (DCD, NECSV, NDIMX)
C               ---->CHANGE NDIMX TO NBUS2 IN REDECS CALL
 
                call redecs(dcd, necsv, nbus2)
                call redecs(mdcen, necse, nbus)
C               811103NBS WRITE (L6,5110) (DCD(IL),IL=1,NDIMX)
C               ---->CHANGE NDIMX TO NBUS2 IN WRITE STATEMENT
 
                write (outbuf, 10570)
                call prtout(1)
                do jjj = 1, nbus2, 10
                  kkk = min0(jjj+9, nbus2)
                  write (outbuf, 10580) (dcd(il), il = jjj, kkk)
                  call prtout(1)
                enddo
10570           format ('0DCD TABLE')
10580           format (1x, 10e13.5)
                write (outbuf, 10590)
                call prtout(1)
                do il = 1, nbus
                  write (outbuf, 10600) dcec(il, i), mdcen(il)
                  call prtout(1)
                enddo
10590           format ('0DCE TABLE')
10600           format (1x, 10(a8, i3, 2x))
                nterm = kdcb(2)
                do il = 1, nterm
                  call redecs(dca(1), kdcb(lbt+il), msizea)
                  write (outbuf, 10610) il
                  call prtout(1)
10610             format ('0 AT S5120 DCA TABLE FOR TERMINAL', i2)
                  do k = 1, 6
                    write (outbuf, 10620) k, dca(k)
                    call prtout(1)
                  enddo
                  write (outbuf, 10630) (k, kdca(k), k = 7, 7)
                  call prtout(1)
                  do k = 8, 14
                    write (outbuf, 10620) k, dca(k)
                    call prtout(1)
                  enddo
                  write (outbuf, 10630) (k, kdca(k), k = 15, 15)
                  call prtout(1)
                  do k = 16, 44
                    write (outbuf, 10620) k, dca(k)
                    call prtout(1)
                  enddo
                  do k = 45, 47
                    write (outbuf, 10630) k, kdca(k)
                    call prtout(1)
                  enddo
                  do k = 48, 77
                    write (outbuf, 10630) k, kdca(k)
                    call prtout(1)
                  enddo
                  do k = 78, 82
                    write (outbuf, 10630) k, kdca(k)
                    call prtout(1)
                  enddo
                  do k = 83, msizea
                    write (outbuf, 10620) k, dca(k)
                    call prtout(1)
                  enddo
10620             format (10x, i4, 1x, e10.3)
10630             format (10x, i4, 1x, i10)
                enddo
              enddo
            endif
          endif
          if (ldc1 .gt. 0) then
C           *
C           *   CHECK IF  MULTI DC TABLES HAVE OVERFLOWED KK TABLES IN E
C           *
            if (ktbl .lt. kdcmlt) then
              kecdif = kdcmlt - ktbl
              necdif = (kecdif+511)/512
              write (errbuf(1), 10640)
              write (errbuf(2), 10650) necdif
              call prterr('E', 2)
10640         format ('0  ERROR IN MDCINP AFTER STMT NO. 5200')
10650         format (
     &         '  multi dc tables overlap kk tables in ecs. increase job
     & card ecs request by  '
     &         , i5, ' blocks and try again')
              call erexit()
              goto 430
            endif
          endif
        endif
      endif
  420 return
  430 end
