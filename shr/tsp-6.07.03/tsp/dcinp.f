C    %W% %G%
      subroutine dcinp
  
C     THIS SUBROUTINE READS THE DC DATA CARDS AND DC TABLES  
C     FROM THE POWER FLOW AND FORMS THE INITIAL 2 TERMINAL  
C     DC DATA TABLES.  IT ALSO CALLS MDCINP TO FORM THE  
C     INITIAL MULTI TERMINAL DC DATA TABLES, MODINP AND GAMINP  
C     TO FOR DC MODULATION TABLES.  IT IS CALLED BY INITL2.  
  
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
      include 'tspinc/toler.inc'  
      include 'tspinc/dcinfo.inc'  
      include 'tspinc/busnum.inc'  
      include 'tspinc/busvolt.inc'  
      include 'tspinc/namec.inc'  
      include 'tspinc/rddtai_mv.inc'  
      include 'tspinc/dcard.inc'  
      include 'tspinc/nsavdc.inc'  

      common /ndxktn/ndxktn(2, MAXLS)  

      character*1 msw, iblksw, lmod  
      character*8 dcatc(200), tabc(200), mc, busc, namec, i1c, j1c, 
     &            id1, id2, iconc, kdxtac(45, 10)  
      integer knvt(80), kckt(80), ktblec(10), ibrnhn(3, 10), kdca(100), 
     &        kdcb(3300)  
      logical debug  
  
      dimension bsbr(64), dcat(MAXGEN), kdcat(MAXGEN), kdcold(10),   
     &          kdcnew(10)  
      equivalence (dcat, kdcat), (bsbr(1), pnet), (bsbr(2), qnet),
     &            (dca, kdca), (dcb, kdcb)  
  
C     -  Local variables  
C     ******* ADD/DEL DUE TO SIMP. DC MODEL  **************************  
  
C     -     Begin     Begin     Begin     Begin     Begin     Begin  
      debug = .false.  
      call mpost('DCINP')  
      write (outbuf, 10000)  
      call prtout(1)  
10000 format ('0SUBROUTINE DCINP ')  
      kpdc = kdc2  
      kdc1 = kigent + isg  
      kdc2 = 0  
      kdc3 = 0  
      kdc4 = 0  
      kdc5 = 0  
      kdcmlt = kdc1  
      altorn = 6.2831853/360.  
      mbrk = 45  
      idclm = idcl - 45  
      lbt = 28  
      msizea = 96  
      msizeb = 0  
      n100 = 100  
      pi = 3.14159265  
      pisq = pi*pi  
      jdcswt = 0  
  
C     IGAM COUNTS THE NUMBER OF GAMMA MODULATION CARDS  
  
      igam = 0  
  
C     IMODKT COUNTS THE NUMBER OF OTHER DC MODULATION CARDS  
  
      imodkt = 0  
  
C     READING DC INFORMATION FROM POWER FLOW TAPE  
C     JDCTOT IS THE TOTAL NUMBER OF DC DATA CARDS ENTERED  
C     IN THE SWING PROGRAM INCLUDES DS CARDS  
C     JDC IS THE NUMBER OF DC TERMINAL DATA CARDS ENTERED  
C     IN THE SWING PROGRAM  
C     LDC IS THE NUMBER OF 2 TERMINAL DC TERMINALS IN THE PF PROGRAM  
C     KMDCBS IF THE NUMBER OF MULTITERMINAL TERMINALS IN THE PF  
  
      if (.not. (ldc .eq. 0 .and. kmdcbs .eq. 0 .and. jdc .eq. 0)) then  
        if (.not. (ldc .eq. 0 .and. kmdcbs .eq. 0 .and. jdc .gt. 0))   
     &   then  
          if ((ldc .gt. 0 .or. kmdcbs .gt. 0) .and. jdc .eq. 0) then  
            write (errbuf(1), 10010)  
10010       format ('0',   
     &       'POWER FLOW DC INFORMATION EXITS, BUT NO STABILITY DATA car  
     &ds exits.'  
     &       )  
            call prterr('E', 1)  
            jdcswt = 1  
            iabort = 1  
            goto 140  
          elseif ((ldc .gt. 0 .or. kmdcbs .gt. 0) .and. jdc .gt. 0)   
     &     then  
  
C           READ DC DATA ENTERED IN STABILITY PROGRAM  
  
            if (jdc .ne. 0) then  
              kwords = jdctot*8  
  
C             DCCARD CONTAINS THE CARD IMAGES OF ALL DC DATA CARDS  
C             PRESENT IN THE STABILITY INPUT FILE  
  
              call whrec1('DC ', ilo, ihi, isz)	!D  
              ihi = ihi + 1	!D  
              if (debug) then	!D  
                call dbgeko(  
     &           'DCINP - reading DC stuff from temp file # 1')	!  
                call dbgwri('  record # = ', ihi)	!  
                !D  
              endif  
              read (l1, rec = ihi) ((dccard(i, j), i = 1, 8), j = 1,   
     &         jdctot)	!  
  
C             IDCBUC CONTAINS THE BUS NAMES OF ALL DC CONVERTERS  
C             IDCBUN CONTAINS THE BASE KVS OF ALL DC CONVERTERS  
C             IDCINN(1,NN) CONTAINS THE SECOND REMOTE BUS NUMBER  
C             ON THE DS CARD  
C             IDCINN(2,NN) CONTAINS THE BUS NUMBER OF THE FIRST REMOTE BUS  
C             ON THE MODULATION CARD (DS)  
C             IDCINN(3,NN) CONTAINS THE BUS NUMBER OF THE CONVERTER  
C             IDCECN(1,NN) CONTAINS THE INDEX TO THIS CONVERTER IN THE       G  
C             DCCARD TABLE. IDCECN(2,NN) CONTAINS THE INDEX TO THE FIRST DS  
C             CARD FOR THIS CONVERTER IN THE DCCARD TABLE.  
C             IDCECN(3,NN) CONTAINS THE INDEX TO THE SECOND DS CARD FOR THIS  
C             CONVERTER IN THE DCCARD TABLE  
C             IDCECN(4,NN) INDEX TO D(C) CARD IN DCCARD TABLE  
C             IDCECN(5,NN) INDEX TO D(V) CARD IN DCCARD TABLE  
  
              if (keybrd(22) .ne. 0) then  
                do i = 1, jdctot  
                  write (outbuf, 10020) (dccard(j, i), j = 1, 8)  
10020             format (1x, 8a10)  
                  call prtout(1)  
                enddo  
                do i = 1, jdc  
                  write (outbuf, 10030) idcbuc(i), idcbun(i), (idcinn  
     &             (j, i), j = 1, 3), (idcecn(j, i), j = 1, 5)  
10030             format ('0', a8, 9i5)  
                  call prtout(1)  
                enddo  
              endif  
  
C             CONVERT BUS NUMBERS IN SUPPLEMENTAL DC TABLE TO NEW ORDER  
  
              do i = 1, jdc  
                i3 = idcinn(1, i)  
                i2 = idcinn(2, i)  
                i1 = idcinn(3, i)  
                k1save(i) = i1  
                if (i1 .ne. 0) i1 = indx2n(i1)  
                if (i2 .ne. 0) i2 = indx2n(i2)  
                if (i3 .ne. 0) i3 = indx2n(i3)  
                idcinn(1, i) = i3  
                idcinn(2, i) = i2  
                idcinn(3, i) = i1  
              enddo  
            endif  
            if (ldc .ne. 0) then  
C               
C             CONVERT EXTERNAL BUS NUMBERS IN DC TABLE TO INTERNAL SWING  
C             NUMBER  
C               
              do i = 1, kdtot  
                ipf = kdxtan(1, i)  
                kdxtan(1, i) = indx2n(ipf)  
                ipf = kdxtan(3, i)  
                kdxtan(3, i) = indx2n(ipf)  
                ipf = kdxtan(33, i)  
                kdxtan(33, i) = indx2n(ipf)  
                ipf = kdxtan(34, i)  
                kdxtan(34, i) = indx2n(ipf)  
              enddo  
            endif  
            kecs = kdc  
            ikt = 0  
            if (ldc .ne. 0) then  
              do k = 1, kdtot  
                if (kdxtan(1, k) .lt. 10000) then  
                  iswt = 0  
                  kki = 1  
                  ii = 0  
  
C                 K7 IS 7 FOR A RECTIFIER AND = 22 FOR AN INVERTER  
  
                  k7 = 7  
                  kdsknt = 0  
                  do ll = 1, idclm  
                    tab(ll) = 0.0  
                  enddo  
                  dcdtan(20, k) = 0.0  
                  if (jdc .ne. 0) then  
                    do while (.true.)  
                      do j = 1, jdc  
                        if (idcinn(1, j)+idcinn(2, j)+idcinn(3, j) .ne.   
     &                   0) then  
                          ii3 = idcinn(1, j)  
                          ii2 = idcinn(2, j)  
                          ii1 = idcinn(3, j)  
                          if (kdxtan(kki, k) .eq. ii1) goto 100  
                        endif  
  
                      enddo  
  
                      iabort = 1  
                      if (iswt .eq. 0) then  
                        write (errbuf(1), 10040)  
                        call prterr('E', 1)  
10040                   format ('0', 'A RECTIFIER CARD IS MISSING.')  
                      else  
                        write (errbuf(1), 10050)  
                        call prterr('E', 1)  
10050                   format ('0', 'AN INVERTER CARD IS MISSING.')  
                      endif  
                      intdc = kdxtan(kki, k)  
                      write (errbuf(1), 10060) exnamc(intdc), basekv  
     &                 (ixnamn(intdc))  
                      call prterr('E', 1)  
10060                 format ('0', 'THE POWER FLOW BUS NAME WAS ', a8,   
     &                 f6.1)  
                      if (iswt .eq. 0) goto 110  
                      goto 130  
  100                 if ((ii3 .eq. ii2) .and. (ii2 .ne. 0)) kdsknt =   
     &                 kdsknt + 1  
                      knt = idcecn(1, j)  
                      jnt = idcecn(2, j)  
                      lnt = idcecn(3, j)  
  
C                     DECODE 2 TERMINAL DC DATA CARD  
  
                      read (dccd80(knt), 10070) lmod, busc, base  
10070                 format (bz, 1x, a1, 1x, a8, f4.0)  
                      if (lmod .eq. 'B') then  
  
C                       SIMPLIFIED MODEL  
  
                        read (dccd80(knt), 10080) tabc(ii+7), vsch25,   
     &                   tab(ii+8), tab(ii+9), tab(ii+1), tab(ii+2),   
     &                   tab(ii+10), msw  
10080                   format (bz, 16x, a1, 10x, f5.4, 5x, 2f5.4, 15x,   
     &                   2f5.4, f5.1, 2x, a1)  
                        ilmod = 2  
                        ectimc = 0.  
                        tab(ii+5) = 0.001  
                        tab(ii+6) = 1000.  
                      else  
  
C                       FULL DC MODEL  
  
                        ilmod = 1  
                        read (dccd80(knt), 10090) (tab(ii+i), i = 1, 6)  
     &                   , tabc(ii+7), (tab(ii+i), i = 8, 10), ectimc,   
     &                   vsch25, iblksw, msw  
10090                   format (bz, 15x, 5f5.4, f5.2, a1, f4.3, f6.1,   
     &                   f5.1, f5.4, 5x, f3.3, 4x, 2a1)  
                      endif  
                      if (k7 .eq. 7) then  
  
C                       STORE MODEL FLAG  
  
                        itab(119) = ilmod  
  
C                     COMPARE THE MODEL FLAGS  
  
                      elseif (ilmod .ne. itab(119)) then  
                        write (errbuf(1), 10100)  
10100                   format (10x,   
     &                   'MODEL FLAGS FOR REC & INV ARE DIFFERENT')  
                        call prterr('E', 1)  
                      endif  
                      if (k7 .eq. 7) ibkswr = 0  
                      if (k7 .eq. 22) ibkswi = 0  
                      if (iblksw .ne. ' ') then  
                        write (errbuf(1), 10110) busc, base  
10110                   format (5x, ' DC TERMINAL ', a8, 1x, f5.1,   
     &                   ' IS BEING BLOCKED.')  
                        call prterr('W', 1)  
                        if (k7 .eq. 7) ibkswr = 1  
                        if (k7 .eq. 22) ibkswi = 1  
                      endif  
  
C                     SET COMMUTATING VOLTAGE TIME DELAY TO DEFAULT VALUE  
  
                      if (ectimc .le. 0.0) ectimc = 0.01  
  
C                     SET VOLTAGE FOR CURRENT LIMITING TO DEFAULT VALUE  
  
                      if (vsch25 .le. 0.0) vsch25 = .25  
                      if (vsch25 .gt. 1.0) then  
                        write (errbuf(1), 10130) (dccard(ix, knt), ix =   
     &                   1, 8)  
                        write (errbuf(2), 10120)  
10120                   format ('0', 5x,   
     &                   'VLIM MUST BE LESS THAN 1.0 PU ')  
                        call prterr('E', 2)  
                        iabort = 1  
                      endif  
  
C                     IF MSU IS BLANK OR ZERO , THERE IS NO MARGIN SWITCHING  
  
                      if (msw .eq. ' ' .or. msw .eq. '0') dcdtan(20, k)   
     &                 =  - 1.0  
                      if (tab(ii+8) .eq. 0.0) then  
                        write (errbuf(1), 10130) (dccard(ix, knt), ix =   
     &                   1, 8)  
                        write (errbuf(2), 10140)  
                        call prterr('E', 2)  
10130                   format ('0', 8a10)  
10140                   format (' IMAX CAN NOT BE BLANK OR ZERO')  
                        iabort = 1  
                      endif  
                      if (k7 .eq. 22) then  
                        tab(149) = ectimc  
                        tab(147) = vsch25  
                        if ((tab(ii+9) .lt. .05) .or. (tab(ii+9) .gt.   
     &                   .3)) then  
                          tab(ii+9) = .3  
                          write (errbuf(1), 10130) (dccard(ix, knt), ix   
     &                     = 1, 8)  
                          write (errbuf(2), 10150)  
10150                     format (  
     &                     ' IMARGIN MUST BE BETWEEN .05 AND .30. PROGRA  
     &M '  
     &                     , 'HAS SET MARGIN TO 0.3')  
                          call prterr('W', 2)  
                        endif  
                      else  
                        tab(146) = vsch25  
                        tab(148) = ectimc  
                      endif  
  
C                     SET K1SAVE( ) TABLE ENTRY FOR 2TERM TO NEG.  
  
                      ikt = ikt + 1  
                      loc = 14*(ikt-1)  
                      k1save(j) =  - k1save(j)  
                      do ik = 1, 6  
                        dcat(loc+ik) = tab(ii+ik)  
                      enddo  
                      dcat(loc+8) = tab(ii+8)  
                      dcat(loc+9) = tab(ii+9)  
                      dcat(loc+11) = base  
                      kdcat(loc+12) = k  
  
C                     TABC(K7) = I MEANS CONSTANT CURRENT MODE  
C                     TABC(K7) = P MEANS CONSTANT POWER MODE  
  
                      if (tabc(k7) .eq. 'I') then  
                        itab(k7) = 2  
                        modepc = 2  
                      else  
                        itab(k7) = 1  
                        modepc = 1  
                      endif  
  
C                     ZERO OUT TABLES TO SIGNAL THAT THIS TERMINAL   
C                     HAS BEEN PROCESSED  
  
                      idcinn(1, j) = 0  
                      idcinn(2, j) = 0  
                      idcinn(3, j) = 0  
                      kdcat(loc+7) = modepc  
  
C                     ISWT = 1 IMPLIES THE INVERTER CARD IS BEING PROCESSED  
  
                      if (iswt .eq. 1) goto 120  
                      itab(34) = kdxtan(33, k)  
                      itab(35) = kdxtan(1, k)  
                      itab(36) = kdxtan(34, k)  
                      itab(37) = kdxtan(3, k)  
  
C                     BUS NAME AND BASE KV CODE  
  
                      kdxtac(44, k) = exnamc(itab(35))  
                      kdxtan(44, k) = ixnamn(itab(35))  
                      kdxtac(45, k) = exnamc(itab(37))  
                      kdxtan(45, k) = ixnamn(itab(37))  
  
C                     DECODE DS CARD (MODULATION) FOR RECTIFIER  
  
                      if (jnt .ne. 0) then  
                        imodkt = imodkt + 1  
                        moderi = 1  
                        call modinp(imodkt, jnt, 1)  
                        if (modcod .ne. 0) then  
                          itab(118) = imodkt  
                          itab(114) = modcod  
                        endif  
                      endif  
                      i1 = itab(35)  
  
C                     CALCULATE EQUIV G,B FOR DC POWERS--RECTIFIER  
  
                      i1 = itab(35)  
                      call redecs(bsbr, kkp(1, i1)+kecst, kkp(2, i1))  
                      ee1 = eyr(i1)  
                      ff1 = eyi(i1)  
                      emgs = ee1*ee1 + ff1*ff1  
                      tab(38) =  - pnet/emgs  
                      tab(39) = qnet/emgs  
  
C                     INVERTER CALCULATION OF DIAGONAL ADMITTANCE TERMS  
  
                      i3 = itab(37)  
C                     ****  
C                     *STORE DC CONVERTER BUS NO. FOR PROPER  LOAD ADMITT IN INITAL3  
                      nsavdc(ndc) = itab(35)  
                      ndc = ndc + 1  
                      nsavdc(ndc) = itab(37)  
                      ndc = ndc + 1  
                      call redecs(bsbr, kkp(1, i3)+kecst, kkp(2, i3))  
                      ee2 = eyr(i3)  
                      ff2 = eyi(i3)  
                      emgs = ee2*ee2 + ff2*ff2  
                      tab(40) =  - pnet/emgs  
                      tab(41) = qnet/emgs  
  
  110                 iswt = 1  
                      jecsav = knt  
                      kntsav = knt  
                      jntsav = jnt  
                      kki = 3  
                      ii2sav = ii2  
                      ii = 15  
                      k7 = 22  
                    enddo  
                  endif  
  
  
C                 BOTH INVERTER AND RECTIFIER CARDS HAVE BEEN READ  
  
  120             continue  
                  if (itab(7) .ne. itab(22)) then  
                    kki = 1  
                    iabort = 1  
                    write (errbuf(1), 10160)  
                    call prterr('E', 1)  
10160               format ('0',   
     &               'THE MODE ON THE RECTIFER AND INVERTER CARD DO NOT   
     &agree.'  
     &               )  
                    write (outbuf, 10170) (dccard(ix, knt), ix = 1, 8)  
                    call prtout(1)  
10170               format (1x, 8a10)  
                    write (outbuf, 10180) (dccard(ix, jecsav), ix = 1,   
     &               8)  
                    call prtout(1)  
10180               format (1x, 8a10)  
                  endif  
                  if (ibkswr .ne. ibkswi) then  
                    write (errbuf(1), 10190)  
10190               format (5x,   
     &               'THE BLOCKING SWITCHES ON THE INVERTER AND RECTIFIE  
     &r'  
     &               , ' DO NOT AGREE.')  
                    call prterr('E', 1)  
                    iabort = 1  
                    write (outbuf, 10170) (dccard(ix, knt), ix = 1, 8)  
                    call prtout(1)  
                    write (outbuf, 10180) (dccard(ix, jecsav), ix = 1,   
     &               8)  
                    call prtout(1)  
                  endif  
                  itab(129) = 0  
                  if (ibkswr .ne. 0) itab(129) = 1  
                  if (jnt .ne. 0) then  
  
C                   DECODE SECOND MODULATION CARD  
  
                    read (dccd80(jnt), 10200) lohi  
10200               format (bz, 53x, i1)  
  
C                   LOHI = 5 IMPLIES GAMMA MODULATION  
  
                    if (lohi .eq. 5) then  
                      igam = igam + 1  
                      itab(118) = igam  
                      itab(114) = 5  
  
C                     GAMINP READS GAMMA MOD CARD AND FORMS DATA TABLES  
  
                      call gaminp(igam, jnt)  
                      if (modcod .ne. 0) then  
                        itab(114) = 5  
                        itab(118) = igam  
                      endif  
                    else  
                      if (.not. ((kki .eq. 3) .and. (ii3 .eq. ii2)   
     &                 .and. (ii2 .ne. 0))) then  
                        iabort = 1  
                        write (errbuf(1), 10210)  
                        write (errbuf(2), 10220) (dccard(i2785, jnt),   
     &                   i2785 = 1, 8)  
                        call prterr('E', 2)  
10210                   format ('0',   
     &                   ' THIS DS CARD NOT PERMITTED WITH INVERTER')  
10220                   format (1x, 8a10)  
                        kki = 1  
                      endif  
  
C                     DECODE SECOND DS CARD (INVERTER DUAL FREQUENCY )  
  
                      moderi = 2  
                      call modinp(imodkt, jnt, 2)  
                      if (modcod .ne. 0) then  
                        itab(118) = imodkt  
                        itab(114) = modcod  
                      endif  
                    endif  
                  endif  
                  kki = 1  
                  call ritecs(kdxtan(1, k), kecs, 45)  
                  kecs = kecs + 45  
                  call ritecs(tab, kecs, idclm)  
                  kecs = kecs + idclm  
                endif  
  
  
  130           continue  
              enddo  
              ilast = 12*ikt  
            endif  
C             
C           COMPARING DC LINE SWITCHING AGAINST TWO TERMINAL POWER   
C           FLOW DATA  
C             
            do i = 1, ifcd  
              if (mflt(i) .eq. 5) then  
                idcsw1 = 1  
                i1 = iftabn(i)  
                j1 = jftabn(i)  
                idcsw2 = 0  
                do k = 1, kdtot  
                  if (kdxtan(1, k) .eq. i1) then  
                    if (kdxtan(3, k) .eq. j1) idcsw2 = 2  
                  endif  
                enddo  
                if (idcsw2 .ne. 2) then  
                  write (errbuf(1), 10230)  
                  call prterr('E', 1)  
10230             format ('0',   
     &             'NO DC DATA FROM POWER FLOW CORRESPONDS TO DC LINE SW  
     &itching card from input.'  
     &             )  
                  iabort = 1  
                endif  
              endif  
              idcsw1 = 0  
            enddo  
  
C           CALL MDCINP TO PROCESS MULTITERMINAL DATA IF IT EXISTS  
  
            if (kmdcbs .ne. 0) call mdcinp()  
            goto 150  
          endif  
        endif  
        write (errbuf(1), 10240)  
10240   format ('0',   
     &   'NO 2 OR 3 TERMINAL DC INFORMATION FROM POWER FLOW, BUT stabili  
     &ty data cards do exist.'  
     &   )  
        call prterr('E', 1)  
  140   iabort = 1  
      endif  
  150 return  
      end  
