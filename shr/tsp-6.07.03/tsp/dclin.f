C    %W% %G%
      subroutine dclin(iter)

C     THIS SUBROUTINE SOLVES THE TWO TERMINAL DC LINE
C     EQUATIONS AND THE AC/DC INTERFACE.  IT IS CALLED
C     BY DERIV AND CALLS SUBROUTINES GAMSOL, DCMOD,AND DCREG

      include 'tspinc/params.inc'
      include 'tspinc/blkcom1.inc'
      include 'tspinc/blkcom2.inc'
      include 'tspinc/contrl.inc'
      include 'tspinc/cntrl2.inc'
      include 'tspinc/ecsind.inc'
      include 'tspinc/ectba.inc'
      include 'tspinc/mdctbl.inc'
      include 'tspinc/dcblk.inc'
      include 'tspinc/matrow.inc'
      include 'tspinc/newton.inc'
      include 'tspinc/fltopt.inc'
      include 'tspinc/param.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/rk.inc'
      include 'tspinc/pointr.inc'
      include 'tspinc/vrgov.inc'
      include 'tspinc/gamma.inc'
      common /toler/divn, delang, twodt
      common /netwk/imax, cmax, dsum
      dimension drv(14)
      equivalence (drv, tmpdy)
      equivalence (tmpy(1), nsubt), (tmpy(2), nstmx), (tmpy(3), timh),
     &            (tmpy(4), timl), (tmpy(5), noith), (tmpy(6), noitl),
     &            (tmpy(7), loopdc)
      equivalence (itab(106), mdcl), (itab(105), mdcflt)

C     EC DELAY...

      equivalence (erio, tab(164)), (eroo, tab(165)), (eron, tab(166)),
     &            (eiio, tab(167)), (eioo, tab(168)), (eion, tab(169)),
     &            (ectim1, tab(148)), (ectim2, tab(149))
      include 'tspinc/busvolt.inc'
      include 'tspinc/bcur.inc'
      include 'tspinc/dcmodd.inc'
      include 'tspinc/bname.inc'
      include 'tspinc/buskv.inc'
      character*8 name1, name2

      save

      jecs = kdc
      do idclin = 1, ldc
C       
C       IDCBLK = 1 MEANS LINE DYNAMICS ARE BLOCKED
C       
        if (idcblk(idclin) .eq. 1) goto 250
        call redecs(ptab, jecs, idcl)
C       
C       IF ITAB(119)=2 THIS IS A SIMPLIFIED LINE MODEL SO CALL SDC
C       
        if (itab(119) .eq. 2) then
          call sdclin(iter)
          goto 240
        else
C         
C         CALL DCMOD TO CALCULATE HIGH LEVEL, LOW LEVEL, OR DUAL
C         FREQUENCY MODULATION SIGNAL
C         
          imdp = itab(114)
          if (imdp .gt. 0 .and. imdp .ne. 5) then
            idcmod = itab(118)
            call dcmod(idcmod, imdp, iter)
          endif
          if (iter .ne. 1 .or. lppwr .ne. 0) then
C           
C           GET COMMUTATION VOLTAGE
C           
            n10 = 10
            n100 = 100
            n110 = 110
            n120 = 120
            ieo1 = itab(34)
            evr1 = eyr(ieo1)
            evi1 = eyi(ieo1)
            ecsq1 = evr1*evr1 + evi1*evi1
            eco1 = sqrt(ecsq1)*tab(99)
            if (eco1 .eq. 0.0) eco1 = 0.0001

C           TEST FOR ZERO VALVESIDE VOLT

            ivo1 = itab(35)
            ev1 = eyr(ivo1)
            fv1 = eyi(ivo1)
            evsq1 = ev1*ev1 + fv1*fv1
            if (evsq1 .lt. 0.0001) eco1 = 0.0001
            tab(68) = eco1
            ieo2 = itab(36)
            evr2 = eyr(ieo2)
            evi2 = eyi(ieo2)
            ecsq2 = evr2*evr2 + evi2*evi2
            eco2 = sqrt(ecsq2)*tab(100)
            if (eco2 .eq. 0.0) eco2 = 0.0001

C           TEST FOR ZERO VALVESIE VOLT

            ivo2 = itab(37)
            ev2 = eyr(ivo2)
            fv2 = eyi(ivo2)
            evsq2 = ev2*ev2 + fv2*fv2
            if (evsq2 .lt. 0.0001) eco2 = 0.0001
            tab(69) = eco2
C           
C           AT A DISCONTINUITY JUMP TO AC/DC INTERFACE EQUATIONS
C           
            if (idsw .eq. 7) then
C             
C             UPDATE LINE CURRENT PARAMETERS AT A DISCONTINUITY
C             
              cc1 = tab(87)
              cc2 = tab(157)
              ec1 = f135*eco1
              dco1 = ec1*tab(95) - cc1*rpib3*tab(11)
              ec2 = f135*eco2
              dco2 =  - ec2*tab(96) + rpib3*cc2*tab(26)
              es1 = dco1 - cc1*rpib18*tab(12) - ptab(13)
              tab(85) = es1
              dcsto(13) = es1
              es2 = dco2 + cc2*rpib18*tab(27) + ptab(16)
              tab(86) = es2
              dcsto(14) = es2
C             
C             PTAB(27) = LSR ; PTAB(28) = LSI TAB(13) = LR TAB(28
C             TOTL IS THE  TOTAL LINE INDUCTANCE
C             
              totl = ptab(27) + tab(13) + tab(28) + ptab(28)
              if (totl .le. .00001) totl = 1.
              totlr = 1.0/totl
C             
C             TAB(14) + TAB(15) IS THE TOTAL LINE RESISTANCE
C             DES12 IS THE VOLTAGE DIFFERENCE ACROSS THE LINE FOLLOWING
C             A DISCONTINUITY.
C             
              des12 = es1 - (tab(14)+tab(29))*cc1 - es2
              tab(110) = es1 - des12*ptab(27)*totlr
              tab(111) = es2 + des12*ptab(28)*totlr
              dcsto(18) = tab(110)
              dcsto(19) = tab(111)
              if (imdp .eq. 5) then
                igam = itab(118)
                if (igamrb(igam) .eq. 0) then
                  vaco1(igam) = sqrt(ecsq2)
                else
                  ig = igamrb(igam)
                  vaco1(igam) = sqrt(eyr(ig)*eyr(ig)+eyi(ig)*eyi(ig))
                endif
                if (igamdc(igam) .eq. 1) vaco1(igam) = tab(111)
              endif
              goto 210
            else
C             
C             IF THIS IS THE FIRST ITERATION OF A TIME STEP. SET U
C             MARGIN SWITCHING UNIT
C             
              if (lppwr .eq. 1) then
C               
C               BYPASS MARGIN SW LOGIC IF PTAB(36)=-1.0
C               
                if (ptab(36) .ne. -1.0) then
C                 
C                 CHECK MSU TIMER
C                 
                  if (ptab(36) .eq. 0.0) then
C                   
C                   SET INITIAL MSU TIMER PTAB(37) = 15 CYCLES
C                   
                    ptab(36) = ptab(37)
                  else
C                   
C                   DECREMENT TIMER
C                   
                    ptab(36) = ptab(36) - ddt2
                    if (ptab(36) .le. 0.001) then
C                     
C                     RESET MSU TIMER PTAB(38) = 30 CYCLES
C                     
                      ptab(36) = ptab(38)
C                     
C                     TAB(51) IS PAST VALUE OF THE MARGIN SWITCHIN
C                     FOR THE RECTIFIER
C                     TAB(71) IS PRESENT VALUE OF THE MARGIN SWITC
C                     FOR THE RECTIFIER
C                     TAB(74) IS THE MEASURED CURRENT AT THE RECTI
C                     TAB(88) IS THE RECTIFIER IMAX
C                     TAB(50) IS THE MARGIN CONSTANT 72 AMPS
C                     TAB(107)IS THE DESIRED CURRENT AT THE RECTIF
C                     TAB(9) IS THE CURRENT MARGIN
C                     
                      if (tab(51) .eq. 0.0) then
                        if (tab(74) .lt. tab(88)-tab(9)) then
                          if (tab(74) .lt. tab(107)-0.75*tab(9)) then
                            name1 = bname(itab(35))
                            dckv = buskv(itab(35))
                            tim = tsim + edt
                            write (errbuf(1), 10000) name1, dckv, tim
10000                       format ('0', 5x,
     &                       'MARGIN SWITCHING TURNED ON AT ', a8, 1x,
     &                       f5.1, ' AT ', f7.2, ' CYCLES.')
                            call prterr('W', 1)
                            goto 100
                          endif
                        endif
                      elseif (tab(74) .le. tab(88)+tab(50)) then
                        if (tab(74) .le. tab(107)+0.75*tab(9)) goto 100
                      endif
                      tab(71) = 0.0
                      if (tab(51) .ne. 0.0) then
                        name1 = bname(itab(35))
                        dckv = buskv(itab(35))
                        tim = tsim + edt
                        write (errbuf(1), 10010) name1, dckv, tim
10010                   format ('0', 5x,
     &                   'MARGIN SWITCHING TURNED OFF AT ', a8, 1x,
     &                   f5.1, ' AT ', f7.2, ' CYCLES.')
                        call prterr('W', 1)
                      endif
                      goto 110
  100                 tab(71) = tab(9)
  110                 if (tab(52) .eq. 0.0) then
                        if (tab(80) .lt. tab(89)-tab(24)) then
                          if (tab(80) .lt. tab(108)-0.75*tab(24)) then
                            name1 = bname(itab(37))
                            dckv = buskv(itab(37))
                            tim = tsim + edt
                            write (errbuf(1), 10000) name1, dckv, tim
                            call prterr('W', 1)
                            goto 120
                          endif
                        endif
                      elseif (tab(80) .le. tab(89)+tab(70)) then
                        if (tab(80) .le. tab(108)+0.75*tab(24)) goto
     &                   120
                      endif
                      tab(72) = 0.0
                      if (tab(52) .ne. 0.0) then
                        name1 = bname(itab(37))
                        dckv = buskv(itab(37))
                        tim = tsim + edt
                        write (errbuf(1), 10010) name1, dckv, tim
                        call prterr('W', 1)
                      endif
                      goto 130
  120                 tab(72) = tab(24)
                    endif
                  endif
                endif

C               PRECESS VARIABLES

  130           continue
                do i = 1, 17
                  tab(50+i) = tab(70+i)
                enddo
                delm = edt/divn
                tab(112) = tab(110)
                tab(113) = tab(111)
                tab(97) = tab(95)
                tab(98) = tab(96)
                tab(156) = tab(157)
                eroo = eron
                eioo = eion
              endif
C             
C             STORE PRESENT DC INFO
C             
              do i = 1, 15
                dcsto(i) = tab(52+i)
              enddo
              dcsto(16) = tab(92)
              dcsto(17) = tab(94)
              erio = tab(92)
              eiio = tab(94)
              dcsto(18) = tab(112)
              dcsto(19) = tab(113)
              dcsto(20) = tab(97)
              dcsto(21) = tab(98)
              cosa1 = tab(97)
              cos1 = tab(97)
              cosa2 = tab(98)
              cos2 = tab(98)
              dcsto(27) = tab(156)
              dcsto(29) = eroo
              dcsto(30) = eioo
              difvr = tab(68) - tab(92)
              difvi = tab(69) - tab(94)
              totdel = delm
              imdp = itab(114)
C             
C             IF IMDP = 5 THEN GAMMA MODULATION IS PRESENT CALL GA
C             SOLVE GAMMA DIFFERENTIAL EQUATIONS
C             
              if (imdp .eq. 5) then
                igam = itab(118)
                if (igamrb(igam) .eq. 0) then
                  vac(igam) = sqrt(ecsq2)
                else
                  ig = igamrb(igam)
                  vac(igam) = sqrt(eyr(ig)*eyr(ig)+eyi(ig)*eyi(ig))
                endif
                if (igamdc(igam) .eq. 1) vac(igam) = tab(111)
                call gamsol(igam)
              endif
            endif
          else
C           
C           INITIALIZE DELTA CURR. STORAGE...
C           
            ptab(29) = 0.0
            ptab(30) = 0.0
            cmax = 10.0
            if (idsw .ne. 7) then
              tab(92) = tab(68)
              tab(94) = tab(69)
            endif
            goto 240
          endif
        endif
C
C       ****************************************************************
C       START OF ITERATIVE LOOP-- AC COMMUTATION VOLTAGES, DC TERMINAL
C       VOLTAGES, AND DC LINE CURRENT FOR THE SUB TIME STEP ARE CALCULAT
C       FIRST.  THEN DCREG IS CALLED TO CALCULATE THE ESTIMATE OF
C       FIRING ANGLE
C       FIRST SET UP FOR DC LINE CURRENT CALCULATION
C       ****************************************************************

        do while (.true.)
          loopdc = 0
          do while (.true.)
            iiswn = 1
            loopdc = loopdc + 1
 
C           IF ITERATIVE LOOP HAS BEEN EXHAUSTED, EXIT
 
            if (loopdc .gt. 20) goto 190
            ce1 = f135
            ce2 = f135
            twodt = 2.0/delm
            fmpy = totdel/edt
 
C           DCSTO(16) AND ECRIN ARE THE AC COMMUTATING VOLTAGES FOR THIS
C           STEP FOR THE RECTIFIER
C           DCSTO(17) AND ECIIN ARE THE AC COMMUTATING VOLTAGES FOR THIS
C           STEP FOR THE INVERTER
 
            dcsto(16) = tab(92) + fmpy*difvr
            dcsto(17) = tab(94) + fmpy*difvi
            ecrin = tab(92) + fmpy*difvr
            eciin = tab(94) + fmpy*difvi
 
C           CTI = RCR +XCR COMMUTATING IMPEDANCE RECTIFIER
C           CT2 = RCI + XCI COMMUTATING IMPEDANCE INVERTER
 
            ct1 = rpib3*tab(11) + rpib18*tab(12)
            ct2 = rpib3*tab(26) + rpib18*tab(27)
            do while (.true.)
              if (iblck .eq. 2) cosa1 = ablck
              if (mdcflt .ne. 1) goto 150
              rldt = (tab(13)+tab(28)+ptab(27)+ptab(28))*twodt
 
C             CC1 = DC LINE CURRENT FOR THIS SUB TIME STEP
 
              cc1 = (dcsto(13)-dcsto(14)-(ptab(8)-rldt)*dcsto(15)+ce1
     &         *dcsto(16)*cosa1-ptab(13)+ce2*dcsto(17)*cosa2-ptab(16))/
     &         (rldt+ptab(8)+ct1+ct2)
              if (cc1 .lt. 0.0) cc1 = 0.0
              cc2 = cc1
              tab(87) = cc1
              tab(157) = cc1
  140         if (iiswn .eq. 2) goto 170
 
C             TEST FOR ABNORMAL CONDITION
 
              term1 = f135*dcsto(16)
              term2 = term1*cosa1 - cc1*rpib3*tab(11)
              if (term2 .lt. -term1) then
                term2 =  - term1
                cosa1 = 1.0
                ce1 =  - ce1
                ct1 = rpib18*tab(12)
                iiswn = 2
              endif
              term3 = f135*dcsto(17)
              term4 =  - term3*cosa2 + cc2*rpib3*tab(26)
              if (term4 .gt. term3) then
                term4 = term3
                cosa2 = 1.0
                ce2 =  - ce2
                ct2 = rpib18*tab(27)
                iiswn = 2
              endif
              if (iiswn .eq. 1) goto 170
              if (iiswn .eq. 2) goto 160
 
C             SOLVE SHORTCKT EQNS
 
  150         rldt1 = twodt*coilr
              cc1 = (dcsto(13)-(resr-rldt1)*dcsto(15)+ce1*dcsto(16)
     &         *cosa1-ptab(13))/(rldt1+resr+ct1)
              if (cc1 .le. 0.0) cc1 = 0.0
              tab(87) = cc1
              rldt2 = twodt*coili
              cc2 = (-dcsto(14)-(resi-rldt2)*dcsto(27)+ce2*dcsto(17)
     &         *cosa2-ptab(16))/(rldt2+resi+ct2)
              if (cc2 .le. 0.0) cc2 = 0.0
              cninv = cc2
              goto 140
  160         continue
            enddo
 
C           ES1 IS THE DC VOLTAGE AT THE SMOOTHING REACTOR OF RECTIFIER
C           ES2 IS THE DC VOLTAGE ATE THE SMOOTHING REACTOR OF INVERTER
 
  170       es1 = term2 - cc1*rpib18*tab(12) - ptab(13)
            tab(85) = es1
 
C           EDC1 IS THE DC TERMINAL VOLTAGE OF RECTIFIER
C           EDC2 IS THE DC TERMINAL VOLTAGE OF INVERTER
 
            edc1 = es1 + dcsto(13) - twodt*ptab(27)*(cc1-dcsto(15)) -
     &       dcsto(18)
            tab(110) = edc1
            es2 = term4 + cc2*rpib18*tab(27) + ptab(16)
            tab(86) = es2
            edc2 = es2 + dcsto(14) + twodt*ptab(28)*(cc2-dcsto(27)) -
     &       dcsto(19)
            tab(111) = edc2
 
C           LOGIC TO REPRESENT DC CURRENT AND VOLTAGE MEASURING DEVICES
 
            twotv1 = twodt*tab(2)
            tvp1 = twotv1 + 1.0
            tvm1 = twotv1 - 1.0
            twotv2 = twodt*tab(17)
            tvp2 = twotv2 + 1.0
            tvm2 = twotv2 - 1.0
            twotc1 = twodt*tab(1)
            tcp1 = twotc1 + 1.0
            tcm1 = twotc1 - 1.0
            twotc2 = twodt*tab(16)
            tcp2 = twotc2 + 1.0
            tcm2 = twotc2 - 1.0
 
C           TAB(73) IS THE OUTPUT OF THE DC VOLTAGE MEASURING DEVICE REC
C           TAB(79) IS THE OUTPUT OF THE DC VOLTAGE MEASURING DEVICE INV
 
            tab(73) = (dcsto(1)*tvm1+edc1+dcsto(18))/tvp1
            if (tab(2) .eq. 0.0) tab(73) = edc1
            tab(79) = (dcsto(7)*tvm2+edc2+dcsto(19))/tvp2
            if (tab(17) .eq. 0.0) tab(79) = edc2
 
C           TAB(74) IS THE OUTPUT OF THE DC CURRENT MEASURING DEVICE REC
C           TAB(80) IS THE OUTPUT OF THE DC CURRENT MEASURING DEVICE INV
 
            tab(74) = (dcsto(2)*tcm1+cc1+dcsto(15))/tcp1
            if (tab(1) .eq. 0.0) tab(74) = cc1
            tab(80) = (dcsto(8)*tcm2+cc2+dcsto(27))/tcp2
            if (tab(16) .eq. 0.0) tab(80) = cc2
 
C           COMMUTATION DELAY LOGIC
 
            twoec1 = twodt*ectim1
            ecpls1 = twoec1 + 1.0
            ecmns1 = twoec1 - 1.0
            twoec2 = twodt*ectim2
            ecpls2 = twoec2 + 1.0
            ecmns2 = twoec2 - 1.0
 
C           ERON IS THE DELAYED COMMUTATION VOLTAGE RECTIFIER
C           EION IS THE DELAYED COMMUTATION VOLTAGE INVERTER
 
            eron = (ecrin+erio+ecmns1*dcsto(29))/ecpls1
            eion = (eciin+eiio+ecmns2*dcsto(30))/ecpls2
 
C           CALL DCREG TO CALCULATE A NEW ESTIMATE OF FIRING ANGLE USING
C           MOST RECENT AC AND DC QUANTITIES
 
            call dcreg()
            if (iblck .eq. 2) tab(95) = ablck
 
C           TEST FOR DC FIRING ANGLE CONVERGENCE
 
            difang = abs(cos1-tab(95)) + abs(cos2-tab(96))
            cosa1 = tab(95)
            cos1 = tab(95)
            cosa2 = tab(96)
            cos2 = tab(96)
 
C           IF COS ESTIMATES ARE WITHIN TOLERANCE, GO TO NEXT SUB TIME S
C           ELSE, CONTINUE TO ITERATE
 
            if (difang .lt. 0.0001) goto 180
          enddo
 
C         SETUP FOR NEXT SUB-TIMESTEP
C         IF SUB TIMESTEPS ARE COMPLETE GO TO AC/DC INTERFACE
 
  180     if (abs(totdel-edt) .lt. 1.0e-4) goto 200
          totdel = totdel + delm
          dcsto(18) = tab(110)
          dcsto(19) = tab(111)
          dcsto(20) = tab(95)
          dcsto(21) = tab(96)
          dcsto(27) = tab(157)
          do i = 1, 15
            dcsto(i) = tab(72+i)
          enddo
          dcsto(29) = eron
          dcsto(30) = eion
          erio = ecrin
          eiio = eciin
        enddo
  190   name1 = bname(ivo1)
        name2 = bname(ivo2)
        write (errbuf(1), 10020) name1, name2
10020   format ('0TWO TERM DC LINE ', a8, 1x, a8, 
     &          ' CANNOT SOLVE IN DCSUB')
        call prterr('W', 1)
        link = 13
        goto 230
C       
C       AC/DC INTERFACE EQUATIONS
C       
  200   cc1 = tab(87)
        cc2 = tab(157)
        ec1 = f135*eco1
        dco1 = ec1*tab(95) - rpib3*cc1*tab(11)
        ec2 = f135*eco2
        dco2 =  - ec2*tab(96) + rpib3*cc2*tab(26)
  210   dco11 = dco1 - rpib18*cc1*tab(12)
        dco22 = dco2 + rpib18*cc2*tab(27)
        arg1 = dco1/ec1
        if (abs(arg1) .gt. 1.0) arg1 = 1.0
        fe1 = acos(arg1)
C       
C       CALCULATE EXTINCTION ANGLE FOR OUTPUT
C       
        if (idsw .ne. 7) then
          gama1 = tab(95) - (sqr2*tab(11)*tab(87))/tab(68)
          if (gama1 .gt. 1.0) gama1 = 1.
          if (gama1 .lt. -1.0) gama1 =  - 1.
          tab(121) = acos(gama1)
        endif

C       YISOLN, NEWTON OPTION

        if (inewts .ne. 1) then
          if (mdeyoj .eq. 2) then

C           NEWTON ADDITION

            tab(46) =  - dco11*cc1*tab(101)
            tab(47) =  - (dco1*tan(fe1)-rpib18*tab(11)*cc1)*cc1*tab
     &       (101)
            i1 = itab(35)
            if (evsq1 .gt. 0.0001) then

C             FORM CURR. VECTOR + DIAGONAL ADDITION FOR NEWTON METHOD

              tab(42) = 2.*(tab(46)*ev1+tab(47)*fv1)/evsq1
              tab(43) = 2.*(tab(46)*fv1-tab(47)*ev1)/evsq1
              edif = ev1*ev1 - fv1*fv1
              esq = evsq1*evsq1
              gnewt(2*i1-1) = (tab(46)*edif+2.*tab(47)*ev1*fv1)/esq
              gnewt(2*i1) =  - gnewt(2*i1-1)
              bnewt(2*i1-1) = (-tab(47)*edif+2.*tab(46)*ev1*fv1)/esq
              bnewt(2*i1) = bnewt(2*i1-1)
            else
              tab(42) = 0.0
              tab(43) = 0.0
              gnewt(2*i1-1) = tab(38)
              gnewt(2*i1) = tab(38)
              bnewt(2*i1-1) =  - tab(39)
              bnewt(2*i1) = tab(39)
            endif
            arg2 = dco2/ec2
            if (abs(arg2) .gt. 1.0) arg2 = 1.0
            fe2 = acos(arg2)
            tab(48) = dco22*cc2*tab(101)
            tab(49) =  - (dco2*tan(fe2)-rpib18*tab(26)*cc2)*cc2*tab
     &       (101)
            i1 = itab(37)
            if (evsq2 .gt. 0.0001) then
              tab(44) = 2.*(tab(48)*ev2+tab(49)*fv2)/evsq2
              tab(45) = 2.*(tab(48)*fv2-tab(49)*ev2)/evsq2
              edif = ev2*ev2 - fv2*fv2
              esq = evsq2*evsq2
              gnewt(2*i1-1) = (tab(48)*edif+2.*tab(49)*ev2*fv2)/esq
              gnewt(2*i1) =  - gnewt(2*i1-1)
              bnewt(2*i1-1) = (-tab(49)*edif+2.*tab(48)*ev2*fv2)/esq
              bnewt(2*i1) = bnewt(2*i1-1)
            else
              tab(44) = 0.0
              tab(45) = 0.0
              gnewt(2*i1-1) = tab(40)
              gnewt(2*i1) = tab(40)
              bnewt(2*i1-1) =  - tab(41)
              bnewt(2*i1) = tab(41)
            endif
            goto 220
          endif
        endif
        tab(46) =  - dco11*cc1*tab(101)
        tab(47) =  - (dco1*tan(fe1)-rpib18*tab(11)*cc1)*cc1*tab(101)
        if (evsq1 .le. 0.0001) then
          tab(42) = 0.0
          tab(43) = 0.0
        else
          gnet1 = tab(46)/evsq1 + tab(38)
          bnet1 =  - tab(47)/evsq1 + tab(39)
          tab(42) = ev1*gnet1 - fv1*bnet1
          tab(43) = ev1*bnet1 + fv1*gnet1
        endif
        arg2 = dco2/ec2
        if (abs(arg2) .gt. 1.0) arg2 = 1.0
        fe2 = acos(arg2)
        tab(48) = dco22*cc2*tab(101)
        tab(49) =  - (dco2*tan(fe2)-rpib18*tab(26)*cc2)*cc2*tab(101)
        if (evsq2 .le. 0.0001) then
          tab(44) = 0.0
          tab(45) = 0.0
        else
          gnet2 = tab(48)/evsq2 + tab(40)
          bnet2 =  - tab(49)/evsq2 + tab(41)
          tab(44) = ev2*gnet2 - fv2*bnet2
          tab(45) = ev2*bnet2 + fv2*gnet2
        endif
  220   if (keybrd(22) .ne. 0) then
C         
C         DEBUG OUTPUT
C         
          write (outbuf, 10030) iptab(37), iptab(38), iptab(40), 
     &                          iptab(41)
10030     format ('0IPTAB(37,38,40,41),PTAB(1,,45)=', 4i5)
          call prtout(1)
          do jjj = 1, 45, 8
            kkk = min0(jjj+7, 45)
            write (outbuf, 10040) (i, ptab(i), i = jjj, kkk)
10040       format (1x, 8(i3, e13.4))
            call prtout(1)
          enddo
          write (outbuf, 10050) itab(22), itab(33), itab(105), 
     &                          itab(106)
10050     format ('0ITAB(22,33,105,106),TAB(1,...,177=', 4i5)
          call prtout(1)
          do jjj = 1, 177, 8
            kkk = min0(jjj+7, 177)
            write (outbuf, 10060) (i, tab(i), i = jjj, kkk)
10060       format (1x, 8(i3, e13.4))
            call prtout(1)
          enddo
          write (outbuf, 10070) loopdc
10070     format (1x, 'LOOPDC = ', i5)
          call prtout(1)

C         DEBUG OUTPUT FOR NEW DC CONTROL

          if (itab(114) .eq. 6) then
            write (outbuf, 10080) itab(114), itab(119), itab(120)
10080       format ('0ITAB(114,119,120)  TAB(178...205)=', 3i5)
            call prtout(1)
            do jjj = 178, 205, 8
              kkk = min0(jjj+7, 205)
              write (outbuf, 10090) (i, tab(i), i = jjj, kkk)
10090         format (1x, 8(i3, e13.4))
              call prtout(1)
            enddo
            write (outbuf, 10100) itab(143), itab(144), itab(146)
            call prtout(1)
10100       format (' ITR1,ITHR2,IDO', 3i6)
          endif
        endif
  230   if (link .gt. 10) goto 260
        j = itab(35)
        bcurr(j) = tab(42)
        bcuri(j) = tab(43)
        sumcr1 = abs(tab(42)) + abs(tab(43))
        cdel1 = abs(ptab(29)-sumcr1)
        ptab(29) = sumcr1
        dsum = dsum + cdel1
        if (cmax .le. cdel1) then
          imax = j
          cmax = cdel1
        endif
        j = itab(37)
        bcurr(j) = tab(44)
        bcuri(j) = tab(45)
        sumcr2 = abs(tab(44)) + abs(tab(45))
        cdel2 = abs(ptab(30)-sumcr2)
        ptab(30) = sumcr2
        dsum = dsum + cdel2
        if (cmax .le. cdel2) then
          imax = j
          cmax = cdel2
        endif
  240   call ritecs(ptab, jecs, idcl)
  250   jecs = jecs + idcl
      enddo
  260 return
      end
