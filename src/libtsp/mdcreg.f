C    %W% %G%
      subroutine mdcreg
C     
C     THIS SUBROUTINE SOLVES THE DIFFERENTIAL EQUATIONS FOR
C     FOR THE CURRENT REGULATOR MODEL USED IN THE MULTI-
C     TERMINAL DC MODEL
C     
      include 'tspinc/params.inc'
      include 'tspinc/blkcom1.inc'
      include 'tspinc/blkcom2.inc'
      include 'tspinc/cntrl2.inc'
      include 'tspinc/ecsind.inc'
      include 'tspinc/ecstbj.inc'
      include 'tspinc/ldcc.inc'
      include 'tspinc/ectba.inc'
      include 'tspinc/mdcfil.inc'
      include 'tspinc/dcpul.inc'
      include 'tspinc/mdctbl.inc'
      include 'tspinc/param.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/rk.inc'
      include 'tspinc/gamma.inc'
      include 'tspinc/dcmodd.inc'
      include 'tspinc/vrgov.inc'
      common /toler/divn, delang, twodt

      dimension drv(14)
      equivalence (erio, tab(164)), (eroo, tab(165)), (eron, tab(166)),
     &            (eiio, tab(167)), (eioo, tab(168)), (eion, tab(169)),
     &            (ectimc, tab(148))
      equivalence (tmpdy, drv)
      equivalence (tmpy(1), nsubt), (tmpy(2), nstmx), (tmpy(3), timh),
     &            (tmpy(4), timl), (tmpy(5), noith), (tmpy(6), noitl),
     &            (tmpy(7), loopdc)
      include 'tspinc/spare1.inc'
      include 'tspinc/bname.inc'
      character*8 name

      timh = 500.
      timl = 0.0
      noith = 500
      noitl = 0
      if (.not. (lppwr .gt. noith .or. 
     &           lppwr .lt. noitl .or. 
     &           to .gt. timh .or. to .lt. timl)) then
        if (.not. (nsubt .gt. nstmx .or. loopdc .gt. nstmx)) then
          if (keybrd(16) .ne. 0) then
            call skipln(3)
            write (outbuf, 10000)
            call prtout(1)
          endif
10000     format (40x, '---- SUBROUTINE MDCREG ----')
        endif
      endif
      lterm = 0
 
C     'INCM' POINTS TO THE ADDRESS OF THE DCA TABLE FOR THE TERMINAL
C     WITH CURRENT MARGIN .
 
      incm = nterm
      itdc = 1
      sumio = 0.0
      ind = 0
      nbusp1 = nbus + 1
      nbusp2 = nbus + 2
C     
C     LOOP OVER ALL THE D C CONVERTERS
C     
      do while (.true.)
        ind = ind + 1
C       
C       'LTERM' COUNTS THE NUMBER OF TERMINALS THAT HAVE BEEN
C       PROCESSED.  'IND1' POINTS TO THE STARTING ADDRESS OF THE
C       TABLE FOR THIS TERMINAL.
C       
        lterm = lterm + 1
        if (lterm .gt. nterm) goto 130
        if (lterm .eq. nterm) ind = incm
        ind1 = lbt + ind
        call redecs(dca, idcb(ind1), msizea)
C       
C       MODCOD IS A CODE FOR THE TYPE OF MODULATION BEING USED
C       MODCOD = 1 IS LOW LEVEL WITH BRANCH POWER INPUT
C       MODCOD = 2 IS LOW LEVEL WITH BRANCH CURRENT INPUT
C       MODCOD = 3 IS HIGH LEVEL WITH BRANCH POWER INPUT
C       MODCOD = 4 IS HIGH LEVEL WITH BRANCH CURRENT INPUT
C       MODCOD = 5 IS GAMMA MODULATION
C       MODCOD = 6 IS DUAL FREQUENCY MODULATION
C       
        modcod = idca(78)
        if (modcod .ne. 0) imod = idca(79)
C       
C       VOLTAGE AND CURRENT TRANSDUCER LAG BLOCKS
C       
        twotv = twodt*dctv
        tvp = twotv + 1.0
        tvm = twotv - 1.0
        twotc = twodt*dctc
        tcp = twotc + 1.0
        tcm = twotc - 1.0
        edcan = (dcstor(3, ind)*tvm+vdcnew(ibus)+vdcsto(ibus))/tvp
        if (abs(edcan) .lt. 0.0001) edcan = 0.0001
        if (dctv .eq. 0.) edcan = vdcnew(ibus)
        cdcn = currt(ind)
        fdin = (dcstor(4, ind)*tcm-csign*(cdcn+dcstor(9, ind)))/tcp
        if (dctc .eq. 0.) fdin =  - csign*cdcn
C       
C       MODERI=1 FOR RECTIFIER, MODERI=2,3 FOR INVERTER  ***
C       
        if (moderi .eq. 2) then
          if (lterm .ne. nterm) then
            incm = ind
            lterm = lterm - 1
            goto 120
          endif
        endif
C       
C       MODEPC = 1 CONSTANT POWER MODE, =2 CONSTANT CURRENT MODE
C       
        if (modepc .eq. 1) then
          cdesrd = csign*(pinit+dlpord)/edcan
          cdprim = cdesrd
C         
C         MODE CHANGE TO CONST. I FROM CONT P BELOW V CUT OFF
C         ADD HYSTERIS EFFECT SO THAT MODEL DOES NOT RETURN
C         TO CONSTANT I UNTIL VOLTAGE RISES TO 0.85 OF RATED VOLTA
C         
          if (idca(81) .gt. 0) then
            if (edcan .lt. (dca(89)*vsched)) then
              cdprim = csign*(pinit+dlpord)/(vsched)
              idca(81) = 2
            endif
            if (idca(81) .eq. 2 .and. edcan .lt. (dca(95)*vsched)) then
              cdprim = csign*(pinit+dlpord)/(vsched)
              idca(81) = 2
            else
              idca(81) = 1
            endif
C           
C           DESIRED CURRENT (CDESRD) IS FILTERED THRU A TIME CONST
C           TT
C           
            twott = dca(96)*twodt
            ttp = twott + 1.
            ttm = twott - 1.
            cdesrd = (cdprim+dcfil(4, ldcc)+dcfil(3, ldcc)*ttm)/ttp
            dcfil(1, ldcc) = cdesrd
            dcfil(2, ldcc) = cdprim
          endif
C         
C         ADD HIGH LEVEL MODULATION SIGNAL IF PRESENT
C         
          if (modcod .eq. 3 .or. modcod .eq. 4 .or. modcod .eq. 6)
     &     cdesrd = cdesrd + sighim(imod)/edcan
        endif
        if (modepc .eq. 2) then
          cdesrd =  - csign*(cinit-dliord)
C         
C         ADD HIGH LEVEL MODULATION SIGNAL IF PRESENT
C         
          if (modcod .eq. 3 .or. modcod .eq. 4 .or. modcod .eq. 6)
     &     cdesrd = cdesrd + sighim(imod)/vsched
        endif
C       
C       VOLTAGE-DEPENDENT CURRENT ORDER LIMIT
C       
        fipm = cmx
C       
C       THIS IS THE VDCOL FROM THE 'D ' CARD (SIMPLIFIED)
C       
        if (idca(80) .lt. 1) then
          fimin = 0.1*crated
          vsch25 = dca(77)*vsched
          if (edcan .lt. vsch25) fipm = fimin + (fipm-fimin)*edcan
     &     /vsch25
C         
C         PUT CURRENT ORDER THROUGH LIMITER. FIORD IS ORDERED CURR
C         
          fiord = amax1(amin1(fipm, cdesrd), fimin)
        else
C         
C         DETAILED VDCL CHARACTERISTICS FROM THE 'DC' CARD
C         
          vchv1 = dca(85)*vsched
          vchv2 = dca(86)*vsched
          fimin = dca(83)*crated
          fimy1 = dca(84)*crated
          if (edcan .lt. vchv1) then
            fipm = fimin + (fimy1-fimin)/vchv1*edcan
          elseif (edcan .ge. vchv1 .and. edcan .lt. vchv2) then
            fipm = fimy1 + (fipm-fimy1)/(vchv2-vchv1)*(edcan-vchv1)
          endif
          fiord = amax1(amin1(fipm, cdesrd), fimin)
        endif
C       
C       MODERI... 1=REC,2=INV WITH DELI,3=INV WITHOUT DELI
C       
        if (moderi .eq. 2) then
C         
C         INVERTER WITH CURRENT MARGIN
C         
          cmarg = delcm
C         
C         CURRENT AT THE INVT. WITH CURRENT MARGIN TAKES UP SLACK
C         ITS ORDER IS THE SUMMATION OF ALL OTHER TERMINAL ORDERS
C         
          fiord = sumio
        elseif (moderi .eq. 3) then
C         
C         INVERTER
C         
          cmarg = 0.0
C         
C         OMIT ADDING IORD TO SLACK TERMINAL IF INV. IS DISCONNECT
C         
          if (cdcn .ge. 1.e-4) sumio = sumio - fiord
        else
C         
C         RECTIFIER
C         
          cmarg = 0.0
C         
C         OMIT ADDING IORD TO SLACK TERMINAL IF THIS RECTFIER IS
C         DISCONNECTED .
C         
          if (cdcn .le. -1.0e-4) then
            sumio = sumio + fiord
C           
C           ADD LOW LEVEL MODULATION SIGNAL IF PRESENT
C           
            if (modcod .eq. 1 .or. modcod .eq. 2) fiord = fiord +
     &       siglom(imod)
          endif
          cosm = cosmin
          goto 100
        endif
        cosm = costop
  100   eo = sqr2*eocdn*rpib3
C       
C       COMPUTE UPPER LIMIT OF CURRENT AMPLIFIER
C       
        vc = 2.0*xc*fdin*rpib3
        dmax = eo*(cosgam+cosm)
C       
C       IF ICCDE = 1 THIS IS CONSTANT EXTINCTION ANGLE CONTROL
C       IF ICCDE = 2 THIS IS CONSTANT BETA ANGLE CONTROL
C       
        iccde = idca(93)
        if (iccde .ne. 1) then
          if (moderi .eq. 1) then
            vcord = 2.*xc*rpib3*fdin
          else
            vcord = 2.*xc*rpib3*fiord
          endif
          dmax = dmax - vcord
        endif
C       
C       CURRENT REGULATOR INTEGRATION
C       
        v1n = fdin - fiord + cmarg + bias
        tt1 = twodt*dct1
        ttp1 = tt1 + 1.0
        ttm1 = tt1 - 1.0
        tt2 = twodt*dct2
        ttp2 = tt2 + 1.0
        ttm2 = tt2 - 1.0
        tt3 = twodt*dct3
        ttp3 = tt3 + 1.0
        ttm3 = tt3 - 1.0
        v2n = (dcstor(6, ind)*ttm2+ckalph*(v1n+dcstor(5, ind)))/ttp2
        v3n = (dcstor(7, ind)*ttm3+v2n*ttp1-dcstor(6, ind)*ttm1)/ttp3
C       
C       TEST FOR LIMIT VIOLATION
C       
        if (v3n .gt. dmax) then
          v3n = dmax
        elseif (v3n .ge. 0.0) then
          goto 110
        else
          v3n = 0.0
        endif
C       
C       SET UP VARIABLES FOR LIMIT CONDITION
C       
        v2n = v3n
        v1n = v3n/ckalph
C       
C       CALCULATE COSINE ALPHA
C       
  110   valph = v3n
C       
C       IF GAMMA MODULATION IS PRESENT GET MODULATED EXTINCTION AN
C       
        cosgmx = cosgam
        if (modcod .eq. 5) cosgmx = cos(gama(imod))
C       
C       CHECK TO SEE IF A PLUSE IS BEING APPLIED TO THE EXTINCTION
C       
        if (idcp(1, ldcc) .eq. 1) then
          tnow = tsim + edt
          if (tnow .ge. dcp(1, ldcc) .and. tnow .le. dcp(2, ldcc)) then
            gam = acos(cosgam) + dcp(3, ldcc)
            cosgmx = cos(gam)
          endif
        endif
C       
C       CHECK TO SEE IF WE ARE PROCESSING THE CURRENT MARGIN TERMINAL AN
C       HENCE MUST ALTER CHARACTERISTICS FOR CONSTANT SLOPE .
C       
        fbias = 0.
C       
C       RECTIFIERS USE THE ACTURAL CURRENT TO CALULATE VCORD
C       INVERTERS USE THE ORDERED CURRENT FOR NUMERICAL STABILIY
C       
        if (iccde .ne. 2) then
          if (moderi .eq. 2) then
            if (vc .gt. v3n) valph = vc
            fiordp = eo*(cosgap-cosgmx)/(2.*rpib3*xc) + fiord
            vcord = 2.*xc*rpib3*fiordp
            if (vcord .gt. valph) valph = vcord
            fbias =  - cosgap + cosgmx
          endif
        endif
        eo1 = eo
        if (iccde .eq. 2) valph = valph + vcord
        dca(61) = valph
        if (eo1 .lt. 0.0001) eo1 = 0.0001
        cosan = valph/eo1 - cosgmx + fbias
C       
C       CONSTANT DC VOLTAGE OPERATION
C       
        if (idca(82) .gt. 0) then
          if (moderi .eq. 1) then
            cosan1 = dca(91) + fdin*(xc*rpib3+dca(92)) + twovd/2.
            cosan1 = cosan1/eo
            cosan = amin1(cosan, cosan1)
          else
            cosan1 =  - dca(91) + fdin*(xc*rpib3+dca(92)) + twovd/2.
            cosan1 = cosan1/eo
            cosan = amax1(cosan, cosan1)
          endif
        endif
C       
C       LIMIT COSINE OF FIRING ANGLE TO ABS. VALUE .LT. 1
C       
        if (cosan .lt. -1.0) cosan =  - 1.0
        if (cosan .gt. 1.0) cosan = 1.0
C       
C       CHECK FOR VALVE BLOCKING
C       
        if (idcf(ind) .eq. 2 .or. idcf(ind) .eq. 4) cosan = cosblk
        if (abs(cosan-dcstor(12, ind)) .gt. 0.0001) itdc = 2
        dcstor(12, ind) = cosan
C       
C       COMPUTE THE RIGHT-HAND-SIDE IN CASE OF ANOTHER ITERATION
C       
        if (.not. (idcf(ind) .eq. 2 .or. idcf(ind) .eq. 3)) ymtrx(ibus,
     &   nbusp1) = (biim+csign*f135*eocn*cosan)*yiop + ymtrx(ibus,
     &   nbusp1)
        call ritecs(dca(1), idcb(ind1), msizea)
C       
C       DEBUG PRINT OUT ONLY AT SELECTED INTERVALS
C       
        if (.not. (lppwr .gt. noith .or. lppwr .lt. noitl .or. to .gt.
     &   timh .or. to .lt. timl)) then
          if (.not. (nsubt .gt. nstmx .or. loopdc .gt. nstmx)) then
            if (keybrd(16) .ne. 0) then
              write (outbuf, 10010) itdc, ibus
              call prtout(1)
              write (outbuf, 10020) cosan, eocn, fiord, ymtrx(ibus,
     &         nbusp1), v1n, v2n, v3n, v4n, dmax, fdin
              call prtout(1)
            endif
10010       format (
     &       '0S667 ITDC,IBUS,COSAN,EOCN,FIORD,YMTRX,V1-4,DMAX,FDIN',
     &       2i10)
10020       format (1x, 10e13.5)
          endif
        endif
C       
C       LOOP BACK FOR ANOTHER TERMINAL
C       
  120   continue
      enddo
  130 return
      end
