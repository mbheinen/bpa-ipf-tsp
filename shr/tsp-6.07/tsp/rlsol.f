C    %W% %G%
      subroutine rlsol
C    
C     THIS SUBROUTINE CONTAINS SOLUTION LOGIC FOR THE
C     DEFAULT DISTANCE RELAY.  IT IS CALLED BY RELAY
C     IT CALLS TRPHIS AND LLDROP
C    
      include 'tspinc/params.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/vrgov.inc'
      include 'tspinc/busvolt.inc'
      include 'tspinc/bname.inc'
      include 'tspinc/buskv.inc'
      include 'tspinc/relays.inc'
      include 'tspinc/param.inc'
      include 'tspinc/matrow.inc'
      include 'tspinc/ldropn.inc'
      include 'tspinc/bypass.inc'

      dimension dfltd(30000), idflt(30000)
      equivalence (store, dfltd, idflt)
      character idpar * 1
C    
C      DEFINE CONSTANTS FOR DEFAULT DISTANCE RELAY LOGIC
C      TAN60=TAN(60 DEG), A1=(.85/2)COS(75 DEG),
C      B1=(.85/2)SIN(75 DEG),
C    
      data a1/0.1099980938/
      data b1/0.4105184763/
      data a2/0.1617619026/
      data b2/0.6037036416/
      data tan60/1.732050808/

      index = 0
      ij = 1
      lgnth = ndfltd
      nglth = lgnth
C    
C     TEST FOR FIRST TEN CYCLE BYPASS
C    
      if (mrelay .eq. 1) then
        if (to .le. 10.0) goto 240
      endif
      do while (lgnth .ne. 0)
        if (ij .eq. 2) then
          indexo = index
          index = index + nline*4
          ij = 2
          if (index .gt. nglth) goto 230
        elseif (ij .eq. 3) then
          goto 110
        endif
  100   indexo = index
        index = index + 3
        ij = 1
        if (index .gt. nglth) goto 230
        ibus = idflt(index-2)
        ckx = dfltd(index-1)
        nline = idflt(index)
        ei = eyr(ibus)
        fi = eyi(ibus)
        name1 = bname(ibus)
        bkv1 = buskv(ibus)
  110   do while (.true.)
          nline = nline - 1
          if (nline .lt. 0) goto 100
          indexo = index
          index = index + 4
          ij = 3
          if (index .gt. nglth) goto 220
          jbus = idflt(index-3)
          ntrip = idflt(index-2)
          ttimer = dfltd(index-1)
          rtimer = dfltd(index)
          if (ntrip .eq. 2) goto 210
          if (ntrip .ne. 3) then
            ej = eyr(jbus)
            fj = eyi(jbus)
            if (ttimer .eq. -1.) then
              oposit = ei*fj - ej*fi
              adjcnt = ei*ej + fi*fj
              oposit = abs(oposit)
              adjcnt = abs(adjcnt)
              if (oposit .le. tan60*adjcnt) goto 210
            endif
C          
C            60 DEGREE TEST POSITIVE   CHECK FOR IMPEDANCE VALUES
C          
            ilvolt = 0
            eij = ei - ej
            fij = fi - fj
            efsq = eij*eij + fij*fij
            if (efsq .lt. 0.0001) goto 210
            ckefi = ckx*ei - fi
            eckfi = ei + ckx*fi
            alph1 = (ckefi*eij+eckfi*fij)/efsq
            beta1 = (eckfi*eij-ckefi*fij)/efsq
            alf1sq = alph1*alph1
            bet1sq = beta1*beta1
            z21 = alf1sq + bet1sq - 2.*(alph1*a2+beta1*b2)
            iz2 = 1
            if (z21 .le. 0.) then
              iz2 = 2
              if (ckx .gt. ck230) goto 130
            endif
          elseif (rtimer .gt. to+0.0001) then
            goto 190
          else
            name2 = bname(jbus)
            bkv2 = buskv(jbus)
            write (outbuf, 10000) name1, bkv1, name2, bkv2, to
            call prtout(1)
10000       format ('0', 5x,
     &       'default distance relay reclosing between ', a8, 1x, f5.1,
     &       ' and ', a8, 1x, f5.1, ' at ', f7.2, ' cycles.')
            ircall = 8
            ibusn = ibus
            jbusn = jbus
            itparn = 99
            icden = 1
C          
C           CALL TRPHIS TO FORM A TABLE OF LINES THAT HAVE BEEN TR
C          
            call trphis(itparn, ibusn, jbusn, gijt, bijt, giot, biot,
     &       gjot, bjot, icden, idpar, jobdo, mcode)
            if (jobdo .ne. 0) call lldrop()
            ttimer =  - 1.0
            rtimer =  - 1.0
            ntrip = 1
            goto 200
          endif
  120     eji =  - eij
          fji =  - fij
          ckefj = ckx*ej - fj
          eckfj = ej + ckx*fj
          alph2 = (ckefj*eji+eckfj*fji)/efsq
          beta2 = (eckfj*eji-ckefj*fji)/efsq
          alf2sq = alph2*alph2
          bet2sq = beta2*beta2
          if (ilvolt .eq. 1) goto 150
          z22 = alf2sq + bet2sq - 2.*(alph2*a2+beta2*b2)
          if (z22 .gt. 0.) goto 140
          if (iz2 .eq. 2) then
            iz2 = 4
          else
            iz2 = 3
          endif
          if (ckx .le. ck230) goto 140
C        
C          COMPUTE Z11
C        
  130     z11 = alf1sq + bet1sq - 2.*(alph1*a1+beta1*b1)
          if (z11 .le. 0.) goto 180
          if (iz2 .gt. 2) goto 150
          ilvolt = 1
          goto 120
  140     if (iz2 .eq. 2 .or. iz2 .eq. 3) goto 170
          if (iz2 .eq. 4) goto 180
          goto 160
  150     ilvolt = 0
          z12 = alf2sq + bet2sq - 2.*(alph2*a1+beta2*b1)
          if (z12 .le. 0.) goto 180
          if (iz2 .ne. 1) goto 170
  160     if (ttimer .ne. -1.0) then
            name2 = bname(jbus)
            bkv2 = buskv(jbus)
            write (outbuf, 10010) name1, bkv1, name2, bkv2, to
            call prtout(1)
10010       format ('0', 5x, 'default distance relay between ', a8, 1x,
     &       f5.1, ' and ', a8, 1x, f5.1, ' has reset at ', f7.2,
     &       ' cycles.')
          endif
          ttimer =  - 1.0
          rtimer =  - 1.0
          goto 200
  170     if (ttimer .eq. -1.0) then
            ttimer = rdelay + to
          elseif (ttimer .le. to+0.0001) then
            goto 180
          endif
          if (ttimer .lt. dnxrly) dnxrly = ttimer
          goto 200
  180     name2 = bname(jbus)
          bkv2 = buskv(jbus)
          write (outbuf, 10020) name1, bkv1, name2, bkv2, to
          call prtout(1)
10020     format ('0', 5x, 'default distance relay between ', a8, 1x,
     &     f5.1, ' and ', a8, 1x, f5.1, ' tripped at ', f7.2,
     &     ' cycles.')
          ircall = 7
          ibusn = ibus
          jbusn = jbus
          if (ibus .ge. jbus) then
            ibusn = jbus
            jbusn = ibus
          endif
          itparn = 99
          icden =  - 1
C        
C         CALL TRPHIS TO FORM A TABLE OF LINES THAT HAVE BEEN TRIP
C        
          call trphis(itparn, ibusn, jbusn, gijt, bijt, giot, biot,
     &     gjot, bjot, icden, idpar, jobdo, mcode)
          if (jobdo .ne. 0) call lldrop()
          if (trclse .gt. 0.) then
            ntrip = 3
            rtimer = trclse + to
          else
            ntrip = 2
            goto 200
          endif
  190     if (rtimer .lt. dnxrly) dnxrly = rtimer
  200     dfltd(index) = rtimer
          dfltd(index-1) = ttimer
          idflt(index-2) = ntrip
  210     continue
        enddo
  220   nline = nline + 1
  230   index = indexo
        lgnth = lgnth - index
      enddo
  240 return
      end
