C    %W% %G%
      subroutine tslsol(slp)
C     
C     THIS SUBROUTINE CALCULATES THE DIFFERENTIAL EQUATIONS
C     ASSOCIATED WITH THE TRANSIENT STABILITZER MODEL
C     AND COMPUTES THE OUTPUT FUNCTION EFUN
C     
      include 'tspinc/params.inc'
      include 'tspinc/tsldat.inc'
      include 'tspinc/nameid.inc'
      include 'tspinc/vrgov.inc'
      include 'tspinc/comvar.inc'
      include 'tspinc/param.inc'
      include 'tspinc/lnk12.inc'
      include 'tspinc/lnk1a.inc'
      include 'tspinc/outaux.inc'
      include 'tspinc/prt.inc'
C     
C     IF ITRIG = 3, THE STABILIZER HAS TRIGGERED AND TIMED OUT
C     SO GO TO CALCULATION OF EFUN
C     
      if (itrig(itsl) .ne. 3) then
C       
C       CALCULATE TRIGGER FUNCTION UFUN
C       
        if (tslfir(itsl) .gt. 0.0) goto 110
C       
C       CALCULATE STATE VECTORS
C       
        dw1 = tsldt3(itsl)*slp - tslhb3(itsl)
        dw2 = (dw1+tslhb4(itsl))/tsldt4(itsl)
        dw4 = (2.*dw2/ddt2+tslhb5(itsl))/tsldt5(itsl)
C       
C       CALULATE UFUN TRIGGER PARAMETER
C       
        tfun =  - tslkt(itsl)*dwt2(itsl) - dw4 + dw2*tslkt(itsl)
        if (dw4 .gt. 0.0) tfun = 0.0
        if (dw2 .gt. dwt1(itsl)) tfun = 0.0
C       
C       IF TFUN IS LESS THAN ZERO, THE STABILIZER HAS NOT TRIGGERE
C       
        if (tfun .le. 0.0) then
          efun(itsl) = 0.0
          itrig(itsl) = 0
        else
C         
C         IF ITRIG = 0, THIS IS THE INITIAL TRIP SO SET ITRIG =1 A
C         CALCULATE TBASE, THE TIME WHEN THE TIMER WILL RUN OUT
C         
          if (itrig(itsl) .eq. 0) then
            itrig(itsl) = 1
            tbase(itsl) = tsim + tdlay(itsl)
            write (outbuf, 10000) name, bkv, idm, tsim
10000       format ('0', 5x, 'TRANSIENT STABILIZER AT ', a8, 1x, f5.1,
     &       1x, a1, ' TRIGGERED AT ', f7.2, ' CYCLES. ')
            call prtout(1)
          endif
C         
C         IF ITRIG = 1 THE TRAJECTORY IS STILL IN THE TRIP ZONE
C         SO CHECK TO SEE IF THE TIMER HAS RUN OUT
C         
          if (tsim .lt. tbase(itsl)) then
            efun(itsl) = 0.0
          else
C           
C           STABILIZER HAS TRIPPED
C           
            itrig(itsl) = 3
            tbase(itsl) = tsim
            write (outbuf, 10010) name, bkv, idm, tsim
10010       format ('0', 5x, 'TRANSIENT STABILIZER AT ', a8, 1x, f5.1,
     &       1x, a1, ' TIMED OUT AT ', f7.2, ' CYCLES. ')
            call prtout(1)
            goto 100
          endif
        endif
C       
C       IT THE TIME STEP HAS CHANGED, GET NEW TIME FACTORS
C       
        if (al .ne. 0.0) then
          tsldt3(itsl) = tsldt3(itsl)*tfac
          tsldt4(itsl) = tsldt4(itsl)*tfac - tfac + 1.0
          tsldt5(itsl) = tsldt5(itsl)*tfac - tfac + 1.0
        endif
C       
C       CALCULATE PAST VALUE FACTORS
C       
        tslhb3(itsl) = tsldt3(itsl)*slp + dw1
        tslhb4(itsl) = dw1 + dw2*(tsldt4(itsl)-2.)
        tslhb5(itsl) = dw4*(tsldt5(itsl)-2.0) - 2.*dw2/edt
        goto 110
      endif
C     
C     CALCULATE OUTPUT FUNCTION EFUN, IF LPPWR IS .GT. 0 THE EFUN
C     HAS ALREADY BEEN CALCULATED
C     
  100 if (lppwr .le. 0) then
        tme = tsim + edt - tbase(itsl)
        if (tslt1(itsl) .eq. 0.0) then
          tcon1 = 0.0
        else
          tcon1 = exp(-tme/tslt1(itsl))
        endif
        tcon2 = exp(-tme/tslt2(itsl))
        efun(itsl) = tslk(itsl)*(1.0-tcon1)*tcon2
      endif
  110 return
      end
