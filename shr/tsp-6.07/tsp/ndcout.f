C    %W% %G%
      subroutine ndcout
C     
C     THIS SUBROUTINE FORMS FINAL TABLES FOR REQUESTED DC OUTPUT
C     DATA AND WRITES THE TABLES TO THE OUTPUT FILE (FOR006).
C     IT IS CALLED BY NOUT2.
C
C     Revs:
C     May/07/92 - DEM: Added new parm DNAME to call to DCAUX to support
C     display of branch name with all t-parm lists
C     for new aux file format

      include 'tspinc/params.inc'
      include 'tspinc/blkcom1.inc'
      include 'tspinc/comn56.inc'
      include 'tspinc/link56.inc'
      include 'tspinc/dc.inc'
      include 'tspinc/worst.inc'
      include 'tspinc/dcname.inc'
      include 'tspinc/newtab.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/room.inc'
      include 'tspinc/prtmax.inc'

      character*8 ib1c, ib2c, ibdumc
      dimension idcsw(18)
      equivalence (msub1, msub(1))
      dimension msub1(2), ksubd(7)
      character*20 modlbl(7)
      character dname*40

C     -  local in-line functions

      logical prntop, auxop

      prntop(i001) = (i001 .eq. 1 .or. i001 .eq. 3 .or. 
     &                i001 .eq. 5 .or. i001 .eq. 7)
      auxop(i001) = (i001 .ge. 4)

      data modlbl/'LOW LEVEL P INPUT KA', 'LOW LEVEL I INPUT KA',
     &     'HI LEVEL P INPUT  MW', 'HI LEVEL I INPUT  MW',
     &     'GAMMA MOD - DEGREES ', 'DUAL FREQUENCY -  MW',
     &     'EXTINCTION ANG - DEG'/

C     -     Begin     Begin     Begin     Begin     Begin     Begin

      call mpost('NDCOUT')
      ksubd(1) = kdc1
      ksubd(2) = kdc2
      ksubd(3) = kdc3
      ksubd(4) = kdc4
      ksubd(5) = kdc5
      ksubd(6) = kdc6
      ksubd(7) = kdc7
      istart = 20
      kstrt = 500
      kstrt1 = 501
c     
c      SEPARATE ALL DC VARIABLES FOR EACH TIME STEP INTO EACH DC
c      VARIABLE FOR ALL TIME STEPS
c     
      do ii = 1, 7

C       ICOUNT IS NUMBER OF DISTINCT POINTS FOR ANY OUTPUT QUANTITY
C       OVER ALL TIME STEPS

        do i = 1, icount
          call redecp(msub1(2), ksubd(ii)+i, 1)
          call readmp (1, work, 20, 1)
          do j = 1, nddat
            work(istart+(j-1)*icount+i) = work(j)
          enddo
        enddo
        j = nddat*icount

C       ARRAY STARTING AT WORK(21) CONTAINS BLOCKS OF ICOUNT EACH.
C       FOR II=1 COS ALPHA FOR TERMNL  OR  CURRENT FOR BRANCH
C       FOR II=2 CURRENT FOR TERMNL  OR  L.H. BUS VOLTS FOR BRANCH
C       FOR II=3 VOLTAGE FOR TERMNL  OR  R.H. BUS VOLTS FOR BRANCH
C       FOR II=4 MOD SIG FOR TERMNL
C       FOR II = 5 V ALPHA
C       FOR II = 6 VALPHA PRIME
C       FOR II = 7 EXTINCTION ANGLE
C       J IS THE LENGTH OF EACH OF THESE TABLES
C       
        if (j .gt. 0) then
          call writmp (1, work(21), j, 1)
C         
C         WE NEED ONLY 7 ECS ADDRESSES TO STORE MASS STORAGE INDICES
C         SO WE REDEFINE KSUBD(1) - KSUBD(7)
C         
          ksubd(ii) = kdc1 + ii - 1
          call ritecp(msub1(2), ksubd(ii), 1)
        endif
      enddo
      istm = j
C     
c     REDEFINE KWORK SUCH THAT IT IS CLEAR OF KSUBD(7)
C     
      kwork = max0(kwork, (ksubd(7)+1))
      ndc3 = 2*ndc2
C     
C     THE PACKED ARRAY WRITTEN AT KDCBRN HAS BEEN
C     REPLACED WITH ARRAYS DCNME2 AND IDCBK2.   
C     SEE OUTPUT2.                               
c     TRANSFER DC BRANCH TABLE TO A NEW LOCATION( AFTER OTHER NETWOR
c     DATA) IN ECS.  REDEFINE KWORK
C     
      kwork = kwork + ndc3
C     
C     FORMING A WORKING TABLE THAT CONTAINS ALL ANGLES,CURRENTS, AND
C     VOLTAGES FOR ALL TIME STEPS FOR EACH DC LINE WHERE OUTPUT HAS
C     BEEN REQUESTED
C     
      do ii = 1, 7
        call redecp(msub1(2), ksubd(ii), 1)
C       
C       WORK TABLE THAT WAS WRITTEN TO MS EARLIER FOR II=1,2,3 IS NO
C       RETRIEVED AND WRITTEN TO ECS IN THE SAME ORDER
C       
        call readmp (1, work, istm, 1)
        kadr = kwork + (ii-1)*istm
        call ritecp(work, kadr, istm)
      enddo
C     
c     KDCPLT IS THE ECS WORK AREA ADDRESS TO BE USED IN PLOT CALC.
C     
      kdcplt = kwork + 7*istm
      ijk = 0
C     
c     DO LOOP 900 HANDLES EACH OUTPUT REQUEST INDIVIDUALLY
C     
      do i = 1, iddat
        ij = 0
        if (.not. (idcxn(1, i) .eq. 0 .and. idcxn(2, i) .eq. 0 .and.
     &   idcxn(3, i) .eq. 0)) then
C         
C         IN ARRAY WORK WE READ IN FROM ECS ALL THE QUANTITIES PERTA
C         TO THIS REQUEST. FOR TWO TERMNLS IT WILL BE COSALPHA1,COSA
C         I1,I2,V1,V2 VA1,VA2,VAP1,VAP2,DELTA1,DELTA2 IN THAT ORDER
C         IF IT IS A BRANCH WE WILL HAVE I12,V1,V2.  V1 AND V2 WILL
C         CORROSPOND TO LH AND RH BUSSES OF THE BRANCH AS REQUESTED
C         NOT AS IN THE IN THE DC BRANCH TABLE
C         
          k1 = idcxn(1, i)
          k2 = idcxn(2, i)
          ibrn = idcxn(3, i)
          if (ibrn .gt. 0) then
            do ii = 1, 7
              ji = 0
              ji = ji + 1
              if (ii .ne. 1) then
                if (k1 .ge. k2) then
                  kadr = kwork + (7-ii)*istm + (ijk)*icount
                  goto 100
                endif
              endif
              kadr = kwork + (ii-1)*istm + (ijk)*icount
  100         istart = kstrt1 + (ii-1)*(icount+2)
              call redecp(work(istart), kadr, icount)
            enddo
          else
C           
C           PROCESS TERMINAL OUTPUT REQUESTS
C           
            do ii = 1, 7
              ji = 0
              if (k1 .ne. 0) then
                istart = kstrt1 + 2*(ii-1)*(icount+2)
                kadr = kwork + (ii-1)*istm + (ijk+ji)*icount
                call redecp(work(istart), kadr, icount)
                ji = ji + 1
              endif
              if (k2 .ne. 0) then
                istart = kstrt1 + 2*(ii-1)*(icount+2) + (icount+2)
                kadr = kwork + (ii-1)*istm + (ijk+ji)*icount
                call redecp(work(istart), kadr, icount)
                ji = ji + 1
              endif
            enddo
          endif
          ijk = ijk + ji
          xmax1 = 0.0
          xmax2 = 0.0
          xmin1 = 0.0
          xmin2 = 0.0
          ib1 = idindn(1, i)
          ib2 = idindn(2, i)
C         
C         UNPACK AND STORE OUTPUT OPTIONS
C         
          do k = 1, 18
            idcsw(k) = idindn(k+2, i)
          enddo
          idcsw1 = idcsw(1)
          idcsw2 = idcsw(2)
          idcsw3 = idcsw(3)
          idcsw4 = idcsw(4)
          idcsw5 = idcsw(5)
          idcsw6 = idcsw(6)
          idcsw7 = idcsw(7)
          idcsw8 = idcsw(8)
          idcsw9 = idcsw(9)
          idsw10 = idcsw(10)
          idsw11 = idcsw(11)
          idsw12 = idcsw(12)
          idsw13 = idcsw(13)
          idsw14 = idcsw(14)
          idsw15 = idcsw(15)
          idsw16 = idcsw(16)
          idsw17 = idcsw(17)
          idsw18 = idcsw(18)
          if (ibrn .gt. 0) then
C           
C           OUTPUT FOR DC BRANCHES
C           
            ib1 = idcbk2(2*ibrn-1)
            ib1c = dcnme2(2*ibrn-1)
            ib2 = idcbk2(2*ibrn)
            ib2c = dcnme2(2*ibrn)
C           
C           IF THE BRANCH OUTPUT REQUEST IS IN REVERSE ORDER TURN AROUND
C           AND IB2 FOR CORRECT BUS NAMES ON OUTPUT LISTING
C           
            if (k1 .ge. k2) then
              ibdum = ib1
              ibdumc = ib1c
              ib1 = ib2
              ib1c = ib2c
              ib2 = ibdum
              ib2c = ibdumc
            endif
            kb1 = ib1
            kb2 = ib2
            b1 = basekv(kb1)
            b2 = basekv(kb2)
            write (outbuf, 10000) ib1c, b1, ib2c, b2
            call prtout(1)
10000       format ('0', ' dc branch output   ', a8, 1x, f5.1, 5x, a8,
     &       1x, f5.1)
C           
C           DC BRANCH CURRENT  LEFT TO RIGHT
C           
            istar3 = kstrt
            ij = ij + 1
            ist9 = istar3
C           
C           REVERSE CURRENT SIGN IF BRANCH REQUEST REVERSED
C           
            sign = 1.0
            if (k1 .gt. k2) sign =  - 1.0
C           
C           THE FOLLOWING LOGIC FINDS AND STORES MAX AND MIN OF EACH
C           QUANTITY FROM ARRAY WORK.  POWER COMPUTED AS E * I
C           
            do jj = 1, icount
              work(istar3+jj) = sign*work(istar3+jj)
              if (work(istar3+jj) .lt. xmin1) xmin1 = work(istar3+jj)
              if (work(istar3+jj) .gt. xmax1) xmax1 = work(istar3+jj)
            enddo
            work(istar3+icount+1) = xmax1
            work(istar3+icount+2) = xmin1
C           
C           DC CURRENT OUTPUT
C           
            if (prntop(idcsw1)) then
              istart = ist9
              write (outbuf, 10010) ib1c, b1
10010         format ('0 DC CURRENT (KILOAMPS) INTO DC BRANCH AT BUS ',
     &         a8, 1x, f5.1)
              call prtout(1)
              do jjj = 1, icount, 5
                kkk = min0(jjj+4, icount)
                write (outbuf, 10020) (t(jj), work(istart+jj), jj =
     &           jjj, kkk)
10020         format (5(2x, f7.2, ' cycles ', f8.4))
                call prtout(1)
              enddo
            endif
            if (auxop(idcsw1)) call dcaux(dname,
     &       'DC BRANCH CURRENT, L. TO R.', icount, t, work(ist9+1))
C           1          ('DC BRANCH CURRENT, L. TO R.',ICOUNT,T,WORK(IST9
C           
C           BUS VOLTAGE FOR DC BRANCH L.H. BUS
C           
            istar3 = kstrt + (icount+2)
            ist10 = istar3
            ij = ij + 1
            xmax1 = 0.0
            xmin1 = 0.0
            do jj = 1, icount
              if (work(istar3+jj) .lt. xmin1) xmin1 = work(istar3+jj)
              if (work(istar3+jj) .gt. xmax1) xmax1 = work(istar3+jj)
            enddo
            work(istar3+icount+1) = xmax1
            work(istar3+icount+2) = xmin1
C           
C           DC VOLTAGE OUTPUT L.H. BUS
C           
            if (prntop(idcsw2)) then
              istart = ist10
              write (outbuf, 10110) ib1c, b1
              call prtout(1)
              do jjj = 1, icount, 5
                kkk = min0(jjj+4, icount)
                write (outbuf, 10120) (t(jj), work(istart+jj), jj =
     &           jjj, kkk)
                call prtout(1)
              enddo
            endif
            if (auxop(idcsw2)) call dcaux(dname,
     &       'DC BRANCH VOLTAGE, L.H. BUS', icount, t, work(ist10+1))
C           1          ('DC BRANCH VOLTAGE, L.H. BUS',ICOUNT,T,WORK(IST1
C           
C           BUS VOLTAGE FOR DC BRANCH R.H. BUS
C           
            istar3 = kstrt + (icount+2)*2
            ist11 = istar3
            ij = ij + 1
            xmax1 = 0.0
            xmin1 = 0.0
            do jj = 1, icount
              if (work(istar3+jj) .lt. xmin1) xmin1 = work(istar3+jj)
              if (work(istar3+jj) .gt. xmax1) xmax1 = work(istar3+jj)
            enddo
            work(istar3+icount+1) = xmax1
            work(istar3+icount+2) = xmin1
C           
C           DC VOLTAGE OUTPUT R.H. BUS
C           
            if (prntop(idcsw6)) then
              istart = ist11
              write (outbuf, 10110) ib2c, b2
              call prtout(1)
              do jjj = 1, icount, 5
                kkk = min0(jjj+4, icount)
                write (outbuf, 10120) (t(jj), work(istart+jj), jj =
     &           jjj, kkk)
                call prtout(1)
              enddo
            endif
            if (auxop(idcsw6)) call dcaux(dname,
     &       'DC BRANCH VOLTAGE, R.H. BUS', icount, t, work(ist11+1))
C           1          ('DC BRANCH VOLTAGE, R.H. BUS',ICOUNT,T,WORK(IST1
C           
C           DC BRANCH POWER INTO THE BRANCH AT LEFT BUS
C           
            istar3 = kstrt + 3*(icount+2)
            ist12 = istar3
            ij = ij + 1
            xmax1 = 0.0
            xmin1 = 0.0
C           
C           DC POWER HAS TO BE COMPUTED AS E*I
C           
            do jj = 1, icount
              work(istar3+jj) = work(ist9+jj)*work(ist10+jj)
              if (work(istar3+jj) .lt. xmin1) xmin1 = work(istar3+jj)
              if (work(istar3+jj) .gt. xmax1) xmax1 = work(istar3+jj)
            enddo
            work(istar3+icount+1) = xmax1
            work(istar3+icount+2) = xmin1
C           
C           DC BRANCH POWER INTO LEFT BUS
C           
            if (prntop(idcsw3)) then
              istart = ist12
              write (outbuf, 10030) ib1c, b1
10030         format ('0 DC POWER (MW) INTO DC BRANCH AT BUS ', a8, 1x,
     &         f5.1)
              call prtout(1)
              do jjj = 1, icount, 5
                kkk = min0(jjj+4, icount)
                write (outbuf, 10040) (t(jj), work(istart+jj), jj =
     &           jjj, kkk)
10040         format (5(2x, f7.2, ' cycles ', f8.2))
                call prtout(1)
              enddo
            endif
            if (auxop(idcsw3)) call dcaux(dname,
     &       'DC BRANCH POWER, INTO L. BUS', icount, t, work(ist12+1))
C           1          ('DC BRANCH POWER, INTO L. BUS',ICOUNT,T,WORK(IST
C           
C           DC BRANCH POWER OUT OF THE BRANCH AT RIGHT BUS
C           
            istar3 = kstrt + 4*(icount+2)
            ist13 = istar3
            ij = ij + 1
            xmax1 = 0.0
            xmin1 = 0.0
C           
C           DC POWER HAS TO BE COMPUTED AS E*I
C           
            do jj = 1, icount
              work(istar3+jj) = work(ist9+jj)*work(ist11+jj)
              if (work(istar3+jj) .lt. xmin1) xmin1 = work(istar3+jj)
              if (work(istar3+jj) .gt. xmax1) xmax1 = work(istar3+jj)
            enddo
            work(istar3+icount+1) = xmax1
            work(istar3+icount+2) = xmin1
C           
C           DC BRANCH POWER OUT OF RIGHT BUS
C           
            if (prntop(idcsw7)) then
              istart = ist13
              write (outbuf, 10050) ib2c, b2
10050         format ('0 DC POWER (MW) OUT OF DC BRANCH AT BUS ', a8,
     &         1x, f5.1)
              call prtout(1)
              do jjj = 1, icount, 5
                kkk = min0(jjj+4, icount)
                write (outbuf, 10060) (t(jj), work(istart+jj), jj =
     &           jjj, kkk)
10060         format (5(2x, f7.2, ' cycles ', f8.2))
                call prtout(1)
              enddo
            endif
            if (auxop(idcsw7)) call dcaux(dname,
     &       'DC BRANCH POWER, OUT R. BUS', icount, t, work(ist13+1))
          else
            if (ib1 .gt. 0) then
C             
C             THE PACKED ARRAY NEWTAB WAS WRITTEN AT  
C             KNEWT.  NEW ARRAYS ARE INWTB AND NEWTAB.
C             
              kb1 = inwtb(ib1)
              ib1c = newtbc(ib1)
              b1 = basekv(kb1)
            endif
            if (ib2 .gt. 0) then
              kb2 = inwtb(ib2)
              ib2c = newtbc(ib2)
              b2 = basekv(kb2)
            endif
C           
C           IF LH OR RH TERMNL NAME IS NOT VALID, ZERO OUT IT'S OPTI
C           
            if (k1 .le. 0) then
              idcsw1 = 0
              idcsw3 = 0
              idcsw5 = 0
              idcsw7 = 0
              idcsw9 = 0
              idsw11 = 0
              idsw13 = 0
              idsw15 = 0
              idsw17 = 0
            endif
            if (k2 .le. 0) then
              idcsw2 = 0
              idcsw4 = 0
              idcsw6 = 0
              idcsw8 = 0
              idsw10 = 0
              idsw12 = 0
              idsw14 = 0
              idsw16 = 0
              idsw18 = 0
            endif
C           
C           NUMBER OF QUANTITIES ON AUXILIARY OUTPUT
C           
            nq1 = idcsw1/4 + idcsw2/4 + idcsw3/4 + idcsw4/4
            nq2 = idcsw5/4 + idcsw6/4 + idcsw7/4 + idcsw8/4
            nqty = nq1 + nq2
            write (dname, 10070) ib1c, b1, ib2c, b2, nqty
10070       format (a8, f5.1, 1x, a8, f5.1, i2)
C           IF (NQTY.GT.0) THEN
            if (nqty .gt. 0 .and. auxfmt .eq. 'STD') write (l11, '(2A)'
     &       ) 'D  ', dname
C           WRITE(L11,212) IB1C,B1,IB2C,B2,NQTY
C           212       FORMAT('D  ',A8,F5.1,1X,A8,F5.1,I2)
            if (k1 .gt. 0 .and. (prntop(idcsw1) .or. prntop(idcsw2)
     &       .or. prntop(idcsw3) .or. prntop(idcsw4))) then
              write (outbuf, 10080) ib1c, b1
              call prtout(1)
            endif
            if (k2 .gt. 0 .and. (prntop(idcsw5) .or. prntop(idcsw6)
     &       .or. prntop(idcsw7) .or. prntop(idcsw8))) then
              write (outbuf, 10080) ib2c, b2
              call prtout(1)
            endif
10080       format ('0', ' output for dc terminal  ', a8, 1x, f5.1)
C           
C           INITIALIZE ARRAYS FOR MAX AND MIN PRINT DATA
C           
            do itrr = 1, 18
              prtmax(itrr) =  - 10000.
              prtmin(itrr) = 10000.
            enddo
C           
C           THE FOLLOWING LOGIC TAKES EACH OUTPUT QUANTITY STORED IN
C           WORK AND FINDS AND STORES MAX AND MIN VALUES
C           COS ALPHAS ARE CHANGED TO DEGREES. POWER IS COMPUTED AS
C           
C           DC BUS FIRING ANGLE
C           
            if (k1 .ne. 0) then
              istart = kstrt
              ist1 = istart
              ij = ij + 1
              do jj = 1, icount
                work(istart+jj) = acos(work(istart+jj))*degov
                if (t(jj) .ge. wtim1 .and. t(jj) .le. wtim2 .or. wtim2
     &           .eq. -1.0) then
                  if (work(istart+jj) .gt. prtmax(1)) prtmax(1) = work
     &             (istart+jj)
                  if (work(istart+jj) .lt. prtmin(1)) prtmin(1) = work
     &             (istart+jj)
                endif
                if (work(istart+jj) .lt. xmin2) xmin2 = work(istart+jj)
                if (work(istart+jj) .gt. xmax2) xmax2 = work(istart+jj)
              enddo
              work(istart+icount+1) = xmax2
              work(istart+icount+2) = xmin2
            endif
            if (k2 .ne. 0) then
              istar2 = kstrt + 1*(icount+2)
              ist2 = istar2
              ij = ij + 1
              xmax1 = 0.0
              xmax2 = 0.0
              xmin1 = 0.0
              xmin2 = 0.0
              do jj = 1, icount
                work(istar2+jj) = acos(work(istar2+jj))*degov
                if (t(jj) .ge. wtim1 .and. t(jj) .le. wtim2 .or. wtim2
     &           .eq. -1.0) then
                  if (work(istar2+jj) .gt. prtmax(2)) prtmax(2) = work
     &             (istar2+jj)
                  if (work(istar2+jj) .lt. prtmin(2)) prtmin(2) = work
     &             (istar2+jj)
                endif
                if (work(istar2+jj) .lt. xmin1) xmin1 = work(istar2+jj)
                if (work(istar2+jj) .gt. xmax1) xmax1 = work(istar2+jj)
              enddo
              work(istar2+icount+1) = xmax1
              work(istar2+icount+2) = xmin1
            endif
C           
C           DC BUS CURRENT
C           
            if (k1 .ne. 0) then
              istart = kstrt + 2*(icount+2)
              ist3 = istart
              ij = ij + 1
              xmax1 = 0.0
              xmax2 = 0.0
              xmin1 = 0.0
              xmin2 = 0.0
              do jj = 1, icount
                if (t(jj) .ge. wtim1 .and. t(jj) .le. wtim2 .or. wtim2
     &           .eq. -1.0) then
                  if (work(istart+jj) .gt. prtmax(3)) prtmax(3) = work
     &             (istart+jj)
                  if (work(istart+jj) .lt. prtmin(3)) prtmin(3) = work
     &             (istart+jj)
                endif
                if (work(istart+jj) .lt. xmin2) xmin2 = work(istart+jj)
                if (work(istart+jj) .gt. xmax2) xmax2 = work(istart+jj)
              enddo
              work(istart+icount+1) = xmax2
              work(istart+icount+2) = xmin2
            endif
            if (k2 .ne. 0) then
              istar2 = kstrt + 3*(icount+2)
              ist4 = istar2
              ij = ij + 1
              xmax1 = 0.0
              xmax2 = 0.0
              xmin1 = 0.0
              xmin2 = 0.0
              do jj = 1, icount
                if (t(jj) .ge. wtim1 .and. t(jj) .le. wtim2 .or. wtim2
     &           .eq. -1.0) then
                  if (work(istar2+jj) .gt. prtmax(4)) prtmax(4) = work
     &             (istar2+jj)
                  if (work(istar2+jj) .lt. prtmin(4)) prtmin(4) = work
     &             (istar2+jj)
                endif
                if (work(istar2+jj) .lt. xmin1) xmin1 = work(istar2+jj)
                if (work(istar2+jj) .gt. xmax1) xmax1 = work(istar2+jj)
              enddo
              work(istar2+icount+1) = xmax1
              work(istar2+icount+2) = xmin1
            endif
C           
C           DC BUS VOLTAGE
C           
            if (k1 .ne. 0) then
              istart = kstrt + 4*(icount+2)
              ist5 = istart
              ij = ij + 1
              xmax1 = 0.0
              xmax2 = 0.0
              xmin1 = 0.0
              xmin2 = 0.0
              do jj = 1, icount
                if (t(jj) .ge. wtim1 .and. t(jj) .le. wtim2 .or. wtim2
     &           .eq. -1.0) then
                  if (work(istart+jj) .gt. prtmax(5)) prtmax(5) = work
     &             (istart+jj)
                  if (work(istart+jj) .lt. prtmin(5)) prtmin(5) = work
     &             (istart+jj)
                endif
                if (work(istart+jj) .lt. xmin2) xmin2 = work(istart+jj)
                if (work(istart+jj) .gt. xmax2) xmax2 = work(istart+jj)
              enddo
              work(istart+icount+1) = xmax2
              work(istart+icount+2) = xmin2
            endif
            if (k2 .ne. 0) then
              istar2 = kstrt + 5*(icount+2)
              ist6 = istar2
              ij = ij + 1
              xmax1 = 0.0
              xmax2 = 0.0
              xmin1 = 0.0
              xmin2 = 0.0
              do jj = 1, icount
                if (t(jj) .ge. wtim1 .and. t(jj) .le. wtim2 .or. wtim2
     &           .eq. -1.0) then
                  if (work(istar2+jj) .gt. prtmax(6)) prtmax(6) = work
     &             (istar2+jj)
                  if (work(istar2+jj) .lt. prtmin(6)) prtmin(6) = work
     &             (istar2+jj)
                endif
                if (work(istar2+jj) .lt. xmin1) xmin1 = work(istar2+jj)
                if (work(istar2+jj) .gt. xmax1) xmax1 = work(istar2+jj)
              enddo
              work(istar2+icount+1) = xmax1
              work(istar2+icount+2) = xmin1
            endif
C           
C           DC MODULATION SIGNAL
C           
            if (k1 .ne. 0) then
              istar3 = kstrt + 6*(icount+2)
              ist7 = istar3
              ij = ij + 1
              xmax1 = 0.0
              xmax2 = 0.0
              xmin1 = 0.0
              xmin2 = 0.0
C             
C             IF GAMMA MODULATION CONVERT FROM RADIANS TO DEGREE
C             
              if (idcmd1(i) .eq. 5 .or. idcmd1(i) .lt. 1) then
                radcon = 57.29578
              else
                radcon = 1.
              endif
              do jj = 1, icount
                work(istar3+jj) = work(istar3+jj)*radcon
                if (t(jj) .ge. wtim1 .and. t(jj) .le. wtim2 .or. wtim2
     &           .eq. -1.0) then
                  if (work(istar3+jj) .gt. prtmax(7)) prtmax(7) = work
     &             (istar3+jj)
                  if (work(istar3+jj) .lt. prtmin(7)) prtmin(7) = work
     &             (istar3+jj)
                endif
                if (work(istar3+jj) .lt. xmin2) xmin2 = work(istar3+jj)
                if (work(istar3+jj) .gt. xmax2) xmax2 = work(istar3+jj)
              enddo
              work(istar3+icount+1) = xmax2
              work(istar3+icount+2) = xmin2
            endif
            if (k2 .ne. 0) then
              istar3 = kstrt + 7*(icount+2)
              ist8 = istar3
              ij = ij + 1
              xmax1 = 0.0
              xmax2 = 0.0
              xmin1 = 0.0
              xmin2 = 0.0
C             
C             IF GAMMA MODULATION CONVERT FROM RADIANS TO DEGREE
C             
              if (idcmd2(i) .eq. 5 .or. idcmd2(i) .lt. 1) then
                radcon = 57.29578
                xmax1 = 24.
              else
                radcon = 1.
              endif
              do jj = 1, icount
                work(istar3+jj) = work(istar3+jj)*radcon
                if (t(jj) .ge. wtim1 .and. t(jj) .le. wtim2 .or. wtim2
     &           .eq. -1.0) then
                  if (work(istar3+jj) .gt. prtmax(8)) prtmax(8) = work
     &             (istar3+jj)
                  if (work(istar3+jj) .lt. prtmin(8)) prtmin(8) = work
     &             (istar3+jj)
                endif
                if (work(istar3+jj) .lt. xmin1) xmin1 = work(istar3+jj)
                if (work(istar3+jj) .gt. xmax1) xmax1 = work(istar3+jj)
              enddo
              work(istar3+icount+1) = xmax1
              work(istar3+icount+2) = xmin1
            endif
C           
C           DC BUS POWER
C           
            xmax1 = 0.0
            xmax2 = 0.0
            xmin1 = 0.0
            xmin2 = 0.0
            if (k1 .ne. 0) then
              istart = kstrt + 16*(icount+2)
              ist17 = istart
              ij = ij + 1
              do jj = 1, icount
                work(istart+jj) = work(ist3+jj)*work(ist5+jj)
                if (t(jj) .ge. wtim1 .and. t(jj) .le. wtim2 .or. wtim2
     &           .eq. -1.0) then
                  if (work(istart+jj) .gt. prtmax(9)) prtmax(9) = work
     &             (istart+jj)
                  if (work(istart+jj) .lt. prtmin(9)) prtmin(9) = work
     &             (istart+jj)
                endif
                if (work(istart+jj) .lt. xmin2) xmin2 = work(istart+jj)
                if (work(istart+jj) .gt. xmax2) xmax2 = work(istart+jj)
              enddo
              work(istart+icount+1) = xmax2
              work(istart+icount+2) = xmin2
            endif
            if (k2 .ne. 0) then
              istar2 = kstrt + 17*(icount+2)
              ist18 = istar2
              ij = ij + 1
              xmax1 = 0.0
              xmax2 = 0.0
              xmin1 = 0.0
              xmin2 = 0.0
              do jj = 1, icount
                work(istar2+jj) = work(ist4+jj)*work(ist6+jj)
                if (t(jj) .ge. wtim1 .and. t(jj) .le. wtim2 .or. wtim2
     &           .eq. -1.0) then
                  if (work(istar2+jj) .gt. prtmax(10)) prtmax(10) =
     &             work(istar2+jj)
                  if (work(istar2+jj) .lt. prtmin(10)) prtmin(10) =
     &             work(istar2+jj)
                endif
                if (work(istar2+jj) .lt. xmin1) xmin1 = work(istar2+jj)
                if (work(istar2+jj) .gt. xmax1) xmax1 = work(istar2+jj)
              enddo
              work(istar2+icount+1) = xmax1
              work(istar2+icount+2) = xmin1
            endif
C           
C           EXTINCTION ANGLE
C           
            if (k1 .ne. 0) then
              radcon = 57.29578
              istart = kstrt + 8*(icount+2)
              ist9 = istart
              ij = ij + 1
              xmax2 = 0.0
              xmin2 = 0.0
              do jj = 1, icount
                work(istart+jj) = work(istart+jj)*radcon
                if (t(jj) .ge. wtim1 .and. t(jj) .le. wtim2 .or. wtim2
     &           .eq. -1.0) then
                  if (work(istart+jj) .gt. prtmax(11)) prtmax(11) =
     &             work(istart+jj)
                  if (work(istart+jj) .lt. prtmin(11)) prtmin(11) =
     &             work(istart+jj)
                endif
                if (work(istart+jj) .lt. xmin2) xmin2 = work(istart+jj)
                if (work(istart+jj) .gt. xmax2) xmax2 = work(istart+jj)
              enddo
              work(istart+icount+1) = xmax2
              work(istart+icount+2) = xmin2
            endif
            if (k2 .ne. 0) then
              radcon = 57.29578
              istar2 = kstrt + 9*(icount+2)
              ist10 = istar2
              ij = ij + 1
              xmax1 = 0.0
              xmin1 = 0.0
              do jj = 1, icount
                work(istar2+jj) = work(istar2+jj)*radcon
                if (t(jj) .ge. wtim1 .and. t(jj) .le. wtim2 .or. wtim2
     &           .eq. -1.0) then
                  if (work(istar2+jj) .gt. prtmax(12)) prtmax(12) =
     &             work(istar2+jj)
                  if (work(istar2+jj) .lt. prtmin(12)) prtmin(12) =
     &             work(istar2+jj)
                endif
                if (work(istar2+jj) .lt. xmin1) xmin1 = work(istar2+jj)
                if (work(istar2+jj) .gt. xmax1) xmax1 = work(istar2+jj)
              enddo
              work(istar2+icount+1) = xmax1
              work(istar2+icount+2) = xmin1
            endif
C           
C           V ALPHA
C           
            if (k1 .ne. 0) then
              istart = kstrt + 10*(icount+2)
              ist11 = istart
              ij = ij + 1
              xmax2 = 0.0
              xmin2 = 0.0
              do jj = 1, icount
                if (t(jj) .ge. wtim1 .and. t(jj) .le. wtim2 .or. wtim2
     &           .eq. -1.0) then
                  if (work(istart+jj) .gt. prtmax(13)) prtmax(13) =
     &             work(istart+jj)
                  if (work(istart+jj) .lt. prtmin(13)) prtmin(13) =
     &             work(istart+jj)
                endif
                if (work(istart+jj) .lt. xmin2) xmin2 = work(istart+jj)
                if (work(istart+jj) .gt. xmax2) xmax2 = work(istart+jj)
              enddo
              work(istart+icount+1) = xmax2
              work(istart+icount+2) = xmin2
            endif
            if (k2 .ne. 0) then
              istar2 = kstrt + 11*(icount+2)
              ist12 = istar2
              ij = ij + 1
              xmax1 = 0.0
              xmin1 = 0.0
              do jj = 1, icount
                if (t(jj) .ge. wtim1 .and. t(jj) .le. wtim2 .or. wtim2
     &           .eq. -1.0) then
                  if (work(istar2+jj) .gt. prtmax(14)) prtmax(14) =
     &             work(istar2+jj)
                  if (work(istar2+jj) .lt. prtmin(14)) prtmin(14) =
     &             work(istar2+jj)
                endif
                if (work(istar2+jj) .lt. xmin1) xmin1 = work(istar2+jj)
                if (work(istar2+jj) .gt. xmax1) xmax1 = work(istar2+jj)
              enddo
              work(istar2+icount+1) = xmax1
              work(istar2+icount+2) = xmin1
            endif
C           
C           V ALPHA PRIME
C           
            if (k1 .ne. 0) then
              istart = kstrt + 12*(icount+2)
              ist13 = istart
              ij = ij + 1
              xmax2 = 0.0
              xmin2 = 0.0
              do jj = 1, icount
                if (t(jj) .ge. wtim1 .and. t(jj) .le. wtim2 .or. wtim2
     &           .eq. -1.0) then
                  if (work(istart+jj) .gt. prtmax(15)) prtmax(15) =
     &             work(istart+jj)
                  if (work(istart+jj) .lt. prtmin(15)) prtmin(15) =
     &             work(istart+jj)
                endif
                if (work(istart+jj) .lt. xmin2) xmin2 = work(istart+jj)
                if (work(istart+jj) .gt. xmax2) xmax2 = work(istart+jj)
              enddo
              work(istart+icount+1) = xmax2
              work(istart+icount+2) = xmin2
              if (k2 .ne. 0) then
                istar2 = kstrt + 13*(icount+2)
                ist14 = istar2
                ij = ij + 1
                xmax1 = 0.0
                xmin1 = 0.0
                do jj = 1, icount
                  if (t(jj) .ge. wtim1 .and. t(jj) .le. wtim2 .or.
     &             wtim2 .eq. -1.0) then
                    if (work(istar2+jj) .gt. prtmax(16)) prtmax(16) =
     &               work(istar2+jj)
                    if (work(istar2+jj) .lt. prtmin(16)) prtmin(16) =
     &               work(istar2+jj)
                  endif
                  if (work(istar2+jj) .lt. xmin1) xmin1 = work(istar2
     &             +jj)
                  if (work(istar2+jj) .gt. xmax1) xmax1 = work(istar2
     &             +jj)
                enddo
                work(istar2+icount+1) = xmax1
                work(istar2+icount+2) = xmin1
              endif
            endif
C           
C           ANGLE OF OVERLAP
C           
            if (k1 .ne. 0) then
              istart = kstrt + 14*(icount+2)
              ist15 = istart
              ij = ij + 1
              xmax2 = 0.0
              xmin2 = 0.0
              do jj = 1, icount
                work(istart+jj) = work(ist9+jj) - work(ist1+jj)
                if (t(jj) .ge. wtim1 .and. t(jj) .le. wtim2 .or. wtim2
     &           .eq. -1.0) then
                  if (work(istart+jj) .gt. prtmax(17)) prtmax(17) =
     &             work(istart+jj)
                  if (work(istart+jj) .lt. prtmin(17)) prtmin(17) =
     &             work(istart+jj)
                endif
                if (work(istart+jj) .lt. xmin2) xmin2 = work(istart+jj)
                if (work(istart+jj) .gt. xmax2) xmax2 = work(istart+jj)
              enddo
              work(istart+icount+1) = xmax2
              work(istart+icount+2) = xmin2
            endif
            if (k2 .ne. 0) then
              istar2 = kstrt + 15*(icount+2)
              ist16 = istar2
              ij = ij + 1
              xmax1 = 0.0
              xmin1 = 0.0
              do jj = 1, icount
                work(istar2+jj) = work(ist10+jj) - work(ist2+jj)
                if (t(jj) .ge. wtim1 .and. t(jj) .le. wtim2 .or. wtim2
     &           .eq. -1.0) then
                  if (work(istar2+jj) .gt. prtmax(18)) prtmax(18) =
     &             work(istar2+jj)
                  if (work(istar2+jj) .lt. prtmin(18)) prtmin(18) =
     &             work(istar2+jj)
                endif
                if (work(istar2+jj) .lt. xmin1) xmin1 = work(istar2+jj)
                if (work(istar2+jj) .gt. xmax1) xmax1 = work(istar2+jj)
              enddo
              work(istar2+icount+1) = xmax1
              work(istar2+icount+2) = xmin1
            endif
C           
C           CONVERT THE EXTINCTION ANGLE TO EXTINCTION ADVANCE ANG
C           IF THIS IS AN INVERTER
C           
            if (k1 .ne. 0 .and. work(ist1+1) .gt. 90.) then
              prtmax(11) =  - 10000.
              prtmin(11) = 10000.
              do jj = 1, icount
                work(ist9+jj) = 180. - work(ist9+jj)
                if (t(jj) .ge. wtim1 .and. t(jj) .le. wtim2 .or. wtim2
     &           .eq. -1.0) then
                  if (work(ist9+jj) .gt. prtmax(11)) prtmax(11) = work
     &             (ist9+jj)
                  if (work(ist9+jj) .lt. prtmin(11)) prtmin(11) = work
     &             (ist9+jj)
                endif
              enddo
              worksp(ist9+icount+1) = 180. - worksp(ist9+icount+1)
              worksp(ist9+icount+2) = 180. - worksp(ist9+icount+2)
            endif
            if (k2 .ne. 0 .and. work(ist2+1) .gt. 90.) then
              prtmax(12) =  - 10000.
              prtmin(12) = 10000.
              do jj = 1, icount
                work(ist10+jj) = 180. - work(ist10+jj)
                if (t(jj) .ge. wtim1 .and. t(jj) .le. wtim2 .or. wtim2
     &           .eq. -1.0) then
                  if (work(ist10+jj) .gt. prtmax(12)) prtmax(12) = work
     &             (ist10+jj)
                  if (work(ist10+jj) .lt. prtmin(12)) prtmin(12) = work
     &             (ist10+jj)
                endif
              enddo
              worksp(ist10+icount+1) = 180. - worksp(ist10+icount+1)
 
              worksp(ist10+icount+2) = 180. - worksp(ist10+icount+2)
            endif
C           
C           THE FOLLOWING LOGIC LISTS QUANTITIES IF OPTION SO SPECIF
C           OUTPUT DC BUS CURRENT
 
            if (idcsw3 .gt. 10) then
              idcsw3 = idcsw3 - 10
              call mvdcx(ib1, 2, ist3)
            endif
            if (prntop(idcsw3)) then
              istart = ist3
              write (outbuf, 10090) ib1c, b1, prtmax(3), prtmin(3)
10090         format ('0DC BUS CURRENT (KILOAMPS) AT ', a8, f7.1, 5x,
     &         ' MAX = ', f8.4, ' MIN = ', f8.4)
              call prtout(1)
              do jjj = 1, icount, 5
                kkk = min0(jjj+4, icount)
                write (outbuf, 10100) (t(jj), work(istart+jj), jj =
     &           jjj, kkk)
10100         format (5(2x, f7.2, ' CYCLES ', f8.4))
                call prtout(1)
              enddo
            endif
            if (auxop(idcsw3)) call dcaux(dname, 'DC BUS1 CURRENT',
     &       icount, t, work(ist3+1))
C           1                     ('DC BUS1 CURRENT',ICOUNT,T,WORK(IST3+
 
C           OUTPUT DC BUS VOLTAGE
 
            if (idcsw5 .gt. 10) then
              idcsw5 = idcsw5 - 10
              call mvdcx(ib1, 3, ist5)
            endif
            if (prntop(idcsw5)) then
              istart = ist5
              write (outbuf, 10110) ib1c, b1, prtmax(5), prtmin(5)
10110         format ('0DC BUS VOLTAGE (KILOVOLTS) AT ', a8, f7.1, 5x,
     &         ' MAX =', f8.2, ' MIN = ', f8.2)
              call prtout(1)
              do jjj = 1, icount, 5
                kkk = min0(jjj+4, icount)
                write (outbuf, 10120) (t(jj), work(istart+jj), jj =
     &           jjj, kkk)
10120         format (5(2x, f7.2, ' CYCLES ', f8.2))
                call prtout(1)
              enddo
            endif
            if (auxop(idcsw5)) call dcaux(dname, 'DC BUS1 VOLTAGE',
     &       icount, t, work(ist5+1))
C           1              ('DC BUS1 VOLTAGE',ICOUNT,T,WORK(IST5+1))
 
C           OUTPUT DC BUS POWER
 
            if (idsw17 .gt. 10) then
              idsw17 = idsw17 - 10
              call mvdcx(ib1, 9, ist17)
            endif
            if (prntop(idsw17)) then
              istart = ist17
              write (outbuf, 10130) ib1c, b1, prtmax(9), prtmin(9)
10130         format ('0DC BUS POWER (MW) AT ', a8, f7.1, 5x, ' MAX = '
     &         , f8.2, ' MIN = ', f8.2)
              call prtout(1)
              do jjj = 1, icount, 5
                kkk = min0(jjj+4, icount)
                write (outbuf, 10140) (t(jj), work(istart+jj), jj =
     &           jjj, kkk)
10140           format (5(2x, f7.2, ' CYCLES ', f8.2))
                call prtout(1)
              enddo
            endif
            if (auxop(idsw17)) call dcaux(dname, 'DC BUS1 POWER',
     &       icount, t, work(ist17+1))
C           1               ('DC BUS1 POWER',ICOUNT,T,WORK(IST17+1))
 
C           OUTPUT DC BUS FIRING ANGLE
 
            if (idcsw1 .gt. 10) then
              idcsw1 = idcsw1 - 10
              call mvdcx(ib1, 1, ist1)
            endif
            if (prntop(idcsw1)) then
              istart = ist1
              write (outbuf, 10150) ib1c, b1, prtmax(1), prtmin(1)
10150         format ('0DC BUS FIRING ANGLE (DEGREES) AT ', a8, f7.1,
     &         5x, ' MAX = ', f7.2, ' MIN = ', f7.2)
              call prtout(1)
              do jjj = 1, icount, 5
                kkk = min0(jjj+4, icount)
                write (outbuf, 10160) (t(jj), work(istart+jj), jj =
     &           jjj, kkk)
10160           format (5(2x, f7.2, ' CYCLES ', f7.2))
                call prtout(1)
              enddo
            endif
            if (auxop(idcsw1)) call dcaux(dname, 'DC BUS1 FIRING ANGLE'
     &       , icount, t, work(ist1+1))
C           1                ('DC BUS1 FIRING ANGLE',ICOUNT,T,WORK(IST1+
 
C           OUTPUT DC MODULATION SIGNAL
 
            if (idcsw7 .gt. 10) then
              idcsw7 = idcsw7 - 10
              call mvdcx(ib1, 4, ist7)
            endif
            if (prntop(idcsw7)) then
              istart = ist7
              write (outbuf, 10170) ib1c, b1, prtmax(7), prtmin(7)
10170         format ('0DC BUS MODULATION SIGNAL AT ', a8, f7.1, 5x,
     &         ' MAX = ', f8.2, ' MIN = ', f8.2)
              call prtout(1)
              lblcde = idcmd1(i)
              if (lblcde .le. 0) lblcde = 7
              write (outbuf, 10180) modlbl(lblcde)
              call prtout(1)
10180         format ('0', 5x, a20)
              do jjj = 1, icount, 5
                kkk = min0(jjj+4, icount)
                write (outbuf, 10190) (t(jj), work(istart+jj), jj =
     &           jjj, kkk)
10190           format (5(2x, f7.2, ' CYCLES ', f8.2))
                call prtout(1)
              enddo
            endif
            if (auxop(idcsw7)) call dcaux(dname, 'DC BUS1 MOD SIG',
     &       icount, t, work(ist7+1))
C           1                     ('DC BUS1 MOD SIG',ICOUNT,T,WORK(IST7+
 
C           OUTPUT V ALPHA
 
            if (idsw11 .gt. 10) then
              idsw11 = idsw11 - 10
              call mvdcx(ib1, 6, ist11)
            endif
            if (prntop(idsw11)) then
              istart = ist11
              write (outbuf, 10200) ib1c, b1, prtmax(13), prtmin(13)
10200         format ('0DC BUS V ALPHA (PER UNIT) AT ', a8, f7.1, 5x,
     &         ' MAX = ', f7.2, ' MIN = ', f7.2)
              call prtout(1)
              do jjj = 1, icount, 5
                kkk = min0(jjj+4, icount)
                write (outbuf, 10210) (t(jj), work(istart+jj), jj =
     &           jjj, kkk)
10210           format (5(2x, f7.2, ' CYCLES ', f7.2))
                call prtout(1)
              enddo
            endif
            if (auxop(idsw11)) call dcaux(dname, 'DC BUS1 V ALPHA',
     &       icount, t, work(ist11+1))
C           1                     ('DC BUS1 V ALPHA',ICOUNT,T,WORK(IST11
C           
C            OUTPUT V ALPHA PRIME
C           
            if (idsw13 .gt. 10) then
              idsw13 = idsw13 - 10
              call mvdcx(ib1, 7, ist13)
            endif
            if (prntop(idsw13)) then
              istart = ist13
              write (outbuf, 10220) ib1c, b1, prtmax(15), prtmin(15)
10220         format ('0DC BUS V ALPHA PRIME (PER UNIT) AT ', a8, f7.1,
     &         5x, ' MAX = ', f7.2, ' MIN = ', f7.2)
              call prtout(1)
              do jjj = 1, icount, 5
                kkk = min0(jjj+4, icount)
                write (outbuf, 10210) (t(jj), work(istart+jj), jj =
     &           jjj, kkk)
                call prtout(1)
              enddo
            endif
            if (auxop(idsw13)) call dcaux(dname,
     &       'DC BUS1 V ALPHA PRIME', icount, t, work(ist13+1))
C           1             ('DC BUS1 V ALPHA PRIME',ICOUNT,T,WORK(IST13+1
C           
C            OUTPUT EXTINCTION ANGLE
C           
            if (idcsw9 .gt. 10) then
              idcsw9 = idcsw9 - 10
              call mvdcx(ib1, 5, ist9)
            endif
            if (prntop(idcsw9)) then
              istart = ist9
              write (outbuf, 10230) ib1c, b1, prtmax(11), prtmin(11)
10230         format ('0DC BUS EXTINCTION ANGLE (DEGREES) AT ', a8,
     &         f7.1, 5x, ' MAX = ', f7.2, ' MIN = ', f7.2)
              call prtout(1)
              do jjj = 1, icount, 5
                kkk = min0(jjj+4, icount)
                write (outbuf, 10240) (t(jj), work(istart+jj), jj =
     &           jjj, kkk)
10240           format (5(2x, f7.2, ' CYCLES ', f7.2))
                call prtout(1)
              enddo
            endif
            if (auxop(idcsw9)) call dcaux(dname, 'DC BUS1 EXTINC ANGLE'
     &       , icount, t, work(ist9+1))
C           1              ('DC BUS1 EXTINC ANGLE',ICOUNT,T,WORK(IST9+1)
C           
C            OUTPUT ANGLE OF OVERLAP
C           
            if (idsw15 .gt. 10) then
              idsw15 = idsw15 - 10
              call mvdcx(ib1, 8, ist15)
            endif
            if (prntop(idsw15)) then
              istart = ist15
              write (outbuf, 10250) ib1c, b1, prtmax(17), prtmin(17)
10250         format ('0DC BUS ANGLE OF OVERLAP (DEGREES) AT ', a8,
     &         f7.1, 5x, ' MAX = ', f7.2, ' MIN = ', f7.2)
              call prtout(1)
              do jjj = 1, icount, 5
                kkk = min0(jjj+4, icount)
                write (outbuf, 10240) (t(jj), work(istart+jj), jj =
     &           jjj, kkk)
                call prtout(1)
              enddo
            endif
            if (auxop(idsw15)) call dcaux(dname,
     &       'DC BUS1 OVERLAP ANGLE', icount, t, work(ist15+1))
C           1            ('DC BUS1 OVERLAP ANGLE',ICOUNT,T,WORK(IST15+1)
 
C           OUTPUT DC BUS CURRENT
 
            if (idcsw4 .gt. 10) then
              idcsw4 = idcsw4 - 10
              call mvdcx(ib2, 2, ist4)
            endif
            if (prntop(idcsw4)) then
              istart = ist4
              write (outbuf, 10090) ib2c, b2, prtmax(4), prtmin(4)
              call prtout(1)
              do jjj = 1, icount, 5
                kkk = min0(jjj+4, icount)
                write (outbuf, 10260) (t(jj), work(istart+jj), jj =
     &           jjj, kkk)
10260           format (5(2x, f7.2, ' CYCLES ', f8.4))
                call prtout(1)
              enddo
            endif
            if (auxop(idcsw4)) call dcaux(dname, 'DC BUS2 CURRENT',
     &       icount, t, work(ist4+1))
C           1                 ('DC BUS2 CURRENT',ICOUNT,T,WORK(IST4+1))
 
C           SWITCHING FOR DC BUS VOLTAGE
 
            if (idcsw6 .gt. 10) then
              idcsw6 = idcsw6 - 10
              call mvdcx(ib2, 3, ist6)
            endif
            if (prntop(idcsw6)) then
              istart = ist6
              write (outbuf, 10110) ib2c, b2, prtmax(6), prtmin(6)
              call prtout(1)
              do jjj = 1, icount, 5
                kkk = min0(jjj+4, icount)
                write (outbuf, 10270) (t(jj), work(istart+jj), jj =
     &           jjj, kkk)
10270           format (5(2x, f7.2, ' CYCLES ', f8.2))
                call prtout(1)
              enddo
            endif
            if (auxop(idcsw6)) call dcaux(dname, 'DC BUS2 VOLTAGE',
     &       icount, t, work(ist6+1))
C           1               ('DC BUS2 VOLTAGE',ICOUNT,T,WORK(IST6+1))
 
C           SWITCHING FOR DC BUS POWER
 
            if (idsw18 .gt. 10) then
              idsw18 = idsw18 - 10
              call mvdcx(ib2, 9, ist18)
            endif
            if (prntop(idsw18)) then
              istart = ist18
              write (outbuf, 10130) ib2c, b2, prtmax(10), prtmin(10)
              call prtout(1)
              do jjj = 1, icount, 5
                kkk = min0(jjj+4, icount)
                write (outbuf, 10280) (t(jj), work(istart+jj), jj =
     &           jjj, kkk)
10280           format (5(2x, f7.2, ' CYCLES ', f8.2))
                call prtout(1)
              enddo
            endif
            if (auxop(idsw18)) call dcaux(dname, 'DC BUS2 POWER',
     &       icount, t, work(ist18+1))
C           1                ('DC BUS2 POWER',ICOUNT,T,WORK(IST18+1))
 
C           OUTPUT DC BUS FIRING ANGLE
 
            if (idcsw2 .gt. 10) then
              idcsw2 = idcsw2 - 10
              call mvdcx(ib2, 1, ist2)
            endif
            if (prntop(idcsw2)) then
              istart = ist2
              write (outbuf, 10150) ib2c, b2, prtmax(2), prtmin(2)
              call prtout(1)
              do jjj = 1, icount, 5
                kkk = min0(jjj+4, icount)
                write (outbuf, 10290) (t(jj), work(istart+jj), jj =
     &           jjj, kkk)
10290           format (5(2x, f7.2, ' CYCLES ', f7.2))
                call prtout(1)
              enddo
            endif
            if (auxop(idcsw2)) call dcaux(dname, 'DC BUS2 FIRING ANGLE'
     &       , icount, t, work(ist2+1))
C           1                ('DC BUS2 FIRING ANGLE',ICOUNT,T,WORK(IST2+
 
C           OUTPUT DC MODULATION SIGNAL
 
            if (idcsw8 .gt. 10) then
              idcsw8 = idcsw8 - 10
              call mvdcx(ib2, 4, ist8)
            endif
            if (prntop(idcsw8)) then
              istart = ist8
              write (outbuf, 10170) ib2c, b2, prtmax(8), prtmin(8)
              call prtout(1)
              lblcde = idcmd2(i)
              if (lblcde .le. 0) lblcde = 7
              write (outbuf, 10300) modlbl(lblcde)
              call prtout(1)
10300         format ('0', 5x, a20)
              do jjj = 1, icount, 5
                kkk = min0(jjj+4, icount)
                write (outbuf, 10310) (t(jj), work(istart+jj), jj =
     &           jjj, kkk)
10310           format (5(2x, f7.2, ' CYCLES ', f8.2))
                call prtout(1)
              enddo
            endif
            if (auxop(idcsw8)) call dcaux(dname, 'DC BUS2 MOD SIG',
     &       icount, t, work(ist8+1))
C           1                     ('DC BUS2 MOD SIG',ICOUNT,T,WORK(IST8+
 
C           OUTPUT V ALPHA
 
            if (idsw12 .gt. 10) then
              idsw12 = idsw12 - 10
              call mvdcx(ib2, 6, ist12)
            endif
            if (prntop(idsw12)) then
              istart = ist12
              write (outbuf, 10200) ib2c, b2, prtmax(14), prtmin(14)
              call prtout(1)
              do jjj = 1, icount, 5
                kkk = min0(jjj+4, icount)
                write (outbuf, 10210) (t(jj), work(istart+jj), jj =
     &           jjj, kkk)
                call prtout(1)
              enddo
            endif
            if (auxop(idsw12)) call dcaux(dname, 'DC BUS2 V ALPHA',
     &       icount, t, work(ist12+1))
C           1                 ('DC BUS2 V ALPHA',ICOUNT,T,WORK(IST12+1))
C           
C           OUTPUT V ALPHA PRIME
C           
            if (idsw14 .gt. 10) then
              idsw14 = idsw14 - 10
              call mvdcx(ib2, 7, ist14)
            endif
            if (prntop(idsw14)) then
              istart = ist14
              write (outbuf, 10220) ib2c, b2, prtmax(16), prtmin(16)
              call prtout(1)
              do jjj = 1, icount, 5
                kkk = min0(jjj+4, icount)
                write (outbuf, 10210) (t(jj), work(istart+jj), jj =
     &           jjj, kkk)
                call prtout(1)
              enddo
            endif
            if (auxop(idsw14)) call dcaux(dname,
     &       'DC BUS2 V ALPHA PRIME', icount, t, work(ist14+1))
C           1             ('DC BUS2 V ALPHA PRIME',ICOUNT,T,WORK(IST14+1
C           
C           OUTPUT EXTINCTION ANGLE
C           
            if (idsw10 .gt. 10) then
              idsw10 = idsw10 - 10
              call mvdcx(ib2, 5, ist10)
            endif
            if (prntop(idsw10)) then
              istart = ist10
              write (outbuf, 10230) ib2c, b2, prtmax(12), prtmin(12)
              call prtout(1)
              do jjj = 1, icount, 5
                kkk = min0(jjj+4, icount)
                write (outbuf, 10240) (t(jj), work(istart+jj), jj =
     &           jjj, kkk)
                call prtout(1)
              enddo
            endif
            if (auxop(idsw10)) call dcaux(dname, 'DC BUS2 EXTINC ANGLE'
     &       , icount, t, work(ist10+1))
C           1               ('DC BUS2 EXTINC ANGLE',ICOUNT,T,WORK(IST10+
C           
C           OUTPUT ANGLE OF OVERLAP
C           
            if (idsw16 .gt. 10) then
              idsw16 = idsw16 - 10
              call mvdcx(ib2, 8, ist16)
            endif
            if (prntop(idsw16)) then
              istart = ist16
              write (outbuf, 10250) ib2c, b2, prtmax(18), prtmin(18)
              call prtout(1)
              do jjj = 1, icount, 5
                kkk = min0(jjj+4, icount)
                write (outbuf, 10240) (t(jj), work(istart+jj), jj =
     &           jjj, kkk)
                call prtout(1)
              enddo
            endif
            if (auxop(idsw16)) call dcaux(dname,
     &       'DC BUS2 OVERLAP ANGLE', icount, t, work(ist16+1))
C           1              ('DC BUS2 OVERLAP ANGLE',ICOUNT,T,WORK(IST16+
          endif
C         1          ('DC BRANCH POWER, OUT R. BUS',ICOUNT,T,WORK(IST13+
C         
C         STORE ARRAY WORK IN MASS STORAGE FOR USE IN PLOT
C         
          call writmp (1, work(kstrt1), 18*(icount+2), 1)
          call ritecp(msub1(2), kdcplt+i, 1)
        endif
      enddo
      if (idcbr .eq. 0) then
      endif
c     
c     CALCULATE AND OUTPUT DC LINE QUANTITIES
c     
      kwork = kdcplt + iddat + 1
      return
      end
