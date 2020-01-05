C    @(#)solv_cmde.f	20.8 5/27/99
C****************************************************************
C
C       File: solv_cmde.f
C       Purpose: Routine to simulate COMMON-MODE-OUTAGE jout
C
C       Author: Walt Powell  Date: 7 Mar 1995
C                            Modified:
C       Parameters:
C
C          NOCONV = 1    If powerflow did not converge
C                 = 0    Otherwise
C          IQSWW  = 1    If type switching occured in one or more
C                        iterations of the power flow
C                 = 0    Otherwise.
C          JOUT          Commom mode outage index
C
C       Called by: out_cmde
C
C****************************************************************
	integer function solv_cmde (noconv, iqsww, jout)
 
        include 'ipfinc/parametr.inc'
 
        include 'ipfinc/comm_mode.inc'
        include 'ipfinc/apcom.inc'
        include 'ipfinc/intbus.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/time1.inc'
 
        common /toler/ toler(MXCPBS)
        common /capxxx/ out
        integer out
 
        common /sort_cmde/ num_sort, sort(100)
        integer num_sort, sort

c       Note: This variable can be changed with a symbolic debugger
        common /term_debug/ iterm
 
        logical finished
        real c(2)
 
        solv_cmde = 0
        noconv = 0

C       KP and KQ are switches. See Stott's paper. 
 
C       ****************************************************************** 
        kp = 1  ! P-constraints have not converged 
        kq = 1  ! Q-constraints have not converged 

        if (ipqsln .ne. 0) kq = 0 
 
C       IQSWW: 0  - no switching ocuured in any of the iterations 
C              1  - switching occured in one or more iterations 
 
        iqsww = 0 
        kount = 0 
        ktypsw = 0 
        iflag = 0

        mode = 1
        modelast = 0

C       Compute right-hand-side. 
C       Note: RPAD2 contains delta_P and delta_Q without division 
c       by V 

        call stats(cpub, clkt, pflts, dirio, bufio) 
        call getpq() 
        call stats(cpue, clkt, pflts, dirio, bufio) 
        time(5) = time(5) + cpue - cpub 
c
c       Force the computed injection error of any deleted buses to 0.0
c
        do i = 1, num_sort
           kt = sort(i)
           delp(kt) = 0.0
           delq(kt) = 0.0
        enddo

        finished = .false.
        do while (.not. finished)
c
c         Note that this loop is configured such that only one
c         major block can be logically true
c
          if (mode .eq. 1) then
c 
c           Solve P-equations
C           Find max real-power mismatch 
 
            kount = kount + 1 
            perr = 0.0 
            iperr = 1 
            do i = 2, iq 
              if (dim(abs(delp(i)), toler(i)) .gt. perr) then 
                perr = dim(abs(delp(i)), toler(i)) 
                iperr = i 
              endif 
            enddo 
 
            if (iterm .ne. 0) write (*, 10010) kount, jout, perr, 
     &        iperr 
 
            if (idebug .ne. 0) then 
              write (dbug, 10010) kount, jout, perr, iperr 
10010         format (' MAX P MISMATCH - KOUNT ', i2, 
     &         ' COMMON MODE OUTAGE ', i5, ' MAX DELP(K) ', 
     &         f10.3, i10) 
            endif 
C
C           Solve for delta-theta
C
            call stats (cpub,clkt,pflts,dirio,bufio)
            call srsfb (iq,delp(2),ipfbp,fbp,jfbp,dgbp)
c
c           Force the computed angle correction of any deleted buses to 
C           0.0
c
            do i = 1, num_sort
              kt = sort(i)
              delp(kt) = 0.0
            enddo

            if (iterm .ne. 0) then
              call fxmxmg (delp(2),1,c,iq)
              icc = ifix(c(2)) + 1
              write (*,190) kount, jout, c(1), icc
            endif
 
            if (idebug .ne. 0) then
              call fxmxmg (delp(2),1,c,iq)
              icc = ifix(c(2)) + 1
              write (dbug,190) kount, jout, c(1), icc
  190         format (' MAX DEL T ADJ  - KOUNT ', i2,
     &               ' COMMON MODE OUTAGE ', i5, ' MAX DELP(K) ',
     &               f10.3, i10)
            endif
 
            call stats (cpue,clkt,pflts,dirio,bufio)
            time(6) = time(6) + cpue - cpub
            delp(1) = 0.
C
C           RPAD1 has updated theta without compensation
C
            call fvadd (thi, 1, delp, 1, thi, 1, nbus)
C
C           Test if any angle exceeds CX2 radians
C
            call fxmxmg (thi,1,c,nbus)
            if (abs(c(1)) .gt. cx2) then

C             BUS PHASE DRIFTED BEYOND 6.28 RADS 
 
              iflag = 1000.0*amin1(9.99, abs(c(1))) 
              iflag = 10*iflag + 2 
              finished = .true.
            else 
 
              if (perr .le. 0.0) kp = 0

C             Real power converged 

              call stats(cpub, clkt, pflts, dirio, bufio) 
              call getpq() 
              call stats(cpue, clkt, pflts, dirio, bufio) 
              time(5) = time(5) + cpue - cpub 
c
c             Force the computed injection error of any deleted buses 
c             to 0.0
c
              do i = 1, num_sort
                kt = sort(i)
                delp(kt) = 0.0
                delq(kt) = 0.0
              enddo
            endif

            if (iflag .ne. 0) then
              finished = .true.
              nextmode = 0
            else if (kount .gt. itmax) then

C             TOO MANY ITERATIONS 

              iflag = 10*kount + 1 
              finished = .true.
              nextmode = 0
            else if (perr .gt. 1.0) then
              nextmode = 1
            else if (kp .eq. 0 .and. kq .eq. 0)  then
              finished = .true.
            else if (ipqsln .eq. 0) then
              nextmode = 2
            else
              nextmode = 1
            endif

          else if (mode .eq. 2) then
c 
c           Solve Q-equations
C           Find maximum delta Q mismatch 
 
            qerr = 0.0 
            iqerr = 1 
            do i = 2, iq 
              if (dim(abs(delq(i)), toler(i)) .gt. qerr) then 
                qerr = dim(abs(delq(i)), toler(i)) 
                iqerr = i 
              endif 
            enddo 
 
            if (iterm .ne. 0) write (*, 10030) kount, jout, qerr, 
     &        iqerr 
 
            if (idebug .ne. 0) then 
              write (dbug, 10030) kount, jout, qerr, iqerr 
10030         format (' MAX Q MISMATCH - KOUNT ', i2,
     &             ' COMMON MODE OUTAGE ', i5, ' MAX DELQ(K) ',
     &             f10.3, i10)
            endif 
 
C           Solve for delta V 
 
            call stats(cpub, clkt, pflts, dirio, bufio) 
            call srsfb(iq, delq(2), ipfbpp, fbpp, jfbpp, dgbpp) 
            call stats(cpue, clkt, pflts, dirio, bufio) 
            time(6) = time(6) + cpue - cpub 
c
c           Force the computed voltage correction of any deleted 
c           buses to 0.0
c
            do i = 1, num_sort
              kt = sort(i)
              delq(kt) = 0.0
            enddo

            if (iterm .ne. 0) then 
              call fxmxmg(delq(2), 1, c, iq) 
              icc = c(2) + 1 
              write (*, 10040) kount, jout, c(1), icc 
            endif 
 
            if (idebug .ne. 0) then 
              call fxmxmg(delq(2), 1, c, iq) 
              icc = c(2) + 1 
              write (dbug, 10040) kount, jout, c(1), icc 
10040         format (' MAX DEL V ADJ  - KOUNT ', i2, 
     &         ' COMMON MODE OUTAGE ', i5,  
     &         ' MAX DELQ(K) ', f10.3, i10) 
            endif 
 
            call fvadd(ei(2), 1, delq(2), 1, ei(2), 1, iq) 
 
C           Test if any voltage falls below CX4 P.U. 
 
            call fxminv(ei(2), 1, c, iq) 
            if (c(1) .lt. cx4) then

C             BUS VOLTAGE TOO LOW 
 
              iflag = 1000.0*amax1(c(1), 0.0) 
              iflag = 10*iflag + 3 

            else 

              if (qerr .le. 0.0) kq = 0
 
C             Reactive power converged 

              call stats(cpub, clkt, pflts, dirio, bufio) 
              call getpq() 
              call stats(cpue, clkt, pflts, dirio, bufio) 
              time(5) = time(5) + cpue - cpub 
c
c             Force the computed injection error of any deleted buses 
c             to 0.0
c
              do i = 1, num_sort
                kt = sort(i)
                delp(kt) = 0.0
                delq(kt) = 0.0
              enddo
 
            endif

            if (iflag .ne. 0) then
              finished = .true.
              nextmode = 0
            else if (kount .gt. itmax) then

C             TOO MANY ITERATIONS 

              iflag = 10*kount + 1 
              finished = .true.
              nextmode = 0
            else if (perr .lt. 1.0 .and. qerr .lt. 1.0) then
              if (modelast .eq. 1) then
                nextmode = 3
              else
                nextmode = 1
              endif
            else
              nextmode = 1
            endif

          else if (mode .eq. 3) then

C           Call type-switching if P- and Q-constraints have converged 
 
            ktypsw = ktypsw + 1 
            call stats(cpub, clkt, pflts, dirio, bufio) 
            call typesw(iqsw) 
            call stats(cpue, clkt, pflts, dirio, bufio) 
            time(10) = time(10) + cpue - cpub 
 
C           If type switching occured in TYPESW, form and refactor B" 
 
            if (iqsw .ne. 20000) then 
  
              if (iterm .ne. 0) then 
                call fxmxmg(delq(2), 1, c, iq) 
                icc = c(2) + 1 
                write (*, 10000) kount, jout, c(1), icc 
              endif 
 
              if (idebug .ne. 0) then 
                call fxmxmg(delq(2), 1, c, iq) 
                icc = c(2) + 1 
                write (dbug, 10000) kount, jout, c(1), icc 
10000           format (' SWITCHING QMAX - KOUNT ',i2,
     &               ' COMMON MODE OUTAGE ', i5, ' MAX DELQ(K) ', 
     &               f10.3, i10)
              endif 
 
              call stats(cpub, clkt, pflts, dirio, bufio) 
              call bdprim() 
              call stats(cpue, clkt, pflts, dirio, bufio) 
              time(9) = time(9) + cpue - cpub 
              iqsww = 1 
 
              call stats(cpub, clkt, pflts, dirio, bufio) 
              call getpq() 
              call stats(cpue, clkt, pflts, dirio, bufio) 
              time(5) = time(5) + cpue - cpub 
c
c             Force the computed injection error of any deleted buses 
c             to 0.0
c
              do i = 1, num_sort
                kt = sort(i)
                delp(kt) = 0.0
                delq(kt) = 0.0
              enddo
              kq = 1 

            endif

            if (kp .eq. 0 .and. kq .eq. 0)  then
              finished = .true.
              nextmode = 0
            else if (kq .eq. 1 .and. iqsw .ne. 20000) then
              nextmode = 2
            else
              nextmode = 1
            endif
          endif
c
c         Determine next step
c 
          if (nextmode .eq. 0) then
            finished = .true.
          else
            modelast = mode
            mode = nextmode
          endif
        enddo

        if (iflag .ne. 0) then
          if (iterm .ne. 0) write (*, 10050) kount 
          if (idebug .ne. 0) write (dbug, 10050) kount 
10050     format ('  DIVERGENCE AFTER', i4, ' ITERATIONS') 

          if (iib .lt. MXIBAD) then 
            ibad(iib) = 0 
            bad(iib) = iflag 
            iib = iib + 1 
          elseif (iib .eq. MXIBAD) then 
            ibad(iib) = 0 
            bad(iib) = iflag 
            iib = iib + 1 
            write (errbuf(1), 10060) jout, nout, iib 
10060       format (' Overflow of "IBAD" array: JOUT,NOUT,IIB =', 3i8) 
            call prterx('E', 1) 
          endif 
          noconv = 1 
        else
          if (idebug .ge. 1) then 
            write (dbug, 10070) (i, ei(i), thi(i), i = 1, nbus) 
10070       format ('0 LAST VOLTAGE SOLUTION', /, (i10, 2f15.6)) 
          endif 
        endif

        return
        end
