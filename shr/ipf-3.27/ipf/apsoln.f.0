C    @(#)apsoln.f	20.4 8/20/98
      subroutine apsoln

C     SOLUTION PORTION OF FAST OUTAGE PROGRAM
C     DIMENSIONED FOR 2000 NODES AND 3000 BRANCHES
C     ROUTINE SHOULD BE COMPILED WITH /NOI4 OPTIO

      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/apcom.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/comm_mode.inc'

      common /time1/time(10), idebug
      common /toler/toler(MXCPBS)

      common /common_mode_only/ common_mode_only
      logical common_mode_only
c
c     Note: This variable can be changed with a symbolic debugger or
c           with the logical name DEBUG_OUTAGE_SIMULATION = ON

      common /term_debug/ iterm
c
      integer pflts, dirio, bufio, error
      character temp*20, tempname*60
C
C     ACTUAL CODING FOR FAST OUTAGE BEGINS HERE
C     INITIALIZE ADMITTANCE-DATA ARRAYS AND OTHER ARRAYS
C     FOR COMPUTING P AND Q

      tempname = 'DEBUG_OUTAGE_SIMULATION' // char(0)
      call getenvir  (tempname, temp)
      if (temp .eq. 'ON') then
        iterm = 1
      else
        iterm = 0
      endif

      if (iterm .ne. 0) call outdbg

      ipyu(1, 1) = 1
      ipyl(1, 1) = 1
      do k = 1, nbus
        pnet1(k) = pnetu(k)	
        qnet1(k) = qnetu(k)	
        if (ntypu(k) .eq. 1 .or. ntypu(k) .eq. 4 .or.
     &      ntypu(k) .eq. 5 .or. ntypu(k) .eq. 10 .or.
     &      ntypu(k) .eq. 12) then	
          ipqv(k) = 1
        else
          ipqv(k) = 0
        endif
        cykk(1, k) = gkku(k)	
        cykk(2, k) =  - bkku(k)	
c
c	make sure all types match for change from single to double 
c
        tol = amax1(cx1, 5.0e-6* sngl(dabs(bkku(k))))	
        toler(k) = amin1(tol,(10.0*cx1))
        iptr = ipyu(1, k)
        ipyu(2, k) = 0
        iptrl = ipyl(1, k)
        ipyl(2, k) = 0
        kf = km(k)
        kl = kf + kmlen(k) - 1
        do i = kf, kl
          m = ikmu(i)	!uur
          if (m .lt. k) then
            ipyl(2, k) = ipyl(2, k) + 1	! CODE FOR THE LOWER TRIANGULAR
            mfarl(iptrl) = m
            ykml(1, iptrl) = gkmu(i)	!uur
            ykml(2, iptrl) =  - bkmu(i)	!uur
            iptrl = iptrl + 1
          else
            ipyu(2, k) = ipyu(2, k) + 1	! CODE FOR THE UPPER TRIANGULAR
            mfaru(iptr) = m
            ykmu(1, iptr) = gkmu(i)	!uur
            ykmu(2, iptr) =  - bkmu(i)	!uur
            iptr = iptr + 1
          endif
        enddo
        ipyu(1, k+1) = iptr
        ipyl(1, k+1) = iptrl
      enddo
      if (idebug .gt. 1) then
        write (dbug, 10000) (i, pnet1(i), qnet1(i), cykk(1, i), cykk(2, 
     &   i), i = 1, nbus)
10000   format (/, ' PNET1,QNET1,CYKK', /, (1x, i5, 4f15.5))
        write (dbug, 10010) (j, ipyu(1, j), ipyu(2, j), j = 1, nbus)
10010   format (/, ' IPYU ARRAY', /, (3i10))
        k = ipyu(1, nbus+1)
        write (dbug, 10020) (i, mfaru(i), ykmu(1, i), ykmu(2, i), i = 
     &   1, k)
10020   format (/, ' MFARU,YKMU', /, (2i10, 2f15.5))
        write (dbug, 10030) (j, ipyl(1, j), ipyl(2, j), j = 1, nbus)
10030   format (/, ' IPYL ARRAY', /, (3i10))
        k = ipyl(1, nbus+1)
        write (dbug, 10040) (i, mfarl(i), ykml(1, i), ykml(2, i), i = 
     &   1, k)
10040   format (/, ' MARL,YKML', /, (2i10, 2f15.5))
      endif
      do i = 1, igenq	! Modify for bus types at limits
        kt = iqlim(i)
        if ( kt .gt. 0 ) then
            if (qstate(i) .eq. 3) then
              ipqv(kt) =  - 1
            elseif (qstate(i) .eq. 4) then
              ipqv(kt) =  - 2
            endif
         endif
      enddo
      do i = 1, 10	! Call AP subroutine for the inner loop
        time(i) = 0.
      enddo
      call stats(cpub, clktb, pflts, dirio, bufio)
      if (common_mode_only) then
        nout_temp = nout
        nout = 0
      endif
      call outage()
      if (num_comm_mode .gt. 0) call out_cmde(error)
      call stats(cpue, clkte, pflts, dirio, bufio)
      time(1) = cpue - cpub
      write (dbug, 10050) time(1), time(2)
10050 format (//, ' AP CPU TIME=', f10.5, ' SEC, ELAPSED TIME=', f10.5, 
     & ' SEC')
      call prtime('"AP" SOLN')	! Print out results in IBAD and BAD
      if (idebug .ge. 1) then
        k = nout + 1
        write (dbug, 10060) (ipbad(i), i = 1, k)
10060   format (/, ' IPBAD ARRAY', /, (1x, 20i4))
        write (dbug, 10070) (i, ibad(i), bad(i), i = 1, iib)
10070   format (/, ' BAD AND IBAD', /, (2i10, e15.5))
      endif
      call outlis()
      if (idebug .ge. 1) then
        write (dbug, 10080) (i, ei(i), thi(i), i = 1, nbus)
10080   format ('1'/, ' LAST VOLTAGE SOLUTION', /, (i10, 2f15.6))
      endif
      return
      end
