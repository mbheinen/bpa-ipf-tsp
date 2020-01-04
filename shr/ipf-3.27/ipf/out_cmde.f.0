C****************************************************************
C
C       File: out_cmde.f
C       Purpose: Routine to solve COMMON-MODE-OUTAGE 
C
C       Author: Walt Powell  Date: 7 Mar 1995
C                            Modified:
C       Called by: apsoln
C
C****************************************************************
	subroutine out_cmde (error)
        integer error
 
        include 'ipfinc/parametr.inc'
 
        include 'ipfinc/comm_mode.inc'
        include 'ipfinc/apcom.inc'
        include 'ipfinc/intbus.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/time1.inc'
 
c       Note: This variable can be changed with a symbolic debugger
        common /term_debug/ iterm
 
        common /sort_cmde/ num_sort, sort(200)
        integer num_sort, sort

        common /scratch/ kolum(MAXBUS), net(200), mtrx(MAXBUS), 
     &                   last_out, comm_out(MAXBUS), num_isol,
     &                   isol(100)
        integer last_out, comm_out

        integer solv_cmde, status
 
C       Loop over all the outages
 
        do jout = 1, num_comm_mode
           ipbad(nout+jout) = iib
           ibad_rs(nout+jout) = 0
           if (iterm .ne. 0) then
              write (*, 10000) jout, comm_mode(jout)
           else if (mod (jout, 10) .eq. 0) then
              ipct = 100.0 * float (jout) / float (num_comm_mode)
              write (*, 10000) jout,  comm_mode(jout), ipct
           endif
 
           if (idebug .ne. 0) then
              write (dbug, 10000) jout, comm_mode(jout)
10000         format (' Commom-mode outage No. ', i4, 2x, a, 
     &                ' % completed ', i4)
           endif
 
C          Copy base-case voltage and angle to EI and THI
C          as initial guesses
 
           call fvmov(vmag, 1, ei, 1, nbus)
           call fvmov(vangle, 1, thi, 1, nbus)
           error = 0
c
c          Modify data as imputed by outage.  Note that subroutine
c          "ymod_cmde" defines array "sort(num_sort)", which consists
c          of the list of deleted buses.
c
           noconv = 0
           call ymod_cmde (jout, -1, noconv)
           iq = nbus - 1
c
c          Check for isolated subsystems
c
           call cknt_cmde (jout, error)
           if (error .eq. 0) then

C             Form and factor B' and B" matrices
 
              call fvmov(vmag, 1, ei, 1, nbus)
              call bprim()
              call bdprim()
 
C             Find power flow for outage
 
              status = solv_cmde (noconv, iqsww, jout)
 
              if ( noconv .eq. 1 .and. ipqsln .eq. 0 ) then

c#############  solv_cmde  pass 2  with no reactive   ###################

                noconv = 0
c  allow 2 "bad" entries
c                iib = iib - 1
c                ibad(iib) = 0
c                bad(iib) = 0.
                ibad_rs(nout+jout) = 1
                ipqsln = 1

                call fvmov(vmag, 1, ei, 1, nbus)
                call fvmov(vangle, 1, thi, 1, nbus)
 
                if (iqsww .ne. 0) then
                  do i = 1, igenq
                    k = iqlim(i)
                    if (k .gt. 0) then

C                     First check any BQ_min or BQ_max which were restored
C                     to PV.

                      if (qstate(i) .eq. 1) then

C                       Unconditionally restore all other PQ_limit buses
C                       to PV.

                      if (ipqv(k) .lt. 0) ipqv(k) = 0
                      elseif (qstate(i) .eq. 3) then
                        ipqv(k) =  - 1
                        qnet1(k) = qlow(i)
                      elseif (qstate(i) .eq. 4) then
                        ipqv(k) =  - 2
                        qnet1(k) = qhi(i)
                      endif
                    endif
                  enddo

                endif

                call bprim()
                call bdprim()

                ilog = solv_cmde(noconv, iqsww, jout)
                ipqsln = 0
                if ( noconv .eq. 1 ) ibad_rs(nout+jout) = 2

              endif
c########  End of solv_cmde pass 2   ###################################

C             Test for abnormal errors
 
              if (status .gt. 0) then
                 iflag = 4
                 if (iib .lt. MXIBAD) then
                    ibad(iib) = 0
                    bad(iib) = iflag
                    iib = iib + 1
                 elseif (iib .eq. MXIBAD) then
                    ibad(iib) = 0
                    bad(iib) = iflag
                    iib = iib + 1
                    write (errbuf(1), 10010) jout, nout, iflag, iib
10010               format (' Overflow of "IBAD" array: ', 
     &                'JOUT,NOUT,IFLAG,IIB = ', 4i8)
                    call prterx('E', 1)
                 endif
                 noconv = 1
 
                 if (iterm .ne. 0) write (*, 10020) jout
 
                 if (idebug .ne. 0) write (dbug, 10020) jout
10020            format ('0  Abnormal termination - Outage ', i4)
 
              endif
           else
              noconv = 1
c
c             Flag individual buses "Outage isolates buses"
c
              do k = 1, num_isol
                 iflag = -isol(k)
                 if (iib .lt. MXIBAD) then
                    ibad(iib) = 0
                    bad(iib) = iflag
                    iib = iib + 1
                 elseif (iib .eq. MXIBAD) then
                    ibad(iib) = 0
                    bad(iib) = iflag
                    iib = iib + 1
                    write (errbuf(1), 10010) jout, nout, iflag, iib
                    call prterx('E', 1)
                 endif
              enddo
           endif
 
C          Restore admittance array to original form
 
           call ymod_cmde(jout, 1, noconv)
 
C          Check limits
 
           if (noconv .eq. 0) then
              call check(jout+nout)
           else
           endif 
C          Clean up IPQV array and restore B" before next outage
 
           if (iqsww .ne. 0) then
              do i = 1, igenq
                 k = iqlim(i)
                 if (k .gt. 0) then
 
C                   First check any BQ_min or BQ_max which were 
c                   restored to PV.
 
                    if (qstate(i) .eq. 1) then
 
C                      Unconditionally restore all other PQ_limit buses 
C                      to PV.
 
                       if (ipqv(k) .lt. 0) ipqv(k) = 0
                    elseif (qstate(i) .eq. 3) then
                       ipqv(k) =  - 1
                       qnet1(k) = qlow(i)
                    elseif (qstate(i) .eq. 4) then
                       ipqv(k) =  - 2
                       qnet1(k) = qhi(i)
                    endif
                 endif
              enddo
 
           endif
           if (iib .gt. MXIBAD) goto 900
           ipbad(nout+num_comm_mode+1) = iib
        enddo
        goto 910
 
  900   do i = jout+nout+1, nout+num_comm_mode+1
           ipbad(i) = iib
        enddo
  910   return
        end
