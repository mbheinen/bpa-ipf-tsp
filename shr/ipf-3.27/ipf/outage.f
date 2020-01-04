C    %W% %G%
      subroutine outage

C     This subroutine calls a sequence of subroutines which
C     solves the matrix for one continency.

      include 'ipfinc/parametr.inc'

      include 'ipfinc/apcom.inc'
      include 'ipfinc/intbus.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/time1.inc'

c     Note: This variable can be changed with a symbolic debugger
      common /term_debug/ iterm

      logical ilog, dlflow
      integer status, open_file, pflts, dirio, bufio

C     IQ is the size of the B" matrix.  In version 1, it is
C     computed in BDPRIM.

      iq = nbus - 1

C     Form and factor B' and B" matrices

      call stats(cpub, clkt, pflts, dirio, bufio)
      call fvmov(vmag, 1, ei, 1, nbus)
      call bprim()
      call bdprim()

c***C     Store factored B'' on scratch file in blocks of 2000 words
c***
c***C     Open scratch file
c***
c***      lunusr = 25
c***      call close_file(lunusr)
c***      status = open_file(lunusr, 'fastout.scr', 'U', 'W', iostat)
c***
c***      rewind lunusr
c***      write (lunusr) (ipfbpp(i), i = 1, nbus)
c***      write (lunusr) (dgbpp(i), i = 1, nbus)
c***      k = ipfbpp(nbus)
c***      do i = 1, k, 2000
c***        last = min0(i+1999, k)
c***        write (lunusr) (jfbpp(j), j = i, last)
c***      enddo
c***      do i = 1, k, 2000
c***        last = min0(i+1999, k)
c***        write (lunusr) (fbpp(j), j = i, last)
c***      enddo

c      Store factored B''  into "save" arrays

      do i = 1, nbus
         ipfbpp_save(i) = ipfbpp(i)
         dgbpp_save(i) = dgbpp(i)
      enddo
      k = ipfbpp(nbus)
      do i = 1, k
         jfbpp_save(i) = jfbpp(i)
         fbpp_save(i) = fbpp(i)
      enddo

      call stats(cpue, clkt, pflts, dirio, bufio)
      time(9) = time(9) + cpue - cpub

C     IIB is the pointer in BAD and IBAD arrays

      iib = 1

C     Loop over all the outages

      do jout = 1, nout

        ipbad(jout) = iib
        ibad_rs(jout) = 0

        k1 = klnc(1, jout)
        k2 = klnc(2, jout)
        id = mod (klnc(4, jout), 1000)
        if (iterm .ne. 0) then
          write (*, 10000) jout, intbus(k1), intbas(k1), intbus(k2), 
     &     intbas(k2), char(id)
        else if (mod (jout, 100) .eq. 0) then
          ipct = 100.0 * float (jout) / float (nout)
          write (*, 10000) jout, intbus(k1), intbas(k1), intbus(k2), 
     &     intbas(k2), char(id), ipct
        endif

        if (idebug .ne. 0) then
          write (dbug, 10000) jout, intbus(k1), intbas(k1), intbus(k2), 
     &     intbas(k2), char(id)
10000     format (' N-1 Outage No. ', i4, 2x, a8, f7.1, 2x, a8, f7.1, a,
     &     ' % completed ', i4)
        endif

C       Copy base-case voltage and angle to EI and THI
C       as initial guesses

        call fvmov(vmag, 1, ei, 1, nbus)
        call fvmov(vangle, 1, thi, 1, nbus)

C       Find vectors X1,X2 and scalars C1,C2 for compensation

        call getx1(jout)

C       Check for system separation. If so ignore this outage

        if (c1 .ne. 0.) then

          call getx2(jout)

C         Check for system separation. If so ignore this outage

          if (c1 .ne. 0.) then

C           Modify admittance for P and Q computations

            noconv = 0
            call ymod(jout, -1, noconv)

C           Find power flow for outage

            call stats(cpub, clkt, pflts, dirio, bufio)
            ilog = dlflow(noconv, iqsww, jout)

            if ( noconv .eq. 1 .and. ipqsln .eq. 0 ) then

c#############  dlflow  pass 2  with no reactive   ###################

              noconv = 0
c  allow 2 "bad" entries
c              iib = iib - 1
c              ibad(iib) = 0
c              bad(iib) = 0.
              ibad_rs(jout) = 1
              ipqsln = 1
              call fvmov(vmag, 1, ei, 1, nbus)
              call fvmov(vangle, 1, thi, 1, nbus)
              if (iqsww .ne. 0) then
                do i = 1, igenq
                  k = iqlim(i)
                  if (k .gt. 0) then

C                   First check any BQ_min or BQ_max which were restored 
C                   to PV.

                    if (qstate(i) .eq. 1) then

C                     Unconditionally restore all other PQ_limit buses 
C                     to PV.

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

c               Restore factored B''  from "save" arrays

                do i = 1, nbus
                   ipfbpp(i) = ipfbpp_save(i)
                   dgbpp(i) = dgbpp_save(i)
                enddo
                k = ipfbpp(nbus)
                do i = 1, k
                   jfbpp(i) = jfbpp_save(i)
                   fbpp(i) = fbpp_save(i)
                enddo

              endif

              ilog = dlflow(noconv, iqsww, jout)
              ipqsln = 0
              if ( noconv .eq. 1 ) ibad_rs(jout) = 2

            endif
c########  End of dlflow pass 2   ###################################

C           Test for abnormal errors

            if (.not. ilog) then
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
10010           format (' Overflow of "IBAD" array: ', 
     &           'JOUT,NOUT,IFLAG,IIB = ', 4i8)
                call prterx('E', 1)
              endif
              noconv = 1
              if (iterm .ne. 0) write (*, 10020) jout
              if (idebug .ne. 0) write (dbug, 10020) jout
10020         format ('0  Abnormal termination - Outage ', i4)
            endif

            call stats(cpue, clkt, pflts, dirio, bufio)
            time(2) = time(2) + cpue - cpub

            if (idebug .ge. 2) write (dbug, 10030) jout, (i, ei(i), thi
     &       (i), i = 1, nbus)
10030       format (//, ' Voltage solution for the', i3, 'th outage', 
     &       /, (i10, 2f15.5))

C           Restore admittance array to original form

            call ymod(jout, 1, noconv)

C           Check limits

            call stats(cpub, clkt, pflts, dirio, bufio)
            if (noconv .eq. 0) call check(jout)
            call stats(cpue, clkt, pflts, dirio, bufio)
            time(7) = time(7) + cpue - cpub

C           Clean up IPQV array and restore B" before next outage

            if (iqsww .ne. 0) then
              do i = 1, igenq
                k = iqlim(i)
                if (k .gt. 0) then

C                 First check any BQ_min or BQ_max which were restored 
C                 to PV.

                  if (qstate(i) .eq. 1) then

C                   Unconditionally restore all other PQ_limit buses 
C                   to PV.

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

              call stats(cpub, clkt, pflts, dirio, bufio)
              call fvmov(vmag, 1, ei, 1, nbus)

c***C             Retrieve factored B'' from scratch file
c***
c***              rewind lunusr
c***              read (lunusr) (ipfbpp(i), i = 1, nbus)
c***              read (lunusr) (dgbpp(i), i = 1, nbus)
c***              k = ipfbpp(nbus)
c***              do i = 1, k, 2000
c***                last = min0(i+1999, k)
c***                read (lunusr) (jfbpp(j), j = i, last)
c***              enddo
c***              do i = 1, k, 2000
c***                last = min0(i+1999, k)
c***                read (lunusr) (fbpp(j), j = i, last)
c***              enddo

c              Restore factored B''  from "save" arrays

              do i = 1, nbus
                 ipfbpp(i) = ipfbpp_save(i)
                 dgbpp(i) = dgbpp_save(i)
              enddo
              k = ipfbpp(nbus)
              do i = 1, k
                 jfbpp(i) = jfbpp_save(i)
                 fbpp(i) = fbpp_save(i)
              enddo

              call stats(cpue, clkt, pflts, dirio, bufio)
              time(9) = time(9) + cpue - cpub
            endif
          endif
        endif

        if (iib .gt. MXIBAD) goto 100
        ipbad(nout+1) = iib
      enddo
      goto 110
  100 continue
      do i = jout + 1, nout + 1
        ipbad(i) = iib
      enddo
  110 return
      end
