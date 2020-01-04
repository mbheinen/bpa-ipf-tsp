C    @(#)solton.f	20.13 5/27/99
      subroutine solton

      include 'ipfinc/parametr.inc'

      include 'ipfinc/addtbx.inc'
      include 'ipfinc/alpha.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/chgprm.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/epridc.inc'
      include 'ipfinc/gendrp.inc'
      include 'ipfinc/ikk.inc'
      include 'ipfinc/intbus.inc'
      include 'ipfinc/itrhis.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/ltcsln.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/tbx.inc'
      include 'ipfinc/tbxsrt.inc'
      include 'ipfinc/update.inc'
      include 'ipfinc/usranl.inc'
      include 'ipfinc/ycomp.inc'
      include 'ipfinc/comdrx.inc'
      include 'ipfinc/xblim.inc'
      include 'ipfinc/miscfile.inc'

      common /drptmp/ drptmp

      common /ycpsln/ dytot, ditot, kownty, ycpscr(6,MXYCMP)

      common /ownflg/ ownflg
      logical ownflg

      common /is_batch / is_batch

      common /sol_temp/ idchgx, kswsln, itlow

      integer fichsv, crtsv, status, chkerr, error, open_file, ckzoneup
      logical batch, chkbch, epdc, opened

      external nrpqv

      character prmfil*60, tmpfil*60, tempc*60, bigbuf*512, ljstfy*512

C     Initialize solution arrays.

      do i = 1, MAXLTC
         ltcsln(i) = 0
      enddo
      do i = 1, MAXTBX
         tbxsrt(i) = 0
         tbxrat(i) = 1.0
         do j = 1, 6
            tbxslp(j,i) = 0.0
         enddo
      enddo
      do i = 1, kxtot
         xblim(1,i) = -9999.0
         xblim(2,i) = -9999.0
         xvlim(1,i) = 0.0
         xvlim(2,i) = 0.0
         xvrlim(1,i) = 0.0
         xvrlim(2,i) = 0.0
      enddo
      do i = 1, MXYCMP
         do j = 1, 6
            ycpscr(j,i) = 0.0
         enddo
      enddo
      if (nepbus .gt. 0) then
         epdc = .true.
      else
         epdc = .false.
      endif
      lprtsv=lprtsw
      fichsv=fichsw
      crtsv=crtsw
      crtsw=0
      koptsw = 0
      lskp = 1
      ownflg = .false.             ! Set flag to force updating OWNLOS
      update(1) = 1                ! Set flag to force updating ZSUM
      ickzone = ckzoneup(status)   ! Determine whether it is necessary
C                                  ! to update ZSUM each solution
      call presln
 
      if ( chkbch (batch)) then
         crtsw=0
      else
         crtsw=crtsv
      endif
 
      if (iopton(20) .eq. 1) then
         crtsw=crtsv
         write(outbuf, 59)
   59    format('0     Base case solution voltages used by request    ')
         call prtout (1)
         call nrbase
         crtsw=0
         go to 200
      else if (iopton(20) .eq. 2) then
         crtsw=crtsv
         write(outbuf, 60)
   60    format('0     Alternate solution voltages obtained by ',
     &          '/LOAD_SOLUTION command   ')
         call prtout (1)
         call nrbase
         crtsw=0
         go to 200
      else if (iopton(20) .eq. 3) then
         crtsw=crtsv
         write(outbuf, 61)
   61    format('0     Alternate starting voltages obtained by ',
     &          '/LOAD_SOLUTION command   ')
         call prtout (1)
      endif
 
      lprtsv=lprtsw
      fichsv=fichsw
      lprtsw=1
      if (kspare(16) .ge. 0) fichsw=1
C
C     Check override of default decoupled solution iteration.
C
      if (iopton(1) .eq. 2 .and. numdrp .gt. 0) then
         iopton(1) = 6
         write (errbuf(1), 62) iopton(1)
   62    format('/GEN_DROP has forced ', i2, ' DECOUPLED iterations.')
         call prterx ('W',1)
      endif
 
      call sumsol
C
C     Open scratch file
C
      inquire (unit = lunscr, opened = opened)
      if (.not. opened) then
        status = open_file (lunscr, ' ', 'U', 'W', iostat)
      endif
 
      if (epdc) then
C
C        Initialization of variables.
C
         call epdcin
C
C        Calculation of per unit constants and the per unitizing of
C        data.
C
         call lfdcpu
C
C        Determination of VECBUS.
C
         call solvec (jdw1, jdw2, jdw3, jdw4)
C
C        Topology checking.
C
         call dctopo
C
C        Generation of nodal conductance matrix.
C
         call bldgdc
C
C        Printing of DC input data.
C
C        CALL PRTDCI
C
      end if
 
      call opsln1
 
      kerr=chkerr('F')
      if (kerr .gt. 0) then
         write (errbuf(1), 80)
   80    format(' Case aborted by fatal errors.')
         if (is_batch .eq. 0) then
            call prterx ('E',1)
            return
         else
            call prterx ('F',1)
            call erexit()
         endif
      endif
 
      lprtsw=lprtsv
      fichsw=fichsv
      crtsw=1
      call slnhdr
      drptmp = drptot
      if (iopton(1).gt.0) then
         if (numdrp .gt. 0) then
            call comdrp ('DC Iteration', 1, num1, drptmp, num2, drop2,
     1                    status, nrpqv, itrx)
         endif
C
C        Set switches and counters to store the decoupled solution
C        history data.
C
         itrtyp = 1
         call savsln
         call dcstrt  (kerr)
C
C        Reset switches and counters to store the Newton_Raphson
C        solution history data.
C
         itrhis(1) = ittot - 1
         itrhis(2) = 0
         itrtyp = 2
         if (kerr.ne.0) call retsln
      else
C
C        Otherwise, obtain X-bus sensitivities.
C
         if (kxtot .gt. 0) call getxsn(error)
      endif
      if (iopton(2).gt.0) then
         if (numdrp .gt. 0) then
            call comdrp ('NX Iteration', 1, num1, drptmp, num2, drop2,
     1                    status, nrpqv, itrx)
         endif
 
         call savsln
         call cursol (kerr)
         if (kerr .ne. 0) call retsln
      endif
      call nrcnvt
      lprtsv=lprtsw
      fichsv=fichsw
      lprtsw=1
      if (kspare(16) .ge. 0) fichsw=1
 
      ittot=0
      kswsln = 1
      idchgx = 0
 
      if (numdrp .gt. 0) then
         drop2 = 1.0e10
         itx = 1
         do while (itx .le. 4 .and. drop2 .ge. 10.0)
            call comdrp ('NR Iteration', 1, num1, drptmp, num2, drop2,
     1                    status, nrpqv, itrx)
            if (itx .gt. 1) ittot = 3
            call nrsoln (ittot, iopton(3), idchgx, kswsln, itlow)
            if (itrhis(2) .eq. 0) itrhis(2) = itrhis(1) + ittot
            itx = itx + 1
         enddo
      else
         call nrsoln (ittot, iopton(3), idchgx, kswsln, itlow)
         if (itrhis(2) .eq. 0) itrhis(2) = itrhis(1) + ittot
      endif
C
C     Perform parameter perturbation summary.
C
      if (index (buf,'CHANGE_PAR') .ne. 0 .or.
     1    index (buf,'CHANGEPAR') .ne. 0) then
         if (lskp .eq. 3) then
            write (errbuf(1), 83)
   83       format ('The following / CHANGE_PARAMETERS ',
     &              'commands aborted by failed solution.')
            call prterx ('W', 1)
C
C           Restore solution
C
            call retsln
            error = 1
C
C           Skip current and subsequent / CHANGE_PARAMETER records.
C
            do while (buf(1:1) .eq. '/' .and.
     &               (index (buf,'CHANGE_PAR') .ne. 0 .or.
     &                index (buf,'CHANGEPAR') .ne. 0))

               write (errbuf(1), 84) buf(1:80)
   84          format (' COMMAND IGNORED (', a, ')')
               call prterx ('W', 1)
               card = buf(1:1)
 
               read (inp, 105, end=85) buf
               card = buf(1:1)
               last = lastch (buf)
               do while (buf(last:last) .eq. '-') 
                  write (errbuf(1), 103) buf(1:80)
                  call prterx ('W', 1)
                  read (inp, 105, end=85) buf
                  card = buf(1:1)
                  last = lastch (buf)
               enddo
C
C              Examine next input record
C
               do while (card .eq. '.')
                  read (inp, 105, end=85) buf
                  card = buf(1:1)
               enddo
            enddo
            go to 86
 
   85       buf = '(STOP) SOLTON'
            card = buf(1:1)
 
   86       call ctlpow
         else

            bigbuf = buf   
            call space (1)  
            write (outbuf, 87) buf(1:80)
   87       format (' Solution Command (', a, ')')
            last = lastch (bigbuf) 
            do while (bigbuf(last:last) .eq. '-')
               read (inp, fmt='(a)', end=88) buf
               write (outbuf, 87) buf(1:80)
               call prtout (1) 
               if (buf(1:1) .ne. '.') 
     &           bigbuf(last:) = ' ' // ljstfy (buf) 
               last = lastch (bigbuf) 
               buf = bigbuf
            enddo
   88       continue
C
C           Save initial solution in case we get a failed solution.
C
            call savsln
C
C           Open file for parameters
C
            inquire (unit=inp, name=prmfil)
            i = index (prmfil, ']')
            j = index (prmfil(i+1:), '.') + i
            if (j .eq. 0) then
               j = lastch (prmfil) + 1
               prmfil(j:j) = '.'
            endif
            tempc = prmfil
            prmfil = tempc(1:j) // 'QVPT'
            status = open_file (23, prmfil, 'F', 'W', iostat)
            numprm = 0
C
C           Open OUTPUT files.  If none are explicitly defined,
C           create default names.
C
            do iuser = 1, numusr
               if (usrfil(iuser) .eq. ' ') then
                  write (usrfil(iuser), 92) prmfil(1:j),
     1               'USER_REPORT_', iuser
   92             format (a, a, i2.2)
               endif
               lun(iuser) = 26
               close(unit=lun(iuser),err=93)
   93          open (unit = lun(iuser),
     1               file = usrfil(iuser),
     2               status = 'NEW',
     3               form = 'FORMATTED',
     5               iostat = ios,
     6               err = 94)
               go to 97

   94          last = lastch(usrfil(iuser))
               write (errbuf(1), 95) ios, usrfil(iuser)(1:last),
     1            lun(iuser)
   95          format (' Error ', i3, ' opening file ', a,
     1             ' on logical unit ', i2)
               call prterx ('W', 1)

   97          close(unit=lun(iuser))
               numupx(iuser) = 0
            enddo
C
C           Write user-specified report if requested.
C
            if (numusr .gt. 0) then
               call postsl
               update(1) = ickzone
               call updzon()
               call prtusr (lun, numupx)
            endif
 
  100       call getprm (bigbuf, error)
C
C           We start at iteration 3 because bus switching is permitted.
C           The bus history logic has a maximum iteration limit of 50,
C           which could easily be violated with several parameter studie
C
            ittot = 3
            call nrsoln (ittot, iopton(3), idchgx, kswsln, itlow)
            if (itrhis(2) .eq. 0) itrhis(2) = itrhis(1) + ittot
            if (lskp .ne. 3 .and. numdrp .gt. 0) then
               itrtot = 1
               if (numdrp .gt. 0) then
                  drop2 = 1.0e10
                  itx = 1
                  do while (itx .le. 4 .and. drop2 .ge. 10.0)
                     call comdrp ('NR Iteration', 1, num1, drptmp,
     1                             num2, drop2, status, nrpqv, itrx)
                     ittot = 3
                     call nrsoln (ittot, iopton(3), idchgx, kswsln,
     1                            itlow)
                     if (itrhis(2) .eq. 0) then
                        itrhis(2) = itrhis(1) + ittot
                     endif
                     itx = itx + 1
                  enddo
               endif
            else
               ittot = 3
               call nrsoln (ittot, iopton(3), idchgx, kswsln, itlow)
               if (itrhis(2) .eq. 0) itrhis(2) = itrhis(1) + ittot
            endif
            kerr=chkerr('F')
            if (lskp .eq. 3 .or. kerr .gt. 0) then
               write (errbuf(1), 101)
  101          format ('The following / CHANGE_PARAMETERS ',
     &                 'commands aborted by failed solution.')
               call prterx ('W', 1)
C
C              Restore solution
C
               call retsln
               error = 1
C
C              Skip current and subsequent / CHANGE_PARAMETER records.
C
               do while (card .eq. '/' .and.
     &                  (index (buf,'CHANGE_PAR') .ne. 0 .or.
     &                  index (buf,'CHANGEPAR') .ne. 0))
                  write (errbuf(1), 103) buf(1:80)
  103             format (' COMMAND IGNORED (', a, ')')
                  call prterx ('W', 1)

                  read (inp, 105, end=107) buf
  105             format (a)
                  card = buf(1:1)
                  last = lastch (buf)
                  do while (buf(last:last) .eq. '-') 
                     write (errbuf(1), 103) buf(1:80)
                     call prterx ('W', 1)
                     read (inp, 105, end=107) buf
                     card = buf(1:1)
                     last = lastch (buf)
                  enddo
C
C                 Examine next input record
C
                  do while (card .eq. '.')
                     read (inp, 105, end=107) buf
                     card = buf(1:1)
                  enddo
               enddo
               go to 108
 
  107          buf = '(STOP) SOLTON'
               card = buf(1:1)
 
  108          call ctlpow
            else
               call prtprm (xpoint, ypoint, error)
               call savsln
C
C              Write user-specified report if requested.
C
               if (numusr .gt. 0) then
                  call postsl
                  update(1) = ickzone
                  call updzon()
                  call prtusr (lun, numupx)
               endif
            endif
 
            if (index (buf,'CHANGE_PAR') .ne. 0 .or.
     1         index (buf,'CHANGEPAR') .ne. 0) then

               bigbuf = buf   
               call space (1)  
               write (outbuf, 87) buf(1:80)
               last = lastch (bigbuf) 
               do while (bigbuf(last:last) .eq. '-')
                  read (inp, fmt='(a)', end=109) buf
                  write (outbuf, 87) buf(1:80)
                  call prtout (1) 
                  if (buf(1:1) .ne. '.') 
     &              bigbuf(last:) = ' ' // ljstfy (buf) 
                  last = lastch (bigbuf) 
                  buf = bigbuf
               enddo
  109          continue
               go to 100
            else
C
C              Close CHANGE_PARAMETER file
C
               if (numprm .gt. 0) then
                  inquire (unit=23, name=prmfil)
                  l = lastch (prmfil)
                  write (outbuf, 110) numprm, prmfil(1:l)
  110             format ('0 ', i4, ' points written to file ',
     1                a)
                  call prtout (1)
               endif
               close (unit=23, err=112)
  112          continue
               prmfil = ' '
               numprm = 0
C
C              Close USER_REPORT file
C
               do iuser = 1, numusr
                  if (numupx(iuser) .gt. 0) then
                     inquire (file=usrfil(iuser), name=tmpfil)
                     l = lastch (tmpfil)
                     write (outbuf, 120) numupx(iuser), tmpfil(1:l)
  120                format ('0 ', i4, ' records written to file ',
     1                   a)
                     call prtout (1)
                  endif
                  usrfil(iuser) = ' '
               enddo
            endif
         endif
      endif
C
C     Print out solution summary if failed solution.
C
      if (lskp .eq. 3 .or. iopton(22) .ne. 0) then
         call itrsum
      endif
 
      lprtsw=lprtsv
      fichsw=fichsv
C
C     Perform post-solution update:
C
C     1. Update P- and Q-injections for slack buses, PV buses.
C     2. Update d-c values.
C     3. Update Taps for LTC transformers, Xij and Bis for variable
C        var-compensation.
C
      call postsl
C
C     Summarize Generator Dropping
C
      crtsw=0
      if (numdrp .gt. 0) then
         call sumdrp
      endif
 
      if (index (buf,'BUSSEN') .ne. 0 .or.
     1    index (buf,'BUS_SEN') .ne. 0 .or.
     2    index (buf,'LINESE') .ne. 0 .or.
     3    index (buf,'LINE_SE') .ne. 0 .or.
     4    index (buf,'LOSSSE') .ne. 0 .or.
     5    index (buf,'LOSS_SE') .ne. 0 .or.
     6    index (buf,'TRANSF') .ne. 0) then
         call opsln3
      endif
 
      call nrrstr
 
C     Restore temporary "BF" NTYP created in DCSTRT.

      do i = 1, ntbxad
         jt = ntotb - ntbxad + i
         kt = tbx(2,jt)
         ntypu(kt) = ltbxad(i)                  
      enddo
      ntotb = ntotb - ntbxad

  200 crtsw=crtsv
      ownflg = .false. ! Flag OWNLOS as being obsolete
      update(1) = 1    ! Flag ZSUM as being obsolete
      return
      end
