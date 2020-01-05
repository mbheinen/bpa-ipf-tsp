C    @(#)puf.f	20.7 11/14/00

      subroutine puf (f_argc, f_argv)
      integer f_argc
      character f_argv(*)*60
 
      include 'ipfinc/parametr.inc'
      include 'ipfinc/alpha.inc'
      include 'ipfinc/alpha2.inc'
      include 'ipfinc/amtrx.inc'
      include 'ipfinc/arcntl.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/cbus.inc'
      include 'ipfinc/anlys.inc'
      include 'ipfinc/busanl.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/dc.inc'
      include 'ipfinc/dtaiop.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/filnam.inc'
      include 'ipfinc/jobctl.inc'
      include 'ipfinc/pageno.inc'
      include 'ipfinc/header.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/ordsta.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/com007.inc'
      include 'ipfinc/phase.inc'
      include 'ipfinc/tran.inc'
      include 'ipfinc/ownhash.inc'
      include 'ipfinc/qksrt.inc'
      include 'ipfinc/optim1.inc'
      include 'ipfinc/slnphs.inc'

      common /basecase/casename(2), filename(2)
      character casename*10, filename*60

      common /shellsort/buffer(1000)
      integer buffer

      integer MAX_LINEFLO
      parameter (MAX_LINEFLO = 200)
      common /scratch/ numdiff, ixref(MAXBUS), kdiff(2,MAXBUS),
     &                 fdiff(6,MAXBUS), cdiff(2,MAXBUS),
     &                 lphtx(2,MAXPHS), num_brn, brn_ptr(MAX_LINEFLO),
     &                 pij(MAX_LINEFLO), qij(MAX_LINEFLO)
      character cdiff * 1
      integer brn_ptr

      character bus1*8, bus2*8, id*1, tag(3)*1, text*80, infile*60,
     &          outfile*60, query*1, cutfile*60, version*10, 
     &          excel_char*1, capital*10, argument*10
      logical loadbase, loaded, finished, error, gen_flag, ai_flag,
     &        excel_flag, ps_flag, cut_flag, pv_flag, lossless_flag
      integer status, open_file, scrfil, find_bus, ptr, sen_cut,
     &        open_apnd, buildzbo, kmpvltdif, p_level, ftn_atoi
      external kmpvltdif, swpvltdif

      version = '1.01'
      excel_flag = .false.
      excel_char = '|'
      gen_flag = .false.
      ai_flag = .false.
      ps_flag = .false.
      cut_flag = .false.
      pv_flag = .false.
      lossless_flag = .false.
      p_level = 1
      header = ' '
      repnam = ' '
      coment(1) = ' '
      coment(2) = ' '
      outbuf = ' '
      do i = 1, 5
        call shdlod(i)
      enddo

      i = 1
      do while (i .le. f_argc)
        length = lastch (f_argv(i))
        write (*, 10001) i, f_argv(i)(1:length)
10001   format (' puf: argv[', i2, '] = [', a, ']')

        argument = capital (f_argv(i)(1:10))
        if (argument(1:2) .eq. '-H') then
          write (*, 10003)
10003     format (' Usage: LFA [-options] ', /,
     &      '  Options   Description                       Default ', /,
     &      '  -X<flag>  Excel delimiting character        blank   ', /,
     &      '  -AI       Area Interchange Constraints      Off     ', /,
     &      '  -G        Generator sensitivities only      Bus     ', /,
     &      '  -Lossless Lossless formulation              Lossy   ', /,
     &      '  -PV       All buses PV                      normal  ', /,
     &      '  -PS       Phase shifter sensitivities       (none)  ', /,
     &      '  -L<level> Level of Output detail            1       ', /,
     &      '            1 = sensitivities only                    ', /,
     &      '            2 = sensitivities + detail                ', /,
     &      '  -CUT <filename> Cutting Command file        (none)  ', /,
     &      '  -H        Help (This printout)              (none)  ', /)
          error = .false.
          go to 9000

        else if (argument(1:2) .eq. '-X') then 
C
C         Set Excel flag
C
          excel_flag = .true.
          excel_char = f_argv(i)(3:3)

        else if (argument(1:2) .eq. '-A') then 
C
C         Set Area Interchange Constraint flag
C
          ai_flag = .true.

        else if (argument(1:2) .eq. '-G') then 
C
C         Set Generator flag
C
          gen_flag = .true.

        else if (argument(1:3) .eq. '-PS') then 
C
C         Set Phase shifter sensitivity flag
C
          ps_flag = .true.

        else if (argument(1:3) .eq. '-PV') then 
C
C         Set PV flag
C
          pv_flag = .true.

        else if (argument(1:5) .eq. '-LOSS') then 
C
C         Set Lossless/lossy flag
C
          lossless_flag = .true.

        else if (argument(1:2) .eq. '-L') then 
C
C         Set Level of Printout
C
          p_level = ftn_atoi (f_argv(i)(3:3))
          if (p_level .eq. 0) p_level = 1

        else if (argument(1:2) .eq. '-C') then 
C
C         Set Cut flag
C
          cut_flag = .true.
          cutfile = f_argv(i+1)
          luncut = 23
          status = open_file( luncut, cutfile, 'F', 'R', ios)
          if (status .ne. 0) then
            last = lastch (cutfile)
            write (*, 10030) cutfile(1:last)
            cut_flag = .false.
            luncut = 0
            error = .true.
            go to 9000
          else
            inquire (unit=luncut, name=cutfile)
          endif
          i = i + 1
        endif
        i = i + 1
      enddo

      if (ai_flag .and. cut_flag ) then
        write (*, 10004)
10004   format (' Illegal combination of -AI and -C options (both cannot
     & co-exist)', /,
     &      ' Usage: LFA [-options] ', /,
     &      '  Options   Description                       Default ', /,
     &      '  -X<flag>  Excel delimiting character        blank   ', /,
     &      '  -AI       Area Interchange Constraints      Off     ', /,
     &      '  -G        Generator sensitivities only      Bus     ', /,
     &      '  -Lossless Lossless formulation              Lossy   ', /,
     &      '  -PV       All buses PV                      normal  ', /,
     &      '  -PS       Phase shifter sensitivities       (none)  ', /,
     &      '  -L<level> Level of Output detail            1       ', /,
     &      '            1 = sensitivities only                    ', /,
     &      '            2 = sensitivities + detail                ', /,
     &      '  -CUT <filename> Cutting Command file        (none)  ', /,
     &      '  -H        Help (This printout)              (none)  ', /)
        error = .false.
        go to 9000
      endif

      call init_bdpfdt()
      call init_bdloderr()
      call init_blkdta()
      call init_bd0007()
      call pfinit()
      call initlz(status)
      call set_batch()
      call restop()
      call am_date(rdate)

C     Define logical units

      crtsw = 0

      write (*, 10018) version
10018 format (' > Begin execution of Path Use Factors version ', a)

C     Get Powerflow File Name

      loaded = .false.
      do while (.not. loaded)
        write (*, 10019)
10019   format (' > Enter powerflow BASE file name : ')

        read (*, fmt='(a)') filename(1)

        casename(1) = ' '
        loaded = loadbase(casename(1), filename(1))
      enddo

      if (ntotc .eq. 0) then
        do i = 1, ntot
          jarzn(i) = 1
        enddo
        arcnam(1) = 'NO AREA'
      endif
C
C     Get Output File Name
C
      scrfil = 20
      loaded = .false.
      do while (.not. loaded)
        write(*, 10020)
10020   format(' > Enter Output file name : ')

        read (*, fmt='(a)') outfile
 
        status = open_file( scrfil, outfile, 'F', 'W', ios)
        if (status .ne. 0) then
          last = lastch (outfile)
          write (*, 10030) outfile(1:last)
10030     format (' * Error opening file ', a)
          error = .true.
          go to 9000
        else
          loaded = .true.
          inquire (unit=scrfil, name=outfile)
        endif
      enddo

      if (ntotx .eq. 0) then

        write (*, 10050)
10050   format (
     &   ' * Rebuilding solution arrays. This will take a minute.')

        buf = '(end)'
        card = buf(1:1)
        if (ordtbx .ne. 2) then
          ordymx = 2	! Set flag to not reorder y-matrix
          call chgorder('inp2opt')
        endif

        if (ps_flag) then
          option(9) = 0.1	! Set minimum phase shift representation to 
c                               ! 0.1 degree.
        else
          option(9) = 0.0
        endif
        call presln()
        iopton(18) = 0	! Set flat start off
        kspare(19) = 2  ! Set LTC's on for sensitivity factorization
        kspare(24) = 1	! Do not reinitialize XDATA
        if (ai_flag) then
           iopton(17) = 1  ! Set area interchange control on
           kspare(20) = 1  ! Set area interchange constraints on for
C                          ! sensitivity factorization
        else 
           iopton(17) = 0  ! Set area interchange control off
           kspare(20) = 0  ! Set area interchange constraints off for
C                          ! sensitivity factorization
        endif

        call opsln1()
        iopton(1) = 0	! Set number of decoupled iterations = 0
        call dcstrt(kerr)
        call nrcnvt()
        lprtsw = 1

      endif

C     Get Cutting control File Name

      if (cut_flag) then
        read (luncut, fmt='(a)') buf
        status = sen_cut (luncut)
        if (status .ne. 0) then
          write (*, 10080) cutfile(1:last)
10080     format (' * Data errors processing CUT file ', a)
          error = .true.
          go to 9000
        endif
      endif
C
C     Get Input File Name
C
      loaded = .false.
      luninp = 24
      do while (.not. loaded)
        write(*, 10090)
10090   format(' > Enter input (branch data) file name : ')

        read (*, fmt='(a)') infile
 
        status = open_file( luninp, infile, 'F', 'R', ios)
        if (status .ne. 0) then
          last = lastch (infile)
          write (*, 10030) infile(1:last)
        else
          loaded = .true.
          inquire (unit=luninp, name=infile)
          call close_file (luninp)
        endif
      enddo
C     
C     Recompute the Jacobian matrix. The previous Jacobian matrix      
C     cannot be reused because                                         
C     
C     1.  Only the upper-diagonal portion is stored and
C     2.  Common /AMTRX/ is not physically large enough to accomodate 
C         both upper and lower factors in double precision.
C     
C     To circumvert the second obstacle, the Jacobian is refactored in 
C     single precision, which reduces the physical storage requirements
C     by 50%.                                                          
C     
      write (*, 10100)
10100 format (' * Factoring Sensitivity Matrices. This will take a minut
     &e.')
c
c     Build ownership array
c
      if (numown .eq. 0) status = buildzbo (status)

C     Write Path Use Factors header.

      write (scrfil, 10110) version
10110 format (t11, ' Path Use Factors version ', a)
      write (scrfil, '(1x)')
      if (p_level .gt. 1) then
        write (scrfil, 10120)
10120   format (t11, ' Sensitivity Matrix computed with the following co
     &ntrols:')
        write (scrfil, '(1x)')
        if (kspare(19) .eq. 0) then
          tag(1) = 'X'
          tag(2) = ' '
          tag(3) = ' '
        elseif (kspare(19) .eq. 1) then
          tag(1) = ' '
          tag(2) = ' '
          tag(3) = 'X'
        else
          tag(1) = ' '
          tag(2) = 'X'
          tag(3) = ' '
        endif
        write (scrfil, 10130) tag(1)
10130   format (t18, 'LTC control', t43, '(', a, ')', 3x, 'Off')

        write (scrfil, 10140) tag(2)
10140   format (t18, '--- -------', t43, '(', a, ')', 3x, 
     &    'on (Full control)')

        write (scrfil, 10150) tag(3)
10150   format (t43, '(', a, ')', 3x, 'On (No voltage control)')

        write (scrfil, '(1x)')

        if (kspare(20) .eq. 0) then
          tag(1) = 'X'
          tag(2) = ' '
          tag(3) = ' '
        elseif (kspare(20) .eq. 1) then
          tag(1) = ' '
          tag(2) = 'X'
          tag(3) = ' '
        else
          tag(1) = ' '
          tag(2) = ' '
          tag(3) = 'X'
        endif
        write (scrfil, 10160) tag(1)
10160   format (t18, 'AI control', t43, '(', a, ')', 3x, 'Off')

        write (scrfil, 10170) tag(2)
10170   format (t18, '-- -------', t43, '(', a, ')', 3x, 
     &   'Control (default)')

        write (scrfil, 10180) tag(3)
10180   format (t43, '(', a, ')', 3x, 'Monitor')

        write (scrfil, '(1x)')
      endif
      nb = nslkxx(1,1)
      write (scrfil, 10190) bus(nb), base(nb)
10190 format (t18, 'System slack bus ', a8, f7.1)

      write (scrfil, '(1x)')
      if (ps_flag) then
c
c       Modify system for fixed/LTC phase shifters
c
        call sen_bph2 (0)
      else
        ntxtie_temp = ntxtie
        ntxtie = 0
        ntopt = 0
      endif
      if (pv_flag) then
c
c       Change all buses to type PV     
c
        kspare(39) = 0
        kspare(40) = 0
        do kt = nbslck+1, ntot
          kvolt(kt) = kt
        enddo
      endif
      call sen_fac3 (lossless_flag)

C     Enter candidate line(s) and perform line_flow analysis

      finished = .false.
      error = .false.
      num_brn = 0
      status = open_file( luninp, infile, 'F', 'R', ios)
      do while (.not. finished)
        read (luninp, fmt='(a)', end = 101) text
        read (text, 10200, err=90) bus1, base1, bus2, base2, id
10200   format (t7, a8, f4.0, 1x, a8, f4.0, a1)
        k1 = find_bus(bus1, base1)
        if (k1 .le. 0) then
          write (*, 10210) bus1, base1
10210     format (' LINE_FLOW_ANALYSIS terminal bus ', a8, f7.1, 
     &      ' is not in system.')
          error = .true.
        else
          k2 = find_bus(bus2, base2)
          if (k2 .le. 0) then
            write (*, 10210) bus2, base2
          else
            ptr = numbrn(k1, k2, id, 0)
            if (k1 .gt. 0 .and. k2 .gt. 0 .and. ptr .le. 0) then
              write (*, 10220) text(1:31)
10220         format ('0 LINE_FLOW_ANALYSIS branch ', a, 
     &         ' is not in system.')
              error = .true.
            else if (num_brn .lt. MAX_LINEFLO) then
              num_brn = num_brn + 1
              brn_ptr(num_brn) = ptr
            else
              write (*, 10230) text(1:31), MAX_LINEFLO
10230         format (' More than ', i4, ' LINE_FLOW_ANALYSIS records ',
     &                /, ' This record ignored (', a, ')')
              error = .true.
            endif
          endif
        endif
        go to 100

   90   write (*, 10240) text(1:33)
10240   format (' * Data error in record (', a, ')')
        error = .true.
  100   continue

      enddo
  101 continue
      if (error) go to 9000

      finished = (num_brn .eq. 0 .and. error)
      do while (.not. finished)
C     
C       Compute dLine/dPi, dLine/dQi sensitivities with the following 
C       steps (all within module sen_dldx2): 
C     
C       1. Compute the objective function dLine/dX                     
C       2. Compute the LaGrange multipliers                            
C     
        call sen_dldx2 (num_brn, brn_ptr, pij, qij)
c
c       Print out line sensitivity results
c
        threshold = 0.0
        call sen_out2 (num_brn, brn_ptr, pij, qij, threshold, gen_flag,
     &                 scrfil, excel_flag, excel_char, ps_flag,
     &                 p_level)      
        finished = .true.
      enddo

      last = lastch (outfile)
      write (*, 10260) outfile(1:last)
10260 format (' * Output written to file ', a)
      if (ps_flag) then
c
c       Restore modified system for fixed/LTC phase shifters
c
        call sen_bph2 (1)
      else
        ntxtie = ntxtie_temp
      endif

 9000 if (error) then
        write (errbuf(1), 9010)
 9010   format (' Program aborted by data errors ')
        call prterx ('F', 1)
        call exit()
      endif

      return
      end
