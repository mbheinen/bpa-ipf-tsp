C    @(#)ipf_reports.f	20.4 2/13/96
	subroutine ipf_reports

	include 'ipfinc/parametr.inc'	
	include 'ipfinc/lfiles.inc'
	include 'ipfinc/dtaiop.inc'
	include 'ipfinc/blank.inc'
        include 'ipfinc/arcntl.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/xdata.inc'
        include 'ipfinc/cbus.inc'
        include 'ipfinc/jobctl.inc'
        include 'ipfinc/filnam.inc'
        include 'ipfinc/pageno.inc'
        include 'ipfinc/prt.inc'

        common /basecase/ casename(2), filename(2)
        character casename * 10, filename * 60

	common /shellsort/ buffer(1000)
        integer buffer

	character outfile * 60, query * 1, capital * 1, outform * 1,
     &            old_outfile * 60
        logical loadbase, is_loaded, opened
        integer status, open_file, scrfil

c***    external bdpfdt, bdloderr, blkdta, bd0007

        call init_bdpfdt
        call init_bdloderr
        call init_blkdta
        call init_bd0007
        call pfinit
        call initlz(status)
        call set_batch
        call restop
        call am_date(rdate)
C
C       Define logical units
C
        crtsw = 0
        outfile = ' '
        scrfil = lprt
        old_outfile = ' '
C
C	Get Powerflow File Name
C
        is_loaded = .false.
        do while (.not. is_loaded)
           write(*,100)
  100	   format(' > Enter powerflow BASE file name : ')

	   read (*,110) filename(1)
  110	   format (a)

           casename(1) = ' '
           is_loaded = loadbase(casename(1), filename(1))
        enddo

  180   write (*, 182)
  182   format (' > Enter I, O, or Q (INPUT, OUTPUT, or QUIT) : ')
        read (*, 110) query
        query = capital (query)

        if (query .eq. ' ') query = 'O'
        if (index ('IO', query) .eq. 0) go to 900

        if (outfile .ne. ' ') then
           l = lastch (outfile)
           write (*, 183) outfile(1:l)
  183      format (' * Results saved on file ', a)
           call close_file (scrfil)
           if (old_outfile .ne. ' ') then
              outfile = old_outfile
              status = open_file (scrfil, outfile, 'FF', 'W', iostat)
           endif
        endif
        write (*, 184)
  184   format (' > Enter output device T, P, or F (Terminal, Printer,  
     &or file): ')
 
        read (*, 110) outform
        outform = capital (outform)
        if (outform .eq. 'P') then
           outfile = 'for014.dat'
           scrfil = lprt
           inquire (unit=scrfil, opened=opened)
           if (opened) then
              inquire (unit=scrfil, name=old_outfile)
           else
              old_outfile = ' '
           endif
           call close_file (scrfil)
           status = open_file (scrfil, outfile, 'FF', 'W', iostat)
        else if (outform .eq. 'F') then
  185      write (*, 186)
  186      format (' > Enter output file name : ')
           read (*, 110) outfile
           scrfil = lprt
           call close_file (scrfil)
           status = open_file (scrfil, outfile, 'FF', 'W', iostat)
           if (status .ne. 0) then
              l = lastch (outfile)
              write (*, 190) outfile(1:l)
  190         format (' * Error opening file ', a)
              scrfil = 0
              go to 185
           endif
        else
           outfile = 'for014.dat'
           scrfil = lprt
           call close_file (scrfil)
           status = open_file(scrfil, outfile, 'FF', 'W', iostat)
        endif

        if (query .eq. 'I') then
           call input_rpt (scrfil)
        else if (query .eq. 'O') then
           call output_rpt (scrfil)
        endif
        go to 180
  900   continue
        if (outfile .eq. ' ') then
        else if (outfile .eq. 'for014.dat') then
           call close_file(scrfil)
        else
           l = lastch (outfile)
           write (*, 183) outfile(1:l)
           call close_file(scrfil)
        endif
        return
        end
