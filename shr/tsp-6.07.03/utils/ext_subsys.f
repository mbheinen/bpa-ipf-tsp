	program ext_subsys

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

        common /basecase/ casename(2), filename(2)
        character casename * 10, filename * 60

	common /shellsort/ buffer(1000)
        integer buffer

	character pfcfile * 60, query * 1, NULL * 1, case * 60, 
     &            temp * 60, solfile * 60, sdifile * 60, 
     &            swifileo * 60, swifilen * 60, capital * 10 
        logical loadbase, loaded, finished
        integer status, open_file

        NULL = char(0)

        call init_bd_all()
C
C       Define logical units
C
        datai = 16
        dbug = 19
        lprt = 14
        pfcfile = ' '
        lunsdi = 18
        lunswio = 17
C
C	Get Powerflow File Name
C
        loaded = .false.
        do while (.not.loaded)
           write(*,100)
  100	   format(' > Enter powerflow BASE file name : ')

	   read (*,110) filename(1)
  110	   format (a)

           casename(1) = ' '
           loaded = loadbase(casename(1), filename(1))
        enddo
C
C	Get *.SDI File Name
C
        loaded = .false.
        do while (.not.loaded)
           write(*,112)
  112	   format(' > Enter *.sdi file name : ')

	   read (*,110) sdifile
           if (sdifile .eq. ' ' .or. sdifile(1:1) .eq. NULL) then
              loaded = .true.
              lunsdi = 0
           else
              status = open_file( lunsdi, sdifile, 'U', 'R', ios)
              if (status .ne. 0) then
                 last = lastch (sdifile)
                 write (*, 130) sdifile(1:last)
  130            format (' * Error opening file ', A)
              else
                 loaded = .true.
              endif
           endif
        enddo
C
C	Get *.SWI File Name
C
        loaded = .false.
        do while (.not.loaded)
           write(*,114)
  114	   format(' > Enter *.swi file name : ')

	   read (*,110) swifileo
           if (swifileo .eq. ' ' .or. swifileo(1:1) .eq. NULL) then
              loaded = .true.
              lunswio = 0
           else
              status = open_file( lunswio, swifileo, 'F', 'R', ios)
              if (status .ne. 0) then
                 last = lastch (swifileo)
                 write (*, 130) swifileo(1:last)
              else
                 loaded = .true.
              endif
           endif
        enddo

        finished = .false.
        do while (.not. finished)
           write (*, 170) 
  170      format (' > 1. Append an interconnected generator subsystem t
     &o an infinite bus. ', /
     &             '   2. Append isolated generators to a common infinit
     &e bus.', /
     &             ' Enter choice (1 or 2) : ')
           read (*, 110) query
           if (query .eq. '1') then
              call getsubsys1 (lunsdi, lunswio)
           else
              call getsubsys2 (lunsdi, lunswio)
           endif
           write (*, 180) 
  180      format (' > Extract another subsystem (y or n)? ')
           read (*, 110) query
           if (query .ne. 'Y' .and. query .ne. 'y') finished = .true.
        enddo

  900   continue

        end
