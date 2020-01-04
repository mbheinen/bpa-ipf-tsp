C    @(#)loadbase.f	20.6 7/18/96
        logical function loadbase (casename, filename)
        character casename *(*), filename *(*)

	include 'ipfinc/parametr.inc'
	include 'ipfinc/blank.inc'
	include 'ipfinc/lfiles.inc'
	include 'ipfinc/dtaiop.inc'

        logical yes
	character query*1, capital*1, basecase_dir*60, tempc*60,
     &            tempname*60
        integer open_file

	loadbase = .false.

        i = index(filename,'/')
	if (i.eq.0) i = index(filename,':')
	j=index(filename(i+1:),'.')

	if (j.eq.0) then
	   k = index(filename(i+j+1:),' ')
	   tempc = filename(1:i+j+k-1) // '.bse'
	   filename = tempc
	endif

	inquire(file=filename,exist=yes)

	if (.not. yes) then
	   if (index(filename,'/') .eq. 0) then
	      basecase_dir = ' '
              tempname = 'BASECASE_DIR' // char(0)
              call getenvir (tempname, basecase_dir)
              if (basecase_dir .ne. ' ') then
                last = lastch (basecase_dir)
                tempc = basecase_dir(1:last) // filename
	        filename = tempc
	        inquire(file=filename,exist=yes)
	      endif
	   endif
	   if ( .not. yes) then
	      write (*, 100) filename
  100	      format (' * Powerflow file ',a,' cannot be opened ')
	      loadbase = .false.
	      go to 160
	   endif
	endif

C      	Obtain data from input file
C	Search DATAI OLDBASE for case and load it into common
C
C       LOADED = 0 when case was not found in DATAI  
C              = 1 when case was found and loaded    
C              = 2 when DATAI was not a "PF DATA" history file

c	open (unit = datai, file = filename, status = 'old', readonly, 
c     1        form = 'unformatted', shared, err = 160)
        istatus = open_file( datai, filename, 'U', 'R', iostat )
        if ( istatus .ne. 0 ) goto 160

        write (*, 110)
  110	format (' * Loading base file - this will take a minute.')

       	casename = ' '
       	call rddtai(casename,loaded)                              

        if (count(1) .eq. 0) then                              
           write (*, 120)
  120      format (' * Powerflow Base Case has a failed solution')
           write (*, 122)
  122      format (' > Accept base case (Y or N)? : ',$)
           read (*, 124) query
  124      format (a)
           query = capital (query)
           if (query .eq. ' ') query = 'Y'
           if (query .eq. 'N') then
              loadbase = .false.
           else
              loadbase = .true.
           endif
        else if (loaded .eq. 0) then
           write (*, 130)
  130      format (' Powerflow case is not on Base Case file')
           loadbase = .false.
        else if (loaded .eq. 2) then
           write (*, 140)
  140      format (' Powerflow file in not a valid Base Case file')
           loadbase = .false.
        else if (loaded .ge. 3) then
           write (*, 150)
  150      format (' Powerflow case is not accessible')
           loadbase = .false.
        else
           loadbase = .true.
        endif                                                           
  160	close (unit = datai)
        return
        end
