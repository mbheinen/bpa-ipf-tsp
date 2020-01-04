C    %W% %G%
        subroutine getpfn(pfname)
C
C       This program gets the powerflow file name. It checks the users
C       directory first, the Basecase_Dir second and then the WSCCbase_Dir
C       for the file. If it does not find it in these directories it sends
C       back a message that the Powerflow file cannot be opened.
C
        character pfname*30

        logical yes		
C
C	Get Powerflow File Name
C
100	write(*,200)
200	format(' ENTER POWERFLOW BASE FILE NAME >',$)

	read (*,300) pfname
300	format (a)

	nambeg = index(pfname,']')
	if (nambeg.eq.0) nambeg = index(pfname,':')
	namend=index(pfname(nambeg+1:),'.')

	if (namend.eq.0) then
	   namext = index(pfname(nambeg+namend+1:),' ')
	   pfname = pfname(1:nambeg+namend+namext-1) // '.BSE'
	endif

	inquire(file=pfname,exist=yes)     ! Does oldbase exist in users dir.

	if (.not. yes) then

           if (index(pfname,':').eq.0 .and. index(pfname,']').eq.0) then
	      pfname = 'BASECASE_DIR:' // pfname  ! If not users dir. check
	      inquire(file=pfname,exist=yes)      ! BASECASE_DIR
	      
	      if (.not.yes) then
	        nambeg = index(pfname,':')
	        pfname = 'WSCCBASE_DIR:' // pfname(nambeg+1:)  ! If not BASECASE_DIR
	        inquire (file=pfname,exist=yes)           ! check WSCCBASE_DIR
	      endif

	   endif

           if ( .not. yes) then
              write (*,400)pfname
400           format (' Powerflow file ',a,' cannot be opened ')
              go to 100
           endif

        endif
        return
        end
