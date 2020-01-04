C    %W% %G%
        subroutine getswn(swname)
        character *(*) swname

C       This program gets the swing file name. It checks the users 
C       directory first and the WSCCbase_Dir last for the file. If it
C       does not find the file it send a message that the swing data
C       file cannot be opened.
        

        logical yes
C
C       GET SWING FILE NAME
C
100	write (*,200)
200	format(' ENTER SWING SAVED DATA FILE NAME >',$)

	read (*,300) swname
300	format (a)

	nambeg = index(swname,']')
	if (nambeg.eq.0) nambeg = index(swname,':')
	namend = index(swname(nambeg+1:),'.')

	if (namend.eq.0) then
	   namext = index(swname(nambeg+namend+1:),' ')
	   swname = swname(1:nambeg+namend+namext-1) // '.SDI'
	endif

	inquire(file=swname,exist=yes)     ! Does Swing Data file exist in 
					   ! users directory
	if (.not. yes) then
           inquire(file=swname,exist=yes)

           if (.not.yes) then
              i = index(swname,':')
              swname = 'WSCCBASE_DIR:' // swname(i+1:)    ! If .SDI is not in
              inquire (file=swname,exist=yes)   ! users Dir. check WSCCBASE_DIR
           endif

        endif

        if ( .not. yes) then
           print 400, swname
400        format(' The Swing Data file ',a,' cannot be opened ')
           go to 100
     	endif

        return
        end
