C    @(#)pfopen.f	20.3 2/13/96
        subroutine pfopen
 
c This routine is called only from BPF, IPF_CUT, and possibly some other
c stand alone utilities.
c It is NOT called by IPFSRV.
c It opens the following files:
c
c       Unit   File name
c
c       dbug   debugf
c       inp    inpnm
c       lprt   printf
c       mfich  fichef
c
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c The "dbug" file MUST  be opened first, since "open_file" writes
c to "dbug" !!!
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        include 'ipfinc/blank.inc'
        include 'ipfinc/dtaiop.inc'
        include 'ipfinc/filnam.inc'
        include 'ipfinc/jobctl.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/prt.inc'
 
        character * 60  debugf, printf, fichef, fname, tname
        character * 10  usernm, temp_date, tempc
        character * 1   backslash
        integer err, ofstat
        integer open_file ! function
        logical file_exists, found
 
        backslash = char(92)
        temp_date = ' '
        call xdate(temp_date)
        dte = temp_date
        usernm = ' '
        call get_user(usernm)
        user = usernm
 
        if ( inpnm .eq. ' ' ) then
 
  200      write (*, 210)
  210      format(' Enter Power Flow Control (PFC) file name > ', $ )
 
           read (*,220) inpnm
  220      format(a)
 
           if (inpnm .eq. ' ') then
              write (*, 230) 
  230         format (' Blank file request terminates execution')
              call exit
           endif
           inquire( file=inpnm, exist=file_exists )
           if ( .not. file_exists ) then
              last = lastch (inpnm)
              write (*, 240) inpnm(1:last)
  240         format (' File  "', a, '"  does not exist')
              goto 200
           endif
        endif
 
c*******************************************************************
c********** modified so "dbug" is opened as first file *************
c*******************************************************************

        inquire( file=inpnm, exist=file_exists )
        if ( .not. file_exists ) then
           last = lastch (inpnm)
           write (*, 240) inpnm(1:last)
           call exit
        endif

        fname = inpnm
        i = index (fname, ':')
        if (i .gt. 1) then
           tname = fname(i+1:)
           fname = tname
        endif
        if ( is_it_vms() .eq. 1 ) then
           i = index (fname, ']')
           if (i .gt. 1) then
              tname = fname(i+1:)
              fname = tname
           endif
        else
           i = len(fname)
           found = .false.
           do while ( i .gt. 1  .and.  .not. found )
              i = i - 1
              if ( fname(i:i) .eq. '/'  .or.
     &             fname(i:i) .eq. backslash ) found = .true.
           end do
           if ( found ) then
              tname = fname(i+1:)
              fname = tname
           endif
        endif
        idot = index( fname, '.' )
        if (idot .ne. 0) then
           tname = fname(1:idot-1)
           fname = tname
        endif

        last = lastch (fname)
c***        if ( last .gt. 8 ) lenunm = 8 ! DOS only

        debugf = fname(1:last) // '.pfd'
        printf = fname(1:last) // '.pfo'
        if ( is_it_vms() .eq. 1 ) then
           fichef = 'f_output:' // fname(1:last) // '.pff'
        else
           fichef = fname(1:last) // '.pff'
        endif
c**********************************************************************
c***  do not use "open_file" on "dbug" since "open_file" writes to
c***  "dbug" (before it is open)
        tempc = 'unknown'
        if ( is_it_vms() .eq. 1 ) tempc = 'new'
        open ( unit=dbug, file=debugf, status=tempc,
     &  form='formatted', err=900 )
c***        ofstat = open_file(dbug,debugf,'FF','W',iostat)
c**********************************************************************
        ofstat = open_file(inp,inpnm,'FF','R',iostat)
        if ( ofstat .ne. 0 ) goto 930
        ofstat = open_file(lprt,printf,'FF','W',iostat)
        if ( ofstat .ne. 0 ) goto 950
        ofstat = open_file(mfich,fichef,'FF','W',iostat)
        if ( ofstat .ne. 0 ) goto 970
 
C       Print the first page heading
 
        write (outbuf,330) inpnm
 330    format(' * Power Flow Control (PFC) file is : (', a ,')' )
c
c       Print program version name in debug file
c
        call dbgprt(1)

        write (dbug,350) prgvsn
 350    format('0 BPA powerflow version ', a)
        call fortop
        call prtout(1)
        call space(2)
c
c       turn debug switch back off
c
        call dbgprt(0)
 
        return

  900   i = lastch(debugf)
        write (*,910)debugf(1:i)
  910   format(' Error opening debug file, file = ', a )
        call exit

  930   i = lastch(inpnm)
        write (*,940)inpnm(1:i)
  940   format(' Error opening program control file, file = ', a )
        close ( dbug, status='DELETE' )
        call exit

  950   i = lastch(printf)
        write (*,960)printf(1:i)
  960   format(' Error opening print file, file = ', a )
        close ( dbug, status='DELETE' )
        close ( inp )
        call exit

  970   i = lastch(fichef)
        write (*,980)fichef(1:i)
  980   format(' Error opening fiche file, file = ', a )
        close ( dbug, status='DELETE' )
        close ( lprt, status='DELETE' )
        close ( inp )
        call exit

        end
