C    @(#)opnfil.f	20.4 2/13/96
        subroutine opnfil( lunit, filenm, error )
 
C       Routine to open a sequential, an ASCII file, or a binary file.
C
c       Special entry flags:
c          error = 98 : do not call erexit if file not found
c          error = 99 : file is binary, shared
c       Special exit flags:
c          error = 0  : normal exit
c          error = n  : errs code number if file not found
c
c*****************************************************************
c*** All "open" statements have been "commented out".
c*** The routine "open_file" is called instead.
c*****************************************************************
 
        include 'ipfinc/jobctl.inc'
        include 'ipfinc/prt.inc'

        common /is_batch / is_batch

        integer open_file ! function
        integer lunit, error, olderr, ofstat
        logical yes, file_exists
        character filenm *(*), newnam*60, tnam1*60, tnam2*60, 
     &            basecase_dir*60, tempname*60
 
        olderr = error
        error = 0
c
c       order of search
c
c       Begin: search [user]
c       Is file found?
c          No: search basecase_dir:
c              Is file found?
c
        newnam = filenm
 
        call close_file(lunit)
 
        if (olderr .eq. 99) then

C          binary file (oldbase)

           if ( is_it_vms() .eq. 1 ) then
              inquire ( file=newnam, exist=file_exists )
              if ( .not. file_exists ) then
                 i = lastch( newnam )
                 write(*,210) newnam(1:i), 
     &             'not found, searching BASECASE_DIR'
  210            format (' File  "', a, '"  ', a )
                 tnam2 = newnam
                 i = index (tnam2, ':')
                 tnam1 = tnam2(i+1:)
                 tnam2 = tnam1
                 i = index (tnam2, ']')
                 tnam1 = tnam2(i+1:)
                 tnam2 = tnam1
                 newnam = 'basecase_dir:' // tnam2
                 inquire ( file=newnam, exist=file_exists )
              endif
              if ( .not. file_exists ) then
                 il = lastch( tnam2 )
                 write(*, 210) tnam2(1:il), 
     &             'not found in BASECASE_DIR, searching WSCCEOFC_DIR'
                 newnam = 'wscceofc_dir:' // tnam2
                 inquire ( file=newnam, exist=file_exists )
              endif
              if ( .not. file_exists ) then
                 write(*, 210) tnam2(1:il), 
     &             'not found in WSCCEOFC_DIR, searching WSCCBASE_DIR'
                 newnam = 'wsccbase_dir:' // tnam2
                 inquire ( file=newnam, exist=file_exists )
              endif
              if ( .not. file_exists ) then
                 write(*, 220) tnam2(1:il)
  220            format (' Search Failure on File  "', a, '"' )
                 error = -1
                 goto 10
              endif
           else
              inquire ( file=newnam, exist=file_exists )
              if ( .not. file_exists ) then
	         basecase_dir = ' '
                 tempname = 'BASECASE_DIR' // char(0)
                 call getenvir (tempname, basecase_dir)
                 if (basecase_dir .ne. ' ') then
                    tnam2 = newnam
                    do while (index (tnam2, '/') .ne. 0) 
                       last1 = index (tnam2, '/')
                       tnam1 = tnam2(last1+1:)
                       tnam2 = tnam1
                    enddo
                    last1 = lastch( tnam2 )
                    last2 = lastch (basecase_dir)
                    write(*, 210) tnam2(1:last1), 
     &                 'not found, searching BASECASE_DIR'
                    newnam = basecase_dir(1:last2) // tnam2(1:last1)
                    inquire ( file=newnam, exist=file_exists )
                 endif
                 if ( .not. file_exists ) then
                    write(*, 220) tnam2(1:il)
                    error = -1
                    goto 10
                 endif
              endif
           endif

           ofstat = open_file(lunit, newnam, 'U', 'R', error)
           if ( ofstat .ne. 0 ) goto 10

        else

C          ASCII FILE

           inquire( file=newnam, exist=file_exists )
           if ( .not. file_exists ) then
              error=29
              goto 10
           endif
 
           ofstat = open_file(lunit, newnam, 'F', 'R', error) 
           if ( ofstat .ne. 0 ) goto 10

        endif
 
        newnam = ' '
        inquire (unit=lunit, name=newnam)
        filenm = newnam
 
        return
 
c       ******* error exit *******
 
   10   continue
        if (error .eq. 29) then
          last = lastch (filenm) 
          write (errbuf(1),30) filenm(1:last)
   30     format('0 File ',a,' does not exist--request ignored')
          if (is_batch .eq. 0) then
             call prterx ('E',1)
          else
             call prterx ('F',1)
          endif
 
        else if (error .eq. 43) then
          last = lastch (newnam) 
          write (errbuf(1),50) newnam(1:last)
   50     format('0 File ',a,' has bad specifications.')
          if (is_batch .eq. 0) then
             call prterx ('E',1)
          else
             call prterx ('F',1)
          endif
 
        else
          last = lastch (newnam) 
          write (errbuf(1),60) newnam(1:last), error
   60     format('0 Failure on opening file ',a,' error code =',i2)
          if (is_batch .eq. 0) then
             call prterx ('E',1)
          else
             call prterx ('F',1)
          endif
        endif
 
        return
        end
