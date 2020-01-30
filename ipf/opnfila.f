C    @(#)opnfila.f	20.3 2/13/96
        subroutine opnfila( lunit, filenm, error )
 
C       Routine to open a sequential ASCII file.
C
c       Special exit flags:
c          error = 0  : normal exit
c          error = n  : errs code number if file not found
c
        include 'ipfinc/jobctl.inc'
        include 'ipfinc/prt.inc'

        common /is_batch / is_batch

        integer open_file ! function
        integer lunit, error, ofstat
        logical yes, file_exists
        character filenm *(*), newnam*60, tnam1*60, tnam2*60,
     &            basecase_dir*60, tempname*60
 
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
 
        if ( is_it_vms() .eq. 1 ) then
           inquire ( file=newnam, exist=file_exists )
           if ( .not. file_exists ) then
              i = lastch( newnam )
              write(*,210) newnam(1:i), 
     &          'not found, searching  BASECASE_DIR'
  210         format (' File  "', a, '"  ', a )
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
              write(*,210) tnam2(1:il), 
     &          'not found in basecase_dir, searching  WSCCEOFC_DIR'
              newnam = 'wscceofc_dir:' // tnam2
              inquire ( file=newnam, exist=file_exists )
           endif
           if ( .not. file_exists ) then
              write(*,210) tnam2(1:il), 
     &          'not found in wscceofc_dir, searching  WSCCBASE_DIR'
              newnam = 'wsccbase_dir:' // tnam2
              inquire ( file=newnam, exist=file_exists )
           endif
           if ( .not. file_exists ) then
              write(*,220) tnam2(1:il)
  220         format (' Search Failure on File  "', a, '"' )
              error = -1
              goto 900
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
     &              'not found, searching BASECASE_DIR'
                 newnam = basecase_dir(1:last2) // tnam2(1:last1)
                 inquire ( file=newnam, exist=file_exists )
              endif
              if ( .not. file_exists ) then
                 write(*, 220) tnam2(1:il)
                 error = -1
                 goto 900
              endif
           endif
        endif

        ofstat = open_file(lunit,newnam,'F','R',error)
        if ( ofstat .ne. 0 ) goto 900

        newnam = ' '
        inquire (unit=lunit,name=newnam)
        filenm = newnam
 
  900   continue
        return
        end
