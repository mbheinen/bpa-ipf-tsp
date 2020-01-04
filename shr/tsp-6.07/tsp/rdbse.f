C    %W% %G%
       subroutine rdbse( rtn_status, mvtype )
       implicit none
       integer rtn_status
       integer mvtype
c
c  setup the internal variables for reading a binary base
c  case using the ipf subroutine rddtai
c  input: type
c    1 = read bse data and move to swing solution processing variables
c    2 = read bse data and move to swing reporting variables
c  input/output: rtn_status
c    0 = failed to read file try to read again
c    1 = file read ok just move data according to type
c

c
c tsp includes
c
       include 'tspinc/blkcom1.inc'
c
c ipf includes
c
       include 'ipfinc/lfiles.inc'
       include 'ipfinc/jobctl.inc'
       include 'ipfinc/dtaiop.inc'
       include 'ipfinc/pfdata.inc'
       include 'ipfinc/blank.inc'
       include 'ipfinc/coment.inc'
       include 'ipfinc/header.inc'
c
c  external functions
c
       integer keybrd
c
c temporary rw variables
c
       integer loaded
       character * 10 caseid
       integer file_not_found_exit
       integer file_not_found_open
       character * 255 filename_o
       character * 255 filename_i
       integer icase_vers

       rtn_status = 0
       file_not_found_exit = 99
       file_not_found_open = 98
c
c set ipf variables to tsp equivalents
c
c       filename_i = bsefil
       datai = l3
c
c setup typical ipf initialization for rddtai
c
       if(rtn_status .ne. 1) then
         caseid = ' '
         loaded = -1

         prgvsn = 'IPF  201'
         kspare(33) = 8

         call get_user(usrnam)
         user = usrnam
         call xdate(dte)
         coment(1) = ' '
         coment(2) = ' '
 
c        call opnfil(datai, filename_i, file_not_found_exit)
         rewind(l3)
         call rddtai(caseid, loaded)
c        call close_file(datai)
         rtn_status = loaded
       endif

       if(rtn_status .ne. 1) then
         write(*,'(a,i6)') 'Error - base case not loaded status = ', 
     &     rtn_status
         call erexit
       endif

       icase_vers = kspare(22)
       if(keybrd(0) .ne. 0) then
         write(*, '(a,i6)') 'bse file read ok open/close skipped ',
     &     loaded
         write(*, '(a,i6)') ' icase version is ', icase_vers
       endif

c
c  either move the data from ipf commons to tsp commons
c  for a tsp solution or for tsp reports
c
       if(mvtype .eq. 1) then
         call rdbsemvs
       else if(mvtype .eq. 2) then
         call rdbsemvr
       else
         write(*,'(a,i6)') 'rdbse: Error - bad file type (1 or 2) ', 
     &    mvtype
       endif

c
c old test code
c
c      if(mvtype .eq. 1) then
c      if(icase_vers .lt. 4) then
c         write(*, '(a)') 'rdbse: use old tsp read logic in old bse'
c         rewind(l3)
c         call rdbsets
c       else
c         write(*, '(a)') 'rdbse: move case data from ipf commons'
c         call rdbsemvs
c       endif
c       endif
c     if(mvtype .eq. 2) then
c
c     if(icase_vers .lt. 4) then
c       rewind(l3)
c       call rdbsetr
c       write(*, '(a)') 'rdbse: read old bse data with old tsp output logic'
c     else
c       call rdbsemvr
c       write(*, '(a)') 'rdbse: move bse data ipf to tsp output commons'
c     endif
c     endif

       return
       end
