C    %W% %G%
      subroutine getflcs (caseid, filnam) 
      character caseid*(*), filnam*(*)

c     Gets a case ID from a file name by stripping off the
c     directory name and file extensions.  
c     Called by GETIDS

      character string*60

      if (filnam .eq. ' ') then
        caseid = ' '
        return
      endif

      string = filnam

      if (is_it_vms() .ne. 0) then

c       For VAX VMS, directory names end in ']' or ':'

        do while (index (string,']') .ne. 0)
          ka = index (string,']')
          string = string(ka+1:)
        enddo
        do while (index (string,':') .ne. 0)
          ka = index (string,':')
          string = string(ka+1:)
        enddo

      else

c       For UNIX, directory names end in '/'

        do while (index (string,'/') .ne. 0)
          ka = index (string,'/')
          string = string(ka+1:)
        enddo
   
      endif

      ka = index (string,'.')
      if (ka .gt. 1) then
        caseid = string(1:ka-1)
      else
        caseid = string
      endif
c
      return
      end
