C    %W% %G%
C****************************************************************
C
C   	File: new_znnm.f
C
C   	Purpose: Create a unique zone name and number for the duplicate
C                zone given.
C                                                                      *
C       Input parameters:
C
C             newzonename            - New unique zone name
C
C       Return value:
C
c             status   - N = 0 : Success
C                        N = 1 : failure
C
C
C   	Author: Walt Powell            Date: 5 January 1999
C   	Called by: clnup_ge.f
C
C****************************************************************
C
      integer function new_znnm ( newzonename )
      character *(*) newzonename
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/prt.inc'
      include 'ipfinc/pti_data.inc'

      integer ilast, jlast, fnd_ptiy, fd_gezones
      logical finished
      character prefix*10

      data ilast, jlast / 0, 0/
      data prefix / '_<>+-&!@%^' /
      save ilast, jlast
c	
c     Create a unique 2-character zone name based upon 
C     zone(1:1) = [_<>+-&!@%^] and zone(2:2) = [A-Z].
c
      new_znnm = 0        ! Set default return "Success"

      i = ilast
      j = jlast
      finished = (i .ge. 10 .or. j .ge. 26)
      do while (.not. finished)
        newzonename(1:1) = prefix(i+1:i+1)
        newzonename(2:2) = char (ichar ('A') + j)
        num = fnd_ptiy (newzonename(1:2))  ! Test if name exists
        if (num .eq. 0) num = fd_gezones (0, newzonename(1:2))
        if (num .gt. 0) then
          j = j + 1
          if (j .ge. 26) then
            i = i + 1
            j = 0
          endif
          finished = (i .ge. 10 .or. j .ge. 26)
        else
          finished = .true.
        endif
      enddo

      if (i .ge. 10 .or. j .ge. 26) then
        write (errbuf(1), 10000) num
10000   format (' A unique zone name could on created for unassigned zon
     &e number ', i4)
        call prterx ('W',1)                
        new_znnm = 1
      endif

      ilast = i
      jlast = j

      return
      end
