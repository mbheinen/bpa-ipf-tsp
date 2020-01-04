C    %W% %G%
      function ifunit(iux)
C     
C    THIS FUNCTION IS USED IN CONJUNCTION WITH WRITMP TO
C    SIMULATE A CALL TO A MASS STORAGE DEVICE.  THE VALUE
C    RETURNED BY THE FUNCTION FOR A GIVEN IUX IS:
C       IUX              IFUNIT
C        1                  1
C        2                  2
C        4                  3
C        7                  4
C       10                  5
C       12                  6
C       13                  7
C     IFUNIT IS CALLED BY WRITMP.
C     
      include 'tspinc/prt.inc'
 
      if (iux .eq. 2) then
        ifunit = 2
      else
        if (iux .eq. 3 .or. iux .eq. 5 .or. iux .eq. 6 .or. 
     &      iux .eq. 8 .or. iux .eq. 9 .or. iux .eq. 11) then
          write (errbuf(1), 10000) iux
10000     format ('BAD UNIT NUMBER IN IFUNIT ', i6, 'BAD UNIT')
          call prterr('E', 1)
          call erexit()
        elseif (iux .ne. 4) then
          if (iux .eq. 7) then
            ifunit = 4
          elseif (iux .eq. 10) then
            ifunit = 5
          elseif (iux .eq. 12) then
            ifunit = 6
          elseif (iux .eq. 13) then
            ifunit = 7
          else
            ifunit = 1
          endif
          goto 100
        endif
        ifunit = 3
      endif
  100 return
      end
