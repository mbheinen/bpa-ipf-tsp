C    @(#)gtnwdcno.f	20.1 11/11/97
C****************************************************************
C
C     File: gtnwdcno.f
C
C     Purpose: Function to get new d-c bus number.
C
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: ld_gebdc.f
C
C****************************************************************
      integer function gtnwdcno (num)
      integer error

      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/pti_data.inc'
      include 'ipfinc/prt.inc'
 
      common /num_dcbuses/ count_dcbus_no, dcbus_no(2,MAXMDC),
     &                     highest_bus_no
      integer count_dcbus_no, dcbus_no, highest_bus_no

      integer fnd_ptin
      logical finished

      newnum = 0
      gtnwdcno = 0

      i = 1
      finished = .false.
      do while (i .le. count_dcbus_no .and. .not. finished)
        if (num .eq. dcbus_no(1,i)) then
          newnum = dcbus_no(2,i)
          finished = .true.
        else
          i = i + 1
        endif
      enddo
      
      if (newnum .eq. 0) then
        highest_bus_no = highest_bus_no + 1
c
c       Test "highest_bus_no" for uniqueness
c
        new = fnd_ptin (highest_bus_no)
        if (new .le. 0) then
          newnum = highest_bus_no
          count_dcbus_no = count_dcbus_no + 1
          dcbus_no(1,count_dcbus_no) = num
          dcbus_no(2,count_dcbus_no) = newnum
          write( errbuf(1), 10000) num, newnum
10000     format(' D-C bus no. ', i6, ' is assigned new number  ', 
     &      i6)
          call prterx ('I', 1)
        else
          write( errbuf(1), 10010) num
10010     format(' Cannot generate a unique bus number for d-c bus (',
     &      i6, ')')
          call prterx ('E', 1)
          newnum = -1
        endif
      endif

      gtnwdcno = newnum
      return
      end
