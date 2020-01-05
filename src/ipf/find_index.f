C    @(#)find_index.f	20.2 7/18/96
C****************************************************************
C
C       File: find_index.f
C       Purpose: Routine to obtain the record index associated with
C                the input record.
C
C       Author: Walt Powell  Date: 4 October 1995
C                            Modified: 
C       Called by:
C
C****************************************************************
C
	integer function find_index (text)
        character text *(*)

        include 'ipfinc/parametr.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/cbus.inc'
        include 'ipfinc/xdata.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/arcntl.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/lfiles.inc'

        integer error, status, find_bus, find_cbs, find_ara, sect
        character id*1
        logical finished

        find_index = 0
        if (text(1:1) .eq. 'A') then
          find_index = find_ara (text(4:13))
        else if (text(1:1) .eq. 'I') then
          finished = .false.
          jt = 1
          do while (jt .le. ntotic .and. .not. finished)
            if (arcint(1,jt) .eq. text(4:13) .and.    
     &          arcint(2,jt) .eq. text(15:24)) then    
              finished = .true.
              find_index = jt
            else
              jt = jt + 1
            endif
          enddo
        else if (text(1:1) .eq. 'B') then
          read (text(15:18), '(bz, f4.0)', err=220) base1
          find_index = find_bus (text(7:14), base1)
        else if (text(1:1) .eq. '+') then
          find_index = find_cbs (text)
        else if (text(1:1) .eq. 'X') then
          jt = 1
          finished = .false.
          read (text(15:18), '(bz, f4.0)', err=220) base1
          do while (jt .le. kxtot .and. .not. finished)
            nb = xdata(1,jt)
            if (bus(nb) .eq. text(7:14) .and. base(nb) .eq. base1) then
              finished = .true.
              find_index = jt
            else
              jt = jt + 1
            endif
          enddo
        else if (index ('LRET', text(1:1)) .ne. 0) then
          read (text(15:18), '(bz, f4.0)', err=220) base1
          read (text(28:31), '(bz, f4.0)', err=220) base2
          read (text(33:33), '(bz, i1)', err=220) sect
          k1 = find_bus (text(7:14), base1)
          k2 = find_bus (text(20:27), base2)
          if (k1 .gt. 0 .and. k2 .gt. 0) then
            if (text(1:1) .eq. 'R') then
              id = '*'
            else
              id = text(32:32)
            endif
            find_index = numbrn (k1, k2, id, sect)
          endif
        endif
  220   return
        end
