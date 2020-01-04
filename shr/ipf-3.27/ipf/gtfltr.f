C    %W% %G%
C****************************************************************
C
C   File: gtfltr.f
C   Purpose: Routine to parse filters in /GET_DATA,TYPE=BUS_LIST_RPT 
C            command.
C
C   Author: Walt Powell  Date: 18 May 1992
C   Called by:
C
C****************************************************************
C
        logical function gtfltr(in_buffer)

        character in_buffer * (*)

        include 'ipfinc/parametr.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/arcntl.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/alpha.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/sortuvov.inc'
        include 'ipfinc/zonehash.inc'
        include 'ipfinc/ownhash.inc'
        include 'ipfinc/bsekvhsh.inc'
        include 'ipfinc/zbo.inc'

        common /case_flags/ owner_flag
        logical owner_flag

	character capital * 40, fmt * 8, zn * 2, ar * 10, own * 3, 
     1            bus_name * 8, word(50) * 40, cfmt * 8, temp * 40
        logical change_f

	external kmpuvov, swapuvov, kmpowner, swapowner
        integer kmpuvov, areasr, ownsch, find_bus, find_zon, 
     &          bldowner, h, p, oldp, owner_hash
        character null * 1, linefeed * 1, types * 115, ljstfy * 40

c**************************************************************************
c********************  NOT PORTABLE  **************************************
c***    data types(1:39)  / '* #A*#A?#I #B*#L*#B?#B #BE#BS#BC#BD#BV#' /
c***    data types(40:78) / 'BQ#BG#BT#BX#BM#BF#+ #X #Q #L #LD#LM#E #' /
c***    data types(79:90) / 'T #TP#R #RZ#' /
c**************************************************************************
        types( 1:45) = '* #A*#A?#I #B*#L*#B?#B #BE#BS#BC#BD#BV#BQ#BG#'
        types(46:90) = 'BT#BX#BM#BF#+ #X #Q #L #LD#LM#E #T #TP#R #RZ#'
        types(91:115) = '+*#+ #+N#+I#+F#+S#+P#+A#'
c**************************************************************************

	null = char(0)
	linefeed = char(10)

        call updzon()    ! Conditionally rebuild cross-reference
c                        ! and loss arrays

        do i = 1, 7
           filter(i) = 0
        enddo

        last = index (in_buffer, null) - 1
        if (last .le. 0) last = len (in_buffer)
        call uscan (in_buffer(1:last), word, numwrd, '/=' ,
     &              ', ' // linefeed // null )

c	Remove any "..." bracketing word, convert to upper case
c
        do iwrd = 1, numwrd
           do while (index (word(iwrd), '"') .ne. 0)
              i = index (word(iwrd), '"')
              word(iwrd)(i:) = word(iwrd)(i+1:)
           enddo
           word(iwrd) = capital(word(iwrd))
        enddo

	iwrd = 1
        do while (iwrd .le. numwrd)
           if (word(iwrd)(1:4) .eq. 'AREA') then
              i = iwrd + 1
              if (word(i) .eq. '=') i = i + 1
              do while (i .le. numwrd .and. word(i) .ne. 'AND' .and.
     1                  word(i)(1:1) .ne. '(')

                 ar = word(i)
                 iz = areasr (ar)
                 if (iz .gt. 0) then
                    filter(1) = filter(1) + 1
                    area_filter(filter(1)) = ar
                 else
                    write (errbuf(1), 130) ar
  130               format (' Area (', a, ') is not in system')
                    iz = iabs (iz)
                    ierr = 1
                    do j = iz-1, iz+1
                       if (j .gt. 0 .and. j .le. ntotc) then
                          ierr = ierr + 1
                          write (errbuf(ierr), 132) arcnam(j)
  132                     format (' Adjacent area names ', a10)
                       endif
                    enddo
                    call prterx ('W', ierr)
                 endif
                 i = i + 1
              enddo
              iwrd = i
           else if (word(iwrd)(1:4) .eq. 'ZONE') then
              i = iwrd + 1
              if (word(i) .eq. '=') i = i + 1
              do while (i .le. numwrd .and. word(i) .ne. 'AND' .and.
     1                  word(i)(1:1) .ne. '(')

                 zn = word(i)
                 iz = find_zon (zn)
                 if (iz .gt. 0) then
                    filter(2) = filter(2) + 1
                    zone_filter(filter(2)) = zn
                 else
                    write (errbuf(1), 160) zn
  160               format (' Zone (', a, ') is not in system')
                    ierr = 1
                    iz = iabs (iz)
                    do j = iz-1, iz+1
                       if (j .gt. 0 .and. j .le. nztot) then
                          ierr = ierr + 1
                          write (errbuf(ierr), 162) acznam(j)
                          write (*, 162) acznam(j)
  162                     format (' Adjacent zone names ', a2)
                       endif
                    enddo
                    call prterx ('W', ierr)
                 endif
                 i = i + 1
              enddo
              iwrd = i
           else if (word(iwrd)(1:5) .eq. 'OWNER') then
              i = iwrd + 1
              if (word(i) .eq. '=') i = i + 1

              do while (i .le. numwrd .and. word(i) .ne. 'AND' .and.
     1                  word(i)(1:1) .ne. '(')

                 own = word(i)
                 iz = ownsch (own)
                 if (iz .gt. 0) then
                    filter(3) = filter(3) + 1
                    owner_filter(filter(3)) = own
                 else
                    write (errbuf(1), 180) own
  180               format (' Owner (', a, ') is not in system')
                    ierr = 1
                    iz = iabs (iz)
                    if (.not. owner_flag) then
                       do j = 1, numown
                          own2alf(alf2own(j)) = j
                       enddo
                       owner_flag = .true.
                    endif
c
c                   Note: An intricacy of ownsch is that it
c                   returns the -alpha sort index if the
c                   owner cannot be found. 
c
                    iz = max (1, min (iz, numown))
                    do j = iz-1, iz+1
                       if (j .gt. 0 .and. j .le. numown) then
                          ierr = ierr + 1
                          write (errbuf(ierr), 182) 
     &                       owner_o(alf2own(j))(1:3)
  182                     format (' Adjacent owner names ', a2)
                       endif
                    enddo
                    call prterx ('W', ierr)
                 endif
                 i = i + 1
              enddo
              iwrd = i
           else if (word(iwrd)(1:4) .eq. 'BASE') then
              i = iwrd + 1
              if (word(i) .eq. '=') i = i + 1
              do while (i .le. numwrd .and. word(i) .ne. 'AND' .and.
     1                  word(i)(1:1) .ne. '(')

                 filter(4) = filter(4) + 1
                 last = lastch(word(i))
                 write (fmt, 102) last
  102            format ('(f', i2, '.0)')
                 read (word(i), fmt=fmt, err=199, end=199) 
     1                 voltage_filter(1,filter(4))
                 read (word(i), fmt=fmt, err=199, end=199) 
     1                 voltage_filter(2,filter(4))
                 go to 201

  199            last = lastch (word(i))
                 write (errbuf(1), 200) word(i)(1:last)
  200            format (' Illegal BASE voltage range (', a, ') ')
                 call prterx ('W', 1)

  201            continue
                 i = i + 1
              enddo
              iwrd = i

           else if (word(iwrd)(1:4) .eq. 'TYPE') then
              i = iwrd + 1
              if (word(i) .eq. '=') i = i + 1
              do while (i .le. numwrd .and. word(i) .ne. 'AND' .and.
     1                  word(i)(1:1) .ne. '(')

                 ar = word(i)
                 last = lastch (ar)
                 if (last .lt. 2) ar(last+1:) = ' '
                 if (index (types, ar(1:2)) .ne. 0) then
                    filter(5) = filter(5) + 1
                    type_filter(filter(5)) = ar
                 else
                    write (errbuf(1), 232) ar(1:2)
  232               format (' Invalid area/bus/branch type (', a, ')')
                    call prterx ('W', 1)
                 endif
                 i = i + 1
              enddo
              iwrd = i

           else if (word(iwrd)(1:3) .eq. 'BUS' .or.
     &              word(iwrd)(1:9) .eq. 'AFTER_BUS') then
              i = iwrd + 1
              if (word(i) .eq. '=') i = i + 1
              do while (i .le. numwrd .and. word(i) .ne. 'AND' .and.
     1                  word(i)(1:1) .ne. '(')

                 bus_name = word(i)
                 word(i) = word(i)(9:)
                 word(i) = ljstfy(word(i))
                 if (index(word(i), '.') .eq. 0) then
                    last = lastch(word(i))
                    temp = word(i)(1:last) // '.0'
                    word(i) = temp
                 endif
                 last = lastch(word(i))
                 write (cfmt, 215) last
  215            format ('(f', i2, '.0)')
                 read (word(i), cfmt, err = 220) base_kv
                 if (bus_name .eq. ' ') goto 320
                    nb = find_bus (bus_name, base_kv)
  220               if (nb .le. 0) then
                       write (errbuf(1), 300) bus_name, base_kv
  300                  format (' Bus (', a, f6.1, ') is not in system.')
                       call prterx ('W', 1)
                    else
                       filter(6) = filter(6) + 1
                       bus_filter(filter(6)) = nb
                    endif
  320            continue
                 i = i + 1
              enddo
              iwrd = i

           else if (word(iwrd)(1:3) .eq. 'ALL') then
              iwrd = iwrd + 1
              change_f = .true.

           else if (word(iwrd)(1:7) .eq. 'LOADING') then
              i = iwrd + 1
              if (word(i) .eq. '=') i = i + 1
              do while (i .le. numwrd .and. word(i) .ne. 'AND' .and.
     1                  word(i)(1:1) .ne. '(')

                 last = lastch(word(i))
                 write (fmt, 102) last
                 read (word(i), fmt=fmt, err=330, end=330) range
                 if (filter(7) .eq. 0) then
                    filter(7) = 1
                    range_filter(1) = range
                 else
                    range_filter(2) = range
                 endif
                 go to 350

  330            last = lastch (word(i))
                 write (*, 340) word(i)(1:last)
  340            format (' Meaningless loading factor (', a, ') ')
                 call prterx ('W', 1)

  350            continue
                 i = i + 1
              enddo
              iwrd = i

           else if (word(iwrd)(1:3) .eq. 'AND') then
              iwrd = iwrd + 1

           else if (word(iwrd)(1:1) .eq. '(') then
              iwrd = numwrd + 1

           else
              write (errbuf(1), 234) word(iwrd)
  234         format (' Invalid FILTER (', a, ')')
              call prterx ('W', 1)
              change_f = .false.
              iwrd = iwrd + 1
           endif
        enddo
        gtfltr = change_f
        return
        end
