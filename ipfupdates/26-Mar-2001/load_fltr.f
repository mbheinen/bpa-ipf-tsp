C    %W% %G%
	logical function load_fltr (in_buffer, out_buffer, i2) 
        character in_buffer *(*), out_buffer *(*)

	include 'ipfinc/parametr.inc'
	include 'ipfinc/blank.inc'
	include 'ipfinc/bus.inc'
	include 'ipfinc/arcntl.inc'
        include 'ipfinc/sortuvov.inc'
        include 'ipfinc/ownhash.inc'
        include 'ipfinc/bsekvhsh.inc'
        include 'ipfinc/zbo.inc'

        common /case_flags/ owner_flag
        logical owner_flag
c
c       owner_flag - owner array loaded?
c
	character capital * 72, text * 80, word(20) * 10, 
     &            zn * 2, ar * 10, own * 3, command(20) * 10, 
     &            fmt * 10
        logical change_f, change_def_v, first

	external kmpowner, swapowner
        integer areasr, ownsch, apdoutbuf, find_zon, kmpowner,
     &          bldowner, p, h, oldp, owner_hash
        character null * 1, linefeed * 1, types * 87

        data types 
     &    /'B?$B $BE$BS$BC$BD$BV$BQ$BG$BO$BT$BX$BM$L $LD$LM$$E $T $TP$R 
     &$+*$+ $+N$+I$+F$+S$+P$+A$X '/

	null = char(0)
	linefeed = char(10)

        call updzon()    ! Conditionally rebuild cross-reference
c                        ! and loss arrays

        change_f = .false.

        write (*, 100)
  100   format (' > Enter filter: All, AReas, Zones, Owners, Base range,
     & Type, Loading, or Quit : ')
        read (*, 110) text
  110   format (a)
        if (text .eq. ' ' .or. 
     &      text .eq. 'Q' .or. 
     &      text(1:1) .eq. null .or. 
     &      text(1:1) .eq. linefeed) then
           change_f = .false.
        else
           do i = 1, 7
              filter(i) = 0
           enddo
           call uscan (text, command, numcmd, '$', ',')
           do icmd = 1, numcmd
              command(icmd) = capital(command(icmd))
              if (command(icmd)(1:2) .eq. 'AR') then
                 change_f = .true.
  118            write (*, 120)
  120            format (' > Enter AREA1, AREA2, ... ')
                 read (*, 110) text
                 if (text .ne. ' ' .and. 
     &               text(1:1) .ne. null .and. 
     &               text(1:1) .ne. linefeed) then
                    call uscan (text, word, numwrd, '$', ',')
                    do i = 1, numwrd
                       ar = capital(word(i))
                       iz = areasr (ar)
                       if (iz .gt. 0) then
                          filter(1) = filter(1) + 1
                          area_filter(filter(1)) = ar
                       else
                          write (*, 130) ar
  130                     format (' Area (', a, ') is not in system')
                          iz = 1
                          do while (iz .le. ntotc .and.
     &                             (ar .ge. arcnam(iz)))
                             iz = iz + 1
                          enddo
                          iz = iz - 1
                          do j = iz-1, iz+1
                             if (j .gt. 0 .and. j .le. ntotc) then
                                write (*, 132) arcnam(j)
  132                           format (' Adjacent area names ', a10)
                             endif
                          enddo
                          go to 118
                       endif
                    enddo
                 endif
              else if (command(icmd)(1:1) .eq. 'Z') then
                 change_f = .true.
  218            write (*, 220)
  220            format (' > Enter ZONE1, ZONE2, ... ')
                 read (*, 110) text
                 if (text .ne. ' ' .and. 
     &               text(1:1) .ne. null .and.
     &               text(1:1) .ne. linefeed) then
                    call uscan (text, word, numwrd, '$', ',')
                    change_f = .true.
                    do i = 1, numwrd
                       zn = capital(word(i))
                       iz = find_zon (zn)
                       if (iz .gt. 0) then
                          filter(2) = filter(2) + 1
                          zone_filter(filter(2)) = zn
                       else
                          write (*, 160) zn
  160                     format (' Zone (', a, ') is not in system')
                          iz = iabs (iz)
                          do j = iz-1, iz+1
                             if (j .gt. 0 .and. j .le. nztot) then
                                write (*, 162) acznam(j)
  162                           format (' Adjacent zone names ', a2)
                             endif
                          enddo
                          go to 218
                       endif
                    enddo
                 endif
              else if (command(icmd)(1:1) .eq. 'O') then
                 change_f = .true.

  238            write (*, 240)
  240            format (' > Enter OWNER1, OWNER2, ... ')
                 read (*, 110) text
                 if (text .ne. ' ' .and. 
     &              text(1:1) .ne. null .and.
     &              text(1:1) .ne. linefeed) then
                    change_f = .true.
                    call uscan (text, word, numwrd, '$', ',')
                    do i = 1, numwrd
                       own = capital(word(i))
                       iz = ownsch (own)
                       if (iz .gt. 0) then
                          filter(3) = filter(3) + 1
                          owner_filter(filter(3)) = own
                       else
                          write (*, 180) own
  180                     format (' Owner (', a, ') is not in system')
                          if (.not. owner_flag) then
                             do j = 1, numown
                                own2alf(alf2own(j)) = j
                             enddo
                             owner_flag = .true.
                          endif
                          iz = max (1, min (iabs(iz), numown))
c
c                         Note: An intricacy of ownsch is that it
c                         returns the -alpha sort index if the
c                         owner cannot be found. 
c
                          do j = iz-1, iz+1
                             if (j .gt. 0 .and. j .le. numown) then
                                write (*, 182) 
     &                             owner_o(alf2own(j))(1:3)
  182                           format (' Adjacent owner names ', a3)
                             endif
                          enddo
                          go to 238
                       endif
                    enddo
                 endif
              else if (command(icmd)(1:1) .eq. 'B') then
                 change_f = .true.
  338            write (*, 340)
  340            format (' > Enter BASE KV range : ')
                 read (*, 110) text
                 if (text .ne. ' ' .and. 
     &               text(1:1) .ne. null .and.
     &               text(1:1) .ne. linefeed) then
                    change_f = .true.
                    call uscan (text, word, numwrd, '<>', ' ')
                    if (word(1) .eq. '<') then
                       filter(4) = filter(4) + 1
                       voltage_filter(1,filter(4)) = 0.0
                       last = lastch (word(2))
                       write (fmt, '(''(f'', i2, ''.0)'')') last
                       read (word(2), fmt, err=199, end=199) 
     &                    voltage_filter(2,filter(4))
                    else if (word(1) .eq. '>') then
                       filter(4) = filter(4) + 1
                       last = lastch (word(2))
                       write (fmt, '(''(f'', i2, ''.0)'')') last
                       read (word(2), fmt, err=199, end=199) 
     &                    voltage_filter(1,filter(4))
                       voltage_filter(2,filter(4)) = 9999.0
                    else if (word(2) .eq. '<') then
                       filter(4) = filter(4) + 1
                       last = lastch (word(1))
                       write (fmt, '(''(f'', i2, ''.0)'')') last
                       read (word(1), fmt, err=199, end=199) 
     &                    voltage_filter(1,filter(4))
                       last = lastch (word(3))
                       write (fmt, '(''(f'', i2, ''.0)'')') last
                       read (word(3), fmt, err=199, end=199) 
     &                    voltage_filter(2,filter(4))
                    else if (word(2) .eq. '>') then
                       filter(4) = filter(4) + 1
                       last = lastch (word(1))
                       write (fmt, '(''(f'', i2, ''.0)'')') last
                       read (word(1), fmt, err=199, end=199) 
     &                    voltage_filter(2,filter(4))
                       last = lastch (word(3))
                       write (fmt, '(''(f'', i2, ''.0)'')') last
                       read (word(3), fmt, err=199, end=199) 
     &                    voltage_filter(1,filter(4))
                    else if (numwrd .eq. 1) then
                       filter(4) = filter(4) + 1
                       last = lastch (word(1))
                       write (fmt, '(''(f'', i2, ''.0)'')') last
                       read (word(1), fmt, err=199, end=199) 
     &                    voltage_filter(1,filter(4))
                       last = lastch (word(1))
                       write (fmt, '(''(f'', i2, ''.0)'')') last
                       read (word(1), fmt, err=199, end=199) 
     &                    voltage_filter(2,filter(4))
                    else if (numwrd .eq. 2) then
                       filter(4) = filter(4) + 1
                       last = lastch (word(1))
                       write (fmt, '(''(f'', i2, ''.0)'')') last
                       read (word(1), fmt, err=199, end=199) 
     &                    voltage_filter(1,filter(4))
                       last = lastch (word(2))
                       write (fmt, '(''(f'', i2, ''.0)'')') last
                       read (word(2), fmt, err=199, end=199) 
     &                    voltage_filter(2,filter(4))
                    endif
                    go to 201

  199               last = lastch (text)
                    write (*, 200) text(1:last)
  200               format (' Meaningless voltage range (', a, ') ')
                    go to 338
  201               continue

                 endif
              else if (command(icmd)(1:1) .eq. 'T') then
                 change_f = .true.
  202            write (*, 204)
  204            format (' > Enter type1, type2, ...')
                 read (*, 110) text
                 if (text .ne. ' ' .and. 
     &               text(1:1) .ne. null .and. 
     &               text(1:1) .ne. linefeed) then
                    call uscan (text, word, numwrd, '$', ',')
                    do i = 1, numwrd
                       ar = capital(word(i))
                       if (index (types, ar(1:2)) .ne. 0) then
                          filter(5) = filter(5) + 1
                          type_filter(filter(5)) = ar
                       else
                          write (*, 232) ar(1:2)
  232                     format (' Invalid bus/branch type (', a, ')')
                          go to 202
                       endif
                    enddo
                 endif
              else if (command(icmd)(1:1) .eq. 'L') then
                 change_f = .true.
  534            write (*, 536)
  536            format (' > Enter LOADING1, LOADING2')
                 read (*, 110) text
                 call uscan (text, word, numwrd, '$', ' =,')
                 last = lastch (word(1))
                 write (fmt, '(''(f'', i2, ''.0)'')') last
                 read (word(1), fmt, err=538, end=538) range
                 filter(7) = 1
                 range_filter(1) = range
                 range_filter(2) = range
                 if (numwrd .eq. 2) then
                    last = lastch (word(1))
                    write (fmt, '(''(f'', i2, ''.0)'')') last
                    read (word(1), fmt, err=538, end=538) range
                    range_filter(2) = range
                 endif
                 go to 542

  538            last = lastch (text)
                 write (*, 540) text(1:last)
  540            format (' Meaningless loading range (', a, ') ')

  542            continue
              else if (command(icmd)(1:1) .eq. 'A') then
                 change_f = .true.
              else
                 change_f = .false.
              endif
           enddo
        endif
c
c       Translate filter from options
c
        i2 = index (in_buffer, null)
        first = .true.
        if (filter(1) .gt. 0) then
           ar = area_filter(1)
           if (first) then
              length = apdoutbuf (i2, 
     &            ' WHERE AREAS = "' // ar // '"', 
     &            in_buffer(i2:))
              i2 = i2 + length 
              first = .false.
           else
              length = apdoutbuf (i2, 
     &            ' AND AREAS = "' // ar // '"',
     &              in_buffer(i2:))
              i2 = i2 + length 
           endif
           do i = 2, filter(1)
              ar = area_filter(i)
              length = apdoutbuf (i2, 
     &            ', "' // ar // '"', in_buffer(i2:))
              i2 = i2 + length 
           enddo
        endif

        if (filter(2) .gt. 0) then
           zn = zone_filter(1)
           if (first) then
              length = apdoutbuf (i2, 
     &            ' WHERE ZONES = "' // zn // '"', 
     &            in_buffer(i2:))
              i2 = i2 + length 
              first = .false.
           else
              length = apdoutbuf (i2, 
     &            ' AND ZONES = "' // zn // '"',
     &              in_buffer(i2:))
              i2 = i2 + length 
           endif
           do i = 2, filter(2)
              zn = zone_filter(i)
              length = apdoutbuf (i2, 
     &            ', "' // zn // '"', in_buffer(i2:))
              i2 = i2 + length 
           enddo
        endif

        if (filter(3) .gt. 0) then
           own = owner_filter(1)
           if (first) then
              length = apdoutbuf (i2, 
     &            ' WHERE OWNERS = "' // own // '"', 
     &            in_buffer(i2:))
              i2 = i2 + length 
              first = .false.
           else
              length = apdoutbuf (i2, 
     &            ' AND OWNERS = "' // own // '"',
     &              in_buffer(i2:))
              i2 = i2 + length 
           endif
           do i = 2, filter(3)
              own = owner_filter(i)
              length = apdoutbuf (i2, 
     &            ', "' // own // '"', in_buffer(i2:))
              i2 = i2 + length 
           enddo
        endif

        if (filter(4) .gt. 0) then
           write (text(1:6), '(f6.1)') voltage_filter(1,1) 
           if (first) then
              length = apdoutbuf (i2, 
     &            ' WHERE BASES = "' // text(1:6) // '"', 
     &            in_buffer(i2:))
              i2 = i2 + length 
              first = .false.
           else
              length = apdoutbuf (i2, 
     &            ' AND BASES = "' // text(1:6) // '"',
     &              in_buffer(i2:))
              i2 = i2 + length 
           endif
           do i = 2, filter(4)
              write (text(1:6), '(f6.1)') voltage_filter(1,i) 
              length = apdoutbuf (i2, 
     &            ', "' // text(1:6) // '"', in_buffer(i2:))
              i2 = i2 + length
           enddo
        endif

        if (filter(5) .gt. 0) then
           text(1:2) = type_filter(1)
           if (first) then
              length = apdoutbuf (i2, 
     &            ' WHERE TYPES = "' // text(1:2) // '"', 
     &            in_buffer(i2:))
              i2 = i2 + length 
              first = .false.
           else
              length = apdoutbuf (i2, 
     &            ' AND TYPES = "' // text(1:2) // '"',
     &              in_buffer(i2:))
              i2 = i2 + length 
           endif
           do i = 2, filter(5)
              text(1:2) = type_filter(i)
              length = apdoutbuf (i2, 
     &            ', "' // text(1:2) // '"', in_buffer(i2:))
              i2 = i2 + length 
           enddo
        endif

        if (filter(7) .gt. 0) then
           write (text(1:13), '(f6.1, 1x, f6.1)') range_filter(1),
     &        range_filter(2) 
           if (first) then
              length = apdoutbuf (i2, 
     &            ' WHERE LOADING = ' // text(1:13), 
     &            in_buffer(i2:))
              i2 = i2 + length 
              first = .false.
           else
              length = apdoutbuf (i2, 
     &            ' AND LOADING = ' // text(1:13), 
     &              in_buffer(i2:))
              i2 = i2 + length 
           endif
        endif

        if (first) then
           length = apdoutbuf (i2, ' WHERE ALL ', in_buffer(i2:))
           i2 = i2 + length 
        endif
        length = apdoutbuf (i2, '(END)', in_buffer(i2:))
        i2 = i2 + length 

        load_fltr = change_f
        return
        end
