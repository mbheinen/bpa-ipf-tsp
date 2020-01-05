C    @(#)get_defv.f	20.3 2/13/96
	logical function get_defv()

        include 'ipfinc/parametr.inc'
        include 'ipfinc/sortuvov.inc'

        common /bld_vlimit/ update
        logical update

        character query * 1, capital * 1, text * 72, word(20) * 30, 
     1            null * 1, linefeed * 1, fmt*10
        external kmpvlimit, swpvlimit
        logical error
        logical found, finished

        get_defv = .false.

	null = char(0)
	linefeed = char(10)

        write (*, 5)
    5   format (' > Change default v-limits (No, Yes) ? ',$)
        read (*, 110) query
  110   format (a)
        query = capital(query(1:1))
        text = query
        if (query .eq. 'Y') then

           update = .true.
           do i = 1, num_limit
              write (*, 10) v_max(i), v_min(i), v_range(1,i), '<',
     1                      v_range(2,i)
   10         format (' * Voltage limits : ',2f6.3, f6.1, 1x, a, f6.1)
           enddo

           do while (text .ne. ' ' .and. 
     1               text(1:1) .ne. null .and. 
     2               text(1:1) .ne. linefeed) 
              write (*, 20)
   20         format (' > Enter new limits - Vmax, Vmin, and KV range : 
     &',$)
              read (*, 110) text
              error = .false.

              if (text .eq. ' ') go to 203
              vlow = 0.0
              vhigh = 9999.0
              call uscan (text, word, numwrd, '$', ', ')
   	      if (numwrd .eq. 1) then
                 last = lastch (word(1))
                 write (fmt, '(''(f'', i2, ''.0)'')') last
                 read (word(1), fmt, end=199, err=199) vlow
                 read (word(1), fmt, end=199, err=199) vhigh
              else if (numwrd .ge. 3) then
                 last = lastch (word(1))
                 write (fmt, '(''(f'', i2, ''.0)'')') last
                 read (word(1), fmt, end=199, err=199) vmax
                 last = lastch (word(2))
                 write (fmt, '(''(f'', i2, ''.0)'')') last
                 read (word(2), fmt, end=199, err=199) vmin
                 last = lastch(word(3))
                 i = index(text, word(3)(1:last))
                 call uscan (text(i:), word, numwrd, '<>', ' ,')
                 if (word(1) .eq. '<') then
                    last = lastch (word(2))
                    write (fmt, '(''(f'', i2, ''.0)'')') last
                    read (word(2), fmt, end=199, err=199) vhigh
                 else if (word(1) .eq. '>') then
                    last = lastch (word(2))
                    write (fmt, '(''(f'', i2, ''.0)'')') last
                    read (word(2), fmt, end=199, err=199) vlow
                 else if (word(2) .eq. '<') then
                    last = lastch (word(1))
                    write (fmt, '(''(f'', i2, ''.0)'')') last
                    read (word(1), fmt, end=199, err=199) vlow
                    last = lastch (word(3))
                    write (fmt, '(''(f'', i2, ''.0)'')') last
                    read (word(3), fmt, end=199, err=199) vhigh
                 else if (word(2) .eq. '>') then
                    last = lastch (word(1))
                    write (fmt, '(''(f'', i2, ''.0)'')') last
                    read (word(1), fmt, end=199, err=199) vhigh
                    last = lastch (word(3))
                    write (fmt, '(''(f'', i2, ''.0)'')') last
                    read (word(3), fmt, end=199, err=199) vlow
                 else
                    go to 199
                 endif
              endif
              if (vlow .le. vhigh) go to 201

  199         error = .true.
              last = lastch (text)
              write (*, 200) text(1:last)
  200         format (' Meaningless voltage range (', a, ') ')

  201         continue

              if (.not. error) then
                 i = 1
                 found = .false.
                 finished = .false.
                 do while (i .le. num_limit .and. .not. finished)
c
c                   Test for non-overlapping CROSS-limits
c
                    iov1 = kmpvalue (vlow, v_range(2,i))
                    iov2 = kmpvalue (vhigh, v_range(1,i))
                     
                    if (iov1 .lt. 0 .and. iov2 .lt. 0) then
                       finished = .true.
                    else if (iov1 .gt. 0 .and. iov2 .gt. 0) then
                       i = i + 1
                    else
c
c                      Test for overlapping limits
c
                       iov1 = kmpvalue (vlow, v_range(1,i))
                       iov2 = kmpvalue (vhigh, v_range(2,i))
                     
                       vlowx = v_range(1,i)
                       vhighx = v_range(2,i)
                       vminx = v_min(i)
                       vmaxx = v_max(i)

                       if (iov1 .le. 0 .and. iov2 .ge. 0) then
c
c                         State 1: Replace identically or completely 
c                         overlapped ranges
c
                          if (.not. found) then
                             call edtvlimit(i, 'replace', vmin, vmax, 
     &                                        vlow, vhigh)
                             found = .true.
                             i = i + 1
                          else 
                             call edtvlimit(i, 'delete', vmin, vmax, 
     &                                        vlow, vhigh)
                          endif
                       else if (iov1 .gt. 0 .and. iov2 .lt. 0) then
c
c                         State 4: Partition range into three segments:
c
c                             (vlowx,vlow-0.1), 
c                             (vlow,vhigh),
c                             (vhigh+01.,vhighx)
c
                          call edtvlimit(i, 'replace', vminx, 
     &                                     vmaxx, vlowx, vlow-0.1)
                          i = i + 1
                          if (.not. found) then
c
c                            Insert new entity
c
                             call edtvlimit(i, 'insert', vminx, 
     &                                        vmaxx, vlow, vhigh)
                             i = i + 1
                             found = .true.
                          endif
                          call edtvlimit(i, 'insert', vminx, 
     &                                     vmaxx, vhigh+0.1, vhighx)
                          i = i + 1

                       else if (iov1 .lt. 0 .and. iov2 .lt. 0) then
c
c                         State 2: Partition range into two segments:
c
c                             (vlow,vhigh),
c                             (vhigh+01.,vhighx)
c
                          if (.not. found) then
c
c                            Insert new entity
c
                             call edtvlimit(i, 'insert', vminx, 
     &                                        vmaxx, vlow, vhigh)
                             i = i + 1
                             found = .true.
                          endif
                          call edtvlimit(i, 'replace', vminx, 
     &                                     vmaxx, vhigh+0.1, vhighx)
                          i = i + 1

                       else
c
c                         State 3: Partition range into two segments:
c
c                             (vlowx,vlow-0.1), 
c                             (vlow,vhigh),
c
                          call edtvlimit(i, 'replace', vminx, 
     &                                     vmaxx, vlowx, vlow-0.1)
                          i = i + 1
                          if (.not. found) then
c
c                            Insert new entity
c
                             call edtvlimit(i, 'insert', vminx, 
     &                                        vmaxx, vlow, vhigh)
                             i = i + 1
                             found = .true.
                          endif
                       endif
                    endif
                 enddo
                 if (.not. found) then
                    call edtvlimit(num_limit+1, 'insert', vmin, vmax, 
     &                 vlow, vhigh)
                 endif
              endif
              call qiksrt (1, num_limit, kmpvlimit, swpvlimit)
              do i = 1, num_limit
                 write (*, 10) v_max(i), v_min(i), v_range(1,i), '<',
     1                         v_range(2,i)
              enddo
  203         continue
           enddo
           if (.not. error) get_defv = .true.
        endif
        return
        end
