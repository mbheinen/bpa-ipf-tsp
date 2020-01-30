C    @(#)fd_gezones.f	20.4 5/27/98 
C**************************************************************** 
C 
C     File: fd_gezones.f 
C 
C     Purpose: Routine to find the GE zone number or name from
C              a specified Translation File
C 
C     Author: Walt Powell  Date: 22 Feb 2001 
C     Called by: clnup_ge.f 
C 
C**************************************************************** 
        integer function fd_gezones (zone_number, zone_name)
        character zone_name*(*)
        integer zone_number
 
        include 'ipfinc/parametr.inc' 
 
        include 'ipfinc/blank.inc' 
        include 'ipfinc/prt.inc' 
        include 'ipfinc/lfiles.inc' 
  
        common /ge_zones/ num_gezones, htable_geznum(MAXCZN),
     &                    nextptr_geznum(MAXCZN), 
     &                    htable_geznam(MAXCZN), 
     &                    nextptr_geznam(MAXCZN),
     &                    ge_znum(MAXCZN), 
     &                    ge_znam(MAXCZN)
        integer num_gezones, htable_geznum, nextptr_geznum, 
     &          htable_geznam, nextptr_geznam, ge_znum
        character ge_znam*2

        logical found 
        integer status, h, p, error

        if (zone_number .gt. 0) then
c
c         Hash zone number
c
          h = zone_number
          h = mod (h, MAXCZN-1) + 1
          p = htable_geznum(h)
          found = .false.
          do while (p .gt. 0 .and. .not. found)
            if (zone_number .ne. ge_znum(p)) then
              p = nextptr_geznum(p)
            else
              found = .true.
            endif
          enddo
          if (.not. found) p = 0
          fd_gezones = p

        else if (zone_name .ne. ' ') then
c
c         Hash zone name (Note: name is already in array)
c
          h = 0
          do i = 1, 2
            h = h + h + ichar (zone_name(i:i))
          end do
          h = mod (h, MAXCZN) + 1
          p = htable_geznam(h)
          oldp = 0
          found = .false.
          do while (p .gt. 0 .and. .not. found)
            if (zone_name .ne. ge_znam(p)) then
              oldp = p
              p = nextptr_geznam(p)
            else
              found = .true.
            endif
          enddo
          if (.not. found) p = 0
          fd_gezones = p

        else

          write (errbuf(1), 10000) zone_number, zone_name
10000     format ('Invalid number-name combination - ', i3, 1x, a2)
          call prterx ('W', 1) 
          error = error + 1 
          fd_gezones = -1

        endif

        return 
        end 
