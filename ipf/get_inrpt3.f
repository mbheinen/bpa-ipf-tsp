C    @(#)get_inrpt3.f	20.4 1/7/99
C****************************************************************
C
C       File: get_inrpt3.f
C       Purpose: Routine to generate bus-branch input reports
C                using IPF command
C
C                / REPORTS, SELECT BUS_BR_INPUT
C                  WHERE BUS = "<busname>"
C                (END)
C
C       Author: Walt Powell  Date: 22 July 1992
C                            Modified: 22 July 1992
C       Called by:
C
C****************************************************************
C
	subroutine get_inrpt3 (scrfil)
        integer scrfil

	include 'ipfinc/parametr.inc'	
	include 'ipfinc/lfiles.inc'
	include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/branch.inc'

	common /shellsort/ buffer(1000)
        integer buffer

	external komp_buf, swap_buf

        character in_buffer*(MAXBUFFER), out_buffer*(MAXBUFFER), 
     &            bus_name*8, capital*12, text*120, word(20)*12, 
     &            null*1, linefeed*1
	integer find_bus, busback, first, apdoutbuf, ptr, o1, o2,
     &          findstr
        logical finished
C
C       Input/Output request: Enter kernal bus
C
        null = char(0)
        linefeed = char(10)

        finished = .false.
        do while (.not. finished)
           last = 1
           write (*,210)
  210      format (' > Enter busname, KV, buses-back : ',$)
           read (*,212) text
  212      format (a)
           bus_name = ' '
           base_kv = 0.0
           busback = 0
           if (text .eq. ' ') go to 900
c
C          Parse TEXT into busname, kv, and buses-back
c
           call parse_bus (text, word, nwrd)
           do i = nwrd+1, 3
              word(i) = ' '
           enddo
           bus_name = capital(word(1))
           if (index(word(2), '.') .eq. 0) then
              last = lastch(word(2))
              word(2) = word(2)(1:last) // '.0'
           endif
           read (word(2), 215, err = 220) base_kv
  215      format (f6.0)
           read (word(3), 216, err = 220) busback
  216      format (i1)

           if (bus_name .eq. ' ') go to 900
           nb = find_bus (bus_name,base_kv)
   
           if (nb .gt. 0) then
   	      ilevel = 0
              first = 1
              last = 1
              buffer(last) = nb
              next = last
              do while (ilevel .le. busback .and. next .ge. last) 
                 if (first .lt. last) 
     &              call shellsrt (first, last, komp_buf, swap_buf)
                 do iv = first, last
                    nb = buffer(iv)
                    call bcdbus (nb, text)

                    in_buffer(1:1) = null
                    i1 = index (in_buffer, null)
                    length = apdoutbuf(i1, 
     &                 '/REPORTS, SELECT BUS_BR_INPUT WHERE BUS = '
     &                 // '"' // text(7:18) // '"' // linefeed 
     &                 // '(END)', 
     &                 in_buffer(i1:))
                    out_buffer(1:1) = null
                    ix = findstr (in_buffer, 'BUS_BR_INPUT')
     &                 + lastch ('BUS_BR_INPUT')
                    call busbrinrpt(in_buffer(ix:), out_buffer, 
     &                              scrfil)

                    o1 = 1
                    do while (o1 .lt. MAXBUFFER .and. 
     &                        out_buffer(o1:o1) .ne. null)
                       o2 = nxt_term(out_buffer(o1+1:)) + o1
                       if (out_buffer(o1:o1) .eq. '/') then
                          o2 = MAXBUFFER + 1
                       else
                          write (*, 110) out_buffer(o1:o2-1)
  110                     format (1x, a)
                          write (2, 120) out_buffer(o1:o2-1)
  120                     format (a)
                       endif
                       o1 = o2
                       if (out_buffer(o1:o1) .eq. linefeed) o1 = o1 + 1
                    enddo

                    ptr = kbsdta(16,nb)
                    do while (ptr .gt. 0)
                       do j = 1, next 
                          if (buffer(j) .eq. ky(ptr)) go to 242
                       enddo
                       next = next + 1
                       buffer(next) = ky(ptr)
  242                  continue
                       ptr = brnch_nxt(ptr)
                    enddo
                 enddo
                 ilevel = ilevel + 1
                 first = last + 1
                 last = next
              enddo
           else
              call ermisbus (bus_name, base_kv, text)
           endif
           go to 310

  220      write (*, 224) 
  224      format (' *** Illegal data entry ***')
   
  310      continue
        enddo
  900   continue
        return
        end
