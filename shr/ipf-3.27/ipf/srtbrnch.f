C    @(#)srtbrnch.f	20.6 11/11/97
      	subroutine srtbrnch
c                                                                       
      	include 'ipfinc/parametr.inc'

	include 'ipfinc/blank.inc'
      	include 'ipfinc/branch.inc'
      	include 'ipfinc/bus.inc'
      	include 'ipfinc/prt.inc'
      	include 'ipfinc/qsdup.inc'

        common /scratch/ nbr, array(2,100)
        integer array

        common /is_batch / is_batch

        integer p, pnxt, q, oldp, komparbr, sect, ipack_4
        character id * 1, xbuf * 120

        external komparbr, swap_br
  
        if (ltot .lt. 1) then
           write (errbuf(1), 100)                                            
  100      format('No branch data in system.')                             
           if (is_batch .eq. 0) then
              call prterx ('E',1)
              return
           else
              call prterx ('F',1)
              call erexit()
           endif
      	else
           do i = 1, ntot
              nbr = 0
              p = kbsdta(16,i)
              do while (p .gt. 0)
                 nbr = nbr + 1
                 q = brnch_ptr(p)
                 array(1,nbr) = p
                 k2 = ky(p)
                 id = brid(p)
                 sect = brsect(p)
                 if (sect .gt. 0 .and. q .lt. 0) then
                    sect = 10 - sect
                 endif
                 array(2,nbr) = ipack_4 (inp2alf(k2), ichar(id),
     &                                   sect, brtype(p))
                 p = brnch_nxt(p)
              end do
              if (nbr .eq. 0) then
                 if (bus(i) .ne. '~~~~~~~~') then
                   write (errbuf(1), 110) bus(i), base(i) 
  110              format('No branches for bus ', a8, f6.1)
                   call prterx ('E',1)             
                 endif
              else if (nbr .gt. 1) then
                 dupsw = .false.
                 call shellsrt (1, nbr, komparbr, swap_br)
                 if (dupsw) then
                    j = 1
                    do while (j .lt. nbr)
                       if (array(2,j) .eq. array(2,j+1)) then
                          p = array(1,j)
                          pnxt = array(1,j+1)
                          if (brtype(p) .ne. 1) then
                             write (errbuf(1),122)
  122                        format(' Duplicate branch records.')
                             errbuf(2) = ' '
                             call bcdbrn(p,xbuf)
                             write (errbuf(3),160) xbuf(1:80)
  160                        format(13x,'(',a80,')')
                             call bcdbrn(pnxt,xbuf)
                             write (errbuf(4),160) xbuf(1:80)
                             call prterx ('W',4)
                          endif
                          do k = j+1, nbr-1
                             array(1,k) = array(1,k+1)
                             array(2,k) = array(2,k+1)
                          enddo
                          nbr = nbr - 1
                       else
                          j = j + 1
                       endif
                    enddo
                 endif
                 oldp = array(1,1)
                 kbsdta(16,i) = oldp
                 do j = 2, nbr
                    p = array(1,j)
                    brnch_nxt(oldp) = p 
                    oldp = p
                 end do
                 brnch_nxt(oldp) = 0
              endif
           enddo
        endif
        return
        end
