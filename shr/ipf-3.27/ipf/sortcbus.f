C    @(#)sortcbus.f	20.3 2/13/96
      	subroutine sortcbus
c                                                                       
      	include 'ipfinc/parametr.inc'

	include 'ipfinc/blank.inc'
      	include 'ipfinc/bus.inc'
      	include 'ipfinc/cbus.inc'
      	include 'ipfinc/prt.inc'

        common /scratch/ ncbs, array(100), array_c(100)
        character array_c * 8
        integer array, p, oldp, kmprcbus

        external kmprcbus, swapcbus
  
        do i = 1, ntot
           p = kbsdta(15,i)
           ncbs = 0
           do while (p .gt. 0)
              ncbs = ncbs + 1
              array(ncbs) = p
              write (array_c(ncbs), 310) bctbl(8,p), bctbl(10,p),
     1           bctbl(9,p)                          
  310         format (a1,a3,a2)                                 
              p = bctbl_nxt(p)
           end do
           if (ncbs .gt. 1) then
              call shellsrt (1, ncbs, kmprcbus, swapcbus)
              oldp = array(1)
              kbsdta(15,i) = oldp
              do j = 2, ncbs
                 p = array(j)
                 bctbl_nxt(oldp) = p 
                 oldp = p
              end do
              bctbl_nxt(oldp) = 0
           endif
        enddo
        return
        end
