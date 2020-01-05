C    @(#)sortbus.f	20.3 2/13/96
	subroutine sortbus

        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'

        external kombus, swpbus

	do i = 1, ntot
          alf2inp(i) = i
        enddo
        call qiksrt (1, ntot, kombus, swpbus)

        do i = 1, ntot
          inp2alf(alf2inp(i)) = i
        end do

        ntot_alf = ntot
        do while (ntot_alf .gt. 0 .and.
     &           (bus(alf2inp(ntot_alf)) .eq. srtlst))
          ntot_alf = ntot_alf - 1
        enddo

        return
        end
