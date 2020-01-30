C    @(#)bus_type.f	20.7 7/18/96
	character * (*) function bus_type (nb)

	include 'ipfinc/parametr.inc'	
	include 'ipfinc/blank.inc'
	include 'ipfinc/bus.inc'
	include 'ipfinc/tbx.inc'

        character tbxtyp(6) * 2
        character tbxste(6,6) * 4

        data tbxtyp / 'BV', 'BQ', 'BG', 'BO', 'BX', 'BF' /
        data tbxste / 5*' ', 'BE', 
     1     ' ', ' ', 'QMIN', ' ', 'QMIN', ' ',
     2     'VMIN', 'QMIN', 'QMAX', 'QMIN', 'QMAX', ' ',
     3     'VMAX', 'QMAX', 'VMIN', 'QMAX', 'QDIS', ' ',
     4     6*' ', 6*' ' /

        if (tbx_loaded .eq. 0) then
           do i = 1, ntot
              ptrtbx(i) = 0
           enddo

           do i = 1, ntotb
              kxd = tbx(2, i)
              if (kxd .gt. 0) ptrtbx(kxd) = i
           enddo
           tbx_loaded = 1
        endif

        ktype = kbsdta(1,nb)
        bus_type = 'B'
        call typno (bus_type(2:2), ktype)

C               1   2   3   4   5   6   7   8   9  10  11  12  13)
        go to (90, 90, 90, 90, 90, 10, 10, 10, 10, 90, 10, 90, 90) 
     1    ktype

   10   jt = ptrtbx(nb) 
        ltyp = tbx(1,jt)
        ityp = tbx(7,jt)
        bus_type(3:) = tbxste(ltyp,ityp)

   90   return
        end
