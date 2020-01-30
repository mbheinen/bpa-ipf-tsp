C    @(#)obus_type.f	20.9 8/16/96
	character * (*) function obus_type (nb)

	include 'ipfinc/parametr.inc'	

	include 'ipfinc/alt_case.inc'

        character tbxtyp(6) * 2
        character tbxste(6,6) * 4
        integer ptrtbx(MAXBUS), tbx_loaded

        save

        data tbxtyp / 'BV', 'BQ', 'BG', 'BO', 'BX', 'BF' /
        data tbxste / 5*' ', 'BE', 
     1     ' ', ' ', 'QMIN', ' ', 'QMIN', ' ',
     2     'VMIN', 'QMIN', 'QMAX', 'QMIN', 'QMAX', ' ',
     3     'VMAX', 'QMAX', 'VMIN', 'QMAX', 'QDIS', ' ',
     4     6*' ', 6*' ' /
        data tbx_loaded / 0 /

        if (tbx_loaded .eq. 0) then
           do i = 1, ontot
              ptrtbx(i) = 0
           enddo

           do i = 1, ontotb
              kxd = oltbx(2, i)
              if (kxd .gt. 0) ptrtbx(kxd) = i
           enddo
           tbx_loaded = 1
        endif

        ktype = okbsdta(1,nb)
        obus_type = 'B'
        call typno (obus_type(2:2), ktype)

C               1   2   3   4   5   6   7   8   9  10  11  12  13)
        go to (90, 90, 90, 90, 90, 10, 10, 10, 10, 90, 10, 90, 90) 
     1    ktype

   10   jt = ptrtbx(nb) 
        ltyp = oltbx(1,jt)
        ityp = oltbx(7,jt)
        obus_type(3:) = tbxste(ltyp,ityp)

   90   return
        end
