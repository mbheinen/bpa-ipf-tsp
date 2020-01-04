C    @(#)check_pq.f	20.4 11/11/97
      subroutine check_pq

      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/intbus.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/norder.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/red7.inc'

      common /pkqkxx/pk, qk

      double precision a(6)


      do i = 3, 6
        a(i) = 0.0d0
      enddo

      do kt = 1, ntot
        a(1) = inetr(kt)
        a(2) = ineti(kt)
        lf = km(kt)
        ls = kmlen(kt)
        if (ls .ne. 0) then
          kf = km(kt)
          do l = 1, ls
            ymtrx(1, l) = ikmu(l+lf-1)
            ymtrx(2, l) = gkmu(l+lf-1)
            ymtrx(3, l) = bkmu(l+lf-1)
          enddo
          ix = ymtrx(1, ls) / dble(20000.0)
          if (ix .ne. 0) then
            ymtrx(1, ls) = dmod (ymtrx(1, ls), dble(20000.0))
            ls = ls + 1
            ymtrx(1, ls) = ikmu(ix)
            ymtrx(2, ls) = gkmu(ix)
            ymtrx(3, ls) = bkmu(ix)
          endif
          ls = ls + 1
          ymtrx(1, ls) = kt
          ymtrx(2, ls) = gkku(kt)
          ymtrx(3, ls) = bkku(kt)
          call pkqk1(kt, ls, ymtrx, a)
        endif
      enddo
      return
      end
