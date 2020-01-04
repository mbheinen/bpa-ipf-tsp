C    @(#)sen_cutbr.f	20.2 1/7/99
      integer function sen_cutbr (k1, k2)

      include 'ipfinc/parametr.inc'  

      include 'ipfinc/blank.inc'   
      include 'ipfinc/cut.inc' 
      include 'ipfinc/bus.inc' 
      include 'ipfinc/cbus.inc'  
      include 'ipfinc/branch.inc'   
      include 'ipfinc/ikk.inc' 
      include 'ipfinc/red6.inc'
      include 'ipfinc/dc2t.inc'
      include 'ipfinc/dcmt.inc'
      include 'ipfinc/prt.inc' 
      include 'ipfinc/tbx.inc'
      include 'ipfinc/alpha.inc' 

      complex * 16 y(2,2), oldy(2,2), v(2), s(2), i_16(2)
      real p(2), q(2)
      integer ierr, ptr, oldptr
      logical finished, finished2, debug

      sen_cutbr = 0
      debug = .false.

      oldy(1,1) = dcmplx (0.0, 0.0)
      oldy(1,2) = dcmplx (0.0, 0.0)
      oldy(2,1) = dcmplx (0.0, 0.0)
      oldy(2,2) = dcmplx (0.0, 0.0)
c
c     Obtain total 2-port Y-matrix between k1 and k2
c
      finished = .false.
      oldptr = numbrn (k1, k2, '*', 0)
      ptr = oldptr
      do while (ptr .gt. 0 .and. .not. finished)
        if (brtype(ptr) .eq. 1) then
          call pieqiv (ptr, y, ierr)      
          oldy(1,1) = oldy(1,1) + y(1,1)
          oldy(1,2) = oldy(1,2) + y(1,2)
          oldy(2,1) = oldy(2,1) + y(2,1)
          oldy(2,2) = oldy(2,2) + y(2,2)
          finished2 = .false.
          ptr = brnch_nxt(ptr)
          do while (ptr .gt. 0 .and. .not. finished2)
            if (ky(ptr) .eq. ky(oldptr) .and.
     &          brid(ptr) .eq. brid(oldptr)) then
              ptr = brnch_nxt(ptr)
            else
              finished2 = .true.
              if (ky(ptr) .ne. ky(oldptr)) then
                finished = .true.
              else
                oldptr = ptr
              endif
            endif
          enddo

        else if (brtype(ptr) .eq. 2 .or. 
     &           brtype(ptr) .eq. 4 .or.
     &           brtype(ptr) .eq. 7) then
          ptr = brnch_nxt(ptr)
          if (ptr .gt. 0) then
            if (ky(ptr) .ne. ky(oldptr)) then
              finished = .true.
            else
              oldptr = ptr
            endif
          endif
        else
          call pieqiv (ptr, y, ierr)      
          oldy(1,1) = oldy(1,1) + y(1,1)
          oldy(1,2) = oldy(1,2) + y(1,2)
          oldy(2,1) = oldy(2,1) + y(2,1)
          oldy(2,2) = oldy(2,2) + y(2,2)
          ptr = brnch_nxt(ptr)
          if (ptr .gt. 0) then
            if (ky(ptr) .ne. ky(oldptr)) then
              finished = .true.
            else
              oldptr = ptr
            endif
          endif
        endif
      enddo
c
c     Compute injections into each port
c
      kt = inp2opt(k1)
      mt = inp2opt(k2)
      v(1) = dcmplx (e(kt), f(kt))
      v(2) = dcmplx (e(mt), f(mt))
      i_16(1) = oldy(1,1) * v(1) + oldy(1,2) * v(2)
      i_16(2) = oldy(2,1) * v(1) + oldy(2,2) * v(2)
      s(1) = v(1) * dconjg(i_16(1))
      s(2) = v(2) * dconjg(i_16(2))
c
c     Open branch and replace flows with injections
c
      pnetu(kt) = pnetu(kt) - dreal(s(1)) 
      qnetu(kt) = qnetu(kt) - dimag(s(1)) 
      pnetu(mt) = pnetu(mt) - dreal(s(2)) 
      qnetu(mt) = qnetu(mt) - dimag(s(2)) 
c
c     Leave a small residual for factorization 
c
      gkku(kt) = gkku(kt) - 0.9999 * dreal (oldy(1,1))
      bkku(kt) = bkku(kt) - 0.9999 * dimag (oldy(1,1))
      gkku(mt) = gkku(mt) - 0.9999 * dreal (oldy(2,2))
      bkku(mt) = bkku(mt) - 0.9999 * dimag (oldy(2,2))

      do l = km(kt), km(kt)-1+kmlen(kt) 
        if (ikmu(l) .eq. mt) then
          gkmu(l) = gkmu(l) - 0.9999 * dreal (oldy(1,2))
          bkmu(l) = bkmu(l) - 0.9999 * dimag (oldy(1,2))
        endif
      enddo

      do l = km(mt), km(mt)-1+kmlen(mt) 
        if (ikmu(l) .eq. kt) then
          gkmu(l) = gkmu(l) - 0.9999 * dreal (oldy(2,1))
          bkmu(l) = bkmu(l) - 0.9999 * dimag (oldy(2,1))
        endif
      enddo

      call nrpqv (kt, pk, dpk, qk, dqk, vk)
      if (abs(dpk) .gt. 1.0e-3 .or. abs(dqk) .gt. 1.0e-3) then
        write (*, 10138) bus(k1), base(k1), dpk, dqk
10138   format (' Injection error for bus ', a8, f7.1, 2e12.5)
      endif

      call nrpqv (mt, pk, dpk, qk, dqk, vk)
      if (abs(dpk) .gt. 1.0e-3 .or. abs(dqk) .gt. 1.0e-3) then
        write (*, 10138) bus(k2), base(k2), dpk, dqk
      endif

      if (debug) then
        p(1) = dreal(s(1)) * bmva
        q(1) = dimag(s(1)) * bmva
        p(2) = dreal(s(2)) * bmva
        q(2) = dimag(s(2)) * bmva
        write (*, 100) bus(k1), base(k1), bus(k2), base(k2), p(1),
     &     q(1)
  100   format (' Cut branch ', a8, f6.1, 1x, a8, f6.1, ' S = ',
     &    2f8.1)
      endif

      return
      end
