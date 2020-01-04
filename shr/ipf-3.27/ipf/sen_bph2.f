C    @(#)sen_bph2.f	20.8 8/30/00
      subroutine sen_bph2 (mode)
c
c     Mode = 0: Partition phase shifters subsystem
c            1: Restore original phase shifter subsystems
c
      include 'ipfinc/parametr.inc'

      include 'ipfinc/addtbx.inc'
      include 'ipfinc/alpha.inc'
      include 'ipfinc/alpha2.inc'
      include 'ipfinc/amtrx.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/beta.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/dc.inc'
      include 'ipfinc/dcsln.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/gendrp.inc'
      include 'ipfinc/ikk.inc'
      include 'ipfinc/intbus.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/ntotzt.inc'
      include 'ipfinc/optim1.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/slnphs.inc'
      include 'ipfinc/tran.inc'
      include 'ipfinc/ordsta.inc'
      include 'ipfinc/phase.inc'

      common /scratch/ numdiff, ixref(MAXBUS), kdiff(2,MAXBUS),
     &                 fdiff(6,MAXBUS), cdiff(2,MAXBUS),
     &                 lphtx(2,MAXPHS), phtx(6,MAXPHS)
      character cdiff * 1

      double precision aim, bim, angle
      complex * 16 y_16(2,2)
      integer kerr, k1, k2, komp_ykm, sect, p, error
      character id * 1  
      logical found
      external komp_ykm, swap_ykm
        
      kerr = 0

      if (mode. eq. 0) then
c
c        Mode = 0: Partition phase shifters subsystem
c
c        Link TXTIE with new bus numbers
c
         do jt = 1, max0 (jphno, ntxtie)
            lphtx(1,jt) = 0
            lphtx(2,jt) = 0
         enddo
         do jt = 1, ntxtie
            do j = 1, 6
               phtx(j,jt) = 0.0
            enddo
         enddo

         icount = iflag(ntot+1)
         ntopt = 0
         do jt = 1,ntxtie  
            k1 = txtie(1,jt) 
            k2 = txtie(2,jt) 
            it = 1
            found = .false.
            do while (it .le. jphno .and. .not. found)
               k = jphid(1, it)
               m = jphid(2, it)
               if (ordvlt .eq. 1) then
                  kt = k
                  mt = m
               else
                  kt = inp2opt(k)
                  mt = inp2opt(m)
               endif
               if (kt .eq. k1 .and. mt .eq. k2) then
                  lphtx(1,it) = jt
                  lphtx(2,jt) = it
                  found = .true.
               else if (kt .eq. k2 .and. mt .eq. k1) then
                  lphtx(1,it) = -jt
                  lphtx(2,jt) = -it
                  found = .true.
               else
                  it = it + 1
               endif
            enddo
            if (k1 .lt. k2) then
               ntopt = ntopt + 1
               txtie(8,jt) = ntopt
            endif
            ltc = txtie(7,jt)
            if (ltc .gt. 0) then
               txtie(4,jt) = tap(ltc)
            else if (ltc .lt. 0) then
               txtie(4,jt) = -tap(-ltc)
            endif
         enddo

         do jt = 1,ntxtie  
            k1 = txtie(1,jt) 
            k2 = txtie(2,jt) 
            if (k1 .lt. k2) then
            else
               found = .false.
               i = 1
               do while (i .le. ntxtie .and. .not. found)
                  if (txtie(1,i) .eq. k2 .and. txtie(2,i) .eq. k1) then
                     found = .true.
                     txtie(8,jt) = -txtie(8,i)
                  else
                     i = i + 1
                  endif
               enddo
            endif
         enddo

         num_nodes = ntot
c
c        First pass - low to high
c
         do jt = 1,ntxtie  
            k1 = txtie(1,jt) 
            k2 = txtie(2,jt) 
            if (k1 .lt. k2) then
               lt = iabs(lphtx(2,jt))
               if (lt .gt. 0) then
                 call getchr(1, id, jphid(3,lt))
                 sect = jphid(4,lt)
                 p = numbrn (opt2inp(k1), opt2inp(k2), id, sect)
               else
                 p = numbrn (opt2inp(k1), opt2inp(k2), '*', 0)
               endif
               if (brtype(p) .eq. 4) p = brnch_nxt(p)
               call pieqiv (p, y_16, error)
               gkm = dreal(y_16(1,2))
               bkm = dimag(y_16(1,2))
               gmk = dreal(y_16(1,1))
               bmk = dimag(y_16(1,1))
               angle = txtie(4,jt)
               it = txtie(8,jt)
               itx = iabs(it)
  
               ls = km(k1)  
               lf = kmlen(k1) + ls - 1
               l = ls
               found = .false.
               do while (l .le. lf .and. .not. found)
                  if (ikmu(l) .eq. k2) then
                     found = .true.
                  else
                     l = l + 1
                  endif
               enddo
               if (.not. found) call erexit()
C       
C              Add residual admittance Y_res = 0.01 - j0.05 to 
C              prevent singularity.  
C       
               gkmu(l) = gkmu(l) - gkm - 0.01d0
               bkmu(l) = bkmu(l) - bkm + 0.05d0
               gkku(k1) = gkku(k1) - gmk + 0.01d0
               bkku(k1) = bkku(k1) - bmk - 0.05d0
               pk = pnetu(k1)
               qk = qnetu(k1)
               call nrpqv (k1, pk, dpk, qk, dqk, vk) 
               phtx(1,itx) = dpk
               phtx(2,itx) = dqk
c
c              Insert jflag(*) entity for node k1
c
               if (icount .eq. 0) then
                  do i = 1, num_nodes+1
                     iflag(i) = 1
                  enddo
               endif
               do i = k1+1, num_nodes+1
                  iflag(i) = iflag(i) + 1
               enddo                  
               next = iflag(k1) 
               do i = icount, next, -1
                  jflag(1,i+1) = jflag(1,i)
                  jflag(2,i+1) = jflag(2,i)
               enddo
               jflag(1,next) = 11
               jflag(2,next) = jt
               icount = icount + 1
c
c              Add new pseudo-bus 
c
               kt = ntot + itx
               num_nodes = kt
               e(kt) = e(k1) * cos(-angle) - f(k1) * sin(-angle)
               f(kt) = e(k1) * sin(-angle) + f(k1) * cos(-angle)
               pnetu(kt) = 0.0
               qnetu(kt) = 0.0
               kvolt(kt) = 0
               gkku(kt) = gmk
               bkku(kt) = bmk
               yptr = yptr + 1
               km(kt) = yptr
               kmlen(kt) = 1
               ikmu(yptr) = k2
               gkmu(yptr) = -gmk
               bkmu(yptr) = -bmk
               pk = pnetu(kt)
               qk = qnetu(kt)
               call nrpqv (kt, pk, dpk, qk, dqk, vk) 
               phtx(3,itx) = dpk
               phtx(4,itx) = dqk
c
c              Insert jflag(*) entity for node kt
c
               next = iflag(kt) 
               do i = icount, next+1, -1
                  jflag(1,i+1) = jflag(1,i)
                  jflag(2,i+1) = jflag(2,i)
               enddo
               jflag(1,next) = 12
               jflag(2,next) = jt
               icount = icount + 1
               iflag(kt+1) = icount
C       
C              Compute initial flow Pkm in branch
C       
               mt = ntot + ntopt + itx
               aim = gmk*e(k1) - bmk*f(k1) + gkm*e(k2) - bkm*f(k2)   
               bim = gmk*f(k1) + bmk*e(k1) + gkm*f(k2) + bkm*e(k2)   
               e(mt) = e(k1)*aim + f(k1)*bim 
               f(mt) = -e(k1)*bim + f(k1)*aim
               phtx(1,itx) = phtx(1,itx) - e(mt)
               phtx(2,itx) = phtx(2,itx) - f(mt)
               phtx(3,itx) = phtx(3,itx) + e(mt)
               phtx(4,itx) = phtx(4,itx) + f(mt)
               write (*, 10000) intbus(k1), intbas(k1), intbus(k2), 
     &           intbas(k2), e(mt)*bmva, f(mt)*bmva
10000          format (' Phase shifter ', a8, f6.1, 1x, a8, f6.1, 
     &           ' S = ', 2f8.1)

            endif
         enddo
c
c        Second pass - high to low
c
         do jt = 1,ntxtie  
            k1 = txtie(1,jt) 
            k2 = txtie(2,jt) 
            if (k1 .gt. k2) then
               lt = iabs(lphtx(2,jt))
               if (lt .gt. 0) then
                 call getchr(1, id, jphid(3,lt))
                 sect = jphid(4,lt)
                 p = numbrn (opt2inp(k1), opt2inp(k2), id, sect)
               else
                 p = numbrn (opt2inp(k1), opt2inp(k2), '*', 0)
               endif
               if (brtype(p) .eq. 4) p = brnch_nxt(p)
               call pieqiv (p, y_16, error)
               gkm = dreal(y_16(1,2))
               bkm = dimag(y_16(1,2))
               gmk = dreal(y_16(1,1))
               bmk = dimag(y_16(1,1))
               angle = txtie(4,jt)
               it = txtie(8,jt)
               itx = iabs(it)
  
               ls = km(k1)  
               lf = kmlen(k1) + ls - 1
               l = ls
               found = .false.
               do while (l .le. lf .and. .not. found)
                  if (ikmu(l) .eq. k2) then
                     found = .true.
                  else
                     l = l + 1
                  endif
               enddo
               if (.not. found) call erexit()
C       
C              Replace branch with symmetric admittance, renumber and
C              resort Y-matrix.
C       
               ikmu(l) = ntot + itx
               gkmu(l) = gkmu(l) - gkm - gmk
               bkmu(l) = bkmu(l) - bkm - bmk
               call qiksrt(ls, lf, komp_ykm, swap_ykm)
               pk = pnetu(k1)
               qk = qnetu(k1)
               call nrpqv (k1, pk, dpk, qk, dqk, vk) 
               phtx(5,itx) = dpk 
               phtx(6,itx) = dqk 

            endif
         enddo
c
c        Check modelling accuracy
c
         do jt = 1,ntxtie  
            k1 = txtie(1,jt) 
            k2 = txtie(2,jt) 
            it = txtie(8,jt) 
            itx = iabs (it)
            mt = ntot + itx
            if (k1 .lt. k2) then
               if (abs(phtx(1,itx)) .gt. 1.0e-2 .or. 
     &             abs(phtx(2,itx)) .gt. 1.0e-2) then
                  write (*, 10010) intbus(k1), intbas(k1), phtx(1,itx),
     &               phtx(2,itx)
10010             format (' Phase shift error - terminal 1 ',
     &               a8, f7.1, ' error = ', 2e11.3)
               endif
               if (abs(phtx(3,itx)) .gt. 1.0e-2 .or. 
     &             abs(phtx(4,itx)) .gt. 1.0e-2) then
                  write (*, 10020) intbus(k2), intbas(k2), phtx(3,itx),
     &               phtx(4,itx)
10020             format (' Phase shift error - terminal 2 ',
     &               a8, f7.1, ' error = ', 2e11.3)
               endif
               if (abs(phtx(5,itx)) .gt. 1.0e-2 .or. 
     &             abs(phtx(6,itx)) .gt. 1.0e-2) then
                  intbus(mt) = intbus(k1)(1:7) // '&'
                  intbas(mt) = intbas(k1)
                  write (*, 10030) intbus(mt), intbas(mt), phtx(5,itx),
     &               phtx(6,itx)
10030             format (' Phase shift error - terminal 0 ',
     &               a8, f7.1, ' error = ', 2e11.3)
               endif
            endif
         enddo

      else
c
c        Mode = 1: Restore original phase shifter subsystems
c
         do jt = 1,ntxtie  
            k1 = txtie(1,jt) 
            k2 = txtie(2,jt) 
            gkm=txtie(5,jt)   
            bkm=txtie(6,jt)   
            gmk=txtie(9,jt)   
            bmk=txtie(10,jt)  
            angle = txtie(4,jt)
            it = txtie(8,jt)

            ls = km(k1)  
            lf = kmlen(k1) + ls - 1
            l = ls
            found = .false.
            if (k1 .le. k2) then
               kxx = k2
            else
               kxx = ntot + iabs (it)
            endif
            do while (l .le. lf .and. .not. found)
               if (ikmu(l) .eq. kxx) then
                  found = .true.
               else
                  l = l + 1
               endif
            enddo
            if (.not. found) call erexit()
            if (k1 .lt. k2) then
C       
C              Restore admittance Y_res = 0.01 - j0.05 to prevent
C              singularity.  
C       
               gkmu(l) = gkmu(l) + gkm + 0.01d0
               bkmu(l) = bkmu(l) + bkm - 0.05d0
               gkku(k1) = gkku(k1) + gmk - 0.01d0
               bkku(k1) = bkku(k1) + bmk + 0.05d0
               pk = pnetu(kt)
               qk = qnetu(kt)
               call nrpqv (k1, pk, dpk, qk, dqk, vk) 

            else
C       
C              Restore branch with assymmetric admittance, renumber and
C              resort Y-matrix.
C       
               ikmu(l) = k2
               gkmu(l) = gkmu(l) + gkm + gmk
               bkmu(l) = bkmu(l) + bkm + bmk
               call qiksrt(ls, lf, komp_ykm, swap_ykm)
               pk = pnetu(k2)
               qk = qnetu(k2)
               call nrpqv (k2, pk, dpk, qk, dqk, vk) 
            endif
         enddo
      endif
      return
      end
