C    @(#)sen_fac3.f	20.5 8/30/00
      subroutine sen_fac3 (lossless_flag)
      logical lossless_flag
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha2.inc'
      include 'ipfinc/amtrx.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/beta2.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/gamma.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/tran.inc'
      include 'ipfinc/optim1.inc'
      include 'ipfinc/phase.inc'
 
      common /scratch/ numdiff, ixref(MAXBUS), kdiff(2,MAXBUS),
     &                 fdiff(6,MAXBUS), cdiff(2,MAXBUS),
     &                 lphtx(2,MAXPHS), phtx(6,MAXPHS)
      character cdiff * 1

      logical debug
      integer p, sect
      character id*1

      debug = .false.    ! Set this variable to .true. to test 
c                        ! validity of factorization
      ikec = 1

      do kt = 1, nbslck
         jndex(1,kt) = ikec

C        Normalize row

         amtrx(ikec) = 1.0
         amtrx(ikec+1) = 0.0
         amtrx(ikec+2) = 1.0
         ikec = ikec+3
         jndex(2,kt) = ikec
         amtrx(ikec) = 0.0
         ikec = ikec + 1
      enddo
      jndex(1,nbslck+1) = ikec

C     Define Jacobian elements for bus constraints

      do kt = nbslck+1,ntot+ntopt
         call sen_jac3 (kt, 0, lossless_flag)
         call sen_red2 (kt, ikec)
      enddo

C     Define Jacobian elements for phase shifter constraints

      do jt = 1, ntxtie
         k1 = txtie(1,jt) 
         k2 = txtie(2,jt) 
         if (k1 .lt. k2) then
            ltc = txtie(7,jt)
            lt = txtie(8,jt)
            kt = ntot + ntopt + iabs (lt)
            if (ltc .ne. 0) then
c
c              Determine whether LTC phase shifter is P-constrained
c              (alpha within limits) or alpha-constrained (alpha limit 
c              hit)
c
               it = iabs(lphtx(2,jt))
               call getchr(1, id, jphid(3, it))
               sect = jphid(4, jt)
               p = numbrn (opt2inp(k1), opt2inp(k2), id, sect)
               if (brtype(p) .eq. 4) p = brnch_nxt(p)
               call getlfo (p, 1, pin, qin)
               nbr = brnch_ptr(p)
               if (nbr .gt. 0) then
                  angle = brnch(9,nbr)
               else
                  angle = -brnch(9,-nbr)
               endif               
               ltcx = iabs (ltc)
               kt1 = ltran(1,ltcx)
               kt2 = ltran(9,ltcx)

               if (k1 .eq. kt1 .and. k2 .eq. kt2) then
                  dx = dim (0.0174532*angle+0.001, tran(7,ltcx)) 
     &               -dim (tran(8,ltcx), 0.0174532*angle-0.001)
               else 
                  dx = dim (-0.0174532*angle+0.001, tran(7,ltcx)) 
     &               -dim (tran(8,ltcx), 0.0174532*angle-0.001)
               endif
               if (dx .ne. 0.0) then
c
c                 Limit hit, change to fixed tap phase shifter
c
                  txtie(7,jt) = 0.0
                  ltc = txtie(7,jt)
                  do j = 1, ntxtie
                    if (txtie(1,j) .eq. k2 .and. txtie(2,j) .eq. k1)
     &                 txtie(7,j) = 0.0
                  enddo
               endif
            endif
            if (ltc .eq. 0) then
               korder(0) = 1
               lp = 2
               ko = 1
               max = ntot + iabs (lt)
               kolum(1) = k1
               korder(1) = 2
               rowh(1) = 1.0
               rown(1) = 0.0  
               rowj(1) = 0.0  
               rowl(1) = 1.0
               kolum(2) = ntot + iabs (lt)
               korder(2) = 0
               rowh(2) = -1.0
               rown(2) = 0.0  
               rowj(2) = 0.0  
               rowl(2) = -1.0
               mend = 3
            else
               korder(0) = 1
               lp = 1
               ko = 0
               max = ntot + ntopt + iabs (lt)
               kolum(1) = max
               korder(1) = 0
               if (ltc .gt. 0) then
                  rowh(1) = 1.0
                  rown(1) = 0.0  
                  rowj(1) = 0.0  
                  rowl(1) = 1.0
               else
                  rowh(1) = -1.0
                  rown(1) = 0.0  
                  rowj(1) = 0.0  
                  rowl(1) = -1.0
               endif
               mend = 2
            endif
            call sen_red2 (kt, ikec)
         endif
      enddo
c
c     Temporary check
c
      if (debug) then
         call sen_chk3 (lossless_flag)
      endif

C     End of bus loop

      return
      end
