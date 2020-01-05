C    @(#)sen_dldx2.f	20.2 6/27/97
      subroutine sen_dldx2 (num_brn, brn_ptr, pij, qij)
      integer num_brn, brn_ptr(*)
      real pij(*), qij(*)

C     Computes the Jacobian elements dLine/dX for the input lines
C
      include 'ipfinc/parametr.inc'
      include 'ipfinc/alpha.inc'
      include 'ipfinc/alpha2.inc'
      include 'ipfinc/amtrx.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/beta2.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/dc.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/factor.inc'
      include 'ipfinc/gamma.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/optim1.inc'
      include 'ipfinc/pctvr2.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/tran.inc'

c     Local variables 
c
      complex * 8 y_8(2,2)
      complex * 16 y_16(2,2), i_16(2), s_16(2), v_16(2)
      character id * 1
      integer ptr
C 
      do i = 1,ntot+2*ntopt
         dpt(1,i) = 0.0d0
         dpt(2,i) = 0.0d0
      enddo
C       
C     Loop to compute dPij/dX with the following steps:
C       
C     1. Obtain the 2-port Y-matrix for branch K1-K2-ID.
C
      do jt = 1, num_brn
         ptr = brn_ptr(jt)
         k1 = kx(ptr)
         k2 = ky(ptr)
         id = brid(ptr)

         call pieqiv (ptr, y_16, error)

         kt = inp2opt(k1)
         mt = inp2opt(k2)

         v_16(1) = cmplx (e(kt), f(kt))
         v_16(2) = cmplx (e(mt), f(mt))
         i_16(1) = cmplx (0.0, 0.0)
         i_16(2) = cmplx (0.0, 0.0)

         do i = 1, 2
           do j = 1, 2
              i_16(i) = i_16(i) + y_16(i,j) * v_16(j)
              y_8(i,j) = y_16(i,j)
           enddo
         enddo

         s_16(1) = v_16(1) * conjg (i_16(1))
         s_16(2) = v_16(2) * conjg (i_16(2))

         pij(jt) = dreal (s_16(1)) * bmva
         qij(jt) = dimag (s_16(1)) * bmva
C
C     2. Compute the "objective function" dH/dX (actually dPij/dX)
C
         call sen_dhdx2 (kt, mt, y_8)
      enddo
C
C     3. Compute the LaGrange multipliers
C
      call sen_bak2 (1, ntot+2*ntopt)

      return
      end   
