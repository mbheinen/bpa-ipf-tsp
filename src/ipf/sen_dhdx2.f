C    @(#)sen_dhdx2.f	20.2 6/27/97
      subroutine sen_dhdx2 (kt, mt, y)
      complex y(2,2)
C
C     This subroutine computes the Jacobian elements dPij/dX
C     for branch KT-MT.
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/amtrx.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/ecvar.inc'
 
      real ikr, iki, imr, imi
 
      ek = e(kt)
      fk = f(kt)
      em = e(mt)
      fm = f(mt)
      ikr = 0.0
      iki = 0.0
 
      g12 = real (y(1,2))
      b12 = aimag (y(1,2))
      imr = em*g12-fm*b12
      imi = em*b12+fm*g12
      ikr = ikr+imr
      iki = iki+imi
      rh = imr*fk-imi*ek
      rn = imr*ek+imi*fk
      dpt(1,mt) = dpt(1,mt) - rh
      dpt(2,mt) = dpt(2,mt) - rn
 
      vksq = ek**2 + fk**2
      g12 = real (y(1,1)) * vksq
      b12 = aimag (y(1,1)) * vksq
      pk = ikr*ek+iki*fk+g12
      qk = ikr*fk-iki*ek-b12
      rh =  -qk - b12
      rn = pk + g12
      dpt(1,kt) = dpt(1,kt) - rh
      dpt(2,kt) = dpt(2,kt) - rn
      return
      end
