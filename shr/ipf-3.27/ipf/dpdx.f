C    @(#)dpdx.f	20.3 2/13/96
      subroutine dpdx (kt,mt,y)
C
C     This subroutine computes the Jacobian elements dPij/dX
C     for branch KT-MT.
C
 
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/amtrx.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/ecvar.inc'
 
      real ikr,iki,imr,imi
      complex y(2,2)
 
      do 100 i = 1,ntotx-1
         dpt(1,i) = 0.0
         dpt(2,i) = 0.0
  100 continue
 
      ek = e(kt)
      fk = f(kt)
      em = e(mt)
      fm = f(mt)
      ikr = 0.0
      iki = 0.0
 
      g12 = real (y(1,2))
      b12 = aimag (y(1,2))
      imr=em*g12-fm*b12
      imi=em*b12+fm*g12
      ikr=ikr+imr
      iki=iki+imi
      rh=imr*fk-imi*ek
      rn=imr*ek+imi*fk
      mta = mt + ntota
      dpt(1,mta) = -rh
      dpt(2,mta) = -rn
 
      vksq = ek**2 + fk**2
      g12 = real (y(1,1)) * vksq
      b12 = aimag (y(1,1)) * vksq
      pk=ikr*ek+iki*fk+g12
      qk=ikr*fk-iki*ek-b12
      rh= -qk - b12
      rn = pk + g12
      kta = kt + ntota
      dpt(1,kta) = -rh
      dpt(2,kta) = -rn
      return
      end
