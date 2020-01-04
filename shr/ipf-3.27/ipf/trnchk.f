C    @(#)trnchk.f	20.3 2/13/96
      subroutine trnchk

      include 'ipfinc/parametr.inc'

      include 'ipfinc/amtrx.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/beta2.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/gamma.inc'
      include 'ipfinc/slnopt.inc'

      dimension emtrx(50,50)

C                       Store all Jacobian elements
      if (ntotx .gt. 50) return

      do 90 i = 1,ntot
         do 80 j = 1,ntot
            emtrx(j,i) = 0.0
   80    continue
   90 continue
C                    Store Jacobian elements for system slack nodes
      do 130 kt=1,nbslck
         emtrx(kt,kt) = 1.0
 130  continue
C                    Store Jacobian elements for nodes
      do 150 kt = nbslck+1,ntot
         call trnjac (kt,0)
         do 140 l = 1,lp
            mt = kolum(l)
            emtrx(kt,mt) = rowh(l)
  140    continue
  150 continue
C                    Check all columns of Jacobian
      do 210 kt = 1,ntot
      do 160 mt = 1,ntot
         dpt(1,mt) = emtrx(mt,kt)
  160 continue
      call baktrn (0)
      do 180 mt = 1,ntot
      if (kt .eq. mt) then
         if (abs(1.0 - dpt(1,mt)) .gt. 1.0e-6 ) then
            write (*,170) kt,mt,dpt(1,mt)
  170       format (' Sensitivity factorization error - column ',i4,
     &              ' row ',i4,' DPT ',e12.5)
         endif
      else
         if (abs(dpt(1,mt)) .gt. 1.0e-6 ) then
            write (*,170) kt,mt,dpt(1,mt)
         endif
      endif
  180 continue
  210 continue
      return
      end
