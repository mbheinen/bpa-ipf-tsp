C    @(#)linflo.f	20.3 2/13/96
      subroutine linflo (i,ain,pin,qin,c1,dv1,c2,dv2)
C
C     This subroutine calculates the line flow AIN (in amps) and
C     PIN + jQIN (in MW and MVAR) for transfer sensitivity branch I.
C     The voltages are subjected to compensation-perturbation C1*DV1(,)
C     and transfer-perturbation C2*DV2().
C
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
c	Global variables used:
c		bmva
      include 'ipfinc/bus.inc'
c	Global variables used:
c		e(r*8), f(r*8)
      include 'ipfinc/transf.inc'
c	Global variables used:
c		fdata, fymtrx
c
      double precision dv1(2,*) 
      real  dv2(*)
      complex v(2), a(2), ax, vx
 
      if (abs (c1) .ge. 1.0e10 .or. abs (c2) .ge. 1.0e10) then
         pin = 1.0e10
         qin = 1.0e10
         ain = 1.0e10
      else
         k1 = kfdata(1,i)
         k2 = kfdata(2,i)
 
         vk = e(k1)
         ak = f(k1) + c1 * dv1(1,k1) + c2 * dv2(k1)
         ek = vk * cos (ak)
         fk = vk * sin (ak)
         v(1) = cmplx (ek,fk)
 
         vm = e(k2)
         am = f(k2) + c1 * dv1(1,k2) + c2 * dv2(k2)
         em = vm * cos (am)
         fm = vm * sin (am)
         v(2) = cmplx (em,fm)
 
         a(1) = fymtrx(1,i) * v(1) + fymtrx(2,i) * v(2)
C
C        Compute branch voltages and currents into section voltages and
C        currents.
C
         vx =  (cmplx (fdata(15,i),fdata(16,i)) * v(1)
     1        + cmplx (fdata(17,i),fdata(18,i)) * a(1))
         ax =  (cmplx (fdata(19,i),fdata(20,i)) * v(1)
     1        + cmplx (fdata(21,i),fdata(22,i)) * a(1))
 
         ain = cabs (ax) * fdata(10,i)
         pin = real (vx * conjg (ax)) * bmva
         qin = aimag (vx * conjg (ax)) * bmva
      endif
 
      return
      end
