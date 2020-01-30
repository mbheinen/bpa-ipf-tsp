C    @(#)tieflo.f	20.3 2/13/96
      function tieflo (jt,c1,dv1,c2,dv2)
C
C     This function calculates the flow for intertie line JT
C     subject to a voltage perturbation C1*DV1(,)+C2*DV2().
C     Since all variables used in this routine are now double
C     precision, everything, except the function return value
C     has been converted to double precision.
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/arcntl.inc'
c	Global variables used:
c		None
      include 'ipfinc/area.inc'
c	Global variables used:
c		tie(r*8)
      include 'ipfinc/blank.inc'
c	Global variables used:
c		kdtot, bmva, mtdcbs
      include 'ipfinc/bus.inc'
c	Global variables used:
c		e(r*8), f(r*8)
      include 'ipfinc/dc2t.inc'
c	Global variables used:
c		dc2t(r*8)
      include 'ipfinc/dcmt.inc'
c	Global variables used:
c		dcmtln(r*8), dcmtbs(r*8)
 
      dimension dv1(2,*), dv2(*)
c
      double precision dv1, dv2, v1, v2, vk, ak, ek, fk, aik, bik
      double precision vm, am, em, fm, gk11, bk11, gk12, bk12
c
      if (abs (c1) .ge. 1.0e10 .or. abs (c2) .ge. 1.0e10) then
         tieflo = 1.0e10
      else
         k1 = tie(1,jt)
         k2 = tie(7,jt)
         kdc = tie(9,jt)
         if (kdc .gt. 0) then
C
C           Check multi-terminal d-c
C
            do 100 mdc = 1, mtdcbs
 
               m1 = dcmtln(1,mdc)
               m2 = dcmtln(2,mdc)
               if (min0(m1,m2) .ne. min0(k1,k2)) go to 100
               if (max0(m1,m2) .eq. max0(k1,k2)) then
                  if (m1 .eq. k1) then
                     l1 = dcmtln(8,mdc)
                     l2 = dcmtln(9,mdc)
                  else
                     l1 = dcmtln(9,mdc)
                     l2 = dcmtln(8,mdc)
                  endif
 
                  v1 = dcmtbs(20,l1)
                  v2 = dcmtbs(20,l2)
                  tieflo = v1 * (v1-v2) / dcmtln(4,mdc)
                  return
               endif
  100       continue
C
C           Check 2-terminal d-c arrays
C
            do 110 mdc = 1, kdtot
 
               if (dc2t(1,mdc) .eq. k1 .and. dc2t(3,mdc) .eq. k2) then
 
                  tieflo = 0.001 * dc2t(39,mdc) * dc2t(40,mdc)
                  return
 
               else if (dc2t(1,mdc) .eq. k2 .and. dc2t(3,mdc) .eq. k1)
     1            then
 
                  tieflo = -0.001 * dc2t(39,mdc) * dc2t(41,mdc)
                  return
 
               endif
  110       continue
 
            write (*,120)
  120       format (' Abnormal termination: TIEFLO - 110 ')
 
         else
 
            vk = e(k1)
            ak = f(k1) + c1 * dv1(1,k1) + c2 * dv2(k1)
            ek = vk * dcos (ak)
            fk = vk * dsin (ak)
 
            vm = e(k2)
            am = f(k2) + c1 * dv1(1,k2) + c2 * dv2(k2)
            em = vm * dcos (am)
            fm = vm * dsin (am)
 
            gk11 = tie(3,jt)
            bk11 = tie(4,jt)
            gk12 = tie(5,jt)
            bk12 = tie(6,jt)
            aik = ek * gk11 - fk * bk11 + em * gk12 - fm * bk12
            bik = ek * bk11 + fk * gk11 + em * bk12 + fm * gk12
            tieflo = (ek*aik + fk*bik) * bmva
            return
         endif
 
      endif
      end
