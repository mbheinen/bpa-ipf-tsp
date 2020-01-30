C    @(#)deltay.f	20.3 2/13/96
      subroutine deltay (k1,m1,id1,ksect1,itype,deltax,ynew,yznew)
 
C     This subroutine computes a perturbed Y-matrix [Y(B+dB)] where
C     branch element K1-M1-ID-KSECT1 is perturbed an amount DELTAX.
C
C     If ID = '*', then the branch element applies to the entire
C     branch.  Otherwise, is pertains to an individual section.
 
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
c	Global variables used:
c		None
      include 'ipfinc/branch.inc'
c	Global variables used:
c		kbrnch
      include 'ipfinc/bus.inc'
c	Global variables used:
c		inp2opt, kbsdta
      include 'ipfinc/intbus.inc'
c	Global variables used:
c		intbus, intbas
      include 'ipfinc/lfiles.inc'
c	Global variables used:
c		dbug
      include 'ipfinc/prt.inc'
c	Global variables used:
c		None
c
      complex * 16 y(2,2)
c
      character * 1 id1, id2
c
      complex ynew(2,2), yznew(2)
c
      real r, x, g1, b1, g2, b2, xnew, bnew
c 
      do 1010 j = 1, 2
         do 1000 i = 1, 2
            ynew(i,j) = cmplx (0.0,0.0)
 1000    continue
 1010 continue
 
      nsect = 0
      do 1200 i = kbsdta(16,k1), kbsdta(16,k1+1) - 1
      if (kbrnch(12,i) .eq. m1) then
 
         if (kbrnch(1,i) .eq. 4 .or. kbrnch(1,i) .eq. 9) go to 1200
         call getchr(1,id2,kbrnch(13,i))
         if (id1 .eq. '*') then
            if (kbrnch(14,i) .eq. 0) then
               call pieqiv (i,y,ierr)
               do 1110 k = 1, 2
                  do 1100 j = 1, 2
                     ynew(j,k) = ynew(j,k) + cmplx(y(j,k))
 1100             continue
 1110          continue
            endif
         else if (id1 .eq. id2) then
            if (ksect1 .eq. 0) then
               if (kbrnch(14,i) .ne. 0) then
                  if (kbrnch(2,i+1) .ne. k1 .or.
     1                kbrnch(12,i+1) .ne. m1 .or.
     2                kbrnch(13,i+1) .ne. kbrnch(13,i)) then
                     call finsec (ynew)
                  endif
                  go to 1200
               endif
            else
               if (kbrnch(14,i) .eq. 0) go to 1200
            endif
            call pieqiv (i,y,ierr)
            if (kbrnch(14,i) .eq. ksect1) then
               r = real (cmplx(-1.0,0.0)/cmplx(y(1,2)))
               x = aimag (cmplx(-1.0,0.0)/cmplx(y(1,2)))
               g1 = sngl(dreal (y(1,1) + y(1,2)))
               b1 = sngl(dimag (y(1,1) + y(1,2)))
               g2 = sngl(dreal (y(2,2) + y(2,1)))
               b2 = sngl(dimag (y(2,2) + y(2,1)))
               if (itype .eq. 1) then
                  xnew = x + deltax
                  y(1,2) = (cmplx(-1.0,0.0)/cmplx(r,xnew))
                  y(2,1) = y(1,2)
                  y(1,1) = cmplx(g1,b1) - y(1,2)
                  y(2,2) = cmplx(g2,b2) - y(2,1)
               else
                  bnew = b1 + deltax
                  y(1,2) = (cmplx(-1.0,0.0)/cmplx(r,x))
                  y(2,1) = y(1,2)
                  y(1,1) = cmplx (g1,bnew) - y(1,2)
                  y(2,2) = cmplx (g2,b2) - y(2,1)
               endif
               yznew(1) = cmplx(-1.0,0.0) / cmplx(y(1,2))
               yznew(2) = cmplx( y(1,1) + y(1,2) )
 
            endif
 
            nsect = nsect + 1
            if (nsect .eq. 1) then
               call firsecd (y)
            else
               call nexsecd (y)
            endif
 
            if (kbrnch(2,i+1) .ne. k1 .or. kbrnch(12,i+1) .ne. m1 .or.
     1         kbrnch(13,i+1).ne. kbrnch(13,i)) then
               call finsec (ynew)
            endif
         endif
      else if (kbrnch(12,i) .gt. m1) then
         go to 1300
      endif
 1200 continue
 1300 continue
 
      if (id1 .eq. '*') then
         r = real (cmplx(-1.0,0.0)/ynew(1,2))
         x = aimag (cmplx(-1.0,0.0)/ynew(1,2))
         g1 = real (ynew(1,1) + ynew(1,2))
         b1 = aimag (ynew(1,1) + ynew(1,2))
         g2 = real (ynew(2,2) + ynew(2,1))
         b2 = aimag (ynew(2,2) + ynew(2,1))
         if (itype .eq. 1) then
            xnew = x + deltax
            ynew(1,2) = (cmplx(-1.0,0.0)/cmplx(r,xnew))
            ynew(2,1) = ynew(1,2)
            ynew(1,1) = cmplx (g1,b1) - ynew(1,2)
            ynew(2,2) = cmplx (g2,b2) - ynew(2,1)
         else
            bnew = b1 + deltax
            ynew(1,2) = (cmplx(-1.0,0.0)/cmplx(r,x))
            ynew(2,1) = ynew(1,2)
            ynew(1,1) = cmplx(g1,bnew) - ynew(1,2)
            ynew(2,2) = cmplx(g2,b2) - ynew(2,1)
         endif
         yznew(1) = cmplx(-1.0,0.0) / ynew(1,2)
         yznew(2) = ynew(1,1) + ynew(1,2)
      endif
 
      kt = inp2opt(k1)
      mt = inp2opt(m1)
 
      write (dbug,1400) intbus(kt), intbas(kt), intbus(mt),
     1   intbas(mt), id1, ksect1, itype, deltax
 1400 format ('0 Y-matrix perturbation of branch ',a8,f6.1,1x,a8,f6.1,
     1         1x,a1,1x,i1,' ITYPE ',i1,' Delta X ',e12.5,/)
      do 1600 i = 1, 2
      do 1600 j = 1, 2
      write (dbug,1500) i, j, ynew(i,j)
 1500 format ('  [Y(B+dB)] ',2i3,' (',e12.5,',',e12.5,')')
 1600 continue
      return
      end
