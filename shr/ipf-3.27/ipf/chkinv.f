C    @(#)chkinv.f	20.3 2/13/96
      subroutine chkinv
C
C     This subroutine checks the accuracy of the inverse INV.
C
 
      include 'ipfinc/parametr.inc'
 
      include 'ipfinc/aref.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/smallp.inc'
      include 'ipfinc/trmdbg.inc'
 
 
      logical * 4 error, rowerr
      real * 8 aij
 
 9404 format (1h0,12x,'COLUMN ',7(6x,i2,6x))
 9408 format (1h0,12x,'YBASIS ',7(5x,i3,6x))
 9412 format (1h0,16x,'YR ',2x,7(1pe12.4,2x))
 9416 format (1h0,'ROW XBS ',4x,'XR',5x,'INVERSE MATRIX'/)
 9420 format (1x,i3,1x,i3,8(1pe12.4,2x))
 9424 format (1h0,5x,8(5x,i3,6x))
 9428 format (1h0,5x,8(5x,i3,6x))
 9432 format (1h0,5x,8(1pe12.4,2x))
 9436 format (1h0,'ROW',5x,'INVERSE MATRIX CONTINUES'/)
 9438 format (1h ,i3,2x,8(1pe12.4,2x))
 
      error = .false.
 
      do 50 l = 1,size
 
      rowerr = .false.
 
      do 10 j = 1, size
  10  piv(j) = 0.0
 
      do 30 j = 1, size
      i = ybasis(j)
      istart = irow(i)
      last = irow(i+1) - 1
      do 20 look = istart,last
         k = jcol(look)
         if (inbase(k) .gt. 0) then
            mmm = inbase(k)
            aij = aa(look)
            piv(mmm) = piv(mmm) + inv(l,j) * aij
         endif
  20  continue
      if (xbasis(j) .gt. n) then
         if (ybasis(j) .eq. xbasis(j) - n) then
            aij = dble (1.0)
            piv(j) = piv(j) + inv(l,j) * aij
         endif
      endif
  30  continue
 
      do 40 j = 1, size
      if (j .eq. l) then
         if (dabs(piv(j) - dble(1.0)) .gt. 1.0e-6) then
            error = .true.
            rowerr = .true.
         endif
      else
         if (dabs(piv(j)) .gt. 1.0e-6) then
            error = .true.
            rowerr = .true.
         endif
      endif
  40  continue
 
      if (rowerr) then
         write (dbug,48) l,(j,inv(j,l),j=1,size)
  48     format (' INV row ',i3,1p,5(i5,e12.5) /
     1      (12x,5(i5,e12.5)))
         if (iterm .ne. 0) then
            write (*,48) l,(j,inv(j,l),j=1,size)
         endif
      endif
 
  50  continue
 
      if (error) then
         iend = 7
         if (size .le. iend) iend = size
         write (dbug,9404) (l, l = 1,iend)
  410    write (dbug,9408) (ybasis(l), l = 1,iend)
         write (dbug,9412) (yr(l), l = 1,iend)
         write (dbug,9416)
 
         do 430 k = 1,size
  430    write (dbug,9420) k, xbasis(k), xr(k), (inv(k,l),l = 1,iend)
 
  440    if (size.le.iend) go to 500
         istart = iend + 1
         iend = iend + 8
         if (size.le.iend) iend = size
         write (dbug,9424) (l, l = istart,iend)
         write (dbug,9428) (ybasis(l), l = istart,iend)
         write (dbug,9432) (yr(l), l = istart, iend)
         write (dbug,9436)
 
         do 445 k = 1,size
  445    write (dbug,9438) k, (inv(k,l), l = istart,iend)
 
         go to 440
  500    continue
 
      endif
      return
      end
