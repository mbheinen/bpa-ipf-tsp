C    @(#)pqsort.f	20.4 11/12/98
c****************************************************************
c
c   File: pqsort.for
c   Purpose: Sort one p-q curve into ascending order on pgen, 
c            rearranging the qmax and qmin points to match.
C
c
c   Author: Merilyn George    Date: 15 Oct 1992
c           WSCC          Modified: 
c   Called by: sortpqcv.for
c
c****************************************************************
c
      subroutine pqsort (p, qx, qn,ierr)
c
 
      dimension p(15), qx(15), qn(15)

      ierr = 0
      if (p(1) .eq. 0.0 .and. qx(1) .eq. 0.0 .and. qn(1) .eq. 0.0) then
c***     Error - no curve data
         ierr = 1
         return
      endif
      do i = 1, 15
         if (p(i) .lt. 0.0) p(i) = abs( p(i) )
      enddo

 99   i2 = 1
      iswap = 0
100   i1 = i2
      i2 = i1 + 1
      if (i2.eq.11 .or. p(i2).eq.0.0) go to 200
      if (p(i1) .lt. p(i2)) go to 100
c
      if (p(i1) .eq. p(i2)) then
***   Error - duplicate P values
         ierr = 2
         return
      endif
c
      iswap = iswap + 1
      temp = p(i1)
      p(i1) = p(i2)
      p(i2) = temp
      temp = qx(i1)
      qx(i1) = qx(i2)
      qx(i2) = temp
      temp = qn(i1)
      qn(i1) = qn(i2)
      qn(i2) = temp
      go to 100
c
 200  if (iswap .gt. 0) goto 99

c     Done with the sorting of points, begin data check

      do i = 1, i2
         if (qx(i) .gt. 0.0) go to 400
      enddo
***   Warning - all QMAX values are zero
      ierr = 3
      goto 900
  400 continue
      do i = 1, i2
         if (qn(i) .lt. 0.0) go to 900
      enddo
***   Warning - all QMIN values are zero
      ierr = 4

  900 continue
      return
      end
