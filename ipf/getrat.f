C    @(#)getrat.f	20.3 2/13/96
C****************************************************************
C
C       File: getrat.for
C
C       Purpose: Routine to obtain the branch rating for branch index
C                PTR. If extended ratings are present, RATING is the 
C                minimum non-zero extended rating.
C                If extended ratings are not present, RATING is the 
C                nominal rating.
C
C       Input parameters:  
C
C             PTR    - kx(), ky() index.
C       Output parameters:
C      
C             RATING - branch rating
C             RATTAG - 'N' (Nominal)
C                      'T' (Thermal)
C                      'E' (Emergency, Transformer only)
C                      'B' (Bottleneck)
C             RATNOM - nominal branch rating
C             RATTHR - thermal branch rating
C             RATLLF - emergency branch rating
C             RATBTL - bottlenexk branch rating
C
C       Author: Walt Powell  Date: 12 November 1992
C       Called by: gtlnload, gtlfq
C
C****************************************************************
C
      subroutine getrat (ptr, rating, rattag, ratnom, ratthr,
     1                   ratllf, ratbtl)
      integer ptr
      character rattag * (*)
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/branch.inc'
 
      character typeln * 2, typetx * 3
      data typeln / 'TB' /
      data typetx / 'TEB' /
 
      nbr = iabs (brnch_ptr(ptr))
      rating = 0.0
      ratnom = brnch(4,nbr)
      ltype = brtype(ptr)
      if (ltype .eq. 3 .or. ltype .eq. 8) then
         ratthr = rateln(1,nbr)
         ratbtl = rateln(2,nbr)
         ratllf = 0.0
         do i = 1,2
            if (rateln(i,nbr) .gt. 0.0) then
               if (rating .gt. 0.0) then
                  if (rateln(i,nbr) .lt. rating) then
                     rating = rateln(i,nbr)
                     rattag = typeln(i:i)
                  endif
               else
                  rating = rateln(i,nbr)
                  rattag = typeln(i:i)
               endif
            endif
         enddo
      else if (ltype .eq. 5 .or. ltype .eq. 6) then
         ratthr = rateln(1,nbr)
         ratllf = rateln(2,nbr)
         ratbtl = rateln(3,nbr)
         do i = 1,3
            if (rateln(i,nbr) .gt. 0.0) then
               if (rating .gt. 0.0) then
                  if (rateln(i,nbr) .lt. rating) then
                     rating = rateln(i,nbr)
                     rattag = typetx(i:i)
                  endif
               else
                  rating = rateln(i,nbr)
                  rattag = typetx(i:i)
               endif
            endif
          enddo
      endif
      if (rating .eq. 0.0) then
         rating = brnch(4,nbr)
         rattag = 'N'
      endif
 
      return
      end
