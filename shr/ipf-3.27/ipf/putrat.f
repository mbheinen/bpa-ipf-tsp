C    @(#)putrat.f	20.3 2/13/96
      subroutine putrat (ptr, rating)
      integer ptr
 
C     Insert a new branch rating for branch index NBR.
C    
C     If extended ratings are present, RATING is the minimum
C     non-zero extended rating.
C    
C     If extended ratings are not present, RATING is the nominal
C     rating.
C    
C     Input:  PTR    - BRNCH index.
C             RATING - branch rating
C    
C     OUTPUT: RATING is written into branch data.
C    
      include 'ipfinc/parametr.inc'

      include 'ipfinc/branch.inc'
 
      ix = 0
      ratx = 0.0
      nbr = iabs (brnch_ptr(ptr))
      ltype = brtype(ptr)
      if (ltype .eq. 3 .or. ltype .eq. 8) then
         do 2001 i = 1,2
         if (rateln(i,nbr) .gt. 0.0) then
            if (ratx .gt. 0.0) then
               if (rateln(i,nbr) .lt. ratx) then
                  ratx = rateln(i,nbr)
                  ix = i
               endif
            else
               ratx = rateln(i,nbr)
               ix = i
            endif
         endif
 2001    continue
      else if (ltype .eq. 5 .or. ltype .eq. 6) then
         do 2002 i = 1,3
         if (rateln(i,nbr) .gt. 0.0) then
            if (ratx .gt. 0.0) then
               if (rateln(i,nbr) .lt. ratx) then
                  ratx = rateln(i,nbr)
                  ix = i
               endif
            else
               ratx = rateln(i,nbr)
               ix = i
            endif
         endif
 2002    continue
      endif
      if (ix .eq. 0) then
         if (brtype(ptr) .ne. 1) then
            brnch(4,nbr) = rating
         endif
      else
         rateln(ix,nbr) = rating
      endif
 
      return
      end
