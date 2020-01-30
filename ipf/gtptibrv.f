C    @(#)gtptibrv.f	20.8 8/30/00
C****************************************************************
C
C     File: gtptibrv.f
C
C     Purpose: Routine to extract branch data into a PTI-context 
C              array 
C
C     Input parameters:
C
C             p        - branch pointer
C             array    - array(24) in calling program
C
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: ext_ptil.f
C
C****************************************************************
      subroutine gtptibrv (p, array)                                 
      real array(*)
      integer p

      include 'ipfinc/parametr.inc'

      include 'ipfinc/branch.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/prt.inc'

c     set up record type code parameters...
      include 'ipfinc/brtype.inc'

      common /branch_ratings/ ratings(8,MAXBRN),
     &                        ext_ratings(15,MAXBRN),
     &                        ext_date(2,MAXBRN),
     &                        numrat
      real ratings, ext_ratings
      character ext_date*6
      integer numrat

      common /is_batch / is_batch

      integer ptr, q, aq, k1, k2, ltype
                                                                       
      error = 0                                                       
      k1 = kx(p)
      k2 = ky(p)
      ltype = brtype(p)
      q = brnch_ptr(p)
      aq = iabs(q)

      do i = 1, 22
        array(i) = 0.0
      enddo

      if (ltype.eq.BRTYP_L .or. ltype.eq.BRTYP_T .or.
     &    ltype.eq.BRTYP_TP .or. ltype.eq.BRTYP_E) then

         if (ltype .eq. BRTYP_L) then
            array(1) = brnch(5,aq)  
            array(2) = brnch(6,aq)         
            array(3) = 2.0 * brnch(7,aq)
            array(4) = 2.0 * brnch(8,aq)
            array(15) = brnch(9,aq)
         else if (ltype .eq. BRTYP_E) then
            array(1) = brnch(5,aq)  
            array(2) = brnch(6,aq)         
            array(11) = brnch(7,aq)
            array(12) = brnch(8,aq)
            array(13) = brnch(9,aq)
            array(14) = brnch(10,aq)
         else if (ltype .eq. BRTYP_T) then
            if (q .gt. 0) then
              tx1 = brnch(9,aq) / base(k1)
              tx2 = brnch(10,aq) / base(k2)
            else
              tx1 = brnch(10,aq) / base(k1)
              tx2 = brnch(9,aq) / base(k2)
            endif
            array(9) = tx1 / tx2
            array(1) = brnch(5,aq)  * tx2 ** 2
            array(2) = brnch(6,aq)  * tx2 ** 2       
            array(13) = brnch(7,aq) / tx2 ** 2
            array(14) = -brnch(8,aq) / tx2 ** 2
         else if (ltype .eq. BRTYP_TP) then
            array(1) = brnch(5,aq)  
            array(2) = brnch(6,aq)         
            array(13) = brnch(7,aq)
            array(14) = -brnch(8,aq)
            array(9) = 1.0
            if (q .gt. 0) then
              array(10) = brnch(9,aq)
            else
              array(10) = -brnch(9,aq)
            endif
         endif
c
c        If this is a line section containing a transformer, the
c        basekv used must reflect the actual kv base.
c
         if ((ltype .eq. BRTYP_L .or. ltype .eq. BRTYP_E) .and.
     &        base(k1) .ne. base(k2) .and. brsect(p) .gt. 0) then
           basekv = base(k1)
           ptr = kbsdta(16,k1)
           do while (ptr .gt. 0 .and. 
     &              (ky(ptr) .ne. k2 .or. brid(ptr) .ne. brid(p)))
             ptr = brnch_nxt(ptr)
           enddo
           do while (ptr .gt. 0 .and. ptr .ne. p .and.
     &               ky(ptr) .eq. k2 .and. brid(ptr) .eq. brid(p))
             if (brtype(ptr) .eq. 5) basekv = base(k2)
             ptr = brnch_nxt(ptr)
           enddo
         else
           basekv = base(k1)
         endif
         if (ltype .eq. BRTYP_L .or. ltype .eq. BRTYP_E) then
           factor = 0.001 * basekv * sqrt (3.0) 
c
c          If rateln(1,aq) < 0, ratings originate in Branch Data File
c
           if (rateln(1,aq) .ge. 0.0) then
             array(5) = factor * rateln(1,aq)
             array(6) = factor * rateln(1,aq)
             array(7) = factor * rateln(2,aq)
             if (array(5) .eq. 0.0) array(5) = array(7)
             if (array(6) .eq. 0.0) array(6) = array(7)
             if (array(7) .eq. 0.0) array(7) = array(5)
             if (array(5) .eq. 0.0) array(5) = factor * brnch(4,aq)
             if (array(6) .eq. 0.0) array(6) = factor * brnch(4,aq)
             if (array(7) .eq. 0.0) array(7) = factor * brnch(4,aq)
           else
             ix = -rateln(1,aq)
             array(5) = factor * ratings(1,ix)
             array(6) = factor * ratings(2,ix)
             array(7) = factor * ratings(3,ix)
             if (array(5) .eq. 0.0) array(5) = array(7)
             if (array(6) .eq. 0.0) array(6) = array(7)
             if (array(7) .eq. 0.0) array(7) = array(5)
             if (array(5) .eq. 0.0) array(5) = factor * brnch(4,aq)
             if (array(6) .eq. 0.0) array(6) = factor * brnch(4,aq)
             if (array(7) .eq. 0.0) array(7) = factor * brnch(4,aq)
           endif
         else
           if (rateln(1,aq) .ge. 0.0) then
             array(5) = rateln(1,aq)
             array(6) = rateln(2,aq)
             array(7) = rateln(3,aq)
             if (array(5) .eq. 0.0) array(5) = array(6)
             if (array(5) .eq. 0.0) array(5) = array(7)
             if (array(5) .eq. 0.0) array(5) = brnch(4,aq)
             if (array(6) .eq. 0.0) array(6) = array(5)
             if (array(6) .eq. 0.0) array(6) = brnch(4,aq)
             if (array(6) .eq. 0.0) array(6) = array(7)
             if (array(7) .eq. 0.0) array(7) = array(5)
             if (array(7) .eq. 0.0) array(7) = array(6)
             if (array(7) .eq. 0.0) array(7) = brnch(4,aq)
           else
             ix = -rateln(1,aq)
             array(5) = ratings(1,ix)
             array(6) = ratings(2,ix)
             array(7) = ratings(3,ix)
             if (array(5) .eq. 0.0) array(5) = array(7)
             if (array(6) .eq. 0.0) array(6) = array(7)
             if (array(7) .eq. 0.0) array(7) = array(5)
             if (array(5) .eq. 0.0) array(5) = factor * brnch(4,aq)
             if (array(6) .eq. 0.0) array(6) = factor * brnch(4,aq)
             if (array(7) .eq. 0.0) array(7) = factor * brnch(4,aq)
           endif
         endif
      endif
      return
      end
