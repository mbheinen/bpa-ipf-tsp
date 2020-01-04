C    @(#)gt_gebrv.f	20.6 8/30/00
C****************************************************************
c
c     To do:
c            1. 07-Jul-2000 "ratings" changed back to mva when
c               branch data file loaded.  This must be corrected
c               for the ext_ratings file for Paul Ferron.
c
C
C     File: gt_gebrv.f
C
C     Purpose: Routine to extract branch data into a GE-context 
C              array 
C
C     Input parameters:
C
C             p        - branch pointer
C             array    - array(24) in calling program
c             season   - (1,2,3,4) denoting field to receive default
c                        IPF branch ratings.
C
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: ext_ptil.f
C
C****************************************************************
      subroutine gt_gebrv (p, array, season)                                 
      real array(*)
      integer p, season

      include 'ipfinc/parametr.inc'

      include 'ipfinc/branch.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/prt.inc'

      common /branch_ratings/ ratings(8,MAXBRN),
     &                        ext_ratings(15,MAXBRN),
     &                        ext_date(2,MAXBRN),
     &                        numrat
      real ratings, ext_ratings
      character ext_date*6
      integer numrat

c     set up record type code parameters...
      include 'ipfinc/brtype.inc'

      common /is_batch / is_batch

      integer ptr, q, aq, k1, k2, ltype, itor
      real rtoi
      equivalence (itor, rtoi)
                                                                       
      error = 0                                                       
      k1 = kx(p)
      k2 = ky(p)
      ltype = brtype(p)
      q = brnch_ptr(p)
      aq = iabs(q)

      do i = 1, 25
        array(i) = 0.0
      enddo

      if (ltype.eq.BRTYP_L .or. ltype.eq.BRTYP_T .or.
     &    ltype.eq.BRTYP_TP .or. ltype.eq.BRTYP_E) then

         if (ltype .eq. BRTYP_L) then
            array(1) = brnch(5,aq)  
            array(2) = brnch(6,aq)         
            array(3) = 2.0 * brnch(7,aq)
            array(4) = 2.0 * brnch(8,aq)
            array(13) = brnch(9,aq)
         else if (ltype .eq. BRTYP_E) then
            array(1) = brnch(5,aq)  
            array(2) = brnch(6,aq)         
            array(3) = 0.0
            array(4) = 0.0
         else if (ltype .eq. BRTYP_T) then
            if (q .gt. 0) then
              tx1 = brnch(9,aq) / base(k1)
              tx2 = brnch(10,aq) / base(k2)
              array(9) = tx1
              array(10) = tx2
              rtx = brnch(5,aq)
              xtx = brnch(6,aq)
              array(1) = rtx
              array(2) = xtx
              array(3) = brnch(7,aq)
              array(4) = -abs(brnch(8,aq))
            else
              tx1 = brnch(10,aq) / base(k1)
              tx2 = brnch(9,aq) / base(k2)
              array(9) = tx1
              array(10) = tx2
              rtx = brnch(5,aq)
              xtx = brnch(6,aq)
              array(1) = rtx
              array(2) = xtx
              array(3) = brnch(7,aq)
              array(4) = -abs(brnch(8,aq))
            endif
            array(12) = brnch(18,aq)          ! Tx base
         else if (ltype .eq. BRTYP_TP) then
            rtx = brnch(5,aq)
            xtx = brnch(6,aq)
            array(1) = rtx
            array(2) = xtx
            array(3) = brnch(7,aq)
            array(4) = -abs(brnch(8,aq))
            if (q .gt. 0) then
              array(9) = brnch(9,aq)
            else
              array(9) = -brnch(9,aq)
            endif
            array(10) = 1.0
            array(12) = brnch(18,aq)          ! Tx base
         endif
c
c        If this is a line section containing a transformer, the
c        basekv used must reflect the actual kv base.
c
         if ((ltype .eq. BRTYP_L .or. ltype .eq. BRTYP_E) .and.
     &        base(k1) .ne. base(k2) .and. brsect(p) .gt. 0) then
           basekv = base(k1)
           ptr = kbsdta(16,k1)
           do while (ptr .gt. 0 .and. ky(ptr) .ne. k2 .and. 
     &               brid(ptr) .ne. brid(p))
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
             rate1 = factor * rateln(1,aq)
             rate2 = factor * rateln(2,aq)
             if (rate1 .eq. 0.0 .and. rate2 .eq. 0.0) then
               rate1 = factor * brnch(4,aq)
               rate2 = factor * brnch(4,aq)
             else if (rate1 .eq. 0.0) then
               rate1 = rate2
             else if (rate2 .eq. 0.0) then
               rate2 = rate1
             endif

             do i = 15, 25
               array(i) = 0.0
             enddo
c
c            array(2*season+13) = Normal rating (lesser of Thermal or 
c                                 Bottleneck)
c            array(2*season+14) = Same
c
             array(2*season+13) = amin1 (rate1, rate2)
             array(2*season+14) = amin1 (rate1, rate2)

           else
             ix = -rateln(1,aq)
             do i = 1, 15
               ext_ratings(i,ix) = factor * ext_ratings(i,ix)
             enddo
             do i = 1, 8
               array(i+14) = factor * ratings(i,ix) 
             enddo
             array(23) = ix                     ! Index to ext_ratings
             array(24) = 0
             array(25) = factor * brnch(4,aq)   ! Nominal ratings
           endif
         else
c
c          If rateln(1,aq) < 0, ratings originate in Branch Data File
c
           if (rateln(1,aq) .ge. 0.0) then
             rate1 = rateln(1,aq)
             rate2 = rateln(2,aq)
             rate3 = rateln(3,aq)
             if (rate1 .eq. 0.0 .and.
     &           rate2 .eq. 0.0 .and.
     &           rate3 .eq. 0.0) then
               rate1 = brnch(4,aq)
               rate2 = brnch(4,aq)
               rate3 = brnch(4,aq)
             else if (rate1 .eq. 0.0 .and.
     &                rate2 .eq. 0.0) then
               rate1 = rate3
               rate2 = rate3
             else if (rate1 .eq. 0.0 .and.
     &                rate3 .eq. 0.0) then
               rate1 = rate2
               rate3 = rate2
             else if (rate2 .eq. 0.0 .and.
     &                rate3 .eq. 0.0) then
               rate2 = rate1
               rate3 = rate1
             else if (rate1 .eq. 0.0) then
               rate1 = rate3
             else if (rate2 .eq. 0.0) then
               rate2 = rate3
             else if (rate3 .eq. 0.0) then
               rate3 = rate1
             endif
c
c            array(2*season+13) = Normal rating (lesser of Thermal or 
c                                 Bottleneck)
c            array(2*season+14) = Same
c
             do i = 15, 25
               array(i) = 0.0
             enddo

             array(2*season+13) = amin1 (rate1, rate3)
             array(2*season+14) = amin1 (rate2, rate3)

           else
             ix = -rateln(1,aq)
             do i = 1, 8
               array(i+14) = ratings(i,ix)
             enddo
             array(23) = ix                      ! Index to ext_ratings
             array(24) = 0
             array(25) = brnch(4,aq)             ! Nominal ratings
           endif
         endif
      endif
      return
      end
