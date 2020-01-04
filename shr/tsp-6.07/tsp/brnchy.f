C    %W% %G%
      subroutine brnchy(k1, k2, id, ierr, gkm, bkm, gmk, bmk, gk1, bk1, 
     &                  gk2, bk2)
 
C     THIS SUBROUTINE COMPUTES THE PI-EQUIVALENT FOR BRANCH K1-K2-ID.
C     IF ID = '*', ALL PARALLELS ARE COMBINED. GKM AND BKM ARE THE
C     THRU ADMITTANCE OF THE LINE.  GK1 AND BK1 ARE THE SHUNT ADMITTANCE
C     AT BUS K1.  GK2 AND BK2 ARE THE SHUNT ADMITTANCE AT BUS K2.
C     "IERR" FLAGS RETURN STATUS: 0 = NORMAL
C                                -1 = ERROR (BRANCH NOT FOUND)
C                                -3 = ERROR (DC LINE )
C     BRNCHY IS CALLED BY INPUT2.
 
      include 'tspinc/params.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/comn34.inc'
      include 'tspinc/brnch.inc'
      include 'tspinc/namec.inc'
      include 'tspinc/pointr.inc'

      logical found
      character*1 id, idx
 
C     INITIALIZE VARIABLES
 
      ierr =  - 1
      gkm = 0.0
      bkm = 0.0
      gmk = 0.0
      bmk = 0.0
      gk1 = 0.0
      bk1 = 0.0
      gk2 = 0.0
      bk2 = 0.0

C     Remove distinction between ckt " " and "0"

      if (id .eq. '0') id = ' '
 
C     FIND BRANCH STARTING INDEX "NBR" BY BINARY SEARCH.
 
      i1 = 1
      i2 = ltot
      found = .false.

C     NBR is midpoint position in binary search

      do while (i1 .le. i2 .and. .not. found)
        nbr = (i1+i2)/2
        jbus1 = jbrnch(2, nbr)
        jbus2 = jbrnch(12, nbr)
        if (jbus1 .lt. k1) then
          i1 = nbr + 1
        elseif (jbus1 .gt. k1) then
          i2 = nbr - 1
C         -  Bus 1 has matched
        elseif (jbus2 .lt. k2) then
          i1 = nbr + 1
        elseif (jbus2 .gt. k2) then
          i2 = nbr - 1
C         -     Bus 2 has matched, also
        else
          found = .true.
        endif
      enddo
      if (.not. found) goto 140
 
C     ALIGN "NBR" TO FIRST OCCURRENCE OF K1-K2-ID.
 
C     Run backwards through branch table to find 1 before first
C     matching bus pair.  (Could be 0 if very first one)

  100 continue
      do i = nbr, 1, -1
        if (jbrnch(2, i) .ne. k1 .or. jbrnch(12, i) .ne. k2) goto 110
      enddo

C     Here if branch was very first one in table
      i = 0
 
  110 nbr = i + 1
      do i = nbr, ltot
        if (jbrnch(2, i) .ne. k1 .or. jbrnch(12, i) .ne. k2) goto 140
 
C       SKIP "R" AND "RX" RECORDS
 
        if (jbrnch(1, i) .ne. 4 .and. jbrnch(1, i) .ne. 9) then
          if (id .eq. '*') goto 120
          idx = char(jbrnch(13, i))
          if (idx .eq. id) goto 120
        endif
      enddo
      goto 140
C     
C     PARALLELS MATCHED. COMPUTE PI-EQUIVALENT.
C     
  120 continue
      do i = nbr, ltot
        if (jbrnch(2, i) .ne. k1 .or. jbrnch(12, i) .ne. k2) goto 150
        if (jbrnch(1, i) .ne. 4 .and. jbrnch(1, i) .ne. 9) then
          if (id .ne. '*') then
            idx = char(jbrnch(13, i))
            if (idx .eq. id .and. jbrnch(14, i) .eq. 0) goto 130
          elseif (jbrnch(14, i) .eq. 0) then
            ierr = 0
            call pieqix(jbrnch(1, i), g1, b1, g2, b2, g12, b12, g21,
     &       b21)
            gkm = gkm + g12
            bkm = bkm + b12
            gmk = gmk + g21
            bmk = bmk + b21
            gk1 = gk1 + g1
            bk1 = bk1 + b1
            gk2 = gk2 + g2
            bk2 = bk2 + b2
          endif
        endif
      enddo
      goto 150
  130 ierr = 0
      call pieqix(jbrnch(1, i), gk1, bk1, gk2, bk2, gkm, bkm, gmk, bmk)
      goto 150
C     
C     ERROR EXIT - BRANCH NOT FOUND
C     
  140 ierr =  - 1
  150 return
      end
