C    @(#)sfctr.f	20.3 2/13/96
      subroutine sfctr (ne, indx, amtx, iamtx, iodx, fmtx, ifmtx, 
     &                  diag, work)
      integer ifmtx(*), indx(*), iodx(*), iamtx(*)
      real amtx(*), diag(*), fmtx(*), work(*)
C
C          THIS SUBROUTINE FACTORS A REAL SYMMETRIC MATRIX IN TO
C          THE FORM L D L(TRANSPOSE).  THE INPUT IS ASSUMED TO BE
C          STORED IN A SPARSE FORM.
C     ON INPUT:
C          NE      NUMBER OF ROWS
C          INDX    BASE ADDRESS OF EACH ROW IN ARRAY AMTX AND IAMTX
C          AMTX    VALUE OF EACH NON-ZERO ELEMENT.
C          IAMTX   COLUMN NUMBER OF EACH NON-ZERO ELEMENT -- E.G.,
C                  IAMTX(1) = COLUMN NUMBER OF FIRST NON-ZERO ELEMENT IN
C                             ROW 1
C                  AMTX(1)  = VALUE OF THE FIRST NON-ZERO ELEMENT IN
C                             ROW 1
C                  ETC., ALL THE NON-ZERO ELEMENTS ARE STORED.
C          WORK    WORKING ARRAY OF LENGTH NE
C
C     ON OUTPUT:
C          IODX    BASE ADDRESS OF EACH ROW OF L(TRANSPOSE)
C          FMTX    VALUE OF EACH NON-ZERO ELEMENT IN
C                  L(TRANSPOSE). SAME FORMAT AS AMTX.
C          IFMTX   COLUMN NUMBER OF EACH NON-ZERO ELEMENT IN
C                  L(TRANSPOSE). SAME FORMAT AS IAMTX.
C          DIAG    CONTAINS THE RECIPRICOL OF EACH DIAGONAL ELEMENT
C
C
C     HISTORY:
C          WRITTEN BY SHERMAN M. CHAN, AUGUST 1980.
C
      iodx(1) = 1
      iou = 1
      zero = 0.

C        LOOP THROUGH ALL THE ROWS
C
      do 1000 k=1,ne
         ki = indx( k)
         kl = indx( k+1) - ki
C
C        ZERO OUT WORK AND COPY WORKING ROW TO WORK ARRAY
C
         call fvfill (zero,work,1,ne)
C
         do 200 i=1,kl
         i2 = i + ki - 1
  200    work( iamtx(i2) ) = amtx(i2)
C
         if (k.eq.1) go to 600
         k1 = k - 1
C
C        SCAN THE FIRST K-1 ELEMENTS OF THE WORKING ROW
C
         do 500 m=1,k1
         if (work(m) .eq. 0) go to 500
         mi = iodx(m)
         ml = iodx(m+1) - mi
         g = work(m)
C
C        ELIMINATE THE MTH ELEMENT OF THE WORKING ROW
C
         do 400 l=1,ml
            l2 = l + mi -1
            lcol = ifmtx(l2)
  400       work(lcol) = work(lcol) - g*fmtx(l2)
  500    continue
C
C           PROCESS THE DIAGONAL ELEMENT
C
  600 if (work(k) .eq. 0.0) work(k) = 1.0e-6
      g = 1./work(k)
      diag(k) = g
C
C             STORE THE RESULT IN FMTX
C
      if (k.eq.ne) go to 1000
      k1 = k + 1
      do 700 m=k1,ne
          if (work(m) .eq. 0.) go to 700
          ifmtx(iou) = m
          fmtx(iou) = g * work(m)
          iou = iou + 1
  700 continue
      iodx(k1) = iou
 1000 continue
C
      iodx(ne+1) = iodx(ne)
      return
      end
