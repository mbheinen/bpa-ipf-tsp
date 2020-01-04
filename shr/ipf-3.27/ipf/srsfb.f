C    @(#)srsfb.f	20.3 2/13/96
      subroutine srsfb (ne,x, iodx,fmtx,ifmtx, diag)
C
C         THIS SUBROUTINE PERFORMS THE DOWNWARD OPERATION AND
C         BACKWARD SUBSTITUTION ON A REAL SYMMETRIC MATRIX
C         FACTORED AS L D L(TRANSPOSE).  DATA FORMAT IS THE SAME
C         AS THAT FOR THE  FPS SUBROUTINE SRSFB.
C           X    IS THE RIGHT-HAND-SIDE VECTOR. IT CONTAINS THE SOLUTION
C                ON OUTPUT.
C
      integer ifmtx(1)
      dimension x(1),iodx(1),fmtx(1),diag(1)
C
C     DOWNWARD OPERATION -- I.E. SOLVE L Y = B
C
      ne1 = ne - 1
C
      do 200 j=1,ne1
      g = x(j)
      ji = iodx(j)
      jl = iodx(j+1) - 1
      if (ji .gt. jl) go to 200
C
      do 100 m=ji,jl
         mrow = ifmtx(m)
  100 x(mrow) = x(mrow) - g * fmtx(m)
  200 continue
C
C     BACKWARD SUBSTITUTION -- I.E. SOLVE D L(TRANS.) = Y
C
      x(ne) = diag(ne) * x(ne)
      do 400 i=1,ne1
      j = ne - i
      ji = iodx(j)
      jl = iodx(j+1) - 1
      x(j) = x(j) * diag(j)
      if (ji .gt. jl) go to 400
C
      do 300 m=ji,jl
          mcol = ifmtx(m)
  300 x(j) = x(j) - x(mcol) * fmtx(m)
  400 continue
      return
      end
