C    @(#)chacc.f	20.3 2/13/96
        subroutine chacc
 
 
      include 'ipfinc/parametr.inc'
 
      include 'ipfinc/aref.inc'
c	Global variables used:
c		irow, jcol, aa
      include 'ipfinc/lfiles.inc'
c	Global variables used:
c		dbug
      include 'ipfinc/smallp.inc'
c	Global variables used:
c		istate, inbase, n, iseff, tol, xbasis, size, 
c		numslk, yac, y, slack
 
 
        double precision absb, absc, aij, baxsl, err, relerr, yi
 
 9000 format (1h0,'UNACCEPTABLE ERROR OF ',f16.8,
     1            ' FOUND IN B-SLACK-AX-OF CONSTRAINT',i6)

 9004 format (1h0,'UNACCEPTABLE RELATIVE ERROR OF ',f16.8,
     1            ' FOUND IN B-SLACK-AX OF CONSTRAINT',i6 
     2            /1h ,'THE ABSOLUTE ERROR IS ',f16.8,
     3            ' AND B(I) IS ',f16.8)

 9008 format (1h0,'UNACCEPTABLE ERROR OF ',f16.8,
     1            ' FOUND IN YA-C OF BASIC VARIABLE ',i6)

 9012 format (1h0,'UNACCEPTABLE RELATIVE ERROR OF ',f16.8,
     1            ' FOUND IN YA-C OF BASIC VARIABLE, ',i6
     2            /1h ,'THE ABSOLUTE ERROR IS ',f16.8,
     3            ' AND C(J) IS ',f16.8)
 
      if (numslk.eq.0) go to 10
      do 5 k = 1,size
         if (xbasis(k).le.n) go to 5
         i = xbasis(k) - n
         slack(i) = xr(k)
    5 continue

   10 do 20 j = 1,n
         if (inbase(j).le.0) go to 20
         yac(j) = -c(j)
   20 continue

      tol2 = tol(2)
      tol6 = tol(6)
      do 40 i = 1,mnow
         iseffi = iseff(i)
         yi = y(i)
         baxsl = b(i) - slack(i)
         istart = irow(i)
         last = irow(i+1) - 1
         do 30 look = istart,last
            j = jcol(look)
            inj = inbase(j)
            if (inj.eq.0) go to 30
            aij = aa(look)
            baxsl = baxsl - x(j) * aij
            if (inj.gt.0.and.iseffi.ne.0) yac(j) = yac(j) + yi * aij
   30    continue
         err = abs(baxsl)
         if (err.gt.tol2) go to 60
         absb = abs(b(i))
         if (absb.lt.1.0) absb = 1.0
         if (err / absb .gt. tol6) go to 65
   40 continue

      tol7 = tol(7)
      tol4 = tol(4)
      do 50 j = 1,n
         if (inbase(j) .le. 0) go to 50
         err = abs(yac(j))
         if (err .gt. tol4) go to 70
         absc = abs(c(j))
         if (absc.lt.1.0) absc = 1.0
         if (err / absc .gt. tol7) go to 75
   50 continue
      go to 90

   60 write (dbug,9000) err,i
      go to 80

   65 relerr = err / absb
      write (dbug,9004) relerr,i,err,absb
      go to 80

   70 write (dbug,9008) err,j
      go to 80

   75 relerr = err / absc
      write (dbug,9012) relerr,j,err,absc

   80 istate = 7
   90 return
      end
