C    @(#)firstb.f	20.3 2/13/96
      subroutine firstb
 
      include 'ipfinc/parametr.inc'
 
      include 'ipfinc/smallp.inc'

      double precision bb, ss
 
      do 10 j = 1,n
   10 inbase(j) = 0

      do 20 i = 2,mnow
   20 iseff(i) = 0

      driver = 0.0
      neginv = 0
      ss = s(1)
      bb = b(1)
      if (ss .eq. 1.0 .and. bb .ge. 0.0 .or. ss .eq. -1.0 .and. 
     &    bb .le. 0.0 .or. ss .eq. 0.0 .and. bb .eq. 0.0) go to 30
      neginv = 1
      driver = 1.0
      if (bb.gt.0.0) driver = -1.0
   30 size = 1
      size1 = 2
      newx = n + 1
      xbasis(1) = n + 1
      inv(1,1) = 1.0
      xr(1) = bb
      obj = 0.0
      yr(1) = 0.0
      ybasis(1) = 1
      iseff(1) = 1
      numslk = 1
      marki = 1
      markk = 1
      itr = itr + 1
      inrev = 1
      return
      end
