C    %W% %G%
      subroutine cvint(cvrefnum, icv, xi, xo, pmax)

      implicit none

      include 'tspinc/params.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/cv.inc'

      integer iref, icv, i, j, idat
      real cvrefnum, xi, xo, pmax, xipu, xopu

      include 'tspinc/cvdata.inc'

c     search through curves to find cvrefnum.  If not found, write
c     warning and use output=input curve (icv=0).

      iref = int(cvrefnum+0.1)

c     assign negative icv numbers for hardcoded curves.  See cvdata.inc. 
      if (iref.gt.100) then
        icv = 100 - iref
      endif
        
c     process hardcoded curves (icv < 0)
c     note:  calculate backward to get input from output
      if (icv.lt.0) then

        idat = abs(icv) 

        xipu = xi/pmax

        if (xipu.le.outpt(idat,1)) then
          xopu = inpt(idat,1)
        else if (xipu.ge.outpt(idat,totalpts(idat))) then
          xopu = inpt(idat,totalpts(idat))
        else
          do i = 2, totalpts(idat)
            if (xipu.lt.outpt(idat,i)) then
c             interpolate between i-1 and i
              xopu =   inpt(idat,i-1) + (xipu - outpt(idat,i-1))
     &                               *(inpt(idat,i) - inpt(idat,i-1))
     &                               /(outpt(idat,i)  - outpt(idat,i-1))
              xo = xopu*pmax
              return
            endif
          enddo
        endif
        xo = xopu*pmax
        return
      endif

      do icv = 1, cvtot
        if (cvrefnum.eq.cvref(icv)) goto 100
      enddo

      icv = 0

100   continue

c     abort if icv .eq. 0
      if (icv.eq.0) then
        xo = xi
        return
      endif

      idat = cvindx(icv)

c     scale points by pmax/bmva
      do i = 1, cvpts(icv)
        cvin( idat + i ) = cvin( idat + i )*pmax
        cvout(idat + i ) = cvout(idat + i )*pmax
      enddo

c     start computation of output

      if (xi.le.cvout(idat+1)) then
        xo = cvout(idat+1)
      else if (xi.ge.cvout(idat+cvpts(icv))) then
        xo = cvout(idat+cvpts(icv))
      else
        do i = 2, cvpts(icv)
          if (xi.lt.cvout(idat+i)) then
c           interpolate between i-1 and i
            xo = cvin(idat+i-1) + (xi - cvout(idat+i-1) )
     &                            * ( cvin(idat+i) - cvin(idat+i-1) )
     &                            / ( cvout(idat+i)  - cvout(idat+i-1))
          endif
        enddo
      endif
      return
      end

