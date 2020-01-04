C    %W% %G%
      subroutine cvsol(icv, base, xi, xo)

      implicit none

      include 'tspinc/params.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/cv.inc'

      integer icv, idat, i
      real    xi, xo, xipu, xopu, base

c     abort if icv .eq. 0
      if (icv.eq.0) then
        xo = xi
        return
      endif

c     process hardcoded curves (icv < 0)
      if (icv.lt.0) then

        idat = abs(icv) 

        xipu = xi/base

        if (xipu.le.inpt(idat,1)) then
          xopu = outpt(idat,1)
        else if (xipu.ge.inpt(idat,totalpts(idat))) then
          xopu = outpt(idat,totalpts(idat))
        else
          do i = 2, totalpts(idat)
            if (xipu.lt.inpt(idat,i)) then
c             interpolate between i-1 and i
              xopu =   outpt(idat,i-1) + (xipu - inpt(idat,i-1))
     &                               *(outpt(idat,i) - outpt(idat,i-1))
     &                               /(inpt(idat,i)  - inpt(idat,i-1) )
              xo = xopu*base
              return
            endif
          enddo
        endif
        xo = xopu*base
        return
      endif

c     get index to cvin and cvout arrays
      idat = cvindx(icv)

c     compute output
      if (xi.le.cvin(idat+1)) then
        xo = cvout(idat+1)
      else if (xi.ge.cvin(idat+cvpts(icv))) then
        xo = cvout(idat+cvpts(icv))
      else
        do i = 2, cvpts(icv)
          if (xi.lt.cvin(idat+i)) then
c           interpolate between i-1 and i
            xo = cvout(idat+i-1) + (xi - cvin(idat+i-1) )
     &                            * ( cvout(idat+i) - cvout(idat+i-1) )
     &                            / ( cvin(idat +i) - cvin(idat+ i-1) )
            return
          endif
        enddo
      endif

      end

