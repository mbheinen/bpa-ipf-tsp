C    %W% %G%
      real function tzlag (kk,t1,dt,x0,x1,y0,ndivs)

      implicit none
      real kk,t1,dt,x0,x1,y0
      integer ndivs

c     -  solves a first order lag block:
c              kk
c            -------
c            1 + sT1
c
c     -   Sub-time step logic is used if ndivs > 1
c
c     -   subscript 0 means value at start of interval
c             "     1  "      "   "   end  "     "
c     -   x is input var
c         y is output var
c 
      real aa,bb,xa,xb,ya,yb,y1
      real rdivs,r0wt,r1wt,dts
      integer ia,ib,la

c     -     begin     begin     begin     begin     begin     begin

      if (t1 .eq. 0.0) then
        tzlag = kk * x1
        return
      endif
      if (dt .eq. 0.0) then
        tzlag = y0
        return
      endif
      if (ndivs .le. 1) then
        aa = dt / 2.0 / t1
        bb = kk * (x0 + x1)
        tzlag = ( y0 - aa * y0 + aa * bb) / (aa + 1.0)
        return
      endif

c     /*  -  here if using sub-time steps  */

      rdivs = ndivs
      r0wt = rdivs
      r1wt = 0.0
      dts = dt / rdivs
      xa = x0
      xb = x1
      ya = y0
      aa = dts / 2.0 / t1
      do la = 1,ndivs
        r0wt = r0wt - 1.0
        r1wt = r1wt + 1.0
        xb = ((r0wt * x0) + (r1wt * x1)) / rdivs
        bb = kk * (xa + xb)
        yb =  ( ya - aa * ya + aa * bb) / (aa + 1.0)
        ya = yb
        xa = xb
      enddo
      tzlag = yb
c     -
      return
      end
