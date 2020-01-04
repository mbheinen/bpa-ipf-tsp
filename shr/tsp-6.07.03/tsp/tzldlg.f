C    %W% %G%
      real function tzldlg (t2,t1,dt,x0,x1,y0,ndivs)
      implicit none
      real t2,t1,dt,x0,x1,y0
      integer ndivs

c     -  solves a first order lead block:
c            1 + sT2
c            -------
c            1 + sT1
c
c     -  Time step subdivision used if ndivs > 1
c
c     -   subscript 0 means value at start of interval
c             "     1  "      "   "   end  "     "
c     -   x is input var
c         y is output var
c 
      real aa,bb,v0,v1,w0,w1
      real rdivs,r0wt,r1wt,va,vb,wa,wb,dts
      integer la

c     -     begin     begin     begin     begin     begin     begin

      if (t1 .eq. 0.0) then
        if (t2 .eq. 0.0) then
          tzldlg = x1
          return
        else
          write (*,'(a)')
     +      ' TZLDLG [f] Lead-lag block has denom time constant = 0.0.'
          stop
        endif
      endif
      if (dt .eq. 0.0) then
        tzldlg = y0 + ((x1 - x0) * t2 / t1)
        return
      endif
c     -  No time step subdivisions
      if (ndivs .le. 1) then
        aa = dt / 2.0 / t1
        bb = ( 1.0 - (t2 / t1)) * (x0 + x1)
        v0 = y0 - (x0 * t2 / t1)
        v1 = ( v0 - aa * v0 + aa * bb) / (aa + 1.0)
        tzldlg = (x1 * t2 / t1) + v1
        return
      endif
c     -  Using time step subdivisions.  Find v1 = F (w0,w1,v0)
      rdivs = ndivs
      r0wt = rdivs
      r1wt = 0.0
      dts = dt / rdivs
      aa = dts / 2.0 / t1
      w0 = x0 * (1.0 - (t2 / t1))
      w1 = x1 * (1.0 - (t2 / t1))
      wa = w0
      va = y0 - (x0 * t2 / t1)
      do la = 1,ndivs
        r0wt = r0wt - 1.0
        r1wt = r1wt + 1.0
        wb = ((r0wt * w0) + (r1wt * w1)) / rdivs
        bb = wa + wb
        vb = ( va - aa * va + aa * bb) / (aa + 1.0)
        wa = wb
        va = vb
      enddo
      tzldlg = (x1 * t2 / t1) + vb
c 
      return
      end
