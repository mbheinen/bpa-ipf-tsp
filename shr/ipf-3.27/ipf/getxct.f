C    @(#)getxct.f	20.6 8/19/99
      subroutine getxct (jt, kt, mt, nt, vk, qk, ityp, dvdq)

C     Convert BX buses from state PV into state PQ_discrete.

      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/alpha2.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/tbx.inc'
      include 'ipfinc/xdata.inc'
      include 'ipfinc/xblim.inc'
 
      userek = xdata(5,nt)
      usecap = xdata(6,nt)
      vsqr = vk ** 2
      qmin_gen = tbx(4,jt) + qloadu(kt)
      qmax_gen = tbx(3,jt) + qloadu(kt)
      qmax = -qloadu(kt) - amin1 (0.0, userek * vsqr / bmva)
      qmin = -qloadu(kt) - amax1 (0.0, usecap * vsqr / bmva)
      qneut = qmin - vsqr * userek / bmva

      totrek = userek
      totcap = usecap
      isw = 0

      q1 = qneut
      q0 = qneut
      if (qk .lt. qneut .and. userek .lt. 0.0) then
        q3 = tbx(3,jt)
        isw = 1
      else if (qk .gt. qneut .and. usecap .gt. 0.0) then
        q3 = tbx(4,jt)
        isw = 2
      else if (userek .lt. 0.0) then
        q3 = tbx(3,jt)
        isw = 1
      else 
        q3 = tbx(4,jt)
        isw = 2
      endif

      do k = 7, 21, 2
        n = xdata(k,nt)
        if (n .eq. 0) go to 160
        if ((isw .eq. 1) .and. (xdata(k+1,nt) .gt. 0.0)) go to 150
        if ((isw .eq. 2) .and. (xdata(k+1,nt) .lt. 0.0)) go to 150
        do l = 1, n
          q2 = q1
          q1 = q1 + xdata(k+1,nt) * vsqr / bmva
          if (isw .eq. 2) then
            if (qk - qmax_gen .lt. q1) go to 170
          else
            if (qk - qmin_gen .gt. q1) go to 170
          endif
        enddo
  150   continue
      enddo

  160 if (isw .eq. 2 .and. abs(qk - qmax_gen - q1) .gt. option(7)) then
        totrek = 0.0
        go to 180
      else if (isw .eq. 1 .and. abs(qk - qmin_gen - q1) .gt. option(7))
     &   then
        totcap = 0.0
        go to 180
      endif

  170 continue
      qnew = bestep (kt, mt, nt, qk, q1, q2, dvdq)
      used = (qnew - q0) * bmva / vsqr
 
      if (isw .eq. 2) then
        totrek = 0
        totcap = used
      else
        totrek = used
        totcap = 0
      endif

  180 xdata(5,nt) = totrek
      xdata(6,nt) = totcap
      bkku(kt) = bkku(kt) + (totrek + totcap - userek - usecap) / bmva
      qnetu(kt) = dmin1 (tbx(3,jt), dmax1 (tbx(4,jt), dble(q3)))
c
c     Initialize xblim(1,*), xvlim(1,*) as largest B where Vk < Vmin
c     Initialize xblim(2,*), xvlim(2,*) as smallest B where Vk > Vmax
c
      bnew = totrek + totcap
      if (bnew .lt. xdata(4,nt)) then
        if (vk .lt. vlimn(kt)) then
          if (xvlim(1,nt) .eq. 0.0) then
            xblim(1,nt) = bnew
            xvlim(1,nt) = vk
          else if (vk .gt. xvlim(1,nt)) then
            xblim(1,nt) = bnew
            xvlim(1,nt) = vk
          endif
        else if (bnew+0.01 .lt. xblim(1,nt)) then
          if (xvlim(1,nt) .eq. 0.0) then
            xblim(1,nt) = bnew
            xvlim(1,nt) = vk
          else if (xvlim(1,nt) .gt. vlimn(kt) .and.
     &             vk .lt. xvlim(1,nt)) then
            xblim(1,nt) = bnew
            xvlim(1,nt) = vk
          else if (vk .gt. xvlim(1,nt)) then
            xblim(1,nt) = bnew
            xvlim(1,nt) = vk
          endif
        else if (abs(bnew-xblim(1,nt)) .lt. 0.01) then
          xvlim(1,nt) = vk
        endif
      endif
      if (bnew .gt. xdata(3,nt)) then
        if (vk .gt. vlimx(kt)) then
          if (xvlim(2,nt) .eq. 0.0) then
            xblim(2,nt) = bnew
            xvlim(2,nt) = vk
          else if (vk .lt. xvlim(2,nt)) then
            xblim(2,nt) = bnew
            xvlim(2,nt) = vk
          endif
        else if (bnew-0.01 .gt. xblim(2,nt)) then
          if (xvlim(2,nt) .eq. 0.0) then
            xblim(2,nt) = bnew
            xvlim(2,nt) = vk
          else if (xvlim(2,nt) .lt. vlimx(kt) .and.
     &             vk .gt. xvlim(2,nt)) then
            xblim(2,nt) = bnew
            xvlim(2,nt) = vk
          else if (vk .lt. xvlim(2,nt)) then
            xblim(2,nt) = bnew
            xvlim(2,nt) = vk
          endif
        else if (abs(bnew-xblim(2,nt)) .lt. 0.01) then
          xvlim(2,nt) = vk
        endif
      endif
C                                                                      *
      if (idswb .ne. 0) then
        write (dbug,200) jt, kt, userek, usecap,
     &                   totrek, totcap
  200   format (' GETXCT/ Type X ', 2i6, ' X_initial ', 2f10.1,
     &          ' X_final ', 2f10.1)
      endif
      return
      end
