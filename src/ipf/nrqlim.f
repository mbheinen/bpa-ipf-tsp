C    @(#)nrqlim.f	20.18 8/19/99
      subroutine nrqlim
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/alpha2.inc'
      include 'ipfinc/beta2.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/bxlock.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/intbus.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/lndpcp.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/slnphs.inc'
      include 'ipfinc/svc.inc'
      include 'ipfinc/tbx.inc'
      include 'ipfinc/tbxsrt.inc'
      include 'ipfinc/trmdbg.inc'
      include 'ipfinc/xblim.inc'
      include 'ipfinc/xdata.inc'
 
      character flag * 1
      real nrdvdq, temp_bkku, cv(2), ci(2), cz(2)
      integer getsvc, firstxstr
      logical skip_nrdvdq, update_xvrlim, bx_with_qlimits
      external getsvc, nrdvdq   

C     DQV(V) estimates DQ/DV around operating point   

      dqv(v) = amax1 (0.100, 0.250*abs (qk/v -sngl(bkku(kt))*v))

C     QTOL(V) defines the excursion tolerance 
C     threshold in which the bus can restore to state PV. 

      qtol(q2,q1) = amin1 (option(8), 0.020 * (q2 - q1 ))  

C     Process special bus types   
C     BUS TYPE - LTYP  /--------------- ITYP -------------------/   
C                       1      2      3      4      5      6  
C     BV - 1         QSCH   QSCH   VMIN   VMAX   
C     BQ - 2         VSCH   VSCH   QMIN   QMAX   
C     BG - 3         VGEN   QMIN   QMAX   Q%     VMIN   VMAX 
C     BO - 4         VOPT   VOPT   QMIN   QMAX   
C     BX - 5         VGEN   QMIN   QMAX   QDISC  VMIN   VMAX 
C     BF - 6         V      QNET 

      numbf = 0 
      qerrbg = 0.0  
      numbg = 0 
      qerrbf = 0.0  
      skip_nrdvdq = (msw .eq. 3 .and. 
     &               firstxstr(buf(2:), 'CHANGE_PAR*') .gt. 0)

      do 1090 jt = 1,ntotb  
      ltyp=tbx(1,jt)   
      ityp=tbx(7,jt)   
      kt=tbx(2,jt) 
      mt=iabs(ifix(sngl(tbx(8,jt))))
      flag = ' '
      if (ltyp .ge. 10) then
         go to 1090 
      else if (ltyp .lt. 0) then
c
c        Freeze status quo one iteration
c
         ltyp = -ltyp   
         tbx(1,jt) = ltyp  
         go to 1090 
      else if (ityp .lt. 0) then
c
c        Automatic control trouble: freeze to manual control one
c        iteration.
c
         itypo = ityp   
         tbx(7,jt) = -ityp 
         if (ltyp .eq. 3 .or. ltyp .eq. 5) then 
            if (kvolt(kt) .eq. mt .and. mt .gt. 0) then 
               kvolt(kt) = kt   
            endif   
            volt(kt) = 0.0  
         endif  
         go to 1060 
      endif 
        
      if ((ltyp .eq. 3 .or. ltyp .eq. 5) .and. (ityp .gt. 4)) then  
         ityp = 1   
         tbx(7,jt) = ityp  
      endif 
      itypo=ityp
      qmin=tbx(4,jt)
      qmax=tbx(3,jt)
      suscp=tbx(6,jt)   
      ksw=0 
      call nrpqv (kt,pk,dpk,qk,dqk,vk)  
      vsqr = vk**2  
      qsuscp = suscp*vsqr   
        
      if (kvolt(kt) .ne. 0) dqk = 0.0   
C       
C     Override DPK if area slack bus
C       
      do 80 i = iflag(kt),iflag(kt+1)-1 
      if (jflag(1,i) .eq. 3) then   
         call nrarea (jflag(2,i),pk,dpk)
         go to 90   
      endif 
   80 continue  
   90 continue  
C       
C     Process special bus types according to type and state 
C       
      if (ltyp .eq. 1 .and. ityp .eq. 1) then   
         go to 660  
      else if (ltyp .eq. 6 .and. msw .eq. 2) then   
         tbxsrt(jt) = 0 
C       
C        Bus must converge sufficiently before reactive 
C        limits are checked.
C       
         if (abs(dpk).gt.option(4)) then
            go to 1090  
         endif  
      else  
C       
C        Bus must converge sufficiently before reactive 
C        limits are checked.
C       
         if (abs(dpk).gt.option(4)) then
            go to 1090  
         endif  
      endif 
        
      if (ltbxsl(1,jt) .gt. 0 .and. skip_nrdvdq) then 
C       
C        If case is a /CHANGE_PARAMETER study, disable for this 
C        iteration any sensitivity approximation dV/dQ computed in  
C        NRDVDQ. The perturbation may yield false sensitivities.
C       
         ltbxsl(1,jt) = 0   
        
      endif 
        
      go to (610,620,810,630,810,600) ltyp  
C       
C     Process type BF   
C       
  600 dvdq = nrdvdq (jt, ittot, kt, vk, qk, dqv(vk), dvdqap)
        
      if (msw .eq. 1)then   
         if (ityp .eq. 1) then  
            dqk = tbx(5,jt) - qk
            if (abs (dqk) .le. 100.0 * option(4)) then   
C       
C              BF bus in state V with reasonable convergence:   
C              change to load bus   
C       
               ityp=2   
               kvolt(kt) = 0
               qnetu(kt) = tbx(5,jt)   
               dqk = qnetu(kt) - qk                 
               go to 1060   
            else
C       
C              BF bus in state V with near convergence:   
C              perturb voltages in hopes of obtaining closer
C              convergence.
C       
               qerrbf = qerrbf + abs (dqk)  
               numbf = numbf + 1
               tbxsrt(jt) = tbxsrt(jt) + 1  
               dvk = dqk * dvdq   
               if (abs (dvk) .gt. 0.050) dvk = sign (0.050,dvk) 
               if (abs (dvk) .gt. option(24)) then  
                  kvolt(kt)=kt  
                  volt(kt)=dvk/vk   
                  if (abs (dvk) .gt. 0.005) then   
                     kowntb = kowntb + 1
                     flag = '$' 
                  endif 
               else
C       
C                 BF bus in state V with reasonable convergence:   
C                 change to load bus   
C       
                  ityp=2   
                  kvolt(kt) = 0
                  qnetu(kt) = tbx(5,jt)   
                  dqk = qnetu(kt) - qk                 
               endif
               go to 1060   
            endif   
         else   
            go to 1090  
         endif  
      else if (msw .ge. 2) then 
         if (ityp .eq. 1) then  
            tbxsrt(jt) = tbxsrt(jt) + 1 
            dqk = tbx(5,jt) - qk
            if (abs (dqk) .le. 100.0 * option(7)) then   
C       
C              BF bus in state V with reasonable convergence:   
C              change to load bus   
C       
               ityp=2   
               kvolt(kt) = 0
               qnetu(kt) = tbx(5,jt)   
               dqk = qnetu(kt) -qk                 
               go to 1060   
            else if (dim (vk, qmax) .le. option(24) .and.  
     1               dim (qmin, vk) .le. option(24)) then 
C       
C              BF bus in state V with reasonable convergence:   
C              change to load bus   
C       
               ityp=2   
               kvolt(kt) = 0
               qnetu(kt) = tbx(5,jt)   
               dqk = qnetu(kt) - qk                 
               go to 1060   
            else
               qerrbf = qerrbf + abs (dqk)  
               numbf = numbf + 1
               dvk = dqk * dvdq 
        
               if (abs (dvk) .gt. 0.050) dvk = sign (0.050,dvk) 
               if (abs (dvk) .gt. option(24)) then  
                  kvolt(kt)=kt  
                  volt(kt)=dvk/vk   
                  if (abs (dvk) .gt. 0.005) then   
                     kowntb = kowntb + 1
                     flag = '$' 
                  else if (abs (dqk) .gt. 5.0 * option(7)) then 
                     kowntb = kowntb + 1
                     flag = '$' 
                  endif 
               else 
C       
C                 BF bus in state V with reasonable convergence:
C                 change to load bus
C       
                  ityp=2
                  kvolt(kt) = 0 
                  qnetu(kt) = tbx(5,jt)
                  dqk = qnetu(kt) -qk              
                  if (abs (dqk) .gt. 5.0 * option(7)) then  
                     kowntb = kowntb + 1
                     flag = '$' 
                  endif 
               endif
               go to 1060   
            endif   
         else   
            go to 1090  
         endif  
      else  
         go to 1090 
      endif 
C       
C     Process type BV   
C       
  610 dvdq = nrdvdq (jt, ittot, kt, vk, qk, dqv(vk), dvdqap)
        
      go to (660,660,800,800,1090,1090), ityp   
C       
C     Process type BQ   
C       
  620 go to (750,640,700,700,1090,1090), ityp   
C       
C     Process type BO   
C       
  630 go to (750,1090,700,700,1090,1090), ityp  
C       
C     Temporary LTC supression of bus switching 
C       
  640 ityp=1
      go to 1060
C       
C     BV bus in state Q -- check V limits   
C       
  660 dvk = -dim(vk,vlimx(kt)) + dim(vlimn(kt),vk)  
      if (abs(dvk) .le. 0.002) go to 1090  
      if (abs(dqk) .gt. option(7)) go to 1090   
      deltaq = 1.0 / dvdq * dvk 
      if(abs(deltaq) .lt. qtol(qmax,qmin)) go to 1090   
      if (dvk.gt.0) then
        vnew=vlimn(kt)                            
        ityp=3  
      else  
        vnew=vlimx(kt)                            
        ityp=4  
      endif 
C       
C     BV bus in state Q -- set to V-limit   
C       
      dvk=vnew-vk   
      ddtot = ddtot + abs(volt(kt)) 
      kvolt(kt)=kt  
      volt(kt)=dvk/vk   
      go to 1060
C       
C     BQ or BO bus in state Q -- test for restoration.  
C       
  700 dvdq = nrdvdq (jt, ittot, kt, vk, qk, dqv(vk), dvdqap)
        
      dvk=tbx(5,jt)-vk  
      if (abs(dvk) .le. 0.002) go to 1090  
      if (abs(dqk).gt.option(7)) go to 1090 
      deltaq = 1.0 / dvdq * dvk 
      if (qmax - qmin .lt. 0.01) go to 1090
      if (abs(deltaq) .lt. qtol(qmax,qmin)) go to 1090  
C       
C     Subject restoration to dV/dQ > 0  
C       
      if (msw .gt. 1) then  
         if (dvdqap .lt. 0.0) then  
            write (errbuf(1), 701) intbus(kt), intbas(kt), ltyp,
     1         ityp, vk, vlimn(kt), vlimx(kt), dvdq, dvdqap 
  701       format ('Bus restoration deferred: ', a8, f7.1, 
     1         ' type ', 2i2, ' V ', f6.3, ' (', f6.3,',',  
     2         f6.3, ') dQ/dV ', 2e11.4)
            call prterx ('W', 1)
            go to 1090  
         endif  
      endif 
C       
C     Subject restoration to convergence of any radial nodes.   
C       
      if (kmlen(kt) .eq. 1)   then                
         mt = ikmu(km(kt))                 
         call nrpqv (mt,pm,dpm,qm,dqm,vm) 
         if (kvolt(mt) .ne. 0) dqm = 0.0  
         if (abs (dpm) .gt. option(4)) go to 1090 
         if (kvolt(mt) .eq. 0) then   
            if (abs (dqm) .gt. 2.0 * option(7)) go to 1090  
         endif
      endif 
      if (ityp.eq.3) then   
          if (dqk+deltaq.lt.0.0) go to 1090 
          bkku(kt) = bkku(kt)+amax1(0.0,suscp)
      else  
          if (dqk+deltaq.gt.0.0) go to 1090 
          bkku(kt) = bkku(kt)+amin1(0.0,suscp)
      endif 
      vnew=tbx(5,jt)
      ityp=1
C       
C     BQ or BO bus in state Q -- restore to state V 
C       
      dvk=vnew-vk   
      ddtot = ddtot + abs(dvk)  
      kvolt(kt)=kt  
      volt(kt)=dvk/vk   
      go to 1060
C       
C     BQ or BO bus in state V -- check Q limits 
C       
C        Check if voltage is not at scheduled V.  This anachronism may
C        occur if relaxation occurs in NRBKSL immediately following 
C        a previous restoration from PQ-limit to PV-scheduled.  
C       
  750 dvk = -dim(vk,vlimx(kt)) + dim(vlimn(kt),vk)  
      if (abs(dvk) .gt. option(24)) then  
         volt(kt) = dvk/vk  
         if (abs(dvk) .gt. 0.005) then   
            flag = '$'  
            kowntb = kowntb + 1 
         endif  
         go to 1060 
      endif 
      dqk=-dim(qk,qmax)+dim(qmin,qk)
      if (dqk.gt.option(7)) then
         dvdq = nrdvdq (jt, ittot, kt, vk, qk, dqv(vk), dvdqap) 
         ityp=3 
         qnetu(kt) = qmin  
         if (suscp.gt.0.0) then 
            bkku(kt) = bkku(kt)-suscp 
            qnetu(kt) = qnetu(kt)+qsuscp  
         endif  
      else if (dqk.lt.-option(7)) then  
         dvdq = nrdvdq (jt, ittot, kt, vk, qk, dqv(vk), dvdqap) 
         ityp=4 
         qnetu(kt) = qmax  
         if (suscp.lt.0.0) then 
            bkku(kt) = bkku(kt)-suscp 
            qnetu(kt) = qnetu(kt)+qsuscp  
         endif  
      else  
         go to 1090 
      endif 
      kvolt(kt) = 0 
      go to 1060
C       
C     BV bus in state V -- test for restoration 
C       
  800 dqk = tbx(5,jt) - qk  
C       
C        Check if voltage is not at scheduled V.  This anachronism may
C        occur if relaxation occurs in NRBKSL immediately following 
C        a previous restoration from PQ-limit to PV-scheduled.  
C       
         dvk = -dim(vk,vlimx(kt)) + dim(vlimn(kt),vk)   
         if (abs(dvk) .gt. option(24)) then   
            volt(kt) = dvk/vk   
            if (abs(dvk) .gt. 0.005) then  
               kowntb = kowntb + 1  
               flag = '$'   
            endif   
            go to 1060  
         else if (abs(dqk).gt.option(7)) then   
            if ((dqk.lt.0.0).and.(ityp.eq.3)) go to 1090
            if ((dqk.gt.0.0).and.(ityp.eq.4)) go to 1090
            qnetu(kt) = tbx(5,jt)  
            ityp=1  
            kvolt(kt) = 0   
            go to 1060  
         else   
            go to 1090  
         endif  
C       
C     Process BG or BX busses   
C       
  810 lindrp = 0
      jxlock = 0
      isvc = 0  
      update_xvrlim = .false.

      if (ltyp.eq.3) then   
         qmax=qmax-amin1(0.0,qsuscp/bmva)   
         qmin=qmin-amax1(0.0,qsuscp/bmva)   
         dvdq = nrdvdq (jt, ittot, kt, vk, qk, dqv(vk), dvdqap) 
C       
C        LINDRP is an index to LINE_DROP_COMPENSATION. This is  
C        similar to remote voltage control, except the controlled point
C        X% between the bus and its remote bus.  The remote bus is  
C        screened from all normal processing.  It can be found only in
C        the array LNDPCP, which is created as an option in 
C        /CHANGE_BUS_TYPES in module HOTCHG.
C       
         do 811 i = iflag(kt),iflag(kt+1)-1 
            if (jflag(1,i) .eq. 14) then
               lindrp = jflag(2,i)  
               go to 812
            endif   
  811    continue   
  812    continue   
C       
C        ISVC is an index to SVC. This bus is an intercepted type BG bus
C        created with /SOLUTION ... > SVC commands.  As such, the bus is
C        always in state 1.  Its true state is stored in the SVC array.
C       
         do 813 i = iflag(kt),iflag(kt+1)-1 
            if (jflag(1,i) .eq. 15) then
               isvc = jflag(2,i)
               go to 814
            endif   
  813    continue   
  814    continue   
        
      else  

         nt = tbx(5,jt)
         userek=xdata(5,nt) 
         usecap=xdata(6,nt) 
         qmax_old = qmax
         qmin_old = qmin
         qmax = qmax - amin1 (0.0,userek*vsqr/bmva) 
         qmin = qmin - amax1 (0.0,usecap*vsqr/bmva) 
         dvdq = nrdvdq (jt, ittot, kt, vk, qk, 1.0/xsen(nt), dvdqap)
         dvk = -dim(vk,vlimx(kt)) + dim(vlimn(kt),vk)  
         if (xblim(1,nt) .eq. -9999.0 .and. kvolt(kt) .eq. 0) then 
            if (xdata(5,nt) + xdata(6,nt) .lt. xdata(4,nt)) then
               xblim(1,nt) = xdata(5,nt) + xdata(6,nt)
               xvlim(1,nt) = vk
               update_xvrlim = .true.
            endif
         endif  
         if (xblim(2,nt) .eq. -9999.0 .and. kvolt(kt) .eq. 0) then 
            if (xdata(5,nt) + xdata(6,nt) .gt. xdata(3,nt)) then
               xblim(2,nt) = xdata(5,nt) + xdata(6,nt)
               xvlim(2,nt) = vk
               update_xvrlim = .true.
            endif
         endif  
C       
C        Define voltage deadband if bus is flagged JXLOCK.  
C       
         do 816 i = 1, numlck   
            if (inp2opt(nxlock(i)) .eq. kt) then  
               jxlock = i   
               go to 818
            endif   
  816    continue   
  818    continue   
      endif 
        
      dqk=-dim(qk,qmax)+dim(qmin,qk)
      if (jxlock .ne. 0) then   
         dvk=-dim(vk,vxlock(2,jxlock))+dim(vxlock(1,jxlock),vk) 
      else if (isvc .ne. 0) then
         dvk=-ddim(dble(vk),svc(10,isvc))+ddim(svc(9,isvc),dble(vk))  
      else  
         dvk=-dim(vk,vlimx(kt)) + dim(vlimn(kt),vk)   
      endif 
      if (mt .eq. 0 .or. mt .eq. kt) then   
         if (lindrp .gt. 0) then
            if (lndp_type(lindrp) .eq. 1) then
c
c             Process Line Drop Compensation
c
              mx = inp2opt(lndpcp(2,lindrp))
              pct = drppct(lindrp)
              call nrpqv (mx,pm,dpm,qm,dqm,vm)
              if (kvolt(mx) .ne. 0) dqm = 0.0 
              dvm = 0.0   
              vx = (1.0 - drppct(lindrp)) * vk + drppct(lindrp) * vm  
              dv_ldc = -dim(vx,vmax_ldc(lindrp)) 
     &               + dim(vmin_ldc(lindrp),vx)
              if (kvolt(mx) .ne. 0) then  
                 dv_ldc = 0.0
              else
                 if (abs(dpm) .gt. option(4)) go to 1090  
                 if (abs(dqm) .gt. option(4)) go to 1090  
              endif   
            else
              pct = drppct(lindrp)
              cz(1) = xc_ldc(lindrp)
              ci(1) = qk / vk
              cv(1) = vk - ci(1) * cz(1)
              vx = cv(1)
              dv_ldc = -dim(vx,vmax_ldc(lindrp)) 
     &               + dim(vmin_ldc(lindrp),vx)
              if (abs(dpm) .gt. option(4)) go to 1090  
              if (abs(dqm) .gt. option(4)) go to 1090  
            endif
         else if (isvc .gt. 0) then 
            dvm = dvk   
         else   
            dvm = 0.0   
         endif  
      else  
         call nrpqv (mt,pm,dpm,qm,dqm,vm)   
         if (kvolt(mt) .ne. 0) dqm = 0.0
         dvm = -dim(vm,vlimx(mt)) + dim(vlimn(mt),vm)   
         if (kvolt(mt) .ne. 0) then 
         else if (kvolt(kt) .eq. mt) then   
            if (abs(dpm) .gt. option(4)) go to 1090 
            if (kvolt(mt) .eq. 0) then  
               if (abs(dqm) .gt. option(7)) go to 1090  
            endif   
         endif  
         if (update_xvrlim) then
            xvrlim(1,nt) = vm
            xvrlim(2,nt) = vm
         endif
      endif 
      if (ltyp .eq. 3) then 
C       
C        Process SVC buses separately.  
C       
         if (isvc .gt. 0) go to 10820   
         go to (870,960,960,820,870,870) ityp   
      else  
         go to (870,830,830,830,870,870) ityp   
      endif 
C       
C     SVC bus as a BG bus in state PV or PQ -- check V limits and   
C     Q limits. 
C       
c     getsvc is expecting single precision.
c
10820 temp_bkku = bkku(kt)
      ix = getsvc (kt, jt, isvc, vk, qk, dvdq, temp_bkku, msw)
      bkku(kt) = temp_bkku
      if (ix .gt. 0) then   
         go to 1060 
      else  
         go to 1090 
      endif 
C       
C     BG bus in state PQ% -- check Q limits 
C       
  820 if (dqk.gt.option(7)) then
         ityp=2 
         qnetu(kt) = tbx(4,jt) 
      else if (dqk.lt.-option(7)) then  
         ityp=3 
         qnetu(kt) = tbx(3,jt) 
      else  
         go to 1090 
      endif 
      kvolt(kt) = 0 
      go to 1060
C       
C     BX bus in state Q_discrete:   
C       
C     (a) X_BUS = BPA --> "Optimize" voltage.   
C     (b) X_BUS = WSCC -> Adjust steps only if voltage violations occur.
C     (c) X_BUS = VMAX -> Adjust steps to maximize voltage. 
C       
  830 if (abs (dqk) .gt. option(7)) go to 1090  
      if (mt .eq. 0 .or. mt .eq. kt) then   
      else  
         ddtot = ddtot + abs(dvm)   
         if (abs(dpm) .gt. option(4)) go to 1090
         if (kvolt(mt) .eq. 0) then 
            if (abs(dqm) .gt. option(7)) go to 1090 
         endif  
      endif 
C       
C     Find "optimal" discrete step B1 < USED < B2   
C       
      if (jxlock .eq. 0) then   
         call opstep (jt, kt, mt, nt, bold, bnew, dvdq) 
      else  
         call lkstep (jt, kt, mt, nt, bold, bnew, dvdq, 
     1                vxlock(1,jxlock), vxlock(2,jxlock))   
      endif 
      if (abs (bnew-bold) .le. option(7)) go to 1090
C       
C     If a state transition occurred, change from Q_min or Q_max to 
C     to ITYP = 4.  Otherwise, flag ITYPO = -ITYP to indicate no change,
C     but still trip KOWNTB counter later.  
C       
      if (ityp .eq. 4) then 
         itypo = -ityp  
      else  
         ityp = 4   
      endif 
      go to 1060
C       
C     BG or BX BUS in state V -- restore to self regulation 
C     and check Q limits.   
C       
  870 ityp = 1  
        
      if (ltyp .eq. 5) then
         bx_with_qlimits = (qmax_old - qmin_old .gt. 0.05)
      else
         bx_with_qlimits = .true.
      endif

      if (abs(dqk) .gt. option(7)) then 
C       
C        Perturb V in lieu of setting Q-limits. If the perturbation is 
C        insignificant, or MSW > 1, set Q-limits instead.  
C       
C        TBXSRT(*) is the count of V-perturbations.
C       
         if (msw .eq. 1 .and. tbxsrt(jt) .le. iopton(39) .and.  
     &       abs(dqk) .gt. 50.0 * option(7)) then 
            qerrbg = qerrbg + abs (dqk) 
            numbg = numbg + 1   
            tbxsrt(jt) = tbxsrt(jt) + 1 
c
c           Update dvdq if all data provided
c
            if (ltyp .eq. 5) then
              if (abs(xvlim(2,nt) - vk) .lt. 1.0e-6) then
                if (xblim(1,nt) .ne. -9999.0 .and.
     &              xblim(2,nt) .eq. -9999.0) then
                  xblim(2,nt) = qk
                  dv = xvlim(1,nt) - xvlim(2,nt)
                  dq = xblim(1,nt) - xblim(2,nt)
                  if (abs (dv) .gt. 0.001 .and.
     &                abs (dq) .gt. 0.100 .and.
     &                sign (1.0, dv) .eq. sign (1.0, dq)) then
                    dvdq = dv / dq
                    xsen(nt) = dvdq
                  endif
                endif
              endif
            endif
            dvk = dqk * dvdq
            vnew = vk + dvk 
            dvk = dvk - dim(vnew,vlimx(kt)) + dim(vlimn(kt),vnew)   
            if (abs (dvk) .gt. 0.050) dvk = sign (0.050,dvk)
            dqx = dvk / dvdq
C       
C           Attenuate V-adjustment if driven into Q-limits. 
C       
            if (abs (dqx) .gt. 2.0) dqx = sign (2.0, dqx)   
            qnew = qk + dqx 
            dqx = dim (qnew, qmax) - dim (qmin, qnew)   
            dvx = dqx * dvdq
C       
C           The sign convention DVK vs. DVX is opposite here because
C           DQK is interpreted as "corrective adjustment" instead of
C           Q_error.
C       
            if (dvx .ne. 0.0 .and.  
     1          abs(dvk) .gt. abs (dvx) .and.   
     2          sign (1.0,dvk) .ne. sign (1.0,dvx) .and.
     3          abs (dvk + dvx) .gt. option(24)) then
               if (sign(1.0, dvk) .ne. sign (1.0, dvx)) then
                  dvk = dvk + dvx  
               else
                  dvk = dvx
               endif
            endif   
            if (abs (dvk) .gt. option(24)) then 
               kvolt(kt)=kt 
               volt(kt)=dvk/vk  
               if (abs (dvk) .gt. 0.005) then  
                  kowntb = kowntb + 1   
                  flag = '$'
               endif
               if (abs(dvk) .gt. 0.001 .and. ltyp .eq. 5) then
                  xblim(1,nt) = qk
                  xvlim(1,nt) = vk
                  xblim(2,nt) = -9999.0
                  xvlim(2,nt) = vk + dvk
               endif                  
               if (iterm .eq. 1) then   
                  write (*,872) kt,vk,dvk,dqk,vlimn(kt),vlimx(kt)   
  872             format (' NRQLIM/ BG bus - defer Q-limits ',i5,   
     1               5e12.5)
               endif
               if (idswb .gt. 0) then   
                  write (dbug,872) kt,vk,dvk,dqk,vlimn(kt),vlimx(kt)
               endif
               go to 1060   
            else if (dqk .gt. 0) then   
C       
C              The V-perturbation is insignificant. Set Q_min limits.
C       
               ityp=2   
               qnetu(kt) = tbx(4,jt)   
               if (ltyp.eq.5) then  
                  bkku(kt) = bkku(kt) - usecap/bmva  
                  xdata(6,nt) = 0.0
                  xblim(1,nt) = -9999.0
                  xblim(2,nt) = -9999.0
               endif
               kvolt(kt) = 0
            else
C       
C              The V-perturbation is insignificant. Set Q_max limits.
C       
               ityp=3   
               qnetu(kt) = tbx(3,jt)   
               if (ltyp.eq.5) then  
                  bkku(kt) = bkku(kt) - userek/bmva  
                  xdata(5,nt) = 0.0
                  xblim(1,nt) = -9999.0
                  xblim(2,nt) = -9999.0
               endif
               kvolt(kt) = 0
            endif   
         else if (dqk .gt. 0.0) then
            ityp=2  
            qnetu(kt) = tbx(4,jt)  
            if (ltyp.eq.5) then 
               bkku(kt) = bkku(kt) - usecap/bmva 
               xdata(6,nt) = 0.0   
               xblim(1,nt) = -9999.0
               xblim(2,nt) = -9999.0
            endif   
            kvolt(kt) = 0   
         else   
            ityp=3  
            qnetu(kt) = tbx(3,jt)  
            if (ltyp.eq.5) then 
               bkku(kt) = bkku(kt) - userek/bmva 
               xdata(5,nt) = 0.0   
               xblim(1,nt) = -9999.0
               xblim(2,nt) = -9999.0
            endif   
            kvolt(kt) = 0   
         endif  
        
      else if (ltyp .eq. 5 .and. ityp .eq. 1 .and. 
     &         .not. bx_with_qlimits .and.
     &        (msw .ge. 2 .or. 
     &         vk .ge. vlimn(kt) .and. vk .le. vlimx(kt))) then
C       
C        BX bus in state PV -- go to nearest discrete state.
C       
         call getxct (jt, kt, mt, nt, vk, qk, ityp, dvdq)   
         ityp = 4   
         kvolt(kt) = 0  
         xvlim(1,nt) = 0.0
         xvlim(2,nt) = 0.0
         xblim(1,nt) = -9999.0
         xvlim(2,nt) = -9999.0
         go to 1060 
        
      else  
C       
C        Bus is within Q_limits. Disable Delta_V adjustment scheme  
C        if Q_limits are violated in future iterations: Force bus type
C        switching. 
C       
         tbxsrt(jt) = iopton(39) + 1
C       
C        Check if voltage is within V-limits. This anachronism may  
C        occur if relaxation occurs in NRBKSL immediately following 
C        a previous restoration from PQ-limit to PV-scheduled.  
C       
         if (abs(dvk) .gt. option(24)) then   
            kvolt(kt) = kt  
            volt(kt) = dvk/vk   
            if (abs(dvk) .gt. 0.005) then  
               kowntb = kowntb + 1  
               flag = '$'   
            endif   
            go to 1060  
         endif  
C       
C        BG or BX bus is in state PV; Establish Auto/Manual control 
C        subject to the following conditions:   
C       
C     1. Auto control if remote bus is adjacent controlled bus. 
C       
         if (mt .eq. 0 .or. mt .eq. kt) then
C       
C           Generator is self-regulating.  Use Manual control.  
C       
            if (dvk .eq. 0.0) then  
               if (lindrp .eq. 0) then  
                  dvk = amax1 (0.00501, 0.25*dvdq)
                  dvk = amin1 (dvk, vlimx(kt)-vk) 
               else 
                  vnew = vk + dv_ldc/vx*vk 
                  vnew = amax1(amin1(vnew, vlimx(kt)), vlimn(kt))
                  dvk = vnew - vk   
                  if (abs (dvk) .gt. 0.005) dvk = sign (0.005, dvk) 
                  if (iterm .eq. 1) then
                     write (*,873) kt, vk, dvk, vx, dv_ldc, vk, dvk,   
     &                  vlimn(kt),vlimx(kt),vmax_ldc(lindrp),
     &                  vmin_ldc(lindrp)
  873                format (' NRQLIM/ BG bus - LINE COMP ',i5, 
     1                  10f9.5) 
                  endif 
                  if (idswb .gt. 0) then
                     write (dbug,873) kt, vk, dvk, vx, dv_ldc, vk, dvk,
     &                  vlimn(kt),vlimx(kt),vmax_ldc(lindrp),
     &                  vmin_ldc(lindrp)
                  endif 
               endif
               if (abs (dvk) .gt. option(24)) then  
                  dqx = dvk / dvdq  
C       
C                 Attenuate V-adjustment if driven into Q-limits.   
C       
                  if (abs (dqx) .gt. 2.0) dqx = sign (2.0, dqx) 
                  qnew = qk + dqx   
                  dqx = dim (qnew, qmax) - dim (qmin, qnew) 
                  dvx = dqx * dvdq  
                  if (dvx .ne. 0.0 .and.
     1                abs(dvk) .gt. abs (dvx) .and. 
     2                sign (1.0,dvk) .eq. sign (1.0,dvx) .and.  
     3                abs (dvk - dvx) .gt. option(24)) then  
                     if (sign(1.0, dvk) .eq. sign (1.0, dvx)) then
                       dvk = dvk - dvx  
                     else
                       dvk = -dvx
                     endif
                  endif 
               endif
               ksw = 1  
               if (iterm .eq. 1) then   
                  write (*,874) kt,vk,dvk,dqk,vlimn(kt),vlimx(kt)   
  874             format (' NRQLIM/ BG bus - self-adjustment ',i5,  
     1               5e12.5)
               endif
               if (idswb .gt. 0) then   
                  write (dbug,874) kt,vk,dvk,dqk,vlimn(kt),vlimx(kt)
               endif
               go to 894
            else
               go to 1090   
            endif   
         else   
            do 880 l = km(kt), km(kt)-1+kmlen(kt) 
            if (ikmu(l) .eq. mt) go to 890         
  880       continue
            ksw = 1 
  890       continue
         endif  
C       
C     2. Remote bus must PQ.
C       
         if (kvolt(mt).ne.0) then   
            go to 1090  
         endif  
C       
C     3. Generator/remote bus must be reasonably convergent.
C       
         if (abs(dpm) .gt. option(4)) go to 1090
         if (kvolt(mt) .eq. 0) then 
            if (abs(dqm) .gt. option(7)) go to 1090 
         endif  
C       
C     4. Remote bus must violate V-limits   
C       
         if (dvm .eq. 0.0  .and. vlimn(mt) .lt. vlimx(mt)) then  
            if (dvk .eq. 0.0) then  
               dvk = amax1 (0.00501, 0.25*dvdq)
               dvk = amin1 (dvk, 0.75*(vlimx(kt)-vk)) 
               evk = dim (vm+dvk/vk*vm, vlimx(mt)) * vk/vm
               dvk = dvk - evk
               if (abs (dvk) .gt. option(24)) then  
                  dqx = dvk / dvdq  
C       
C                 Attenuate V-adjustment if driven into Q-limits.   
C       
                  if (abs (dqx) .gt. 2.0) dqx = sign (2.0, dqx) 
                  qnew = qk + dqx   
                  dqx = dim (qnew, qmax) - dim (qmin, qnew) 
                  dvx = dqx * dvdq  
                  if (dvx .ne. 0.0 .and.
     1                abs(dvk) .gt. abs (dvx) .and. 
     2                sign (1.0,dvk) .eq. sign (1.0,dvx) .and.  
     3                abs (dvk - dvx) .gt. option(24)) then  
                     if (sign(1.0, dvk) .eq. sign (1.0, dvx)) then
                       dvk = dvk - dvx  
                     else
                       dvk = -dvx
                     endif
                  endif 
               endif
               ksw = 1  
               if (iterm .eq. 1) then   
                  write (*,892) kt,vk,dvk,dqk,vlimn(kt),vlimx(kt)   
  892             format (' NRQLIM/ BG bus - Dead-band adjustment ',
     1               i5, 5e12.5)
               endif
               if (idswb .gt. 0) then   
                  write (dbug,892) kt,vk,dvk,dqk,vlimn(kt),vlimx(kt)
               endif
               go to 894
            else
               go to 1090   
            endif   
         endif  
C       
C     5. Generator control is established. Auto control is  
C        enabled if the following conditions are fulfilled. 
C        Manual control is enabled otherwise.   
C       
         dvk = vk*dvm/vm
  894    vnew = vk + dvk
         dx = -dim(vnew,vlimx(kt)) + dim(vlimn(kt),vnew)
         if (abs(dx) .gt. 1.0e-6) then  
            ksw = 100   
            vnew = vnew + dx
         endif  
         dvk = vnew - vk
C       
C     1. V-adjustment should not exceed 0.050.  
C       
         if (abs(dvk) .gt. 0.050) then  
            dvk = sign (0.050,dvk)  
            ksw = ksw + 50  
         endif  
C       
C     2. Generator should not be driven into V-limits. Make allowances
C        for spurious adjustment. The "0.25" factor is an estimate of
C        the probability that the actual adjustment is opposite of the
C        direction predicted.   
C       
         dv1 = dim(vlimx(kt),vk)                  
         dv2 = -dim(vk,vlimn(kt))                 
         if (amin1(abs(dv1),abs(dv2)) .le. 0.005) ksw = ksw + 10 
         if (dvk .gt. 0.0) then 
            if (-0.25*dvk .lt. dv2) ksw = ksw + 20
         else   
            if (-0.25*dvk .gt. dv1) ksw = ksw + 20
         endif  
C       
C     3. Generator should not be driven into Q-limit.   
C       
         qnew = qk + dvk / dvdq 
C       
C        Attenuate V-adjustment if driven into Q-limits.
C       
         dqx = dim (qnew, qmax) - dim (qmin, qnew)  
         dvx = dqx * dvdq   
         if (dvx .ne. 0.0 .and. 
     1       abs(dvk) .gt. abs (dvx) .and.  
     2       sign (1.0,dvk) .eq. sign (1.0,dvx) .and.   
     3       abs (dvk - dvx) .gt. option(24)) then   
            if (sign(1.0, dvk) .eq. sign (1.0, dvx)) then
              dvk = dvk - dvx  
            else
              dvk = -dvx
            endif
         endif  
         if (abs (dqx) .gt. option(8)) ksw = ksw + 5
C       
C        Set up control 
C       
         if (ksw .ne.0) then
            kvolt(kt) = kt  
            if (abs(dvk) .lt. option(24)) dvk = 0.0
            volt(kt) = dvk/vk   
            if (abs(dvk) .gt. 0.005) then
               kowntb = kowntb + 1  
               flag = '$'   
            endif   
         else   
            kvolt(kt) = mt  
            volt(mt) = dvm/vm   
            if (abs(dvm) .gt. 0.005) then
               kowntb = kowntb + 1  
               flag = '$'   
            endif   
         endif  
      endif 
      go to 1060
C       
C     BG or BX bus in state Q_max or Q_min -- test for restoration  
C       
  960 continue  
      if (abs(dqk) .gt. option(7)) go to 1090 
      if (qmax - qmin .lt. 0.01) go to 1090
C       
C     Subject restoration to dV/dQ > 0  
C       
      if (msw .gt. 1) then  
         if (dvdq .lt. 0.0) then  
            write (errbuf(1), 701) intbus(kt), intbas(kt),  
     1         ltyp, ityp, vk, vlimn(kt),vlimx(kt), dvdq, dvdqap
            call prterx ('W', 1)
            go to 1090  
         endif  
      endif 
C       
C     Subject restoration to convergence of any radial nodes.   
C       
      if (mt .eq. 0) then   
         if (kmlen(kt) .eq. 1) then               
            nt = ikmu(km(kt))              
            call nrpqv (nt, pm, dpm, qm, dqm, vm) 
            if (kvolt(nt) .ne. 0) dqm = 0.0   
            if (abs (dpm) .gt. option(4)) go to 1090  
            if (kvolt(nt) .eq. 0) then
               if (abs (dqm) .gt. 2.0 * option(7)) go to 1090   
            endif 
         endif  
      endif 
      dvm = 0.0 
C       
C     Restore V control under the following two conditions: 
C       
C        Q_max and Vk > Vmax
C        Q_min and Vk < Vmin
C       

      if (dvk .ne. 0.0 .and.
     &   ((dvk .gt. option(24) .and. ityp .eq. 3) .or.   
     &    (dvk .lt. -option(24) .and. ityp .eq. 2))) then 
        goto 1090
      else if (dvk .ne. 0.0 .and.
     &   ((dvk .lt. option(24) .and. ityp .eq. 3) .or.   
     &    (dvk .gt. -option(24) .and. ityp .eq. 2))) then 
C       
C     Test for restoration without voltage control  
C       
      else if (mt .eq. kt .or. mt .eq. 0) then
        dvm = 0.0   
C       
C       Raise voltage only from Q_min.  
C       
        if ((dvk .eq. 0.0) .and. (ityp .eq. 2)) then
           dvk = amax1 (0.00501, 0.25*dvdq)
           dvk = amin1 (dvk, vlimx(kt)-vk)        
        endif   
      else  
        if (abs(dpm) .gt. option(4)) go to 1090 
        if (kvolt(mt) .eq. 0) then  
           if (abs(dqm) .gt. option(7)) go to 1090  
        endif   
        dvm = -dim(vm,vlimx(mt)) + dim(vlimn(mt),vm)
C       
C       Raise voltage only from Q_min.  
C       
        if (dvk .eq. 0.0 .and. dvm .ge. 0.0 .and. 
     &      vlimn(mt) .lt. vlimx(mt) .and. ityp .eq. 2) then
           dvk = amax1 (0.00501, 0.25*dvdq)
           dvk = amin1 (dvk, vlimx(kt)-vk)    
           go to 966
        endif   
        dvk = dvm/vm*vk 
  966   vnew = vk + dvk 
        dx = -dim(vnew,vlimx(kt)) + dim(vlimn(kt),vk)   
        if (abs(dvk) .gt. option(24)) then  
           if (dx/dvk .lt. 0.50) then   
              dvk = dvk + dx
           else 
              dvk = 0.0 
           endif
        endif   
      endif 
      dqx = dvk / dvdq  
C       
C     Attenuate V-adjustment if driven into Q-limits.   
C       
      if (abs (dqx) .gt. 2.0) dqx = sign (2.0, dqx) 
      qnew = qk + dqx   
      dqx = dim (qnew, qmax) - dim (qmin, qnew) 
      dvx = dqx * dvdq  
      if (dvx .ne. 0.0 .and.
     1    abs(dvk) .gt. abs (dvx) .and. 
     2    sign (1.0,dvk) .eq. sign (1.0,dvx) .and.  
     3    abs (dvk - dvx) .gt. option(24)) then  
         if (sign(1.0, dvk) .eq. sign (1.0, dvx)) then
            dvk = dvk - dvx  
         else
            dvk = -dvx
         endif
      endif 
      if (abs (dvk) .lt. option(24)) go to 1090 
      deltaq = dvk / dvdq   
      if (abs(deltaq) .lt. qtol (qmax,qmin)) go to 1090 
      if (ityp.eq.2) then   
         if (dqk+deltaq.lt.0.0) go to 1090  
      else  
         if (dqk+deltaq.gt.0.0) go to 1090  
      endif 
C       
C     BG or BX bus in state Q -- restore to V   
C       
      if (ltyp.eq.5) then   
         nt = tbx(5,jt)
         totrek=xdata(3,nt) 
         totcap=xdata(4,nt) 
         userek=xdata(5,nt) 
         usecap=xdata(6,nt) 
         if (ityp.eq.2) then
            bkku(kt) = bkku(kt) + (totcap - usecap)/bmva  
            xdata(5,nt)=userek  
            xdata(6,nt)=totcap  
         else   
            bkku(kt) = bkku(kt) + (totrek - userek)/bmva  
            xdata(5,nt)=totrek  
            xdata(6,nt)=usecap  
         endif  
      endif 
C       
C     1. V-adjustment should not exceed 0.050.  
C       
      if (abs(dvk) .gt. 0.050) then 
         dvk = sign (0.050,dvk) 
         ksw = ksw + 10 
      endif 
      ityp=1
      ddtot = ddtot + abs(dvk)  
      kvolt(kt) = kt
      volt(kt)= dvk/vk  
      if (abs(dvk) .gt. 0.005) then  
         kowntb = kowntb + 1
         flag = '$' 
      endif 
C       
C     Bus type switched 
C       
 1060 if (ityp.ne.itypo) then   
         tbx(7,jt)=ityp
         kowntb=kowntb+1
         flag = '$' 
      endif 
      if (idswb.ne.0) write (dbug,1080) flag, jt,kt,mt,ltyp,itypo,ityp,
     1   ksw,dqk,deltaq,vk,dvk,vm,dvm   
 1080 format (' NRQLIM/ Bus switching ',a, i4,2i5,4i3,f10.5,e10.3,  
     1   4f8.5) 
 1090 continue  
        
      if (idswb .ne. 0) then
         write (dbug, 1100) ittot, kowntb, numbf, qerrbf, numbg, qerrbg
 1100    format (' NRQLIM/ SUMMARY : ITTOT, KOWNTB: ', 2i4, ' BF ',i4,
     1      f10.5, ' BG ', i4, f10.5)   
      endif 
      return
      end   
