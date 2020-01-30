C    @(#)nrdc.f	20.12 2/28/00
      subroutine nrdc
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc' 
      include 'ipfinc/alpha2.inc'
      include 'ipfinc/amtrx.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/dc.inc'
      include 'ipfinc/dcinit.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/intbus.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/tran.inc'
      include 'ipfinc/work1.inc'
      include 'ipfinc/lfiles.inc'

      common /is_batch / is_batch
C
      dimension a(30,30),iv(10),ic(10),jv(10),adi(10),vdi(10),dai(10)
      character dc_code*1 
 
      save
 
      data c1,c2,c3 / 1.823781306, 0.9549296586, 0.740480489 /
C
C     C1 = 18/PI**2
C     C2 = 3/PI**2
C     C3 = PI/(3*SQRT(2))
C
C
C     "KSW" Identifies entry
C
C           0 -- Normal
C           1 -- DC update after final AC solution
C
      kntry = 0
      go to 100
C
      entry nrdcup
      kntry = 1
  100 continue
C
C     Define constants
C
      kerrsw = 0
      kerrsx = 0
      kowntk = 0
C                         Begin loop of each DC circuit 
      do 820 jckt=1,idckt   
      ksw = 0   
      nec=nckt(jckt)-1  
      ntdc=nckt(jckt+1)-nckt(jckt)  
      if (kntry.ne.0) go to 760 
C                           Determine present state of DC system
      kdchg=0   
      itcnt = 1 
      do 120 i=1, ntdc   
      it=i+nec  
      iv(i)=0   
      ic(i)=0   
      jv(i)=0   
      ivc=dcbus(27,it) 
      ipc=dcbus(31,it) 
      iac=dcbus(32,it) 
      if (ivc.ne.0) iv(i) = 1   
      if (ipc.ne.0) ic(i) = 1   
      if (iac.ne.0) ic(i) = 2   
      ivc=dcbus(28,it) 
      ipc=dcbus(33,it) 
      if (ivc.ne.0) jv(i) = 1   
      if (ipc.ne.0) jv(i) = 2   
  120 continue  
C                         Main loop through DC system   
      kowntx = 0
      do 260 i=1, ntdc   
      it=i+nec  
      kt=dcbus(1,it)

      call getchr_8 (1, dc_code, dcbus(4,it))
      jt=dcbus(15,it)  
      if (jt.ne.0) go to 140
      if (idchg.ne.0) go to 140 
      if (dcbus(3,it).eq.0) go to 140  
C       
C     Examine for commutating LTC'S 
C       
      i1 = iflag(kt)
      i2 = iflag(kt+1) - 1  
      do 134 j = i1,i2  
      if (jflag(1,j).eq.2) then 
         jt = jflag(2,j)
         dcbus(15,it) = jt 
         go to 140  
      endif 
  134 continue  
C       
C     Initialize DC values  
C       
  140 cx = c1   
      if (dcbus(19,it).lt.0) cx=-c1 
      rci=(cx*dcbus(17,it)+c2*dcbus(18,it))*dcbus(7,it) 
      dcbus(23,it) = rci
      vdi(i) = dcbus(20,it) 
      adi(i) = dcbus(19,it)/vdi(i)  
      da = 0
      if (dcbus(3,it).ne.0) then   
c
c       iopton(23) = 0:  BRIDGE_CURRENT_RATING = ON
c                    1:                          OFF
c
        if (iopton(23) .eq. 0) then
           arate = dcbus(9,it)
        else
           arate = 0.0
        endif
C       
C       Check current limits. "0.00101" converts KAMPS to AMPS with 1%  
C       overload tolerance  
C       
        if (arate.ne.0.0) da = dim (abs (adi(i)),0.00101*arate)   
        if (da.ne.0) then   
           if (ic(i).ne.2) ksw = 1  
        endif   
      endif 
      dai(i) = da   
C       
C     update LTC taps   
C       
      jt = dcbus(15,it)
      if (jt.gt.0) dcbus(16,it) = tap(jt)   
      if (idchg.eq.0) then  
        if (dc_code .eq. 'M') then 
           dcbus(13,it) = dcbus(29,it)  
        else
           dcbus(13,it) = dcbus(12,it)  
        endif   
        go to 260   
      endif 
C       
C     Check AC/DC voltage balance   
C       
      rci = dcbus(23,it)
      kc = dcbus(3,it)   
      if (kc.eq.0) go to 260
      call nrpqv (kt,pk,dp,qk,dq,vk)
      if (kvolt(kt) .ne. 0) dq = 0.0
      phi = atan(-qk/pk)
      dv = -dim(vk,vlimx(kt)) + dim(vlimn(kt),vk)   
      vc = dsqrt (e(kc) ** 2 + f(kc) ** 2)   
      vdo_dc = vdi(i) + dcbus(8,it) * dcbus(7,it) + abs (adi(i)) * rci
      vdo_dc = vdo_dc / cos (dcbus(13,it))
      vdo_ac = vc * dcbus(2,it) * dcbus(7,it) * dcbus(16,it) / c3
      dcbus(24,it) = vdo_ac
      dt = vdo_ac - vdo_dc
      if (iopton(15) .ne. 0) then   
         write (dbug,170) jckt, i, kt, iv(i), ic(i), jv(i), dp, dq,
     1      dv, vk, vdo_ac, dt
  170    format(' DC State ', 3i5, 3i2, 6e12.5) 
      endif 
C       
C     Ignore this d-c system control during this iteration if the   
C     converter buses have not converged.   
C       
      if (abs(dp) .gt. option(4) .or. abs(dq) .gt. option(4)) then  
         kowntx = kowntx + 1
         go to 260  
      endif 
      if (abs(dv) .lt. 1.0e-3 .or. ittot .le. 4) go to 260  
C       
C     Converer AC voltage not maintained -- determine if LTC'S  
C     will be effective in ensuing AC solution  
C       
      dt = 0  
      if (ittot.lt.3) go to 260 
      jt = dcbus(15,it)  
      if (jt.gt.0) then 
C       
C        Calls to JACLTC redefine DPT(1,*).  Store old values and   
C        restore DPT(1,*) afterwards.   
C       
         dxold = dpt(1,jt)  
         kstat = jacltc (jt,1)  
         dx = dabs (dpt(1,jt))   
         dpt(1,jt) = dxold  
        
         if (kstat.eq.1) go to 260  
         if (kstat.eq.0) then   
            if (dx .ge. amin1 (0.25*abs(dv),0.001)) go to 260   
            if (iopton(15) .ne. 0) then 
               write (dbug,190) i,jt,kt,kstat,dv   
  190          format(' DC LTC control off ',4i5,e12.5) 
            endif   
         endif  
      endif 
C       
C     A change in DC state is imminent -- determine transition  
C       
      if (dcbus(19,it).gt.0) then   
        amin = dcbus(10,it)   
        amax = dcbus(11,it)   
      else  
        amin = dcbus(12,it)   
        amax = dcbus(11,it)   
        if (dc_code .eq. 'I' .or. dc_code .eq. 'M') amin=dcbus(29,it) 
      endif 
      vdo = dcbus(24,it)
      vdc_old = vdo*dcos(dcbus(13,it)) - rci*abs(adi(i)) 
     &        + dsign(dcbus(8,it),dble(adi(i)))
      ang = (dcbus(20,it) + rci*abs(adi(i)) + 
     &       dsign(dcbus(8,it),dble(adi(i)))) / vdo   
      if (abs(ang) .gt. 1.0) ang = sign(1.0,ang)
      ang = acos(ang) 
      anew = ang - dim(ang,amax) + dim(amin,ang)  
      da = anew - dcbus(13,it)  
      dx = ang - anew   
      if (abs(da).gt.0.002.or.abs(da).gt.0.5*abs(dx)) then  
C       
C       Commutator circuit and converter angle equation is appended 
C       DC constraints. 
C       
        jv(i) = 1   
C       
C       Converter angle order imposed on commutating circuit - some 
C       other DC constraint must be removed.
C       
      else  
        jv(i) = 2   
        dcbus(13,it) = anew 
      endif 
      vdc_new = vdo*dcos(dcbus(13,it)) - rci*abs(adi(i)) 
     &        + dsign(dcbus(8,it),dble(adi(i)))
      if (iopton(15) .ne. 0) then   
         write (dbug,250) i,jv(i),da,anew,amin,amax,vdc_old,vdc_new,
     &   dcbus(20,it) 
  250    format(' DC converter control ',2i3,7e12.5)
      endif 
      kdchg = 1 
  260 continue  
C       
C     If a-c part of d-c system has not converged, bypass constraint
C     checks and d-c resolution.
C       
  270 if (kowntx .gt. 0) then   
         kdchg = 0  
         go to 820  
      endif 
C       
C     Examine state of equations
C       
      m1 = 0
      m2 = 0
      m3 = 0
      m4 = 0
      do 280 i = 1, ntdc 
      if (iv(i).ne.0) m1 = m1 + 1   
      if (ic(i).eq.1) m2 = m2 + 1   
      if (dai(i).ne.0.0) m2 = m2 + 1  
      if (jv(i).eq.1) m3 = m3 + 1   
      if (jv(i).eq.2) m4 = m4 + 1   
  280 continue  
C       
C     Check whether DC system is overconstrained. Eliminate some
C     constraints   
C       
      if (m1+m2+m4.eq.ntdc) go to 400   
C       
C     Substitute current constraint for overloaded converters.  
C       
      do 330 i=1, ntdc   
      it=i+nec  
      if (dai(i).gt.0.0) then 
        if (ic(i).eq.0) then
C       
C       Examine other current constraints for possible substitution 
C       
           do 310 j=1, ntdc  
           jt=j+nec 
           if (ic(j).ne.1) go to 310
           ic(j) = 0
           ic(i) = 2
           if (iopton(15) .ne. 0) then  
              write (dbug,300) i,dai(i),j,dcbus(5,jt)  
  300         format(' IOVLD (',i2,') ',e10.3,' Replaces PSCH (',   
     1           i2,') ', e10.3)
           endif
           m2 = m2 - 1  
           go to 330
  310      continue 
           dai(i) = 0.0
           if (iopton(15) .ne. 0) then  
              write (dbug,320) jckt,i,adi(i),dai(i)
           endif
        else if (ic(i).eq.1) then   
           if (iopton(15) .ne. 0) then  
              write (dbug,300) i,dai(i),i,dcbus(5,it)  
           endif
           ic(i) = 2
           m2 = m2 - 1  
        else
           dai(i) = 0.0
           if (iopton(15) .ne. 0) then  
              write (dbug,320) jckt,i,adi(i),dai(i)
  320         format(' Overloaded converter unable to be constrained ', 
     1          2i4, 2e10.3)
           endif
        endif   
      endif 
  330 continue  
      if (m1+m2+m4.eq.ntdc) go to 382   
C       
C     Substitute commutator constraints for voltage constraints.
C       
      do 380 i = 1, ntdc 
      if (jv(i).ne.2) go to 380 
      if (iv(i).ne.0) then  
C       
C       Replace scheduled voltage constraint with commutator constraint.
C       
        iv(i) = 0   
        if (iopton(15) .ne. 0) then 
           write (dbug,340)i,dcbus(24,it),i,dcbus(6,it)
  340      format(' VDO (',i2,') ',e10.3,' Replaces VSCH (',i2,') ',
     1        e10.3)
        endif   
        m1 = m1 - 1 
        go to 380   
      else  
C       
C       Examine other voltage constraints for possible replacment of
C       with commutator constraint. 
C       
        do 360 j=1, ntdc 
        jt=j+nec
        if (iv(j).eq.1) then
          iv(j) = 0 
          if (iopton(15) .ne. 0) then   
             write(dbug,340)i,dcbus(24,it),j,dcbus(6,jt)   
          endif 
          m1 = m1 - 1   
          go to 380 
        endif   
  360   continue
        kc=dcbus(1,it) 
        write (errbuf(1),370) i,intbus(kc),intbas(kc)   
  370   format ('0 Over-constrained DC system No.',i2,' - converter '   
     1   ,a8,f6.1,' voltage cannot be maintained in AC system.')
        if (is_batch .eq. 0) then
           call prterx ('W',1)
        else
           call prterx ('F',1)
        endif
        jv(i) = 2   
      endif 
  380 continue  
  382 if (iopton(15) .ne. 0) then   
         write (dbug,390) (i,iv(i),ic(i),jv(i),i=1, ntdc)   
  390    format(' New DC state ',4i3/(14x,4i3)) 
      endif 
C       
C     SOLVE DC SYSTEM   
C       
  400 continue  
      if (idchg.ne.0.and.kdchg.eq.0.and.ksw.eq.0) go to 820 
      kdchg = 0 
      m = 3*ntdc
      do 410 i = 1,m
      do 410 j = 1,m
  410 a(j,i) = 0
      do 420 i = 1, ntdc 
      do 420 j = 1, ntdc  
      a(j,i)=dcy(j,i,jckt)  
  420 a(i,i+ntdc) = -1.0
C       
C     Add DC constaints to "A" matrix.  
C       
      m=0   
C       
C     Add voltage constraints   
C       
      do 440 i=1, ntdc   
      it=i+nec  
      dcbus(27,it)=0   
      dcbus(28,it)=0   
      dcbus(31,it)=0   
      dcbus(32,it)=0   
      dcbus(33,it)=0   
C       
C     Add scheduled voltage constraint  
C       
      if (iv(i).ne.0) then  
         m=m+1  
         a(m+ntdc,i) = 1.0  
         dcbus(27,it)=m+ntdc   
         dcbus(31,it)=0
         dcbus(32,it)=0
      else  
C       
C     Add convertor-angle-order constraint  
C       
        if (jv(i).eq.2) then
           m = m + 1
           a(m+ntdc,i) = -1.0   
           a(m+ntdc,i+ntdc) = -dsign(dcbus(23,it),dble(adi(i)))
           dcbus(28,it)=0  
           dcbus(33,it)=m+ntdc 
        endif   
      endif 
  440 continue  
C       
C     Add injection constraints 
C       
      do 460 i=1, ntdc   
      if (m.eq.ntdc) go to 460  
      it=i+nec  
C       
C     Add scheduled power constraints   
C       
      if (ic(i).eq.1) then  
         m=m+1  
         ivc=dcbus(27,it)  
         a(m+ntdc,i) = adi(i)   
         a(m+ntdc,i+ntdc) = vdi(i)  
         dcbus(27,it)=ivc  
         dcbus(31,it)=m+ntdc   
         dcbus(32,it)=0
C       
C     Add scheduled or overloaded current constraints   
C       
      else if (ic(i).eq.2) then 
         m=m+1  
         ivc=dcbus(27,it)  
         a(m+ntdc,i+ntdc) = 1.0 
         dcbus(27,it)=ivc  
         dcbus(31,it)=0
         dcbus(32,it)=m+ntdc   
      endif 
  460 continue  
      if (m .lt. ntdc) then 
         write (errbuf(1),461) jckt 
  461    format ('0 D-C system No.',i2,' is underconstrained.') 
         if (is_batch .eq. 0) then
            call prterx ('W',1)
         else
            call prterx ('F',1)
         endif
         do 463 i = 1, ntdc 
            kc = dcbus(1,i+nec)
            write (errbuf(1), 462) intbus(i), intbas(i), iv(i), ic(i), 
     1        jv(i) 
  462       format(' Converter ', a8, f6.1, ' DC state ',4i3)   
            call prterx ('W', 1)
  463    continue   
         call erexit (1)
      else if (m .gt. ntdc) then
         write (errbuf(1),464) jckt 
  464    format ('0 D-C system No.',i2,' is overconstrained.')  
         if (is_batch .eq. 0) then
            call prterx ('W',1)
         else
            call prterx ('F',1)
         endif
         do 466 i = 1, ntdc 
            kc = dcbus(1,i+nec)
            write (errbuf(1), 465) intbus(i), intbas(i), iv(i), ic(i), 
     1        jv(i) 
  465       format(' Converter ', a8, f6.1, ' DC state ',4i3)   
            call prterx ('W', 1)
  466    continue   
         call erexit (1)
      endif 
C       
C     Add commutator constraint 
C       
      if (m3.gt.0) then 
         do 470 i = 1, ntdc  
         it=i+nec   
         if (jv(i).eq.1) then   
            m = m + 1   
            a(m+ntdc,i) = -1.0  
            a(m+ntdc,i+ntdc) = -dsign(dcbus(23,it),dble(adi(i)))   
            a(m+ntdc,m+ntdc) = -dcbus(24,it)*dsin(dcbus(13,it))  
            dcbus(28,it)=m+ntdc
            dcbus(33,it)=0 
         endif  
  470    continue   
      endif 
      m = m + ntdc  
C       
C     Debug dump
C       
      if (iopton(15).gt.0) then 
         do 490 i=1,m   
  490    write (dbug,500) i,(a(i,j),j=1,m) 
  500    format ('  Row ',i4,10f10.3,/,(10x,10f10.3))   
      endif 
C       
C     Solve for DC voltages and currents
C       
  520 do 530 i = 1,m
  530 x(i) = 0.0d0
      do 590 i=1, ntdc   
      it=i+nec  
      x(i) = adi(i) 
      do 540 j = 1, ntdc 
  540 x(i) = x(i) - a(i,j)*vdi(j)   
      i1=dcbus(27,it)  
      i2=dcbus(31,it)  
      i3=dcbus(32,it)  
      if (i1.gt.0) x(i1) = dcbus(6,it) - vdi(i) 
      if (i2.gt.0) x(i2) = dcbus(5,it) - dcbus(19,it)   
      if (i3.gt.0) x(i3) = -sign(dai(i),adi(i)) 
      i4 = dcbus(28,it)  
      i5 = dcbus(33,it)  
      i3 = max0 (i4,i5) 
      if (i3.gt.0) then 
         x(i3) = -dcbus(24,it)*dcos(dcbus(13,it)) + vdi(i)   
     &          + dsign(dcbus(8,it),dble(adi(i))) 
     &          + dcbus(23,it)*abs(adi(i)) 
      endif 
  590 continue  
      do 600 i=1,m  
      do 600 j=1,m  
  600 aa(j,i)=a(j,i)
      call invert(m)
      if (ierr.gt.0) then   
         write (errbuf(1),610) jckt,m,ierr  
  610    format ('0 Fatal error -- DC circuit No. ',i2,' is a singular' 
     1   ,' system.  Order: ',i2,'  Rank: ',i2) 
         if (is_batch .eq. 0) then
            call prterx ('W',1)
         else
            call prterx ('F',1)
         endif
         kerrsw=1   
         go to 820  
      endif 
      error=0   
      ksw = 0   
C       
C     Compute residual error after iteration correction 
C       
      do 710 i=1, ntdc   
      it=i+nec
      call getchr_8 (1, dc_code, dcbus(4,it))
      vdi(i) = vdi(i) + x(i)
      adi(i) = adi(i) + x(i+ntdc)   
      da = 0.0
      if (dcbus(3,it).gt.0) then   
         arate = dcbus(9,it)
         if (arate.ne.0.0) da = dim (abs (adi(i)),0.00101*arate)  
         if (ic(i).eq.0) da = 0.0   
         if (da.ne.0.0) then  
            if (ic(i).ne.2) ksw = 1 
         endif  
      endif 
      dai(i) = da   
      dcbus(20,it) = vdi(i) 
      dcbus(19,it) = vdi(i)*adi(i)  
      i3=dcbus(28,it)  
      xx = 0.0  
      if (i3.gt.0) then 
         xx = x(i3) 
         anew = dcbus(13,it) + x(i3)
         amin = dcbus(10,it)
         if (adi(i).lt.0) then  
            amin=dcbus(12,it)   
            a2=dcbus(29,it) 
            if (dc_code.eq.'I'.or.dc_code.eq.'M') amin = a2   
         endif  
         da = ddim(dble(anew),dcbus(11,it)) - dim(amin,anew)   
         dcbus(13,it) = anew - da   
         if (da.ne.0.0) then  
            jv(i) = 2   
            ksw = 1 
         endif  
      endif 
      if (iopton(15).ne.0) then 
         write(dbug,670) i,i3,vdi(i),x(i),adi(i),x(i+ntdc), 
     &     dcbus(13,it), xx  
  670    format(' DC sln ',2i3,6e11.4)  
      endif 
      if (ic(i).eq.2) then  
         error = error + dai(i) 
      else if (ic(i).eq.1) then 
         error=error+dabs(dcbus(19,it)-dcbus(5,it))  
      endif 
      if (dcbus(33,it).gt.0) then   
         dv = dcbus(20,it) + dsign(dcbus(8,it),dble(adi(i)))   
     1                     + abs(adi(i)) * dcbus(23,it) 
         vdocos = dcbus(24,it)*dcos(dcbus(13,it))
         error = error + abs(dv - vdocos) / (dcbus(7,it) * dcbus(2,it)) 
      endif 
  710 continue  
      if ((ksw.eq.0).and.(error.le.option(4))) go to 740
      itcnt=itcnt+1 
      if (iopton(15) .ne. 0) then   
         write (dbug,720) jckt,itcnt,error,ksw 
  720    format(' DC ckt ',i2,' iteration ',i2,' error ',e12.5, 
     1      ' KSW ',i1) 
      endif 
      if (itcnt.le.10) then 
         if (ksw.eq.0) go to 520
         go to 270  
      endif 
      kowntk=kowntk+ntdc
  740 if (iopton(15) .ne. 0) then   
         write(dbug,750) (i,vdi(i),adi(i),dcbus(13,i+nec),i=1, ntdc)
  750    format(' New DC terminal values ',i3,3e12.5)   
      endif 
C       
C     Compute AC terminal values from DC values 
C       
  760 continue  
      do 810 i=1, ntdc   
      it=i+nec  
      if (dcbus(3,it).eq.0) go to 810  
      if (kntry.eq.0) then  
C       
C     Compute AC terminal values entirely from DC values
C       
         dv = dcbus(20,it) + dsign(dcbus(8,it),dble(adi(i)))   
         vdo = (dv + dcbus(23,it)*abs(adi(i)))/dcos(dcbus(13,it))
         ec=vdo*c3  
C       
C     Compute AC terminal values from commutator voltage and DC current 
C       
      else  
         kc = dcbus(3,it)  
         jt = dcbus(15,it) 
         if (jt.ne.0) dcbus(16,it) = tap(jt)
         vdi(i) = dcbus(20,it)  
         adi(i) = dcbus(19,it)/vdi(i)   
         vc = dsqrt(e(kc)**2 + f(kc)**2) 
         ec = vc*dcbus(2,it)*dcbus(7,it)*dcbus(16,it)   
         dv = dcbus(20,it) + dsign(dcbus(8,it),dble(adi(i)))   
      endif 
      p = dv*adi(i) 
      ac = adi(i)/c3
      rt=dcbus(17,it)*dcbus(7,it)   
      xt=dcbus(18,it)*dcbus(7,it)   
      cs=(p+rt*ac**2)/(ec*ac)   
      if (abs(cs).gt.1.0) cs=sign(1.0,cs)   
      phi=acos(cs)  
      if (adi(i).gt.0) phi = -phi   
      sn=sin(phi)   
      q=-ec*ac*sn-ac**2*xt  
      vr=ec-ac*(cs*rt-sn*xt)
      vi=-ac*(cs*xt+sn*rt)  
      ev=sqrt(vr**2+vi**2)/(dcbus(2,it)*dcbus(7,it))
      kt=dcbus(1,it)   
      pk=pnetu(kt)                                 
      qk=qnetu(kt)                                 
      pnetu(kt) = -p/bmva  
      qnetu(kt) = -q/bmva  
      dcbus(25,it) = p  
      dcbus(26,it) = q  
      dp = abs(pk-pnetu(kt))                       
      dq = abs(qk-qnetu(kt))                       
      vk=dsqrt(e(kt)**2+f(kt)**2)
      dx1 = vk - ev 
      if (dp.gt.option(4).or.dq.gt.option(4)) kowntd=kowntd+1   
      if (iopton(15) .ne. 0) then   
         write (dbug,790) kt,pnetu(kt),qnetu(kt),ev,phi  
  790    format (' New AC conditions -- node ',i4,4e12.5)   
      endif 
      jt=dcbus(15,it)  
      if (jt.gt.0) then 
         dcbus(16,it) = tap(jt) 
         volt(kt) = ev/vk - 1.0 
      endif 
      vlimn(kt) = ev  
      vlimx(kt) = ev  
  810 continue  
C                   End of DC circuit loop  
  820 continue  
      idchg = 1 
      if (kerrsw.ne.0) ntotcs = 2   
      return
      end   
