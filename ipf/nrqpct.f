C    @(#)nrqpct.f	20.14 8/19/99
        subroutine nrqpct
C
C       This subroutine establishes the controlling/percentage
C       generators and stores their Jacobian elements.
C
C       The steps in which Q% control occurs are summarized below.
C
C       In subroutine NRQLIM (generators are processed individually
C       without regard to %var control) -
C
C       1. Check Q-limits if in state V (KODE = 1), Vmin (KODE = 5),
C          or Vmax (KODE = 6).  
C       2. Restore to state V if voltage control alleviates Q-limits.
C       3. Establish individual voltage control:
C          explicit: KVOLT(KT) = KT;
C          implicit: KVOLT(KT) = MT.
C
C       In subroutine NRQPCT (consider the collective characteristics
C       of %var buses whose tests were omitted by NRQLIM) - 
C
C       4. Check Q- and V-limits if in state Q% (KODE = 4); 
C          Check V-limits if in state V (KODE = 1). 
C       5. Restore to state Q% any buses in state Qmin (KODE = 2) or
C          Qmax (KODE = 3) if the following conditions prevail: 
C          a. No switching occurred in steps 1 (hard to determine) or
C             4,
C          b. P and Q has converged resonably,  
C          c. Establishing Q% alleviates Q-limit, and   
C          d. All Q-limit buses collectively qualify.   
C       6. Establish master generator (ICN) from KODE = 1 or KODE = 4
C          candidates.  
C       7. Convert surplus KODE = 1 candidates (>< ICN) into explicit
C          Q% buses.
C       8. If primal generator is inactive (KODEX(1) = 0), then obtain
C          new primal master generator (ICN) from KODE = 4 candidates.
C       9. Update VARS on PV and pQ% buses to reflect latest Q%.
C      10. Under appropriate conditions, establish implicit control of
C          Q% buses.
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc' 
      include 'ipfinc/alpha2.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/gamma.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/optim1.inc'
      include 'ipfinc/pctvr2.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/tbx.inc'
C
      double precision qtot, xpct, pct
      integer kodex(6), ptr, count, oldkode
      real qpct(25), v_viol(25), q_viol(25), p_viol(25), qmax(25),
     &     qmin(25), nrdvdq   
      logical limitv, limitp, limitq
      external nrdvdq   

C     function  DQV(V)  estimates DQ/DV around operating point   

      dqv(v) = amax1 (0.100, 0.250*abs (qk/v - sngl(bkku(kt))*v))
      diftol (a,b) = abs ((a - b) / b)  

        if (npctvr.eq.0) go to 300  

C       Initialize %var control  

        tolerance = 5.0 * option(7)
        ikec = 1
        do 290 jt = 1,npctvr
        mt = kpctvr(1,jt)   
        vm = dsqrt(e(mt)**2+f(mt)**2)
        dvm = (-dim(vm,vlimx(mt)) + dim(vlimn(mt),vm)) / vm 
        if (abs(dvm) .gt. 0.050) dvm = sign(0.050,dvm)  
        icn = 0 
        pct = 0.0   
        qtot = 0.0  
        do i = 1,6   
           kodex(i) = 0
        enddo
        limitv = .false.
        limitp = .false.
        limitq = .false.

C       4. Check Q- and V-limits if in state Q% (KODE = 4);  
C          Check V-limits if in state V (KODE = 1).  

        ptr = kpctvr(2,jt)        
        count = 0
        do while (ptr .gt. 0)  
           kt = pctvr(1,ptr)   
           if (kt.eq.0) go to 110  
           jtbx = pctvr(2,ptr) 
           ltyp = tbx(1,jtbx)
           pctvr(3,ptr) = 0  
           pctvr(4,ptr) = 0  
           call nrpqv (kt, pk, dpk, qk, dqk, vk)
           dvdq = nrdvdq(jtbx, ittot, kt, vk, qk, dqv(vk), dvdqap)
           pctvr(6,ptr) = dvdq
           count = count + 1
           pctsr(1,count) = kt
           kode = tbx(7,jtbx)   
           oldkode = kode
           if (kode .lt. 0) then
             kode = iabs(kode)
             if (kvolt(kt) .eq. mt) then
               kvolt(kt) = kt   
             endif
             volt(kt) = 0.0  
             tbx(7,jtbx) = kode
             limitv = .true.
           endif  
           pctsr(3,count) = 10000 * kode
           pctsr(5,count) = qk 
           pctsr(6,count) = dqk
           pctsr(7,count) = vk 
           pctsr(8,count) = ptr
           qpct(count) = pctvr(5,ptr) 
           vnew = vk + volt(kt) * vk   
           v_viol(count) = -dim (vnew, vlimx(kt)) 
     &                   + dim (vlimn(kt), vnew)  
           if (abs(dpk) .gt. tolerance) limitp = .true.
           p_viol(count) = dpk
           qsuscp = tbx(6,jtbx) * vk ** 2   
           qmax(count) = tbx(3,jtbx) - amin1(0.0, qsuscp/bmva)   
           qmin(count) = tbx(4,jtbx) - amax1(0.0, qsuscp/bmva)   
           q_viol(count) = -dim (qk, qmax(count)) 
     &                   + dim (qmin(count), qk)
           if (abs(q_viol(count)) .gt. tolerance) 
     &       limitq = .true.
           qtot = qtot + qk
           pct = pct + qpct(count)
           if (kode .eq. 1) then   
              if (diftol (vnew, vlimx(kt)) .le. option(24)) then  
                 kode = 6  
                 tbx(7,jtbx) = kode 
                 pctsr(3,count) = ifix(sngl(pctsr(3,count))) 
     &                          + 1000*kode 
              else if (diftol (vnew, vlimn(kt)) .le. option(24)) then  

                 kode = 5  
                 tbx(7,jtbx) = kode 
                 pctsr(3,count) = ifix(sngl(pctsr(3,count))) 
     &                          + 1000*kode 
              endif
           else if (kode .eq. 2 .or. kode .eq. 3) then 
           else if (kode .eq. 4) then  
              dvk = dvm * vk
              if (abs(dvk) .gt. 0.050) dvk = sign (0.050,dvk)  
              vnew = vk + dvk
              dvkk = - dim (vnew,vlimx(kt)) + dim(vlimn(kt),vnew)  
              if (abs (dvkk) .gt. option(24) .and. 
     &            abs (dqk) .le. tolerance) then  
                 isw = 0   
                 if (sign (1.0,dvk) .eq. sign (1.0,dvkk)) then 
C       
C                   Already out of limits; voltage correction is not   

C                   sufficient to bring it within limits.  
C       
                    isw = 1
                 else  
                    if (abs (dvk) .lt. abs (dvkk)) then
C       
C                      Already outside of limits; voltage correction is

C                      aggravating voltage futher. 
C       
                       isw = 1 
                    endif  
                 endif 
                 if (isw .ne. 0) then  
                    if (dim (vlimx(kt),vnew) .lt. option(24)) then
                       kode = 6
                    else if (dim(vnew,vlimn(kt)) .lt. option(24)) then
                       kode = 5
                    endif  
                    if (kode .ne. 4) then  
                       kvolt(kt) = kt  
                       dvk = dvk + dvkk
                       if (abs (dvk) .gt. 0.050) dvk = sign (0.050,dvk)
                       volt(kt) = dvk / vk 
                       tbx(7,jtbx) = kode   
                       if (kode .ne. 4) tbx(1,jtbx) = -iabs (ltyp)
                       pctsr(3,count) = ifix(sngl(pctsr(3,count))) 
     &                            + 1000*kode   
                       if (abs(volt(kt)) .gt. 0.005) 
     &                    kowntb = kowntb + 1 
                    endif  
                 endif 
              endif
           endif   
           pctsr(2,count) = kode  
           kodex(kode) = kodex(kode) + 1   
        
C          Compute maximum voltage excursion as criterion which will   

C          later evaluate each generator for best primal generator.
        
           vnew = vk + dvm*vk  
           dv1 = dim (vlimx(kt),vnew)                
           dv2 = -dim (vnew,vlimn(kt))               
           if (dvm .gt. 0.0) then  
              dv = dv1 
           else if (dvm .lt. 0.0) then 
              dv = dv2 
           else
              if (abs (dv1) .lt. abs (dv2)) then   
                 dv = dv1  
              else if (abs (dv1) .gt. abs (dv2)) then  
                 dv = dv2  
              else 
                 dv = dv1  
              endif
           endif   
           pctsr(4,count) = dv 
           ptr = pctvr(8,ptr)
        enddo
        
 110    continue
c
c       If any controlling generator truncated a V-limit (KODE < 0)
C       freeze status-quo of all components one iteration.
c
        if (limitv) then
           do i = 1, count
              ptr = pctsr(8,i)
              jtbx = pctvr(2,ptr) 
              ltyp = tbx(1,jtbx)
              tbx(1,jtbx) = -ltyp
           enddo
           goto 272
        endif   
C       
C       5. Restore to state Q% any buses in state Qmin (KODE = 2) or
C          Qmax (KODE = 3) if the following conditions prevail: 
C          a. No switching occurred in steps 1 (hard to determine) or
C             4,
C          b. P and Q has converged resonably,  
C          c. Establishing Q% alleviates Q-limit, and   
C          d. All Q-limit buses collectively qualify.   
C       
        
        if (kodex(2) + kodex(3) .gt. 0 .and. .not. limitp  
     1                                 .and. .not. limitq) then 
        
           icode2 = 0   
           icode3 = 0   
           do i = 1, count
              ptr = pctsr(8,i)
              kode = pctsr(2,i)   
              oldkode = kode
              kt = pctvr(1,ptr)
              jtbx = pctvr(2,ptr) 
              if ((kode .eq. 2 .or. kode .eq. 3) .and. 
     &            abs(pctsr(6,i)) .le. tolerance) then
                 qk = qpct(i) * qtot / pct  
                 dqk = qk - pctsr(5,i)
                 vk = pctsr(7,i)  
                 dvk = dvm * vk   
                 if (abs(dvk) .gt. 0.050) dvk = sign (0.050,dvk)  
                 vnew = vk + dvk  
                 dvk = dvk - dim (vnew,vlimx(kt)) + dim(vlimn(kt),vnew)
                 vnew = vk + dvk  
                 if (kode .eq. 2) then  
                    if (dqk .gt. option(8) .and. dvk .ge. 0.0) then   
                       icode2 = icode2 + 1
                    endif 
                 else 
                    if (dqk .lt. -option(8) .and. dvk .le. 0.0) then  
                       icode3 = icode3 + 1
                    endif 
                 endif
              endif
           enddo        
        
C          Check whether all Q-limit buses can be restored collectively.
        
           if (kodex(2) - icode2 + kodex(3) - icode3 .eq. 0) then   
              do i = 1, count
                 ptr = pctsr(8,i)
                 kt = pctvr(1,ptr) 
                 jtbx = pctvr(2,ptr) 
                 ltyp = tbx(1,jtbx)
                 kode = pctsr(2,i)
                 oldkode = kode
                 if (kode .eq. 2 .or. kode .eq. 3) then 
                 else  
                    go to 122  
                 endif 
                 qk = qpct(i) * qtot / pct  
                 dqk = qk - pctsr(5,i)
                 vk = pctsr(7,i)  
                 ptr = pctsr(8,i)
                 dvdq = pctvr(6,ptr)
                 dvk = dvdq * dqk   
                 vnew = vk + dvk   
                 dvk = dvk - dim (vnew, vlimx(kt)) 
     &                     + dim (vlimn(kt), vnew)  
                 if (abs(dvk) .gt. 0.050) dvk = sign (0.050,dvk)  
                 if ((kode .eq. 2 .and. dvk .lt. 0.0) .or.
     &               (kode .eq. 3 .and. dvk .gt. 0.0)) then
                 else
                    kodex(kode) = kodex(kode) - 1
                    kode = 1
                    kodex(kode) = kodex(kode) + 1   
                    kvolt(kt) = kt
                    volt(kt) = dvk / vk
                    pctsr(2,i) = kode
                    pctsr(3,i) = ifix(sngl(pctsr(3,i))) 
     &                         + 100*kode  
                    tbx(7,jtbx) = kode 
                    if (kode .ne. oldkode) tbx(1,jtbx) = -iabs (ltyp)
                 endif
  122            continue  
              enddo
        
           endif
        
        endif   
        
C       6. Establish master generator (ICN) from KODE = 1 or KODE = 4
C          candidates.  
C       
        if (kodex(1) + kodex(5) + kodex(6) .ge. 1) then 
        
C          Controlling generator selected by following criteria:
        
C          1 -- State PV (KODE = 1) 
C          2 -- DVK (allowable V-excursion in control of VM) NE 0   
C          3 -- Proximity to MT 
C          4 -- MAX(ABS(DVK))   
        
           dvmx = 0.0
        
           do i = 1, count
              ptr = pctsr(8,i)
              jtbx = pctvr(2,ptr)
              kode = pctsr(2,i)   
              oldkode = kode
              if ((kode .eq. 1 .or. kode .eq. 5 .or. kode .eq. 6) .and.
     &            abs(q_viol(i)) .le. tolerance) then 

                 kt = pctsr(1,i)  

C                Select generator with proximity to MT 
c                Note: dv is projected onto dq, subjected to Q-limits, 
c                and then is projected back to dv.

                 do l = km(kt), km(kt)-1+kmlen(kt)   
                    if (ikmu(l) .eq. mt) then            
                       if (pctsr(4,i) .ne. 0.0) then
                          dq = pctsr(4,i) / pctvr(6,ptr)
                          qnew = pctsr(5,i) + dq
                          ddq = dim (qnew, qmax(i)) 
     &                        - dim (qmin(i), qnew)
                          dv = (dq - ddq) / dq * pctvr(6,ptr)
                       else
                          dv = 0.0
                       endif
                       if (abs (qpct(i) * dv) .gt. dvmx) then 
                          icn = i 
                          dvmx = abs(qpct(i) * dv)
                       endif  
                    endif 
                 enddo
              endif
           enddo

C          If ICN is 0, no primal generator has been selected.
C          Try again, but relax restriction of immediate proximity.   
c          Note: dv is projected onto dq, subjected to Q-limits, 
c          and then is projected back to dv.

           if (icn .eq. 0) then 
              do i = 1, count
                 kode = pctsr(2,i)   
                 oldkode = kode
                 if ((kode .eq. 1 .or. kode .eq. 5 .or. kode .eq. 6)
     &              .and. abs(q_viol(i)) .le. tolerance) then 
                    if (pctsr(4,i) .ne. 0.0) then
                       dq = pctsr(4,i) / pctvr(6,ptr)
                       qnew = pctsr(5,i) + dq
                       ddq = dim (qnew, qmax(i)) 
     &                     - dim (qmin(i), qnew)
                       dv = (dq - ddq) / dq * pctvr(6,ptr)
                    else
                       dv = 0.0
                    endif
                    if (abs (qpct(i) * dv) .gt. dvmx) then 
                       icn = i 
                       dvmx = abs(qpct(i) * dv)
                    endif  
                 endif
              enddo
           endif

        else if (kodex(2) + kodex(3) + kodex(4) .eq. count) then 

C          Controlling generator selected by following criteria:
C            1 -- State PQ% (KODE = 4)
C            2 -- DVK (allowable V-excursion in control of VM) NE 0
C            3 -- Proximity to MT
C            4 -- MAX(ABS(DVK))

           dvmx = 0.0
           do i = 1, count
              ptr = pctsr(8,i)
              kode = pctsr(2,i)
              oldkode = kode
              if (kode .eq. 4) then
                 kt = pctsr(1,i)  

C                Select generator with proximity to MT 

                 do l = km(kt), km(kt)-1+kmlen(kt)   
                    if (ikmu(l) .eq. mt) then            
                       if (abs (qpct(i) * pctsr(4,i)) .gt. dvmx) then
                          icn = i
                          dvmx = abs(qpct(i) * pctsr(4,i))
                          go to 142
                       endif
                    endif  
                 enddo
              endif
  142         continue
           enddo

C          If ICN is 0, no primal generator has been selected.
C          Try again, but relax restriction of immediate proximity.

           if (icn .eq. 0) then
              do i = 1, count
                 ptr = pctsr(8,i)
                 kode = pctsr(2,i)
                 oldkode = kode
                 if (kode .eq. 4 .and. abs(q_viol(i)) .le. tolerance) 
     &              then 
                    if (abs(qpct(i) * pctsr(4,i)) .gt. dvmx) then
                       icn = i
                       dvmx = abs (qpct(i) * pctsr(4,i))
                    endif
                 endif
              enddo
           endif
C
C          If ICN .ne. 0, convert to PV.
C
           if (icn .ne. 0) then
              kode = pctsr(2,icn)
              oldkode = kode
              ptr = pctsr(8,icn)
              kt = pctvr(1,ptr)
              kodex(kode) = kodex(kode) - 1
              kode = 1
              kodex(1) = kodex(1) + 1
              kvolt(kt) = kt
              volt(kt) = 0.0
              jtbx = pctvr(2,ptr)
              ltyp = tbx(1,jtbx)
              call setgen (kt, mt, jtbx, sngl(pctsr(5,icn)))
c
c             Don't allow remote voltage control if P has not 
c             converged or if any Q-limits are violated.
c
              if (kvolt(kt) .eq. mt .and. (limitp .or. limitq)) 
     &          then
                kvolt(kt) = kt
                volt(kt) = 0.0
              endif
              tbx(7,jtbx) = kode
              if (kode .ne. oldkode) tbx(1,jtbx) = -iabs (ltyp)
              pctsr(2,icn) = kode
              pctsr(3,icn) = ifix(sngl(pctsr(3,icn))) + 10*kode
           endif

        endif

C       7. Convert surplus KODE = 1 candidates (.NE. ICN) into KODE = 4
C          (Q% buses).
C       
        if (kodex(1) + kodex(5) + kodex(6) .gt. 1) then 
        
           do i = 1, count
              ptr = pctsr(8,i)
              kode = pctsr(2,i)   
              oldkode = kode
              kt = pctvr(1,ptr)
              jtbx = pctvr(2,ptr)   
              ltyp = tbx(1,jtbx)
        
C             Update VARS on PV and pQ% buses to reflect latest Q%.
        
              if (kode .eq. 1 .or. kode .eq. 4 .or. kode .eq. 5 .or.   
     &            kode .eq. 6) then
                 qk = qpct(i) * qtot / pct   
c
c                If target Q% violates Q-limits, set the target just
c                outside those limits to force bus type switching.
c
                 if (qk .gt. qmax(i)) then
                   qnetu(kt) = amin1 (qk, qmax(i)+0.005)
                 else if (qk .lt. qmin(i)) then
                   qnetu(kt) = amax1 (qk, qmin(i)-0.005)
                 else
                   qnetu(kt) = qk
                 endif
              endif
              if (.not. limitp .and. .not. limitq .and.
     &             icn .ne. 0 .and. icn .ne. i) then 
                 if (kode .eq. 1 .or. kode .eq. 5 .or. kode .eq. 6) then
                    vk = pctsr(7,i)

C                   Estimate DVK excursion if constraint changed to Q%.


                    dvk = (qk - pctsr(5,i)) * pctvr(6,ptr)
                    if (abs(dvk) .gt. 0.050) dvk = sign (0.050,dvk)
                    vnew = vk + dvk
                    dvk = dim (vnew,vlimx(kt)) - dim(vlimn(kt),vnew)   

                    if (dvk .eq. 0.0) then 
                       kodex(kode) = kodex(kode) - 1   
                       kode = 4
                       kodex(kode) = kodex(kode) + 1 
                       kvolt(kt) = 0 
                       volt(kt) = 0.0
                       tbx(7,jtbx) = kode   
                       if (kode .ne. oldkode) tbx(1,jtbx) = -iabs (ltyp)
                       pctsr(2,i) = kode  
                       pctsr(3,i) = ifix(sngl(pctsr(3,i))) 
     &                                + 10*kode  
                    endif  
                 endif 
              endif
           enddo
        endif   

C       8. If primal generator is inactive (KODEX(1) = 0), then obtain
C          new primal master generator (ICN) from KODE = 4 candidates.


        if (kodex(1) + kodex(5) + kodex(6) .eq. 0 .and.
     1      kodex(4) .gt. 0 .and. .not. limitp .and. .not. limitq) then


C           Controlling generator selected by following criteria:
C              1 -- State PQ% (KODE = 4)
C              2 -- DVK (allowable V-excursion in control of VM) NE 0
C              3 -- Proximity to MT 
C              4 -- MAX(ABS(DVK))   

           dvmx = 0.0
           do i = 1, count
              ptr = pctsr(8,i)
              kode = pctsr(2,i)   
              oldkode = kode
              if (kode .eq. 4) then

C                Select generator with proximity to MT   

                 kt = pctsr(1,i)  
                 do l = km(kt), km(kt)-1+kmlen(kt)   
                    if (ikmu(l) .eq. mt) then            
                      if (abs (qpct(i) * pctsr(4,i)) .gt. dvmx) then
                         icn = i
                         dvmx = abs(qpct(i) * pctsr(4,i))
                         go to 170
                      endif
                    endif  
                 enddo
              endif
  170         continue 
           enddo

C          If ICN is 0, no primal generator has been selected. 
C          Try again, but relax restriction of immediate proximity.

           if (icn .eq. 0) then
              do i = 1, count
                 ptr = pctsr(8,i)
                 kode = pctsr(2,i)
                 oldkode = kode
                 if (kode .eq. 4) then 
                    if (abs(qpct(i) * pctsr(4,i)) .gt. dvmx) then
                       icn = i
                       dvmx = abs (qpct(i) * pctsr(4,i))
                    endif
                 endif
              enddo
           endif
        endif

C       If ICN .NE. 0, convert to PV.

        if (icn .ne. 0) then 
           kode = pctsr(2,icn)  
           oldkode = kode
           ptr = pctsr(8,icn)
           kt = pctvr(1,ptr) 
           jtbx = pctvr(2,ptr)   
           ltyp = tbx(1,jtbx)

           kodex(kode) = kodex(kode) - 1 
           kode = 1  
           kodex(1) = kodex(1) + 1   
           kvolt(kt) = kt
           volt(kt) = 0.0
           call setgen (kt, mt, jtbx, sngl(pctsr(5,icn)))   
c
c          Don't allow remote voltage control if P has not 
c          converged or if any Q-limits are violated.
c
           if (kvolt(kt) .eq. mt .and. (limitp .or. limitq))
     &       then
             kvolt(kt) = kt
           endif
           tbx(7,jtbx) = kode 
           if (kode .ne. oldkode) tbx(1,jtbx) = -iabs (ltyp)
           pctsr(2,icn) = kode  
           pctsr(3,icn) = ifix(sngl(pctsr(3,icn))) + 10*kode   
        endif   

C       9. Update VARS on PV and pQ% buses to reflect latest Q%. 

        do i = 1, count
           ptr = pctsr(8,i)
           kode = pctsr(2,i)   
           kt = pctvr(1,ptr)
           jtbx = pctvr(2,ptr)   
           if (kode .eq. 1 .or. kode .eq. 4 .or. kode .eq. 5 .or.   
     1         kode .eq. 6) then
              qk = qpct(i) * qtot / pct
c
c             If target Q% violates Q-limits, set the target just
c             outside those limits to force bus type switching.
c
              if (qk .gt. qmax(i)) then
                qnetu(kt) = amin1 (qk, qmax(i)+0.005)
              else if (qk .lt. qmin(i)) then
                qnetu(kt) = amax1 (qk, qmin(i)-0.005)
              else
                qnetu(kt) = qk
              endif
           endif
        enddo
        
C       10. Under appropriate conditions, establish implicit control of
C           Q% buses.
        
        if (kodex(4) .gt. 0 .and. 
     &      .not. limitp .and. .not. limitq .and. .not. limitv .and.
     &     (kodex(1) + kodex(5) + kodex(6) .gt. 0)) then
        
C          Store DJ/DX elements for all generators. 
        
           do i = 1, count
              ptr = pctsr(8,i)
              kt = pctvr(1,ptr)
              pctvr(3,ptr) = ikec
              jtbx = pctvr(2,ptr)   
              xmtrx(ikec) = kt+ntota   
              xmtrx(ikec+1) = pctsr(5,i)   
              xpct = qpct(i)
              xmtrx(ikec+2) = xpct/pct 
              ikec = ikec+3
              call jacbus (kt,1)   
              lp = korder(0)
              do while (lp .gt. 0)
                 xmtrx(ikec) = kolum(lp)   
                 xmtrx(ikec+1) = rowj(lp)  
                 xmtrx(ikec+2) = rowl(lp)  
                 ikec=ikec+3   
                 lp = korder(lp)
              enddo
              pctvr(4,ptr) = ikec - 1
           enddo
        
        endif   
        
  272   if (idswb.gt.0) then
           write (dbug,280) kpctvr(1,jt), kodex, dvm, 
     &       limitp, limitq, ((ifix(sngl(pctsr(i,j))),i=1,3), 
     &       (pctsr(i,j),i=4,7), qpct(j),j=1,count)   
  280      format(' %VAR(NRQPCT) : ',i5,1x,6i2,' DVM ',f6.3,' Error ', 

     1      2i2,' Generator ',3i6, 5f8.3/(67x,3i6,5f8.3))
        endif   
 290    continue
 300    continue
        return  
        end 
