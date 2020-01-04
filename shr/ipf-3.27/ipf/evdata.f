C    @(#)evdata.f	20.4 5/27/98
        subroutine evdata (nb, perr, qerr)
C
C       This subroutine computes injection errors from input data
C
        include 'ipfinc/parametr.inc'
 
        include 'ipfinc/alpha.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/cbus.inc'
        include 'ipfinc/ecvar.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/ordsta.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/tbx.inc'
        include 'ipfinc/tran.inc'
        include 'ipfinc/xdata.inc'
 
        character btyp*1, kode2*1, kownr2*3, kodeyr*2
        integer ptr, oldptr
 
        kt = inp2opt(nb)
        vk = dsqrt (e(kt) ** 2 + f(kt) ** 2)
        call typno (btyp, ntypu(kt))                  
        call allocq (nb, sngl(qnetu(kt)), qgen, qgnmax, qgnmin, qld,
     1               totcap, usecap, totrek, userek, unsked, qerr)
        pnet = busdta(8,nb) - busdta(3,nb)
        pshunt = busdta(5,nb) * vk ** 2
        qshunt = busdta(6,nb) * vk ** 2
        ncb = kbsdta(15,nb)  
        do while (ncb .gt. 0)
           pload2 = bctbl(2,ncb)   
           qload2 = bctbl(3,ncb)   
           call getchr(1,kode2,kbctbl(8,ncb))
           call getchr(2,kodeyr,kbctbl(9,ncb))   
           call getchr(3,kownr2,kbctbl(10,ncb))  
C       
C          Convert constant current and constant admittance loads back 
c          to constant MVA. 
C       
           if (kode2 .eq. 'A') then  
              if (kodeyr .eq. '01' .or. kodeyr .eq. '*I') then   
                 pload2 = pload2*vk  
                 qload2 = qload2*vk  
              endif
           else if (kodeyr .eq. '*I') then   
              pload2 = pload2*vk 
              qload2 = qload2*vk 
           endif 
           pnet = pnet + bctbl(6,ncb) - pload2
           pshunt = pshunt + bctbl(4,ncb) * vk ** 2
           qshunt = qshunt + bctbl(5,ncb) * vk ** 2
           ncb = bctbl_nxt(ncb)
        enddo

        pintot = 0.0
        qintot = 0.0
        ptr = kbsdta(16,nb)
        do while (ptr .gt. 0) 
           ltyp = brtype(ptr)
           if (ltyp .eq. 1) then
c
c             Compute flows from equivlent-pi and ignore individual
c             sections.
c
              call getlfo (ptr, 1, pin, qin )
              pintot = pintot + pin
              qintot = qintot + qin
              oldptr = ptr
              ptr = brnch_nxt(ptr)
              do while (ptr .gt. 0 .and. 
     &                 (ky(ptr) .eq. ky(oldptr) .and.
     &                  brid(ptr) .eq. brid(oldptr)))
                 oldptr = ptr
                 ptr = brnch_nxt(ptr)
              enddo
              ptr = oldptr
           else if (ltyp .eq. 4) then
           else if (ltyp .gt. 0) then
              call getlfo (ptr, 1, pin, qin )
              pintot = pintot + pin
              qintot = qintot + qin
           endif
           ptr = brnch_nxt(ptr) 
        enddo
        perr = pnet - pintot - pshunt
        qerr = qgen - qld - qintot + usecap + userek + unsked
 
        if (abs(perr) .gt. 0.5 .or. abs (qerr) .gt. 0.5) then
           call typno (btyp, kbsdta(1,nb))
           write (errbuf(1), 130) bus(nb), base(nb), 'B' // btyp,
     1        perr, qerr
  130      format (' Assembled data injection error - Bus ', a8, f6.1,
     1        ' type ', a, ' ERROR ', 2e12.5)
           call prterx ('W', 1)
        endif
        return
        end
