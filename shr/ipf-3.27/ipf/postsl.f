C    @(#)postsl.f	20.5 8/20/98
      subroutine postsl
 
C     This subroutine performs post solution updating of several
C     arrays.
 
C     1. Update P- and Q-injections for slack buses, PV buses.
C     2. Update d-c values.
C     3. Update Taps for LTC transformers, Xij and Bis for variable
C        var-compensation.
C     4. Update Branch data values for LTC tap changes.
C     5. Update OLDTBX array with new TBX values.
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/agc.inc'
      include 'ipfinc/alpha.inc' 
      include 'ipfinc/alpha2.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/cbus.inc'
      include 'ipfinc/dcinit.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/oldtbx.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/tbx.inc'
      include 'ipfinc/tran.inc'
      include 'ipfinc/ycomp.inc'
      include 'ipfinc/ordsta.inc'
C
        integer p, q, aq

        character id*1, cbyear*2, cbtyp * 1
        logical found
C
C       Update Xij and Bis for variable var compensation.
C
        do 100 jt = 1,nycomp
        k1 = kycomp(1,jt)   
        k2 = kycomp(2,jt)   
        id = char (kycomp(3,jt))
        ksect = kycomp(4,jt)
        nbr = numbrn (k1,k2,id,ksect)   
        brnch(6,nbr) = ycomp(32,jt) 
        brnch(8,nbr) = ycomp(36,jt) 
        if (ksect .gt. 0) then  
           nbr = numbrn (k1,k2,id,0)
           brnch(4,nbr) = ycomp(8,jt)   
           brnch(5,nbr) = ycomp(9,jt)   
           brnch(6,nbr) = ycomp(10,jt)  
           brnch(7,nbr) = ycomp(11,jt)  
           brnch(8,nbr) = ycomp(12,jt)  
           brnch(9,nbr) = ycomp(13,jt)  
           brnch(10,nbr) = ycomp(14,jt) 
           brnch(11,nbr) = ycomp(15,jt) 
        endif   
        nbr = numbrn (k2,k1,id,ksect)   
        brnch(6,nbr) = ycomp(32,jt) 
        brnch(8,nbr) = ycomp(36,jt) 
        if (ksect .gt. 0) then  
           nbr = numbrn (k2,k1,id,0)
           brnch(4,nbr) = ycomp(14,jt)  
           brnch(5,nbr) = ycomp(15,jt)  
           brnch(6,nbr) = ycomp(12,jt)  
           brnch(7,nbr) = ycomp(13,jt)  
           brnch(8,nbr) = ycomp(10,jt)  
           brnch(9,nbr) = ycomp(11,jt)  
           brnch(10,nbr) = ycomp(8,jt)  
           brnch(11,nbr) = ycomp(9,jt)  
        endif   
  100   continue
C
C       Update Tap1 or Tap2 for LTC transformers and phase shifters.  
C
        if (itsw .gt. 0) then   
           do 120 jt = 1,ntota  
           kt = ltran(1,jt) 
           mt = ltran(9,jt) 
           if (ordltc .eq. 1) then
             k1 = kt
             k2 = mt
           else
             k1 = opt2inp(kt)   
             k2 = opt2inp(mt)   
           endif
           ityp = mod (ltran(10,jt), 100)
           if (ityp .gt. 10) ityp = mod (ityp, 10)
c
c          k1 = fixed tap side, k2 = variable tap side
c
           p = kbsdta(16,k1)
           found = .false.
           do while (p .gt. 0)
              if (ky(p) .eq. k2) then 
                 found = .true.
                 q = brnch_ptr(p)
                 aq = iabs(q)
                 if (brtype(p) .eq. 4 .and. ityp .eq. 3) then  
                    if (q .gt. 0) then
                       tmax = brnch(6,aq) + 0.05 
                       tmin = brnch(7,aq) - 0.05 
                       tap1 = 57.2957795 * tap(jt)
                       tap1 = amin1 (tmax,tap1)   
                       tap1 = amax1 (tmin,tap1)   
                    else
                       tmax = -brnch(7,aq) + 0.05 
                       tmin = -brnch(6,aq) - 0.05 
                       tap1 = 57.2957795 * tap(jt)
                       tap1 = amin1 (tmax,tap1)   
                       tap1 = amax1 (tmin,tap1)   
                    endif
                 else if (brtype(p) .eq. 4) then   
                    tmax = brnch(6,aq) + 0.05 
                    tmin = brnch(7,aq) - 0.05 
                    ck = tran(6,jt)
                    tap1 = ck * base(k1)
                    tap2 = base(k2) * ck / tap(jt) 
                    tap2 = amin1 (tmax,tap2)   
                    tap2 = amax1 (tmin,tap2)   
                 else if (brtype(p) .eq. 5) then   
                    if (q .gt. 0) then
                       brnch(9,aq) = tap1   
                       brnch(10,aq) = tap2   
                    else
                       brnch(10,aq) = tap1   
                       brnch(9,aq) = tap2   
                    endif
                 else if (brtype(p) .eq. 6) then   
                    if (q .gt. 0) then
                       brnch(9,aq) = tap1
                    else
                       brnch(9,aq) = -tap1
                    endif
                 else if (brtype(p) .eq. 9) then   
                 endif 
              else if (inp2alf(ky(p)) .gt. inp2alf(k2)) then
                 if (.not. found) call erexit(1)
                 go to 120 
              endif
              p = brnch_nxt(p)
           enddo
  120      continue
        endif   
C       
C       Update P-injections for area slack buses.   
C       
        if (iasw .gt. 0) then   
           do 1420 j=1,ntotc
              kt=karea(1,j) 
              if (ordtie .eq. 1) then
                kt = inp2opt(kt)   
              endif
              call nrpqv (kt,pk,dpk,qk,dqk,vk)  
              pnetu(kt) = pk   
 1420      continue 
        endif   
C       
C       Update P- and Q-injections for system slack buses.  
C       
        do 1440 kt=1,nbslck 
           call nrpqv (kt,pk,dpk,qk,dqk,vk) 
           pnetu(kt) = pk  
           qnetu(kt) = qk  
 1440   continue
C       
C       Update Q-injections for PV buses.   
C       
        do 1450 kt=nbslck+1,ntot
           if (kvolt(kt) .ne. 0) then   
              call nrpqv (kt,pk,dpk,qk,dqk,vk)  
              qnetu(kt) = qk   
           endif
 1450   continue
C       
C       Update P-injections for / AGC buses.
C       
        do j = 1, numagc   
           nb = kagc(1,j)   
           kt = inp2opt(nb)   
           call nrpqv (kt,pk,dpk,qk,dqk,vk) 
           pnetu(kt) = pk  
           pgen = busdta(8,nb)  
           pload = busdta(3,nb) 
           ncb = kbsdta(15,nb)
           do while (ncb .gt. 0) 
              call getchr(1, cbtyp, kbctbl(8,ncb)) 
              call getchr(2, cbyear, kbctbl(9,ncb))
              if ((cbyear .eq. '*I') .or.
     1            (cbtyp .eq. 'A' .and. cbyear .eq. '01')) then  
              else   
                 pgen = pgen + bctbl(6,ncb)   
                 pload = pload + bctbl(2,ncb) 
              endif  
              ncb = bctbl_nxt(ncb)
           enddo
 1454      dpgen = pk * bmva + pload - pgen 
           busdta(8,nb) = busdta(8,nb) + dpgen  
        enddo
C       
C       1. Update Q-injections for types BG or BX buses in states   
C          PV, PV_min, PV_max, or PQ_discrete.  
C       
C       2. Update OLDTBX.   
C       
        do 1460 jt = 1,ntotb
           ltyp=tbx(1,jt)  
           ityp=tbx(7,jt)  
           if (ltyp .eq. 3 .or. ltyp .eq. 5) then   
              if (ityp .eq. 1 .or. ityp .eq. 4 .or. ityp .eq. 5) then
                 kt=tbx(2,jt)  
                 if (ordtbx .eq. 1) then
                   kt = inp2opt(kt)   
                 endif
                 call nrpqv (kt,pk,dpk,qk,dqk,vk)   
                 qnetu(kt) = qk
              endif 
           endif
           do 1458 i = 1, 8 
 1458      oldtbx(i,jt) = tbx(i,jt)
 1460 continue  
      numtbx = ntotb
C       
C     Update d-c values.
C       
      if (idcsw .eq. 1) then
         if (iopton(38) .eq. 0) then
            call nrdcup 
         else   
            call nrdclu 
         endif  
         call dcfinl
      endif 
      return
      end
