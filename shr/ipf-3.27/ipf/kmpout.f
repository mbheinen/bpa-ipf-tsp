C    @(#)kmpout.f	20.3 2/13/96
        function kmpout (m,n)

C       This function performs ownership sort in the branch outage 
C       index array.
 
        include 'ipfinc/parametr.inc'

        include 'ipfinc/apcom.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/intbus.inc'
        include 'ipfinc/comm_mode.inc'
 
        character id1*1, id2*1, own1*3, own2*3
 
        if (m .gt. nout .or. n .gt. nout) then
           kmpout = m - n
        else if (m .eq. n) then
           kmpout = 0
        else
           i = isort(m)
           j = isort(n)
C
C          KLNC(1,*) = K1
C          KLNC(2,*) = K2
C          KLNC(3,*) = OWNER
C          KLNC(4,*) = 1000 + ID
C
           call getchr(3,own1,klnc(3,i))
           call getchr(3,own2,klnc(3,j))
           if (kompr (own1,own2,kmpout) .eq. 0) then
              k1 = klnc(1,i)
              k2 = klnc(1,j)
              kmpout = kompr (intbus(k1), intbus(k2), junk)
              if (kmpout .eq. 0) then
                 kmpout = 100.0 * (intbas(k1) - intbas(k2))
              endif
              if (kmpout .eq. 0) then
                 m1 = klnc(2,i)
                 m2 = klnc(2,j)
                 kmpout = kompr (intbus(m1), intbus(m2), junk)
                 if (kmpout .eq. 0) then
                    kmpout = 100.0 * (intbas(m1) - intbas(m2))
                 endif
              endif
              if (kmpout .eq. 0) then
                 jd = mod(klnc(4,i),1000)
                 call getchr(1,id1,jd)
                 jd = mod(klnc(4,j),1000)
                 call getchr(1,id2,jd)
                 kmpout = kompr (id1,id2,junk)
              endif
           endif
        endif
        return
        end
