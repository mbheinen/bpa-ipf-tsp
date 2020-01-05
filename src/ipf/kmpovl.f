C    @(#)kmpovl.f	20.3 2/13/96
        function kmpovl (m,n)

C       This function performs ownership sort in the branch overload 
c       index array.
 
        include 'ipfinc/parametr.inc'
 
        include 'ipfinc/apcom.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/intbus.inc'
        include 'ipfinc/outxrf.inc'
 
        character id1*1, id2*1, own1*3, own2*3
 
        if (m .eq. n) then
           kmpovl = 0
        else
           i = isort(m)
           j = isort(n)
C
c          klno(1,*) = k1
c          klno(2,*) = k2
c          klno(3,*) = owner
c          klno(5,*) = id
C
           call getchr(3,own1,klno(3,i))
           call getchr(3,own2,klno(3,j))

           if (kompr (own1,own2,kmpovl) .eq. 0) then
              k1 = klno(1,i)
              k2 = klno(1,j)
              kmpovl = kompr (intbus(k1), intbus(k2), junk)
              if (kmpovl .eq. 0) then
                 kmpovl = 100.0 * (intbas(k1) - intbas(k2))
              endif
              if (kmpovl .eq. 0) then
                 m1 = klno(2,i)
                 m2 = klno(2,j)
                 kmpovl = kompr (intbus(m1), intbus(m2), junk)
                 if (kmpovl .eq. 0) then
                    kmpovl = 100.0 * (intbas(m1) - intbas(m2))
                 endif
              endif
              if (kmpovl .eq. 0) then
                 call getchr(1,id1,klno(5,i))
                 call getchr(1,id2,klno(5,j))
                 kmpovl = kompr (id1,id2,junk)
                 if (kmpovl .eq. 0) then
                    n1 = outxrf(i)
                    n2 = outxrf(j)
                    kmpovl = brsect(n1) - brsect(n2)
                 endif
              endif
           endif
        endif
        return
        end
