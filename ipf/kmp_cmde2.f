C    @(#)kmp_cmde2.f	20.1 8/20/98
        function kmp_cmde2 (m,n)

C       This function performs ownership sort in the branch overload 
c       index array.
 
        include 'ipfinc/parametr.inc'
 
        include 'ipfinc/cmde_com.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/outxrf.inc'
 
        character id1*1, id2*1, own1*3, own2*3
 
        if (m .eq. n) then
           kmp_cmde2 = 0
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

           if (kompr (own1,own2,kmp_cmde2) .eq. 0) then
              k1 = klno(1,i)
              k2 = klno(1,j)
              kmp_cmde2 = inp2alf(k1) - inp2alf(k2)
              if (kmp_cmde2 .eq. 0) then
                 m1 = klno(2,i)
                 m2 = klno(2,j)
                 kmp_cmde2 = inp2alf(m1) - inp2alf(m2)
              endif
              if (kmp_cmde2 .eq. 0) then
                 call getchr(1,id1,klno(5,i))
                 call getchr(1,id2,klno(5,j))
                 kmp_cmde2 = kompr (id1,id2,junk)
                 if (kmp_cmde2 .eq. 0) then
                    n1 = outxrf(i)
                    n2 = outxrf(j)
                    kmp_cmde2 = brsect(n1) - brsect(n2)
                 endif
              endif
           endif
        endif
        return
        end
