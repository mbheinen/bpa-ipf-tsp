C    @(#)ipzo.f	20.3 2/13/96
        function ipzo(zn,own,lprsx1)
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/prt.inc'
      include 'ipfinc/zonlst.inc'
C
        character own*(*),zn*(*)
        ipzo = 0
        if (npzanl.eq.0) then
           if (npoanl.eq.0) then
              ipzo = lprsx1
           else
              do 100 i=1,npoanl
              if (own.eq.poalst(i)) then
                 ipzo = lprsx1
              endif
  100         continue
           endif
        else
           i = 1
  200      if (i.le.npzanl) then
              if (zn.ne.pzalst (i)) then
                 i = i+1
                 go to 200
              else
                 if (npoanl.eq.0) then
                    ipzo=lprsx1
                 else
                    do 300 i = 1,npoanl
                    if (own.eq.poalst(i)) then
                       ipzo = lprsx1
                       go to 200
                    endif
  300               continue
                  endif
              endif
           endif
        endif
        return
        end
