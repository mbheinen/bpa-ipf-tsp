C    @(#)ifzo.f	20.3 2/13/96
        function ifzo(zn,own,fchsx1)
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/prt.inc'
      include 'ipfinc/zonlst.inc'
 
        character own*(*),zn*(*)
        integer fchsx1
        ifzo = 0
        if (nfzanl.eq.0) then
           if (nfoanl.eq.0) then
              ifzo = fchsx1
           else
              do 100 i=1,nfoanl
              if (own.eq.foalst(i)) then
                 ifzo = fchsx1
              endif
  100         continue
           endif
        else
           i = 1
  200      if (i.le.nfzanl) then
              if (zn.ne.fzalst (i)) then
                 i = i+1
                 go to 200
              else
                 if (nfoanl.eq.0) then
                    ifzo=fchsx1
                 else
                    do 300 i = 1,nfoanl
                    if (own.eq.foalst(i)) then
                       ifzo = fchsx1
                       go to 200
                    endif
  300               continue
                  endif
              endif
           endif
        endif
        return
        end
