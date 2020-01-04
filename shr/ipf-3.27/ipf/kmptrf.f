C    @(#)kmptrf.f	20.3 2/13/96
        function kmptrf (m,n)
C
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/bus.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/transf.inc'
 
        character idm * 1, idn * 1
C
        if (m .eq. n) then
           kmptrf = 0
        else
           m1 = kfdata(1,m)
           m2 = kfdata(2,m)
           n1 = kfdata(1,n)
           n2 = kfdata(2,n)
           kmptrf = min0 (m1,m2) - min0 (n1,n2)
 
           if (kmptrf .eq. 0) then
              kmptrf = max0 (m1,m2) - max0 (n1,n2)
 
              if (kmptrf .eq. 0) then
                 idm = char(kfdata(3,m))
                 idn = char(kfdata(3,n))
                 kmptrf = kompr (idm,idn,junk)
 
                 if (kmptrf .eq. 0) then
                    kmptrf = kfdata(4,m) - kfdata(4,n)
 
                    if (kmptrf .eq. 0) then
 
                        write (errbuf(1),100) bus(m1), base(m1),
     1                     bus(m2), base(m2), idm, kfdata(4,m)
  100                   format ('0 Duplicate >OVERLOAD branches ',
     1                           a8,f6.1,1x,a8,f6.1,1x,a1,i2)
                        call prterx ('W',1)
                        error = 1
                    endif
                 endif
              endif
           endif
        endif
        return
        end
