C    @(#)kmptrl.f	20.3 2/13/96
        function kmptrl (m,n)
C
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/bus.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/transf.inc'
 
        character idm * 1, idn * 1
C
        if (m .eq. n) then
           kmptrl = 0
        else
           m1 = kldata(1,m)
           m2 = kldata(2,m)
           n1 = kldata(1,n)
           n2 = kldata(2,n)
           kmptrl = min0 (m1,m2) - min0 (n1,n2)
 
           if (kmptrl .eq. 0) then
              kmptrl = max0 (m1,m2) - max0 (n1,n2)
 
              if (kmptrl .eq. 0) then
                 idm = char(kldata(3,m))
                 idn = char(kldata(3,n))
                 kmptrl = kompr (idm,idn,junk)
 
                 if (kmptrl .eq. 0) then
                    kmptrl = kldata(4,m) - kldata(4,n)
 
                    if (kmptrl .eq. 0) then
 
                        write (errbuf(1),100) bus(m1), base(m1),
     1                     bus(m2), base(m2), idm, kldata(4,m)
  100                   format ('0 Duplicate >OUTAGE branches ',
     1                          a8,f6.1,1x,a8,f6.1,1x,a1,i2)
                        call prterx ('W',1)
                        error = 1
                    endif
                 endif
              endif
           endif
        endif
        return
        end
