C    @(#)kmptrt.f	20.3 2/13/96
        function kmptrt (m,n)
C
        include 'ipfinc/parametr.inc'

        include 'ipfinc/arcntl.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/transf.inc'
 
        if (m .eq. n) then
           kmptrt = 0
        else
           m1 = ktdata(1,m)
           m2 = ktdata(2,m)
           n1 = ktdata(1,n)
           n2 = ktdata(2,n)
           kmptrt = min0 (m1,m2) - min0 (n1,n2)
           if (kmptrt .eq. 0) then
              kmptrt = max0 (m1,m2) - max0 (n1,n2)
              if (kmptrt .eq. 0) then
                 write (errbuf(1),100) arcnam(m1), arcnam(m2)
  100            format ('0 Duplicate >TRANSFER interties ',
     1                   a10,1x,a10)
                 call prterx ('W',1)
                 error = 1
              endif
           endif
        endif
        return
        end
