C    @(#)kmpred.f	20.3 2/13/96
      function kmpred (m,n)
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/bus.inc'
      include 'ipfinc/qksrt.inc'
      include 'ipfinc/red7.inc'
      include 'ipfinc/sortbs.inc'
 
        if (m.eq.n) then
 
           kmpred  = 0
           return
 
        else
 
          go to (130,120,120,110) key
 
  110     kmpred  = ymtrx(1,m) - ymtrx(1,n)
          return
 
 
  120     kmpred  = yred(1,m) - yred(1,n)
          return
 
 
  130     kmpred = 0
          j = nbsort(m)
          k = nbsort(n)
 
          if (kompr(bus(j),bus(k),kmpred).eq.0) then
             kmpred = 100.0*(base(j) - base(k))
          endif
 
          return
 
        endif
      end
