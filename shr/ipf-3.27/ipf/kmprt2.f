C    @(#)kmprt2.f	20.3 2/13/96
      function kmprt2(i,j)
 
      include 'ipfinc/cgrate.inc'
 
      kmprt2 = lrat(1,i) - lrat(1,j)
 
      if (kmprt2 .eq. 0) kmprt2=lrat(2,i)-lrat(2,j)
      if (kmprt2 .eq. 0) kmprt2=lrat(3,i)-lrat(3,j)
 
      return
      end
