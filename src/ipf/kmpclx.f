C    @(#)kmpclx.f	20.3 2/13/96
      function kmpclx (m,n)
C
 
      include 'ipfinc/parametr.inc'

      common /scratch/ kolum(MAXBUS), net(2,2*MAXREI), mtrx(MAXBUS)
 
      if (m .eq. n) then
         kmpclx = 0
      else
         i = mtrx(m)
         j = mtrx(n)
         iclus1 = kolum(i)
         iclus2 = kolum(j)
         if (iclus1 .gt. 0) then
            ipop1 = net(2,iclus1)
         else
            ipop1 = 999
         endif
         if (iclus2 .gt. 0) then
            ipop2 = net(2,iclus2)
         else
            ipop2 = 999
         endif
         kmpclx = ipop1 - ipop2
         if (kmpclx .eq. 0) kmpclx = iclus1 - iclus2
         if (kmpclx .eq. 0) kmpclx = i - j
      endif

      return
      end
