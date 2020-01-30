C    @(#)optsiz.f	20.3 2/13/96
      subroutine optsiz

C     This subroutine determines the optimal size of a retained
c     network which initially is defined by an arbitrary set of 
c     kernel nodes.

      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/elim2.inc'
      include 'ipfinc/ikk.inc'
      include 'ipfinc/red2.inc'
      include 'ipfinc/red7.inc'

      nsize=2*kbrknt+kbrknt/4

C     Define arrays: "KOWNT", "LOC", "KOLUM", and "KORDER".
      next = 0
      last = nsize

      do kt = 1,ntot
         if (kmlen(kt) .eq.0) then
            kownt(1,kt) = 0
            loc(kt) = 0
         else
            loc(kt)=next+1
            do l = km(kt), km(kt)-1+kmlen(kt)
               mt = ikmu(l)  
               next = next + 1
               kolum(next) = mt
               korder(next) = next + 1
            enddo
            korder(next) = 0
            kownt(1,kt) = kmlen(kt) 
        endif
      enddo
      next = next + 1
      do i = next,last
         kolum(i) = 0
         korder(i) = i + 1
      enddo

      korder(last) = 0
      call reord2 (ntot,kase1(33))

      do i = 1,ntot
         i1=ikk(1,i)
         i4=ikk(4,i)
         i5=ikk(5,i)
         if (i1 .eq. 0) then
            i4 = 0
         else if (i4 .gt. 0 .and. i5 .gt. nopt) then
            i1=1
            i4=1
         endif
         ikk(1,i)=i1
         ikk(3,i)=0
         ikk(4,i)=i4
         ikk(5,i)=0
      enddo

C     Optimal selection of the retained system has been
c     completed and the results stored in the "IKK" array.

      return
      end
