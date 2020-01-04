C    @(#)updt_own.f	20.3 2/13/96
      subroutine updt_own

C     This subroutine updates the ownership loss arrays OWNLOS from
c     arrays AVOLT, LOWNER, LOV, and OVLOS. 

      include 'ipfinc/parametr.inc'

      include 'ipfinc/anlys.inc' 
      include 'ipfinc/blank.inc' 
      include 'ipfinc/busanl.inc' 
      include 'ipfinc/losanl.inc' 
      include 'ipfinc/qksrt.inc' 
      include 'ipfinc/zonlst.inc' 
 
      common /sortsw/ sortsw, vltsrt(MAXBUS)
      integer sortsw, vltsrt
 
      common /ownflg/ ownflg
      logical ownflg
 
      external kompan, swapan,  kmpan2, swpan2
 
      do i = 1,jowner
         lstown(i) = i
      enddo
 
      if (jowner.ne.1) then
        call qiksrt(1,jowner,kompan,swapan)
      endif
 
      do i = 1,jvolt
         lstvlt(i) = i
      enddo
 
      if (jvolt .ne. 1) then
         call qiksrt(1,jvolt,kmpan2,swpan2 )
      endif
 
      do i = 1,jowner
         iown = lstown(i)
         ownnam(i) = lowner(iown)
         ownlos(1,i) = 0.0
         ownlos(2,i) = 0.0
         do j = 1,jvolt
            jvlt = lstvlt(j)
            lov = kov(jvlt,iown)
            if (lov.gt.0) then
               do k = 1,5,2
                  ownlos(1,i) = ownlos(1,i) + ovlos(k,lov)
                  ownlos(2,i) = ownlos(2,i) + ovlos(k+1,lov)
               enddo
            endif
         enddo
      enddo
 
      ownflg = .true.
      return
      end
