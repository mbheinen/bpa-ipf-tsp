C    @(#)buserr.f	20.3 2/13/96
      subroutine buserr (k,dp,dq)
 
      include 'ipfinc/ecvar.inc'
c	Global variables used:
c		ittot
      include 'ipfinc/errbus.inc'
c	Global variables used:
c		ierr, buser
      include 'ipfinc/itrhis.inc'
c	Global variables used:
c		itrtyp, itrhis
 
      save
C
      i = 1
      go to 90

      entry buscor (k,dp,dq)
      i = 5

   90 if (itrtyp .eq. 1) then
         iter = ittot
      else
         iter = ittot + itrhis(1)
      endif
      perr = dp*dp + dq*dq
      if (ierr.eq.8) go to 120
      ierr = ierr + 1
      ibuser(i,ierr,iter) = k
      buser(i+1,ierr,iter) = perr
      buser(i+2,ierr,iter) = dp
      buser(i+3,ierr,iter) = dq
      if (ierr.lt.8) go to 130
  100 pmin = buser(i+1,1,iter)
      imin = 1
      do 110 l = 2,8
         if (buser(i+1,l,iter).gt.pmin) go to 110
         pmin = buser(i+1,l,iter)
         imin = l
  110 continue
      go to 130

  120 if (perr.lt.pmin) go to 130
      ibuser(i,imin,iter) = k
      buser(i+1,imin,iter) = perr
      buser(i+2,imin,iter) = dp
      buser(i+3,imin,iter) = dq
      go to 100

  130 return
      end
