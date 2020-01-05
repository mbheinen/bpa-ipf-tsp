C    @(#)getxsn.f	20.6 7/18/96
      subroutine getxsn (error)
      integer error
C
C     obtain X-bus sensitivities when DCSTRT is not
C     executed.
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/amtrx.inc'
      include 'ipfinc/beta.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/dcsln.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/ikk.inc'
      include 'ipfinc/intbus.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/slnphs.inc'
      include 'ipfinc/xdata.inc'
C
      error = 0
      ntotzt = ntotx

C     Convert voltages to polar form

      do 410 kt=1,ntot
         ek=e(kt)
         fk=f(kt)
         vk=sqrt(ek**2+fk**2)
         e(kt)=vk
         akm(kt) = vk
         if (vk.gt.0) f(kt)=atan2(fk,ek)
  410 continue

C     Factorize B".

      ikec=1
      msw=1 
      call dcfact(ikkind)   

C     Restore rectangular form of voltages 

      do 470 kt=1,ntot  
         ek=e(kt)  
         fk=f(kt)  
         e(kt)=ek*cos(fk)  
         f(kt)=ek*sin(fk)  
  470 continue

C     Compute type X bus sensitivities 

      do 750 jt = 1,kxtot   
      xsen(jt) = 0.0
      kt = xdata(1,jt)
      if (ntypu(kt) .ne. 11) go to 750
      do 710 i = nbslck+ntota,ntotx 
         dpt(1,i) = 0.0
  710 continue
      vk = dsqrt (e(kt)**2 + f(kt)**2)   
      kta = kt + ntota  
      if (ikk(1,kta) .eq. 2) then   
         dpt(1,kta) = 1.0   
         call dwnbak (ikkind)   
         xsen(jt) = dpt(1,kta) / vk 
      else  
         call dcjacb
         do 720 i = 1,lp
            mt = kolum(i)
            if (mt .ne. kta) dpt(1,mt) = rowh(i)
  720    continue
         call dwnbak (ikkind)
         xsen(jt) = 0.0
         do 730 i = 1,lp
            mt = kolum(i)
            if (mt .eq. kta) then
               xsen(jt) = xsen(jt) + rowh(i)
            else
               xsen(jt) = xsen(jt) - rowh(i)*dpt(1,mt)
            endif
  730    continue
         xsen(jt) = 1.0 / ( xsen(jt) * vk )
      endif
      if (idswb.ne.0) write (dbug,740) jt,kt,xsen(jt)
  740 format (' X BUS ',2i5,' SENSITIVITY ',e10.3)
  750 continue
      return
      end   
