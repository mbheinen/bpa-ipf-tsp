C    @(#)getsvc.f	20.3 2/13/96
      integer function getsvc (kt, jt, isvc, vk, qk, dvdq, bkk, msw)
 
C     Input parameters:
C
C         KT          Internal bus number
C         JT          Index to TBX array
C         ISVC        Index to SVC array
C         VK          Actual voltage at bus 
C         QK          Actual reactive injection at bus  
C         DVDQ        Sensitivity dV/dQ at bus  
C         BKK         Total self-admittance at bus  
C         MSW         Solution state:   
C                     1 = continuous solution   
C                     2 = transition to discrete solution   
C                     3 = discrete solution 
C                     Output parameters:
C         BKK         Total self-admittance at bus  
C         GETSVC      Return state :
C                     0 = no change 
C                     1 = change
C
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
c	Global variables used:
c		qnetu(r*8), ineti(r*8)
      include 'ipfinc/alpha2.inc'
c	Global variables used:
c		kvolt, volt
      include 'ipfinc/blank.inc'
c	Global variables used:
c		bmva
      include 'ipfinc/bus.inc'
c	Global variables used:
c		None
      include 'ipfinc/cbus.inc'
c	Global variables used:
c		bctbl
      include 'ipfinc/ecvar.inc'
c	Global variables used:
c		kowntb, idswb
      include 'ipfinc/intbus.inc'
c	Global variables used:
c		None
      include 'ipfinc/lfiles.inc'
c	Global variables used:
c		dbug
      include 'ipfinc/prt.inc'
c	Global variables used:
c		None
      include 'ipfinc/slnopt.inc'
c	Global variables used:
c		option
      include 'ipfinc/svc.inc'
c	Global variables used:
c		svc(r*8), numsvc
      include 'ipfinc/tbx.inc'
c	Global variables used:
c		tbx(r*8)
      include 'ipfinc/tbxsrt.inc'
c	Global variables used:
c		None
 
      integer oldity, oldste
      getsvc = 0
      oldste = svc(2,isvc) 
      icb = svc(3,isvc)
      dqk = qk -qnetu(kt)                          
C            Compute SVC quantities. Recall that SVC(14,*) represents   
C            generation, whereas INET(*,*) represents load. 
      svcbkk = bkk - svc(13,isvc)   
      svcini = -ineti(kt) -svc(14,isvc)           
      svctot = svcini + svcbkk * vk 
      oldity = tbx(7,jt)   
        
      if (msw .eq. 1 .and. oldity .eq. 1) then  
         if (abs (dqk) .le. 10.0 * option(7)) then  
C                 Convert SVC bus from state PV to PQ when Q converges  
C                 reasonably.   
            kvolt(kt) = 0   
            volt(kt) = 0.0  
            tbx(7,jt) = 4  
            getsvc = 2  
         else   
C                    Adjust voltage to manually sustain state 2 
            dvk = - 0.5 * dqk * dvdq
            vnew = vk + dvk 
            dvk = dvk - ddim (dble(vnew),svc(10,isvc)) 
     &                + ddim (svc(9,isvc),dble(vnew))
            if (abs (dvk) .gt. 0.050) dvk = sign (0.050,dvk)
            if (abs (dvk) .gt. 1.0e-3) then 
               kvolt(kt) = kt   
               volt(kt) = dvk/vk
               if (abs (dvk) .gt. 5.0e-3) kowntb = kowntb + 1   
            endif   
         endif  
      else if (msw .eq. 2 .and. oldity .eq. 1) then 
C                Force conversion of SVC bus from state PV to PQ during 
C                transition phase.  
         kvolt(kt) = 0  
         volt(kt) = 0.0 
         tbx(7,jt) = 4 
         getsvc = 2 
C                Determine by the voltage the domain of the SVC state.  
      else if (abs(dqk) .lt. 0.005) then
         dvk = ddim (dble(vk),svc(10,isvc)) - 
     &         ddim (svc(9,isvc),dble(vk)) 
         if (oldste .eq. 1) then
            if (dvk .ge. 0.001) then
C                    A transition from state 1 to state 2 is necessary. 
               bkk = bkk - svcbkk + svc(11,isvc)
               ineti(kt) = ineti(kt) +svcini - svc(12,isvc) 
               bctbl(5,icb) = svc(11,numsvc) * bmva 
               bctbl(3,icb) = -svc(12,numsvc) * bmva
C                    Note that SVC(12,*) and SVCINI are represented as  
C                    generation, whereas INET(*,*) is represented as 
c                    load.  
               svc(2,isvc) = 2 
               getsvc = 2   
            endif   
         else if (oldste .eq. 2) then   
            if (dvk .lt. -0.001) then   
C                      A transition from state 2 to state 1 is necessary.   
               bkk = bkk - svcbkk + svc(6,isvc) 
               ineti(kt) = ineti(kt) +svcini
               bctbl(5,icb) = svc(6,numsvc) * bmva  
               bctbl(3,icb) = 0.0   
C                   Note that SVCINI is represented as generation, 
c                   whereas  INET(*,*) is represented as load.   
               svc(2,isvc) = 1 
               getsvc = 1   
            else if (dvk .gt. 0.001) then   
C                    A transition from state 2 to state 3 is necessary. 
               bkk = bkk - svcbkk + svc(5,isvc) 
               ineti(kt) = ineti(kt) +svcini
               bctbl(5,icb) = svc(5,numsvc) * bmva  
               bctbl(3,icb) = 0.0   
C                     Note that SVCINI is represented as generation, 
c                     whereas INET(*,*) is represented as load. 
               svc(2,isvc) = 3 
               getsvc = 3   
            endif   
         else   
            if (dvk .lt. -0.001) then   
C                      A transition from state 3 to state 2 is necessary.   
               bkk = bkk - svcbkk + svc(11,isvc)
               ineti(kt) = ineti(kt) +svcini - svc(12,isvc) 
               bctbl(5,icb) = svc(11,numsvc) * bmva 
               bctbl(3,icb) = -svc(12,numsvc) * bmva
C                      Note that SVC(12,*) and SVCINI are represented as
C                      generation, whereas INET(*,*) is represented as 
c                      load.
               svc(2,isvc) = 2 
               getsvc = 2   
            endif   
         endif  
      endif 
      if (idswb .gt. 0) then
         write (dbug, 100) isvc, kt, oldity, ifix(sngl(tbx(7,jt))), 
     1      oldste, ifix(sngl(svc(2,isvc))), vk, dvk, svc(9,isvc), 
     2      svc(10,isvc),  qk, dqk, svcbkk, svcini, svctot 
  100    format (' GETSVC ', i2, i6, 4i2, 4f8.4, 5e12.5)
      endif 
      return
      end   
