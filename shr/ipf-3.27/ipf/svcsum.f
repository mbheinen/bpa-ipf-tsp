C    @(#)svcsum.f	20.3 2/13/96
      subroutine svcsum   ! prepares the analysis report titled 
C                           'Summary of SVC Compensation'.  
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc' 
c	Global variables used:
c		bkku(r*8), ineti(r*8), vlimn(r*8)
      include 'ipfinc/alpha2.inc' 
c	Global variables used:
c		kvolt
      include 'ipfinc/blank.inc'
c	Global variables used:
c		bmva
      include 'ipfinc/bus.inc'
c	Global variables used:
c		bus, base, inp2opt, opt2inp, e(r*8), f(r*8)
      include 'ipfinc/ecvar.inc'
c	Global variables used:
c		None
      include 'ipfinc/ordsta.inc'
c	Global variables used:
c		ordtbx
      include 'ipfinc/prt.inc'
c	Global variables used:
c		outbuf
      include 'ipfinc/slnopt.inc'
c	Global variables used:
c		None
      include 'ipfinc/svc.inc'
c	Global variables used:
c		numsvc, svc(r*8)
      include 'ipfinc/tbx.inc'
c	Global variables used:
c		tbx(r*8)
      include 'ipfinc/tbxsrt.inc'
c	Global variables used:
c		None
 
C       NUMSVC = number of buses with SVC Compensation. If
C       none exist, jump to bottom of subroutine.
C
        if (numsvc .eq. 0) go to 900
C                     Print tailer, go to next page and print header.
        call forbtm
        write (outbuf,110)
  110   format (t53,' Summary of SVC Buses ')
        call shdlod(1)
        write (outbuf,120)
  120   format('0Generator',t18,'Remote bus ',
     1         t34,'/----------------------- Voltages and limits ',
     2             '-------------------------/',
     3         t107,'Comments')
        call shdlod(2)
        write (outbuf,130)
  130   format(t34,'/---- Generator -----/--- Remote bus -----/-------'
     &            ,' State -------------/')
        call shdlod(3)
        write (outbuf,140)
  140   format(t34,'    min   max  actual    min   max  actual  ',
     &             'state  Qmin   Qmax   Qact ')
        call shdlod(4)
        write (outbuf,142)
  142   format(t83,'(MVAR)  (MVAR)  (MVAR)')
        call shdlod(5)  
        call fortop 
C                           Process SVC Compensation.   
        do 620 jt = 1, numsvc   
C                 K = external(alphabetical) bus number 
C                 KT = internal(optimal) bus number 
C                 M = external(alphabetical) remote controlled bus number   
C                 MT = internal(optimal) remote controlled bus number   
C                 NTBX = TBX index  
        k = svc(1,jt)  
        kt = inp2opt(k)   
        ntbx = svc(3,jt)   
        m = tbx(8,ntbx)
        if (ordtbx .eq. 1) then 
        else if (m .gt. 0) then 
           m = opt2inp(m) 
        endif   
        if (m .eq. 0) m = k 
        mt = inp2opt(m)   
        
        vk = dsqrt (e(kt) ** 2 + f(kt) ** 2) 
        vm = dsqrt (e(mt) ** 2 + f(mt) ** 2) 
        vkmin = svc(9,jt)   
        vkmax = svc(10,jt)  
        if (m .ne. k) then  
           vmmin = vlimn(mt)                      
           vmmax = vlimn(mt)                      
        else
           vmmin = svc(9,jt)
           vmmax = svc(10,jt)   
        endif   
        dvk = dim(vk,vkmax) - dim(vkmin,vk) 
        dvm = dim(vm,vmmax) - dim(vmmin,vm) 
C                           Compare computed and recorded state 
        svcbkk = bkku(kt) -svc(13,jt)              
        svcini = -ineti(kt) -svc(14,jt)           
        svctot = svcbkk * vk + svcini   
        svcbmn = svc(5,jt) * vk ** 2 * bmva 
        svcbmx = svc(6,jt) * vk ** 2 * bmva 
        svcact = svctot * vk * bmva 
        
C       Print out results:  
C       
        write (outbuf,210) bus(k), base(k), bus(m), base(m), vkmin, 
     1     vkmax, vk, vmmin, vmmax, vm, ifix(sngl(svc(2,jt))), svcbmn, 
     2     svcbmx, svcact   
        
  210   format ('0',a8, f6.1, t18, a8, f6.1, t34, 3f7.3, 3f7.3, i5, 
     1     f8.1, f8.1, f8.1)
        
        if (abs(dvm) .gt. 0.001) then   
           write (outbuf(107:), 220) dvm
  220      format ('Vm violation ',f7.3)
           call prtout(1)   
           outbuf = ' ' 
        endif   
        if (svc(2,jt) .eq. 1) then 
           if (abs(svc(8,jt) - svcbkk) .gt. 0.005) then 
              write (outbuf(107:), 230) svcbkk * bmva   
  230         format ('BSVC <> Bmax ',f7.1) 
              call prtout(1)
              outbuf = ' '  
           endif
           if (abs(svcini) .gt. 0.005) then 
              write (outbuf(107:), 240) svcini * bmva   
  240         format ('ISVC <> 0.0 ',f7.1)  
              call prtout(1)
              outbuf = ' '  
           endif
           if (dvk .ge. 0.001) then 
              write (outbuf(107:), 250) dvk 
  250         format ('Improper dVk ',f6.3) 
              call prtout(1)
              outbuf = ' '  
           endif
        else if (svc(2,jt) .eq. 2) then
           viosvc = ddim(dble(svctot), svc(8,jt)) 
     &             -ddim(svc(7,jt), dble(svctot)) 
           if (abs(viosvc) .gt. 0.005) then 
              write (outbuf(107:), 260) viosvc * bmva   
  260         format ('BSVC violation ',f7.1)   
              call prtout(1)
              outbuf = ' '  
           endif
           if (abs(dvk) .ge. 0.001) then
              write (outbuf(107:), 270) dvk 
  270         format ('Improper dVk ',f6.3) 
              call prtout(1)
              outbuf = ' '  
           endif
        else if (svc(2,jt) .eq. 3) then
           if (abs(svc(7,jt) - svcbkk) .gt. 0.1) then   
              write (outbuf(107:), 280) svcbkk * bmva   
  280         format ('BSVC <> Bmin ',f7.1) 
              call prtout(1)
              outbuf = ' '  
           endif
           if (abs(svcini) .gt. 0.1) then   
              write (outbuf(107:), 290) svcini * bmva   
  290         format ('ISVC <> 0.0 ',f7.1)  
              call prtout(1)
              outbuf = ' '  
           endif
           if (dvk .le. 0.001) then 
              write (outbuf(107:), 300) dvk 
  300         format ('Improper dVk ',f6.3) 
              call prtout(1)
              outbuf = ' '  
           endif
        endif   
        if (kvolt(mt) .ne. 0) then  
           outbuf(107:) = 'Remote bus is PV'
           call prtout(1)   
           outbuf = ' ' 
        endif   
        if (outbuf .ne. ' ') then   
           call prtout (1)  
        endif   
        
  620   continue
        
        outbuf = '0End of SVC Compensation Summary '
        call prtout(1)  
        
        outbuf = ' '
        call rpnlod 
        call shdlod (1) 
        call shdlod (2) 
        call shdlod (3) 
        call shdlod (4) 
        call shdlod (5) 
        
        call forbtm 
        
  900   continue
        
        return  
        end 
