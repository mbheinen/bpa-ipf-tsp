C    @(#)varsum.f	20.7 6/27/97
        subroutine varsum   ! summarizes the status of the %var 
C                           ! control scheme.   

        include 'ipfinc/parametr.inc'

        include 'ipfinc/alpha.inc'
        include 'ipfinc/alpha2.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/ecvar.inc'
        include 'ipfinc/pctvr2.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/slnopt.inc'
        include 'ipfinc/tbx.inc'
 
        character type*1
        integer ptr, count

        if (npctvr.eq.0) go to 900  
        call forbtm 
        write (outbuf,110)  
  110   format (t53,' Summary of %Var-controlled Buses ')   
        call shdlod(1)  
        write (outbuf,120)  
  120   format('0Controlled bus ',t18,'---- Voltages ----', 
     1        t39,'Controlling Generator',t60,' Vars ', 
     2        t67,'--- %Vars ---',t85,'---- Voltages ----', 
     3        t107,'Comments')  
        call shdlod(2)  
        write (outbuf,130)  
  130   format(t18,' min   max  actual',t60,'(MVAR)',   
     1         t67,' Sched Actual',t85,' min   max  actual')
        call shdlod(3)  
        outbuf= ' ' 
        call shdlod(4)  
        call shdlod(5)  
        call fortop 

        do 210 jt = 1,npctvr
        mt = kpctvr(1,jt)   
        m = opt2inp(mt)   
        vm = dsqrt(e(mt)**2+f(mt)**2)
        dvm = dim (vm,vlimx(mt)) - dim(vlimn(mt),vm)
        pct = 0.0   
        qtot = 0.0  
        ptr = kpctvr(2,jt)
        count = 0
        do while (ptr .gt. 0)
          count = count + 1
          kt = pctvr(1,ptr)   
          if (kt.eq.0) go to 150  
          k = opt2inp(kt)   
          jtbx = pctvr(2,ptr) 

C         Check Q-limits 

          call nrpqv (kt,pk,dpk,qk,dqk,vk)
          qtot = qtot + qk
          pct = pct + pctvr(5,ptr)   
          kode = tbx(7,jtbx)   
          pctsr(1,count) = kt
          pctsr(2,count) = kode  
          pctsr(3,count) = 100.0 * pctvr(5,ptr)  
          dvk = dvm * vk / vm 
          vnew = vk + dvk 
          pctsr(4,count) = dvk
          pctsr(5,count) = qk*bmva
          pctsr(6,count) = dqk
          pctsr(7,count) = vk 
          pctsr(8,count) = ptr
          ptr = pctvr(8,ptr)
        enddo

  150   continue
        qtot = qtot * bmva  
        if (abs(qtot) .le. 0.1) qtot = 0.0
        do i = 1, count
          kt = pctsr(1,i)
          k = opt2inp(kt)   
          if (qtot .eq. 0.0) then 
            pct = 0.0
          else
            pct = pctsr(5,i)/qtot * 100.0
          endif   
          if (i .eq. 1) then  
            write (outbuf,160) bus(m),base(m),vlimn(mt),           
     1        vlimx(mt),vm,bus(k),base(k),pctsr(5,i),pctsr(3,i),   
     2        pct,vlimn(kt),vlimx(kt),pctsr(7,i)                 
  160       format ('0',a8,f6.1,t18,3f6.3,t39,a8,f6.1,t60,f7.1,  
     1        t67,2f6.1,t85,3f6.3)   
          else
            write (outbuf,170) bus(k),base(k),pctsr(5,i),
     2        pctsr(3,i),pct,vlimn(kt),vlimx(kt),pctsr(7,i)  
  170       format (t39,a8,f6.1,t60,f7.1,t67,2f6.1,t85,3f6.3)
          endif   
          if ((i .eq. 1) .and. (abs(dvm) .gt. 0.001)) then
            write (outbuf(107:),180) dvm 
  180       format ('V violation  =',f7.3)   
            call prtout (1)  
            outbuf = ' ' 
          endif   
          if (kvolt(mt) .ne. 0 .and. i .eq. 1) then   
            call typno (type,kbsdta(1,m))
            write (outbuf(107:),190) type
  190       format ('Controlled bus is type B',a)
            call prtout (1)  
            outbuf = ' ' 
          endif   
          if (pctsr(2,i) .eq. 1) then
            if (pctsr(7,i) .gt. vlimx(kt) + 0.001) then
              evk = pctsr(7,i) - vlimx(kt)        
              write (outbuf(107:),180) evk  
            else if (pctsr(7,i) .lt. vlimn(kt) - 0.001) then   
              evk = pctsr(7,i) -  vlimn(kt)       
              write (outbuf(107:),180) evk  
            else if (abs(pctsr(7,i) - vlimn(kt)) .le. 0.001 ) then 
              outbuf(107:) = 'V-min limit reached ' 
            else if (abs(pctsr(7,i) - vlimx(kt)) .le. 0.001 ) then 
              outbuf(107:) = 'V-max limit reached ' 
            else 
              outbuf(107:) = 'V-control'
            endif
          else if (pctsr(2,i) .eq. 2) then   
            outbuf(107:) = 'Qmin limit reached'  
          else if (pctsr(2,i) .eq. 3) then   
            outbuf(107:) = 'Qmax limit reached'  
          else if (pctsr(2,i) .eq. 4) then   
            outbuf(107:) = 'Q% controlled '  
          else if (pctsr(2,i) .eq. 5) then   
            outbuf(107:) = 'V-min limit reached' 
          else if (pctsr(2,i) .eq. 6) then   
            outbuf(107:) = 'V-max limit reached' 
          endif   
          call prtout (1) 
        enddo
  210   continue
        
        outbuf = ' '
        call rpnlod 
        call shdlod (1) 
        call shdlod (2) 
        call shdlod (3) 
        call shdlod (4) 
        call shdlod (5) 
        
        outbuf = '0End of Var Summary ' 
        call prtout(1)  
        call forbtm 
        
  900   continue
        
        return  
        end 
