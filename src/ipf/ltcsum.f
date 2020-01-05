C    @(#)ltcsum.f	20.4 7/18/96
        subroutine ltcsum
C
C       summarize the status of all LTC control schemes.
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/ltcsln.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/tran.inc'
      include 'ipfinc/ordsta.inc'
c 
        character type*1, tap1*6, tap2*6
 
        diftol (a,b) = abs ((a - b) / b)
        if (ntota .eq. 0) go to 900 
        call forbtm 
        write (outbuf,110)  
  110   format (t53,' Summary of LTC Transformers ')
        call shdlod(1)  
        write (outbuf,120)  
  120   format('0Controlled bus ',t18,'/- Voltages/MVA -/', 
     1        t38,'/----- Controlling LTC -----/',  
     2        t69,'/--- Tap ---//--- Limits ---//Step/',
     3        t105,'/--- Comments') 
        call shdlod(2)  
        write (outbuf,130)  
  130   format(t18,' min   max  actual',
     1         t82,'    min     max ')  
        call shdlod(3)  
        outbuf = ' '
        call shdlod(4)  
        call shdlod(5)  
        call fortop 
        do 210 jt = 1,ntota 
        k = ltran(1,jt) 
        m = ltran(9,jt) 
        if (ordvlt .eq. 1) then
           kt = k
           mt = m
        else
           kt = inp2opt(k)
           mt = inp2opt(m)
        endif
        ityp = mod(ltran(10,jt),100)  
        lt = ltran(10,jt)/100 
        if (ityp .eq. 1) then  
           vk = dsqrt(e(kt)**2 + f(kt)**2)   
           vm = dsqrt(e(mt)**2 + f(mt)**2)   
           tapk = tran(6,jt) * base(k)  
           tapm = tran(6,jt) / tap(jt) * base(m)
           tapmin = tran(6,jt) / tran(7,jt) * base(m)   
           tapmax = tran(6,jt) / tran(8,jt) * base(m)   
           steps = tran(11,jt)  
           if (steps .gt. 1.0) then 
              tdisc = (tapmax - tapmin) / (steps - 1.0) 
              if (tdisc .gt. 0.0) then  
                 tsteps = (tapm - tapmin) / tdisc + 1.01
              else  
                 tsteps = 0.0   
              endif 
           else 
              tdisc = 0.0   
              tsteps = 0.0  
           endif
           dtp = -tdisc / tapm * tap(jt)
           kc = ltran(2,jt) 
           if (kc .eq. -1) then 
              n = k 
              nt = kt   
              vn = vk   
              dvn = dim(vn,vlimx(nt)) - dim(vlimn(nt),vn)
              dtpp = -dvn / vm  
           else if (kc .eq. -2) then
              n = m 
              nt = mt   
              vn = vm   
              dvn = dim(vn,vlimx(nt)) - dim(vlimn(nt),vn)   
              dtpp = dvn * vk / vm**2   
           else 
              n = kc
              if (ordvlt .eq. 1) then
                 nt = kc
              else
                 nt = inp2opt(kc)
              endif
              dv = 0.0  
              vn = dsqrt (e(nt)**2 + f(nt)**2)   
              dvn = dim(vn,vlimx(nt)) - dim(vlimn(nt),vn)   
              dtpp = 0.0
           endif
        
           if (tapk .lt. 1000.0) then   
              write (tap1, 156) tapk
  156         format (f6.2) 
           else 
              write (tap1, 158) tapk
  158         format (f6.1) 
           endif
           if (tapm .lt. 1000.0) then   
              write (tap2, 156) tapm
           else 
              write (tap2, 158) tapm
           endif
           istep = tsteps   
           itstep = steps   
           write (outbuf,160)bus(n),base(n),vlimn(nt),vlimx(nt),vn,   
     1        bus(k), base(k), bus(m), base(m), tap1 // '/' // tap2,
     2        tapmin, tapmax, istep, itstep 
  160      format ('0',a8,f6.1,t18,3f6.3,t38,a8,f6.1,1x,a8,f6.1,
     1        t69,a,t82,2f8.2,i3,'/',i2)
        
           if (abs(dvn) .gt. 0.001) then
              write (outbuf(105:),170) dvn  
  170         format ('V violation  =',f7.3)
              call prtout (1)   
              outbuf = ' '  
           endif
           if (diftol (tapm,tapmin) .lt. 0.001) then
              outbuf(105:) = 'TMIN limit reached'   
              call prtout (1)   
              outbuf = ' '  
              if (abs(dtp) .gt. 0.001 .and. -dtpp .gt. 0.001) then  
                 ts = dtpp / dtp
                 write (outbuf(105:),180) ts
  180            format ('T resolution =',f7.3,' steps')
                 call prtout (1)
                 outbuf = ' '   
              endif 
           else if (diftol (tapm,tapmax) .lt. 0.001) then   
              outbuf(105:) = 'TMAX limit reached'   
              call prtout (1)   
              outbuf = ' '  
              if (abs(dtp) .gt. 0.001 .and. -dtpp .lt. -0.001) then 
                 ts = dtpp / dtp
                 write (outbuf(105:),180) ts
                 call prtout (1)
                 outbuf = ' '   
              endif 
           else if (abs(dtp) .gt. 0.001 .and. abs(dtpp) .gt. 0.001) then
              ts = dtpp / dtp   
              write (outbuf(105:),180) ts   
              call prtout (1)   
              outbuf = ' '  
           endif
           if (ntypu(nt) .eq. 2 .or. ntypu(nt) .eq. 3 .or. 
     1         ntypu(nt) .eq. 5 .or. ntypu(nt) .eq. 12) then 
        
              call typno (type,kbsdta(1,n)) 
              write (outbuf(105:),190) type 
  190         format ('Controlled bus is type B',a) 
              call prtout (1)   
              outbuf = ' '  
           endif
C       
C          The following flag, if true, is set in OPSLN1.   
C       
           if (ltcsln(jt) .eq. 2) then  
              write (outbuf(105:), 192) 
  192         format('Manual: radial circuit.') 
              call prtout (1)   
              outbuf = ' '  
           endif
C       
C          The following flag, if true, is set in OPSLN1.   
C       
           if (ltcsln(jt) .eq. 3) then  
              write (outbuf(105:), 194) 
  194         format('Manual: low TX impedance.')   
              call prtout (1)   
              outbuf = ' '  
           endif
           if (amod(tsteps, 1.0) .gt. 0.02) then
              write (outbuf(105:),196) tsteps   
  196         format ('T error      =',f7.3, ' steps')  
              call prtout (1)   
              outbuf = ' '  
           endif
C       
C          The following flag, if true, is set in OPSLN1.   
C       
           if (ltcsln(jt) .eq. 4) then  
              write (outbuf(105:),198)  
  198         format ('Manual: radial circuit.')  
              call prtout (1)   
              outbuf = ' '  
           endif
           if (outbuf .ne. ' ') call prtout (1) 
        endif   
  210   continue
        
        outbuf = '0End of Summary of LTC Transformers ' 
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
