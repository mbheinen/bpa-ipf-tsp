C    @(#)phasum.f	20.4 1/4/99
        subroutine phasum
C
C       summarize the status of all phase-shifters.
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/phase.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/slnphs.inc'
      include 'ipfinc/tran.inc'
      include 'ipfinc/ordsta.inc'
C
      integer error, sect, p
      complex * 16 y(2,2), v(2), a(2), s(2)
      logical found
      character id * 1
        
C                     Initialize counters/switched  
        error = 0 
        if (jphno .eq. 0) go to 900 
        call forbtm 
        write (outbuf,10)   
   10   format (t53,' Summary of Phase-shifters ')  
        call shdlod(1)  
        write (outbuf,20)   
   20   format('0-------- Phase-shifter ----------',
     1     t36,'  -- Phase shift (degrees) --', 
     2     t66,'  ------ Flow (MW) -----',  
     3     t93,'Sensitivity',   
     4     t105,'Comments') 
        call shdlod(2)  
        write (outbuf,30)   
   30   format(t32,'P S',   
     1         t36,'  actual minimum maximum steps',
     2         t66,'  actual minimum maximum ', 
     3         t93,'(MW/degrees)')  
        call shdlod(3)  
        outbuf = ' '
        call shdlod(4)  
        call shdlod(5)  
        call fortop 
        do 210 jt = 1,jphno 
        k = jphid(1,jt) 
        m = jphid(2,jt) 
        if (ordvlt .eq. 1) then
           kt = k   
           mt = m   
        else
           kt = inp2opt(k)   
           mt = inp2opt(m)   
        endif
        call getchr(1, id, jphid(3,jt))
        sect = jphid(4,jt)  
        angle = 57.29578 * phid(5,jt)   
c
C       Find LTC index. 
c
        do 100 i = 1, ntota 
           if (k .eq. ltran(1,i) .and. m .eq. ltran(9,i)) then  
              ltc = i   
              angmin = 57.29578 * tran(8,i) 
              angmax = 57.29578 * tran(7,i) 
              pmin = tran(5,i) * bmva   
              pmax = tran(4,i) * bmva   
              sen = phse(i) * bmva / 57.29578   
              angle = 57.29578 * tap(i) 
              steps = tran(11,i)
              if (steps .gt. 1.0) then  
                 tdisc = (angmax - angmin) / (steps - 1.0)  
              else  
                 tdisc = 0.0
              endif 
              pdisc = sen * tdisc   
              go to 110 
           else if (k .eq. ltran(9,i) .and. m .eq. ltran(1,i)) then 
              ltc = -i  
              angmin = -57.29578 * tran(7,i)
              angmax = -57.29578 * tran(8,i)
              pmin = -tran(4,i) * bmva  
              pmax = -tran(5,i) * bmva  
              sen = -phse(i) * bmva / 57.29578  
              angle = -57.29578 * tap(i)
              steps = tran(11,i)
              if (steps .gt. 1.0) then  
                 tdisc = (angmax - angmin) / (steps - 1.0)  
              else  
                 tdisc = 0.0
              endif 
              pdisc = sen * tdisc   
              go to 110 
           endif
  100   continue
        ltc = 0 
C ***   
C ***   Find branch pi-equivalent   
C ***   
 110    continue
        p = kbsdta(16,k)
        found = .false.
        do while (p .gt. 0 .and. .not. found)
           if ((brtype(p) .eq. 1 .and. ky(p) .eq. m) .or.
     &         (brtype(p) .eq. 6 .and. ky(p) .eq. m)) then   
              if (id .eq. brid(p)) then 
                 call pieqiv (p, y, error)   
                 v(1) = dcmplx (e(kt),f(kt))  
                 v(2) = dcmplx (e(mt),f(mt))  
                 a(1) = y(1,1) * v(1) + y(1,2) * v(2)
                 a(2) = y(2,1) * v(1) + y(2,2) * v(2)
                 s(1) = v(1) * dconjg (a(1))  
                 s(2) = v(2) * dconjg (a(2))  
                 pin = dreal (s(1)) * bmva
                 if (sect .eq. 0) then   
                    found = .true.
                 else
                    p = brnch_nxt(p)
                    do while (p .gt. 0 .and. .not. found)
                       call pieqiv (p, y, error)
                       v(2) = (a(1) - y(1,1) * v(1)) / y(1,2)
                       a(2) = y(2,1) * v(1) + y(2,2) * v(2)
                       s(2) = v(2) * dconjg(a(2))
                       if (brsect(p) .eq. sect) then
                          pin = dreal (s(1)) * bmva
                          found = .true.
                       else
                          p = brnch_nxt(p)
                          v(1) = v(2)
                          a(1) = -a(2)
                          s(1) = -s(2)

                       endif
                    enddo
                    go to 140   
                 endif  
              endif 
           endif
           if (p .gt. 0) p = brnch_nxt(p)
        enddo
        if (.not. found) then
           write (errbuf(1), 130) jt, bus(k), base(k), bus(m), base(m),
     1        id, sect 
  130      format (' Phase shifter ',i4,1x,a8,f6.1,1x,a8,f6.1,1x,a1,
     1        1x,i1, ' is not in branch data ') 
           call prterx ('W',1) 
           pin = 0.0   
        endif
  140   if (ltc .ne. 0) then
           pviol = dim (pin, pmax) - dim (pmin, pin)
           write (outbuf,160) bus(k),base(k),bus(m),base(m),id,sect,
     1        angle, angmin, angmax, steps, pin, pmin, pmax, sen
  160      format ('0',a8,f6.1,1x,a8,f6.1,1x,a1,1x,i1,t38,3(f6.1,2x),   
     1        f4.0, t67, 3(f7.1,1x), t93, f8.3) 
        
           if (abs(pviol) .gt. 0.1) then
              write (outbuf(105:),170) pviol
  170         format ('P violation  =',f8.1)
              call prtout (1)   
              outbuf = ' '  
           endif
           if (angle .le. angmin + 0.01) then   
              outbuf(105:) = 'TMIN limit reached'   
              call prtout (1)   
              outbuf = ' '  
              if (abs(pviol) .gt. 0.1 .and. -pdisc .gt. 0.1) then   
                 ts = pviol / pdisc 
                 write (outbuf(105:),180) ts
  180            format ('Resolution =',f7.3,' steps')  
                 call prtout (1)
                 outbuf = ' '   
              endif 
           else if (angle .ge. angmax - 0.01) then  
              outbuf(105:) = 'TMAX limit reached'   
              call prtout (1)   
              outbuf = ' '  
              if (abs(pviol) .gt. 0.1 .and. -pdisc .lt. -0.1) then  
                 ts = pviol / pdisc 
                 write (outbuf(105:),180) ts
                 call prtout (1)
                 outbuf = ' '   
              endif 
           else if (abs(pviol) .gt. 0.1 .and. abs(pdisc) .gt. 0.1) then 
              ts = pviol / pdisc
              write (outbuf(105:),180) ts   
              call prtout (1)   
              outbuf = ' '  
           endif
           if (outbuf .ne. ' ') call prtout (1) 
        
        else
        
           write (outbuf,190) bus(k),base(k),bus(m),base(m),id,sect,
     1        angle, pin
  190      format ('0',a8,f6.1,1x,a8,f6.1,1x,a1,1x,i1,t38,f6.1, 
     1        t67, f7.1)
           call prtout(1)   
           outbuf = ' ' 
        
        endif   
  210   continue
        
        outbuf = '0End of Phase-shifter Summary '   
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
