C    @(#)phshftrpt.f	20.6 1/7/99
C****************************************************************
C
C   	File: phshftrpt.f
C
C   	Purpose: Generates a report of all phase shifters, both fixed
c                tap and LTC.
C
C   	Author: Walt Powell            Date: 13 January 1993
C   	Called by: p_report.f
C
C****************************************************************
C
      	integer function phshftrpt (in_buffer, out_buffer, scrfil) 
        integer scrfil

        include 'ipfinc/parametr.inc'

        character in_buffer * (MAXBUFFER)
        character out_buffer * (MAXBUFFER)

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
        complex * 16 y(2,2), v(2), a(2), s(2)
        logical found
	character null * 1, linefeed * 1, header(4) * 80, id * 1,
     &            text * 80
        integer error, sect, p, o2, apdoutbuf

        null = char(0)
        linefeed = char(10)
        phshftrpt = 0
        out_buffer(1:1) = null

C       Initialize counters/switched  

        error = 0 
        if (jphno .eq. 0) go to 900 

        write (header(1), 100) jphno, cspare(30), dte
  100   format (' Summary of phase shifters - Total ', i4, ' Case ', a, 
     &     ' date ', a)

        write (header(2), 110)
  110   format('0-------- Phase-shifter -------- -Phase (degrees)-  ----
     &- flow (mw) ----  dp/dt')

        write (header(3), 120)
  120   format(t30, 'P S    tap  min   max  actual    min    max (MW/d)'
     &)
        o2 = index (out_buffer,null)
        do i = 1, 3
           if (scrfil .gt. 0) write (scrfil, '(a)') header(i)
           length = apdoutbuf(o2, header(i), out_buffer(o2:))
           o2 = o2 + length
        enddo

        jt = 1
        do while (jt .le. jphno .and. o2 .le. MAXBUFFER-80)
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
C          Find LTC index. 
c
           found = .false.
           ltc = 0 
           i = 1
           do while (i .le. ntota .and. .not. found)
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
                 found = .true.
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
                 found = .true.
              else
                 i = i + 1
              endif
           enddo
C ***   
C ***      Find branch pi-equivalent   
C ***   
           p = kbsdta(16,k)
           found = .false.
           do while (p .gt. 0 .and. .not. found)
              if ((brtype(p) .eq. 1 .and. ky(p) .eq. m) .or.
     &            (brtype(p) .eq. 6 .and. ky(p) .eq. m)) then   
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
                          v(1) = v(2)
                          a(1) = -a(2)
                          s(1) = -s(2)
                          v(2) = (a(1) - y(1,1) * v(1)) / y(1,2)
                          a(2) = y(2,1) * v(1) + y(2,2) * v(2)
                          s(2) = v(2) * dconjg(a(2))
                          if (brsect(p) .eq. sect) then
                             v(2) = (a(1) - y(1,1) * v(1)) / y(1,2)
                             a(2) = y(2,1) * v(1) + y(2,2) * v(2)
                             s(2) = v(2) * dconjg(a(2))
                             pin = dreal (s(1)) * bmva
                             found = .true.
                          else
                             p = brnch_nxt(p)
                          endif
                       enddo
                       go to 140   
                    endif  
                 endif 
              endif
              if (p .gt. 0) p = brnch_nxt(p)
           enddo
           if (.not. found) then
              write (errbuf(1), 130) jt, bus(k), base(k), bus(m), 
     1           base(m), id, sect 
  130         format (' Phase shifter ',i4,1x,a8,f6.1,1x,a8,f6.1,1x,a1,
     1           1x,i1, ' is not in branch data ') 
              call prterx ('W',1) 
              pin = 0.0   
           endif
  140      if (ltc .ne. 0) then
              pviol = dim (pin, pmax) - dim (pmin, pin)
              write (text, 160) bus(k), base(k), bus(m), base(m), id,
     &           sect, angle, angmin, angmax, pin, pmin, pmax, sen
  160         format (t2, a8, f6.1, 1x, a8, f6.1, 1x, a1, 1x, i1, t32,
     &        f6.1, 2f7.1, t52, 3f7.1, f7.3) 
              if (scrfil .gt. 0) write (scrfil, '(a)') text
              length = apdoutbuf(o2, text, out_buffer(o2:))
              o2 = o2 + length
        
           else
        
              write (text, 190) bus(k), base(k), bus(m), base(m), id,
     &           sect, angle, pin
  190         format (t2, a8, f6.1, 1x, a8, f6.1, 1x, a1, 1x, i1, t32,
     &           f6.1, t52, f7.1)
              if (scrfil .gt. 0) write (scrfil, '(a)') text
              length = apdoutbuf(o2, text, out_buffer(o2:))
              o2 = o2 + length
        
           endif   

           jt = jt + 1
        enddo
        
  900   continue
        
        return  
        end 
