C    %W% %G%
C****************************************************************
C
C   	File: regxrpt.f
C
C   	Purpose: Generates a filtered report of regulating transformers
C
C   	Author: M. George            Date: November 1994
C   	Called by: p_report.f
C
C****************************************************************
C
      	integer function regxrpt (in_buffer, out_buffer, scrfil)

        character in_buffer *(*), out_buffer *(*)
        integer scrfil
C
        include 'ipfinc/parametr.inc'

        include 'ipfinc/alpha.inc'
        include 'ipfinc/arcntl.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/ecvar.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/phase.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/slnopt.inc'
        include 'ipfinc/ltcsln.inc'
        include 'ipfinc/tran.inc'
        include 'ipfinc/sortuvov.inc'
C
        character  tap1*6, tap2*6
        character null * 1, linefeed * 1
        integer  o2, apdoutbuf, findstr, scrfilx
        logical gtfltr, chkfltr, change_f, repeat, finished
 
c       diftol (a,b) = abs ((a - b) / b)

        save

        regxrpt = 0

        null = char(0)
        linefeed = char(10)
        last = index (in_buffer, null)
        if ( last .eq. 0 ) last = len(in_buffer)
        out_buffer(1:1) = null
        o2 = 1
        maxbuf_out = len( out_buffer ) - 400
c
c       Check for re-entry and continue
c
        i = last
        if ( i .gt. 50 ) i = 50
        repeat = (findstr (in_buffer(1:i), 'CONTINUE') .ne. 0) 
        if (repeat) then
           lastloop = loop
           scrfilx = 0
           go to 200
        else
           lastloop = 0
           scrfilx = scrfil
c
c          Search and align to "WHERE" ...
c
           ix = findstr (in_buffer(1:last), 'WHERE')
           if (ix .gt. 0) then
              ix = ix + len('WHERE')
              change_f = gtfltr (in_buffer(ix:last))
           else
              do i = 1, 7
                 filter(i) = 0
              enddo
           endif
 
C          Initialize counters/switched  


c          Set up the page header

           if (scrfilx. gt. 0) then
              outbuf = 'Summary of Regulating Transformers'
              call rpnlod
    
              write (outbuf, 100) chase1(1), chase1(34), chase1(35)
  100         format('Case: ', a10, ' Project: ', 2a10)
              call hedlod

           endif

           write (outbuf, 110)
  110      format(34x,'Controlled     Desired  Actual')
           length = apdoutbuf(o2, outbuf(1:80), out_buffer(o2:))
           o2 = o2 + length
           if (scrfilx .gt. 0) call shdlod(1)
           write (outbuf, 120)
  120      format('  From Bus       To Bus',t38,'Bus',t51,'Value',
     &        t59,'Value',t69,'Final Tap')
           length = apdoutbuf(o2, outbuf(1:80), out_buffer(o2:))
           o2 = o2 + length
           if (scrfilx .gt. 0) call shdlod(2)

           if (scrfilx .gt. 0) then
              outbuf = ' '
              call shdlod(3)
              call shdlod(4)
              call shdlod(5)
              call comlod(1)
              call comlod(2)
              call forbtm()
              call fortop()
              call prnt_fltr (in_buffer(ix:))
           endif
        endif

  200   if (ntota .eq. 0) go to 900
        jt = lastloop + 1
        lastloop = 0
        finished = .false.
        do while ( jt .le. ntota .and. .not. finished)
           k = ltran(1,jt)
           kt = inp2opt(k)
           m = ltran(9,jt)
           mt = inp2opt(m)
           ltype = ltran(10,jt)
 
           if (ltype .eq. 1) then    ! direct regulation of voltage
 
c  Filter by area or zone on either end of xfmr
              if (chkfltr (arcnam(jarzn(k)), zone(k), '***', 0.0,
     &                     '**', 0) .or.
     &            chkfltr (arcnam(jarzn(m)), zone(m), '***', 0.0,
     &                     '**',0)) then
 
                 vk = sqrt(e(kt)**2 + f(kt)**2)   
                 vm = sqrt(e(mt)**2 + f(mt)**2)   
                 tapk = tran(6,jt) * base(k)  
                 tapm = tran(6,jt) / tap(jt) * base(m)
                 kc = ltran(2,jt) 
                 if (kc .eq. -1) then 
                    n = k 
                    nt = kt   
                    vn = vk   
                 else if (kc .eq. -2) then
                    n = m 
                    nt = mt   
                    vn = vm   
                 else 
                    n = kc
                    nt = inp2opt(kc)
                    dv = 0.0  
                    vn = sqrt (e(nt)**2 + f(nt)**2)   
                 endif
        
                 if (tapk .lt. 1000.0) then   
                    write (tap1, 156) tapk
  156               format (f6.2) 
                 else 
                    write (tap1, 158) tapk
  158               format (f6.1) 
                 endif
                 if (tapm .lt. 1000.0) then   
                    write (tap2, 156) tapm
                 else 
                    write (tap2, 158) tapm
                 endif
c
                 write (outbuf, 160) bus(k), base(k), bus(m), base(m),
     &              bus(n),base(n),vlimx(nt),vn,tap1,tap2
  160            format (t2, a8, f6.1, 1x, a8, f6.1, 3x, a8, f6.1,
     &              2f8.4,3x,a, '/', a)
   
                 if ( o2 .lt. maxbuf_out ) then
                    length = apdoutbuf(o2, outbuf(1:80),out_buffer(o2:))
                    o2 = o2 + 80
                    loop = jt
                 elseif ( repeat ) then
                    finished = .true.
                    ibuf_ful = 1
                 elseif ( ibuf_ful .eq. 0 ) then
                    ibuf_ful = 1
                 endif
                 if (scrfilx .gt. 0) call prtout(1)
              endif   ! type = 1
           endif   ! chkfltr
           jt = jt + 1
        enddo   ! jt = 1,ntota
        
  900   continue
        
c*** remember maxbuf_out is really 400 less than the real buffer size
        if (o2 .gt. maxbuf_out) then
           write (out_buffer(o2:o2+9), 820) linefeed, null
  820      format (a, '*[MORE]', a)
           o2 = o2 + 9
        endif
c
        return  
        end 
