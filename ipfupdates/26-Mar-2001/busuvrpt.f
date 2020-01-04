C    %W% %G%
C****************************************************************
C
C      File: busuvrpt.f
C
C      Purpose: Routine to obtain a filtered bus under voltage
C               report.
C
C      Author: Merilyn George   Date: Dec 1994
C      Called by: p_report.f
C
C****************************************************************
C
        subroutine busuvrpt (in_buf, out_buf, scrfil)

        character in_buf * (*), out_buf * (*)
        integer scrfil
c
c       This subroutine returns output formatted records of filtered
c       buses with voltages under a user-specified value.
c
c       Output parameter:
c
c       in_buf - a character string specifying desired data
c       out_buf - a character string for storing data
c       error      - warning switch (0 means ignore errors,
c                                    1 means observe errors)
c
C       This routine obtains and applies a filter for a bus input 
C       listing.
 
        include 'ipfinc/parametr.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/arcntl.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/owncom.inc'
        include 'ipfinc/alpha.inc'
        include 'ipfinc/oldbus.inc'
        include 'ipfinc/sortuvov.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/lfiles.inc'

        character type * 2, zn * 2, own * 3,
     1            bustyp * 2,  null * 1,
     2            linefeed * 1
        integer o2, scrfilx

        external kmpuvov, swapuvov
        logical gtfltr, chkfltr, change_f, repeat, finished
        integer kmpuvov, apdoutbuf, findstr

        save

        null = char(0)
        linefeed = char(10)
        out_buf(1:1) = null
        o2 = index (out_buf,null)
        maxbuf_in = len( in_buf )
        maxbuf_out = len( out_buf ) - 400
        last = index( in_buf, null )
        if ( last .eq. 0 ) last = maxbuf_in
        ibuf_ful = 0

        i = last
        if ( i .gt. 50 ) i = 50
        repeat = ( findstr( in_buf(1:i), 'CONTINUE') .ne. 0) 
        scrfilx = scrfil 
        if ( repeat ) then
           scrfilx = 0
           go to 200
        endif
        nextloop = 1
c
c       Search and align to "WHERE" ...
c
        ix = findstr (in_buf, 'WHERE') ! findstr is a case-insensitive
c                                        ! version of index
        if (ix .gt. 0) then
           ix = ix + len('WHERE')
           change_f = gtfltr(in_buf(ix:))
        else
           do i = 1, 7
              filter(i) = 0
           enddo
        endif
C                                                                       
        if (filter(7) .eq. 0) then
           outbuf = ' No voltage limit specified.'
           length = apdoutbuf(o2, outbuf(1:80), out_buf(o2:))
           return
        else
           v_tol = range_filter(1)
        endif

C       sortsw: 0 - Sorted by bus names.                      
C               1 - Sorted by area-bus names.              
C                                                                       
        if (sortsw .eq. 0) then
           do i = 1, ntot
              j = alf2inp(i)
              vltsrt(i) = j
           enddo
           sortsw = 1
           if (ntotc .gt. 0) call qiksrt (1, ntot, kmpuvov, swapuvov)  
        endif
                                                                        
c       Set up the page header

        if (scrfilx. gt. 0) then
           outbuf = 'Bus Undervoltage Summary'
           call rpnlod
    
           write (outbuf, 100) chase1(1), chase1(34), chase1(35)
  100      format('Case: ', a10, ' Project: ', 2a10)
           call hedlod

        endif
        write (outbuf,110)
 110    format (t6,'Own Type Bus Name  Base  Zone     Voltage')
        length = apdoutbuf(o2, outbuf(1:80), out_buf(o2:))
        o2 = o2 + length
        if (scrfilx .gt. 0) call shdlod(1)
        write (outbuf,120)
 120    format (t39,'KV   P.U.')
        length = apdoutbuf(o2, outbuf(1:80), out_buf(o2:))
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
           call prnt_fltr (in_buf(ix:))
        endif

  200   continue

        nbx = nextloop
        finished = .false.
        do while ( nbx .le. ntot_alf .and. .not. finished)
           nb = vltsrt(nbx) 
           type = bustyp(nb)
           if ( chkfltr( arcnam(jarzn(nb)), zone(nb), owner(nb),
     1                   base(nb), type, nb )) then
              kt = inp2opt(nb)
              voltpu = sqrt (e(kt) ** 2 + f(kt) ** 2)
              if (voltpu .lt. v_tol) then
                 type = bustyp (nb)
                 zn = zone(nb)                                          
                 own = owner(nb)                                        
                 voltkv = base(nb) * voltpu                           
                 write (outbuf, 130) owner(nb), bustyp(nb), bus(nb),
     1                  base(nb), zone(nb), voltkv, voltpu
  130            format(6x, a3, 2x, a2, 2x, a8, f7.1, 2x, a2,
     1                  f8.2, f7.4)
                 if (o2 .lt. maxbuf_out) then
                    length = apdoutbuf(o2, out_buf(1:80), 
     &                                 out_buf(o2:))
                    o2 = o2 + length
                 else if (repeat) then
                    finished = .true.
                 endif
                 if (scrfilx .gt. 0) call prtout (1)
              endif
           endif
           nbx = nbx + 1
        enddo

c*** remember maxbuf_out is really 400 less than the real buffer size
        if (o2 .gt. maxbuf_out) then
           write (out_buf(o2:o2+9), 820) linefeed, null
  820      format (a, '*[MORE]', a)
           o2 = o2 + 9
        endif

  900   continue
        return
        end
