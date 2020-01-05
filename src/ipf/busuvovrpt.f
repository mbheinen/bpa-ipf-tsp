C    %W% %G%
C****************************************************************
C
C      File: busuvovrpt.f
C
C      Purpose: Routine to obtain a filtered bus under/overvoltage
C               report.
C
C      Author: Walt Powell  Date: 18 May 1992
C      Called by: p_report.f
C
C****************************************************************
C
        subroutine busuvovrpt (in_buf, out_buf, scrfil)

        character in_buf * (*), out_buf * (*)
        integer scrfil
c
c       This subroutine returns output formatted records of filtered
c       bus unver/overvoltages.
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
c
        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/arcntl.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/owncom.inc'
        include 'ipfinc/alpha.inc'
        include 'ipfinc/oldbus.inc'
        include 'ipfinc/sortuvov.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/ordsta.inc'
c
	character type*2, relvlt*5, zn*2, own*3, 
     1            bustyp*2, null*1, 
     2            linefeed*1, header(4)*80, text* 80
        integer o2, scrfilx

	external kmpuvov, swapuvov
        logical gtfltr, chkfltr, change_f, repeat, finished
        integer kmpuvov, oldbsh, apdoutbuf, findstr

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
           go to 100
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
C       sortsw: 0 - Sorted by bus names.                      
C               1 - Sorted by area-bus names.              
C                                                                       
        if (sortsw .eq. 0) then
           do i = 1, ntot
              ix = alf2inp(i)
              vltsrt(i) = ix
           enddo
           sortsw = 1
           if (ntotc .gt. 0) call qiksrt (1, ntot, kmpuvov, swapuvov)  
        endif
                                                                        
        header(1) = ' Own Zone Bus name  Base  Type     Voltage' //
     1        '   Relative  Voltage range  Violation' 
        header(2) = '                                  KV    PU      % 
     1   minimum  maximum    PU' 
        header(3) = '                                                  
     1    PU         PU'
        header(4) = ' '

        do i = 1, 4
           if (scrfilx .gt. 0) write (scrfilx, '(a)') header(i)
           length = apdoutbuf(o2, header(i), 
     1                            out_buf(o2:))
           o2 = o2 + length
        enddo

        if (filter(7) .eq. 0) then
           v_tol = 0.0
        else if (range_filter(1) .gt. 0) then
           v_tol = range_filter(1)
        else if (range_filter(2) .gt. 0) then
           v_tol = range_filter(2)
        else
           v_tol = 0.0
        endif

  100   continue

        ix = nextloop
        finished = .false.
        do while ( ix .le. ntot_alf .and. .not. finished)
           nb = vltsrt(ix) 
           type = bustyp(nb)
           if ( chkfltr( arcnam(jarzn(nb)), zone(nb), owner(nb),
     1                   base(nb), type, nb )) then
              if (ordvlt .eq. 1) then
                 kt = nb
              else
                 kt = inp2opt(nb)
              endif
              voltpu = dsqrt (e(kt) ** 2 + f(kt) ** 2)
              call glbvlt(nb, vmin, vmax)
              dvkv = dim(voltpu,vmax) - dim(vmin,voltpu)  
              if (abs(dvkv) .lt. v_tol) dvkv = 0.0
              if (dvkv .ne. 0) then                                       
                 degree = 57.29578 * datan2 (f(kt), e(kt))
                 type = bustyp (nb)
                 zn = zone(nb)                                          
                 own = owner(nb)                                        
                 voltkv = base(nb) * voltpu                           
                 if (numobs .le. 0) then                            
                    relvlt = ' '                                    
                 else                                               
                    ij = oldbsh (bus(nb), base(nb))                  
                    if (ij .gt. 0) then                              
                       vold = sqrt (olde(ij)**2 + oldf(ij)**2)  
                       ivrel = 100.0 * voltpu / vold + 0.5
                       write (relvlt, 110) ivrel                    
  110                  format ('(', i3, ')')                        
                    else                                            
                       relvlt = ' '                                 
                    endif                                           
                 endif                                              
                 write (text, 120) owner(nb), zone(nb), bus(nb),     
     1                  base(nb), type, voltkv, voltpu, relvlt, 
     2                  vmin, vmax, dvkv  
  120            format(1x, a3, 2x, a2, 2x, a8, f7.1, 2x, a2, 
     1                  f8.2, f7.4, 2x, a5, f9.4, f8.4, f9.4)
                 if (scrfilx .gt. 0) write (scrfilx, '(a)') text
                 if ( o2+80 .lt. maxbuf_out ) then
                    length = apdoutbuf(o2, text, out_buf(o2:))
                    o2 = o2 + length
                 elseif ( repeat ) then
                    nextloop = ix
                    finished = .true.
                    ibuf_ful = 1
                 elseif ( ibuf_ful .eq. 0 ) then
                    nextloop = ix
                    ibuf_ful = 1
                 endif
              endif
           endif
           ix = ix + 1
        enddo

c*** remember maxbuf_out is really 400 less than the real buffer size
        if (o2 .gt. maxbuf_out) then
           length = apdoutbuf(o2, '*[MORE]' , out_buf(o2:))
           o2 = o2 + length
        endif

  900   continue
        return
        end
