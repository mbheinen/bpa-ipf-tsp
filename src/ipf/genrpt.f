C    %W% %G%
C****************************************************************
C      File: genrpt.f
C
C      Purpose: Routine to obtain an IPS-style generator report.
C
C      Author: Merilyn George   November 1994
C      Called by: p_report.f
C
C****************************************************************
C
        subroutine genrpt (in_buf, out_buf, scrfil)

        character in_buf * (*), out_buf * (*)
        integer scrfil
c
c
c       Parameters:
c
c       in_buf - a character string specifying desired data
c       out_buf - a character string for storing data
c       error      - warning switch (0 means ignore errors,
c                                    1 means observe errors)
c
C       This routine obtains and applies a filter for a generator
C       listing.
 
        include 'ipfinc/parametr.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/arcntl.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/owncom.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/alpha.inc'
        include 'ipfinc/oldbus.inc'
        include 'ipfinc/sortuvov.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/lfiles.inc'

       	character  null * 1, linefeed * 1, qlimit * 1,
     &             type * 2, bustyp * 2
        integer o2, scrfilx

        logical gtfltr, chkfltr, change_f, repeat, finished
        integer apdoutbuf, findstr

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
        lastloop = 0
        if ( repeat ) then
           lastloop = loop
           scrfilx = 0
           go to 200
        endif

c       Set up the page header

        if (scrfilx. gt. 0) then
           outbuf = 'Generator Summary'
           call rpnlod
    
           write (outbuf, 100) chase1(1), chase1(34), chase1(35)
  100      format('Case: ', a10, ' Project: ', 2a10)
           call hedlod

        endif
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
        outbuf = '                    PGEN    PMAX    QGEN     QMAX     
     &QMIN    ACTUAL    DESIRED'
        length = apdoutbuf(o2, outbuf(1:80), out_buf(o2:))
        o2 = o2 + length
        if (scrfilx .gt. 0) call shdlod(1)
        outbuf = ' BUS NAME            MW      MW     MVAR     MVAR     
     &MVAR    VOLTAGE   VOLTAGE'
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

        nbx = lastloop + 1
        lastloop = 0
        finished = .false.
        totpgen = 0.0
        totqgen = 0.0
        totmarg = 0.0
        do while ( nbx .le. ntot_alf .and. .not. finished)
           nb = alf2inp(nbx)
c  This is doing it direct from the bus array. Could define type*2,
c          type = bustyp(nb) -- (bustyp is a function which returns the 
c                                2-char. bus type)
c          then check the alpha type instead of numeric.
           type = bustyp (nb)
 
c  Omit DC buses
           if (type .ne. 'BD' .and. type .ne. 'BM') then
c  Apply filter
             if ( chkfltr( arcnam(jarzn(nb)), zone(nb), owner(nb),
     1                     base(nb), type, nb )) then
c  Report only buses with generation
               kt = inp2opt(nb)
               if (pnetu(kt) + pnetu(kt) .gt. 0.01) then
                 kt = inp2opt(nb)
                 voltpu = dsqrt (e(kt) ** 2 + f(kt) ** 2)
                 pgen = bmva * (pnetu(kt) + ploadu(kt))
                 qgen = bmva * (qnetu(kt) + qloadu(kt))
                 qlimit = ' '
                 if (qgen .eq. busdta(9,nb)) qlimit = 'H'
                 if (qgen .eq. busdta(10,nb)) qlimit = 'L'
                 if (qgen .eq. 0.0) qlimit = ' '
                 totpgen = totpgen + pgen
                 totqgen = totqgen + qgen
                 totmarg = totmarg + busdta(7,nb)-pgen
 
c  For bus types with VHOLD, write it under Desired Voltage
                 if ( type .eq. 'BE' .or. type .eq. 'BS' .or. 
     &                type .eq. 'BQ' .or. type .eq. 'BC')  then
                   if (voltpu .gt. (busdta(11,nb)+0.0005) .or.
     &                 voltpu .lt. (busdta(11,nb)-0.0005)) then
                     write (outbuf, 120) bus(nb),base(nb),pgen,
     1                  busdta(7,nb),qgen,qlimit, busdta(9,nb),
     2                  busdta(10,nb),voltpu,busdta(11,nb)
  120                format(1x,a8, f5.1, 2x, 3f8.2,a1,2f8.2, 2f10.3)
                   else
                     write (outbuf, 120) bus(nb),base(nb),pgen,
     1                  busdta(7,nb),qgen,qlimit,busdta(9,nb),
     2                  busdta(10,nb), voltpu
                   endif
                 else
                   write (outbuf, 120) bus(nb),base(nb),pgen,
     1                    busdta(7,nb),qgen,qlimit,busdta(9,nb),
     2                    busdta(10,nb), voltpu
                 endif
                 if (scrfilx .gt. 0) call prtout(1)
                 if ( o2 .lt. maxbuf_out ) then
                   length = apdoutbuf(o2, outbuf(1:80), out_buf(o2:))
                   o2 = o2 + length
                   loop = nbx
                 elseif ( repeat ) then
                   finished = .true.
                   ibuf_ful = 1
                 elseif ( ibuf_ful .eq. 0 ) then
                   ibuf_ful = 1
                 endif
               endif
             endif
           endif
           nbx = nbx + 1
        enddo

        write (outbuf,150) totpgen, totqgen, totmarg
  150   format (' TOTALS:  PGEN = ',f8.2,'   QGEN = ',f8.2,
     &     '   PMAX-PGEN = ',f8.2)
        if ( o2 .lt. maxbuf_out ) then
           length = apdoutbuf(o2, outbuf(1:80), out_buf(o2:))
           o2 = o2 + length
        endif
        if (scrfilx .gt. 0) call prtout (1)
c
c*** remember maxbuf_out is really 400 less than the real buffer size
        if (o2 .gt. maxbuf_out) then
           write (out_buf(o2:o2+9), 820) linefeed, null
  820      format (a, '*[MORE]', a)
           o2 = o2 + 9
        endif

  900   continue
        return
        end
