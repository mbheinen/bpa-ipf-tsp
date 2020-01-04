C    %W% %G%
C****************************************************************
C
C       File: businrpt.f
C
C       Purpose: Routine to obtain filtered bus input data.
C
C       Author: Walt Powell  Date: 18 May 1992
C       Called by: p_gtdata.f
C
C****************************************************************
C
        subroutine businrpt (in_buf, out_buf, scrfil)

        character in_buf * (*), out_buf * (*)
        integer scrfil
c
c       This subroutine returns WSCC-formated input data records.
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
      include 'ipfinc/arcntl.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/owncom.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/sortuvov.inc'
      include 'ipfinc/prt.inc'

        character  text*120, type*2,
     1             bustyp*2, null*1, linefeed*1
        logical gtfltr, chkfltr, change_f, repeat, finished
        external  gtfltr, chkfltr, bustyp
        character header(13) * 80
        integer o2, apdoutbuf, findstr, scrfilx
 
        save

        data header(1) / 'Btc<O>< Bus  ><KV>ZN<Pld><Qld>Psh>Qsh><Pm>Pge
     1n>qmax>qmin><vh><vl><rembus><kv>%q>' /

        data header(2) / 'BDc<O>< Bus  ><KV>ZN< >Br<mH >Amin>Astp>Vdrp>
     1amps><combus><kv>' /

        data header(3) / 'BMc<O>< Bus  ><KV>ZN< >Br<mH >Amin>Astp>Vdrp>
     1amps><combus><kv>c<an<gm<pskd><vdc>' /
        data header(4) / 'L c<O>< Bus1 ><V1>M< Bus2 ><V2>CS<RT>N< R  ><
     1 x  ><g/2 ><b/2 ><mi><      >in>ou>' /
        data header(5) / 'LDc<O>< Bus1 ><V1> < Bus2 ><V2>  <RT>< R  >< 
     1l  >< c  >d<psk><vdc>ast>gmn><mi>' /
        data header(6) / 'LMc<O>< Bus1 ><V1> < Bus2 ><V2>  <RT>< R  >< 
     1l  >< c  >               <mi>in>ou>' /
        data header(7) / 'Tsc<O>< Bus1 ><V1>M< Bus2 ><V2>CS<RT>N<  R ><
     1  x ><  g ><  b ><tp1><tp2>' /
        data header(8) / 'R c<O>< Bus1 ><V1> < Bus2 ><V2>  < BusC ><Vc>
     1taph>tapl>#t<-------->' /
        data header(9) / 'RMc<O>< Bus1 ><V1> < Bus2 ><V2>  < BusC ><Vc>
     1angh>angl>#p<mwh><mwl>' /
        data header(10) / 'E c<O>< Bus1 ><V1>M< Bus2 ><V2>CS<RT>N< R  >
     1< x  >< g1 >< b1 >< g2 >< b2 >in>ou>' /
        data header(11) / 'X c<O>< Bus1 ><V1>--< CBus ><CV>#<Inc>#<Inc>
     1#<inc>#<inc>' /
        data header(12) / 'I C< Area 1 >x< Area 2 >xx<MW 1-2> ' /
        data header(13) / 'A c< Area   ><Slack ><kV>x<Export>xZ1xZ2xZ3x
     1z4x' /

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

c       Set up the page header

        if (scrfilx. gt. 0) then
           outbuf = 'Bus Input Listing'
           call rpnlod
    
           write (outbuf, 90) chase1(1), chase1(34), chase1(35)
   90      format('Case: ', a10, ' Project: ', 2a10)
           call hedlod

           outbuf = ' '
           call shdlod(2)
           call shdlod(3)
           call shdlod(4)
           call shdlod(5)
           call comlod(1)
           call comlod(2)
           call forbtm()
           call fortop()
           call prnt_fltr (in_buf(ix:))
        endif

        do i = 1, 3
           if (scrfilx .gt. 0) write (scrfilx, '(a)') header(i)
           length = apdoutbuf(o2, header(i), 
     1                            out_buf(o2:))
           o2 = o2 + length
        enddo


  100   continue

        ib = nextloop
        finished = .false.
        do while ( ib .le. ntot_alf .and. .not. finished)

           nb = alf2inp(ib)
           type = bustyp(nb)
           if (chkfltr ( arcnam(jarzn(nb)), zone(nb), owner(nb), 
     &                   base(nb), type, nb)) then
              call bcdbus(nb, text)
              if ( text(1:2) .eq. 'BM' ) then
                 ibtyp = 3
              elseif ( text(1:2) .eq. 'BD' ) then
                 ibtyp = 2
              else
                 ibtyp = 1
              endif
              if ( ibtyp .ne. last_ibtyp ) then
                 outbuf = header(ibtyp)
                 if (scrfilx .gt. 0) call prtout(1)
                 if ( o2+80 .lt. maxbuf_out ) then
                    length = apdoutbuf(o2, outbuf(1:80), out_buf(o2:))
                    o2 = o2 + length
                 elseif ( repeat ) then
                    nextloop = ib
                    finished = .true.
                    ibuf_ful = 1
                 elseif ( ibuf_ful .eq. 0 ) then
                    nextloop = ib
                    ibuf_ful = 1
                 endif
              endif
              last_ibtyp = ibtyp
              outbuf = text
              if (scrfilx .gt. 0) call prtout(1)
              if ( o2+80 .lt. maxbuf_out ) then
                 length = apdoutbuf(o2, outbuf(1:80), out_buf(o2:))
                 o2 = o2 + length
              elseif ( repeat ) then
                 nextloop = ib
                 finished = .true.
                 ibuf_ful = 1
              elseif ( ibuf_ful .eq. 0 ) then
                 nextloop = ib
                 ibuf_ful = 1
              endif
           endif
           ib = ib + 1
        enddo

c*** remember maxbuf_out is really 400 less than the real buffer size
        if ( o2+80 .ge. maxbuf_out ) then
           length = apdoutbuf(o2, '*[MORE]' , out_buf(o2:))
           o2 = o2 + length
        endif

        return
        end
