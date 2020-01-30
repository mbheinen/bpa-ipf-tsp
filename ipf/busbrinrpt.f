C    @(#)busbrinrpt.f	20.4 11/12/98
c
        subroutine busbrinrpt (in_buffer, out_buffer, scrfil)
        character in_buffer *(*), out_buffer *(*)
        integer scrfil

        include 'ipfinc/parametr.inc'

        include 'ipfinc/alpha.inc'
        include 'ipfinc/arcntl.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/cbus.inc'
        include 'ipfinc/dtaiop.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/pqcurves.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/sortuvov.inc'
        include 'ipfinc/xdata.inc'

c       Function declarations

	integer apdoutbuf
        logical gtfltr
c    
	character capital * 120, 
     1            text * 120, 
     2            header(16) * 120, null * 1,
     3            linefeed * 1, lasttype * 2
	integer o2, ptr
        logical found, change_f

        data header(1) / 'Btc<O>< Bus  ><KV>ZN<Pld><Qld>Psh>Qsh><Pm>Pge

     1n>qmax>qmin><vh><vl><rembus><kv>%q>' /

        data header(2) / 'BDc<O>< Bus  ><KV>ZN< >Br<mH >Amin>Astp>Vdrp>

     1amps><combus><kv>' /

        data header(3) / 'BMc<O>< Bus  ><KV>ZN< >Br<mH >Amin>Astp>Vdrp>

     1amps><combus><kv>c<an<gm<pskd><vdc>' /
        data header(4) / 'L c<O>< Bus1 ><V1>M< Bus2 ><V2>CS<RT>N< R  ><

     1 x  ><g/2 ><b/2 ><mi><      >in>ou><th><bn>' /
        data header(5) / 'LDc<O>< Bus1 ><V1> < Bus2 ><V2>  <RT>< R  >< 

     1l  >< c  >d<psk><vdc>ast>gmn><mi>  <th><bn>' /
        data header(6) / 'LMc<O>< Bus1 ><V1> < Bus2 ><V2>  <RT>< R  >< 

     1l  >< c  >               <mi>in>ou><th><bn>' /
        data header(7) / 'T c<O>< Bus1 ><V1>M< Bus2 ><V2>CS<RT>N<  R ><

     1  x ><  g ><  b ><tp1><tp2><>in>ou><th><em><bn>' /
        data header(8) / 'R c<O>< Bus1 ><V1> < Bus2 ><V2>  < BusC ><Vc>

     1taph>tapl>#t<-------->' /
        data header(9) / 'RMc<O>< Bus1 ><V1> < Bus2 ><V2>  < BusC ><Vc>

     1angh>angl>#p<mwh><mwl>' /
        data header(10) / 'E c<O>< Bus1 ><V1>M< Bus2 ><V2>CS<RT>N< R  >

     1< x  >< g1 >< b1 >< g2 >< b2 >in>ou><th><bn>' /
        data header(11) / 'X c<O>< Bus1 ><V1>--< CBus ><CV>#<Inc>#<Inc>

     1#<inc>#<inc>' /
        data header(12) / 'I C< Area 1 >x< Area 2 >xx<MW 1-2> ' /
        data header(13) / 'A c< Area   ><Slack ><kV>x<Export>xZ1xZ2xZ3x

     1z4x' /
        data header(14) / '+tc<O>< Bus  ><KV>KY<Pld><Qld>Psh>Qsh>....Pg

     1en>qmax>qmin>' /
        data header(15) / 'TPc<O>< Bus1 ><V1>M< Bus2 ><V2>CS<RT>N<  R ><
     1  x ><  g ><  b ><ang><tp2><>in>ou><th><em><bn>' /
        data header(16) / 'QtcIdA< Bus  ><KV>un<MMVA<Pmax<  1 ><  2 ><
     &3 ><  4 ><  5 ><  6 ><  7 ><  8 ><  9 >< 10 >< 11 >< 12 >< 13 >< 1
     &4 >< 15>' /

        maxbuf_out = len( out_buffer ) - 400
        null = char(0)
        linefeed = char(10)
        out_buffer(1:1) = null
        o2 = 1
c
c       Set up pointers to P/Q curve data (buspqptr) and X data 
c       (busxdtptr).
c
        if (.not. pq_flag) then
           do nb = 1, ntot
              buspqptr(nb) = 0
           enddo

           do i = 1, numcurv
              ieq = pqbusptr(i)
              if (ieq .gt. 0) buspqptr(ieq) = i
           enddo
           pq_flag = .true.
        endif

        if (.not. xdt_flag) then
           do nb = 1, ntot
              busxdtptr(nb)  = 0
           enddo
           do i = 1, kxtot
              kxd = xdata(1,i)
              if (kxd .gt. 0) busxdtptr(kxd) = i
           enddo
           xdt_flag = .true.
        endif
c
c       Search and align to "WHERE" ...
c
        text = in_buffer
        i = index (capital(text), 'WHERE') ! Search WHERE in text 
C                                         ! because capital >= text
        found = .false.
        if (i .gt. 0) then
           i = i + len('WHERE')
           change_f = gtfltr(in_buffer(i:))
           found = .true.
        endif
        if (found) then
           ix = 1
           do while (ix .le. filter(6) .and. o2. le. maxbuf_out)
              nb = bus_filter(ix)
              call bcdbus (nb, text)
              if (text(1:2) .eq. 'BD') then
                 if (scrfil .gt. 0) write (scrfil, '(a)') header(2)
                 length = apdoutbuf(o2, header(2), 
     1                              out_buffer(o2:))
                 o2 = o2 + length
                 lasthdr = 1
              else if (text(1:2) .eq. 'BM') then
                 if (scrfil .gt. 0) write (scrfil, '(a)') header(3)
                 length = apdoutbuf(o2, header(3), 
     1                              out_buffer(o2:))
                 o2 = o2 + length
                 lasthdr = 2
              else
                 if (scrfil .gt. 0) write (scrfil, '(a)') header(1)
                 length = apdoutbuf(o2, header(1), 
     1                              out_buffer(o2:))
                 o2 = o2 + length
                 lasthdr = 3
              endif
              if (scrfil .gt. 0) write (scrfil, '(a)') text
              length = apdoutbuf(o2, text, 
     1                           out_buffer(o2:))
              o2 = o2 + length

              ncb = kbsdta(15,nb)
              do while (ncb .gt. 0)
                 if (scrfil .gt. 0 .and. lasthdr .ne. 14) then
                    write (scrfil, '(a)') header(14)
                    length = apdoutbuf(o2, header(14), 
     1                                     out_buffer(o2:))
                    o2 = o2 + length
                    lasthdr = 14
                 endif
                 call bcdcbs (ncb, text)
                 if (scrfil .gt. 0) write (scrfil, '(a)') text
                 length = apdoutbuf(o2, text, 
     1                              out_buffer(o2:))
                 o2 = o2 + length
                 ncb = bctbl_nxt(ncb)
              enddo
c
c             Process "X" record
c
              kxd = busxdtptr(nb)
              if (kxd .gt. 0) then
                 if (scrfil .gt. 0) write (scrfil, '(a)') header(11)
                 length = apdoutbuf(o2, header(11), 
     1                                     out_buffer(o2:))
                 o2 = o2 + length
                 lasthdr = 11
                 call bcdxdt (kxd, text)
                 if (scrfil .gt. 0) write (scrfil, '(a)') text
                 length = apdoutbuf(o2, text, out_buffer(o2:))
                 o2 = o2 + length
              endif
c
c             Loop through pqcurve data 
c
              kpqd = buspqptr(nb)
              if (kpqd .gt. 0) then
                 if (scrfil .gt. 0) write (scrfil, '(a)') header(16)
                 length = apdoutbuf(o2, header(16), out_buffer(o2:))
                 o2 = o2 + length
                 lasthdr = 16
                 call bcdqpd (kpqd, text)
                 if (scrfil .gt. 0) write (scrfil, '(a)') text
                 length = apdoutbuf(o2, text, out_buffer(o2:))
                 o2 = o2 + length
                 call bcdqxd (kpqd, text)
                 if (scrfil .gt. 0) write (scrfil, '(a)') text
                 length = apdoutbuf(o2, text, out_buffer(o2:))
                 o2 = o2 + length
                 call bcdqnd (kpqd, text)
                 if (scrfil .gt. 0) write (scrfil, '(a)') text
                 length = apdoutbuf(o2, text, out_buffer(o2:))
                 o2 = o2 + length
              endif
c
c             Loop through branch data 
c
              lasttype = ' '
              ptr = kbsdta(16,nb)
              do while (ptr .gt. 0)
                 ltyp = brtype(ptr)
                 if (ltyp .ne. 1) then
                    call bcdbrn (ptr, text)
                    if (lasttype .ne. text(1:2)) then
                       lasttype = text(1:2)
                       do j = 1, 15
                          if (header(j)(1:2) .eq. lasttype) then
                             if (scrfil .gt. 0) then
                                write (scrfil, '(a)') header(j)
                             endif
                             length = apdoutbuf(o2, header(j),
     1                                           out_buffer(o2:))
                             o2 = o2 + length
                             lasthdr = j
                          endif
                       enddo
                    endif
                    if (scrfil .gt. 0) write (scrfil, '(a)') text
                    length = apdoutbuf(o2, text, 
     1                                     out_buffer(o2:))
                    o2 = o2 + length
                 endif
                 ptr = brnch_nxt(ptr)
              enddo
              ix = ix + 1
           enddo

c*** remember maxbuf_out is really 400 less than the real buffer size
           if (o2 .gt. maxbuf_out) then
              write (out_buffer(o2:o2+8), 820) linefeed, null
  820         format (a, '*[MORE]', a)
              o2 = o2 + 9
           endif

        endif
  900   continue
        return
        end
