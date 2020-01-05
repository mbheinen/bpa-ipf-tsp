C    %W% %G%
C****************************************************************
C
C   File: aredifrpt.f
C
C   Purpose: Routine to obtain a filtered line quantity comparison 
C            report.
c
C   Invoked by:
C              / REPORTS, SELECT AR_COMP_EXPORT_MW
C              / REPORTS, SELECT AR_COMP_EXPORT_MVAR
C              / REPORTS, SELECT AR_COMP_FLOW_MW
C              / REPORTS, SELECT AR_COMP_FLOW_MVAR
C              / REPORTS, SELECT AR_COMP_GEN_MW
C              / REPORTS, SELECT AR_COMP_GEN_MVAR
C              / REPORTS, SELECT AR_COMP_LOAD_MW
C              / REPORTS, SELECT AR_COMP_LOAD_MVAR
C              / REPORTS, SELECT AR_COMP_LOSS_MW
C              / REPORTS, SELECT AR_COMP_LOSS_MVAR
C
C   Author: Walt Powell  Date: 3 Aug 1994
C   Called by: p_report.f
C
C****************************************************************
C
        integer function aredifrpt (in_buffer, out_buffer, scrfil)
        character in_buffer *(*), out_buffer *(*)
        integer scrfil

        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/arcntl.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/owncom.inc'
        include 'ipfinc/alpha.inc'
        include 'ipfinc/sortuvov.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/qksrt.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/alt_case.inc'

        common /file_name/ filename
        character filename*60

        common /scratch/ numdiff, ixref(MAXBUS), kdiff(2,MAXBUS), 
     &                   fdiff(6,MAXBUS), cdiff(2,MAXBUS)
        character cdiff * 1

	character type*2, relvlt*3, capital*80, null*1, linefeed*1, 
     &            header(4)*80, text*80, word(20)*60, dict(10)*10, 
     &            unit_diff(3,11)*10
        integer o2, sect, ptr1, ptr2, findstr, rpt_type, 
     &          scrfilx, loaded
        logical found, repeat, finished

	external kmpvltdif, swpvltdif
        logical gtfltr, change_f
        integer apdoutbuf, tempfile, ldaltbse, status, kmpvltdif
        character tag1*1, tag2*1, id*1, brntyp*2, bus_1c*8, bus_2c*8,
     &            own*3, code*4, base_1c*4, base_2c*4

        save

        data dict / 'EXPORT_MW', 'EXPORT_MVAR', 'FLOW_MW', 'FLOW_MVAR',
     &              'GEN_MW', 'GEN_MVAR', 'LOAD_MW', 'LOAD_MVAR', 
     &              'LOSS_MW', 'LOSS_MVAR' /

        data (unit_diff(1,i), i=1,10) 
     &    / '(MW)' , '(MVAR)', '(MW)', '(MVAR)', '(MW)', '(MVAR)',
     &      '(MW)' , '(MVAR)', '(MW)', '(MVAR)' /
        data (unit_diff(2,i), i=1,6)
     &    / '(p.u.)' , '(MVAR)', '(MW)', '(MVAR)', '(MW)', '(MVAR)' /
        data (unit_diff(3,i), i=1,11)
     &    / '(percent)', '(MW)', '(MVAR)', '(MW)', '(MVAR)', '(p.u.)', 
     &       '(p.u.)', '(KV)', '(KV)', '(p.u.)', '(p.u.)' /

        maxbuf_out = len( out_buffer ) - 400
        null = char(0)
        linefeed = char(10)

        aredifrpt = 0
c
c       Check for re-entry and continue
c
        last = index( in_buffer, null )
        i = last
        if ( i .gt. 50 ) i = 50
        if (findstr (in_buffer(1:i), 'CONTINUE') .ne. 0) then
           repeat = .true.
           scrfilx = 0
           lastloop = loop
           go to 250
        endif

        repeat = .false.
        scrfilx = scrfil
        lastloop = 0
c
c       Search for subtype of report
c
        ix = 1
        i1 = 1
        i2 = index (in_buffer, null)
        call uscan (in_buffer(i1:i2), word, nwrd, '=', ', ' // linefeed)
c
c       Search for subtype of report
c
        iwrd = 1
        do while (iwrd .le. nwrd .and. 
     &            (findstr(word(iwrd), 'AR_COMP_') .eq. 0))
           iwrd = iwrd + 1
        enddo
        found = .false.
        if (iwrd .le. nwrd) then
           i = findstr(word(iwrd), 'AR_COMP_') 
           text = capital (word(iwrd)(i+8:))
           rpt_type = 1
           do while (rpt_type .le. 10 .and. .not. found)
              if (dict(rpt_type) .eq. text) then
                 found = .true.
              else
                 rpt_type = rpt_type + 1
              endif
           enddo
        endif
        if (.not. found) go to 900

        iwrd = 1
        do while (iwrd .lt. nwrd .and. 
     &            (capital(word(iwrd)) .ne. 'FILE'))
           iwrd = iwrd + 1
        enddo
        if (iwrd .ge. nwrd .and. 
     &     (ofilename .eq. ' ' .or. ofilename(1:1) .eq. null)) then
           write (errbuf(1), 90)
   90      format(' No alternate base case loaded ')
           call prterx ('W', 1)
           aredifrpt = 1
           filename = ' '
           go to 900
        else if (capital(word(iwrd)) .ne. 'FILE') then
           filename = ofilename
           go to 110
        endif
        iwrd = iwrd + 1
        if (word(iwrd) .eq. '=') iwrd = iwrd + 1
        filename = word(iwrd)
        tempfile = 20           
        ierr = 99    ! Indicate that input file is binary formatted
        call opnfil (tempfile, filename, ierr)
        if (ierr .ne. 0) then
           write (errbuf(1), 100)
  100      format(' Alternate base case cannot be opened ')
           write (errbuf(2), 102) filename
  102      format(' File ', a)
           call prterx ('W', 2)
           aredifrpt = 1
           go to 900
        endif
c
c       Function ldaltbse (load alternate base) installs "ocase"
c       and "ofilename"
c
        ocase = ' '
        write (*, 104)
  104	format (' * Loading alternate base file - this will take a minute.
     &')
        status = ldaltbse (tempfile, filename, ocase, loaded)
        if (status .ne. 0) then
           write (errbuf(1), 106)
  106      format(' Alternate base case cannot be loaded ')
           write (errbuf(2), 108) filename
  108      format(' File ', a)
           call prterx ('W', 2)
           aredifrpt = 1
           go to 900
        endif
c
c       Align in_buffer past <filename>
c
        last = lastch(word(iwrd))
        ix = index (in_buffer, word(iwrd)(1:last)) + last
c
c       Search in_buffer and align past "WHERE" ...
c
  110   found = .false.
        i = findstr (in_buffer(ix:), 'WHERE') ! findstr is a 
c                                             ! case-insensitive
c                                             ! version of index
        if (i .gt. 0) then
           ix = ix + i + len('WHERE') - 1
           change_f = gtfltr(in_buffer(ix:))
           found = .true.
        endif

        if (.not .found) then
           do i = 1, 7
              filter(i) = 0
           enddo
        endif
        flowdiff = range_filter(1)
        numdiff = 0
        out_buffer(1:1) = null   
        o2 = index (out_buffer,null)

  230   if (numdiff .gt. 0) then
           do i = 1, numdiff
              ixref(i) = i
           enddo
c
c          Set "key" = 2 to inform kmpvltdif that kdiff(1,*) and
c          kdiff(2,*) are branch indices.
c
           key = 2
           call qiksrt (1, numdiff, kmpvltdif, swpvltdif)

           last = lastch (dict(rpt_type))
           write (header(1), 240) dict(rpt_type)(1:last), cspare(30), 
     &        ocase
  240      format (' Area difference report ',  a, t44, a, t59, a)
           write (header(2), 242) dict(rpt_type)(1:last), 
     &                            dict(rpt_type)(1:last)
  242      format (' Ty Own Bus1         Bus2         IS  ', t44, a, 
     &             t59, a, t70, 'dif')
 
           last = lastch (unit_diff(3,rpt_type))
           write (header(3), 244) unit_diff(3,rpt_type)(1:last), 
     &                            unit_diff(3,rpt_type)(1:last), 
     &                            unit_diff(3,rpt_type)(1:last)
  244      format (t44, a, t59, a, t70, a)

           do i = 1, 3
              if (scrfil .gt. 0) write (scrfil, '(a)') header(i)
              length = apdoutbuf(o2, header(i), out_buffer(o2:))
              o2 = o2 + length
           enddo
        endif

  250   ix = lastloop
        if (ix .eq. 0) ix = 1
        loop = ix

        finished = .false.
        do while (ix .le. numdiff .and. .not. finished)
           i = ixref(ix)
           ptr1 = kdiff(1,i)           
           ptr2 = kdiff(2,i)         
           quan1 = fdiff(2,i)
           quan2 = fdiff(3,i)
           tag1 = cdiff(1,i)
           tag2 = cdiff(2,i)
           if (ptr1 .gt. 0 .and. ptr2 .gt. 0) then
              relvlt = ' '
              k1 = kx(ptr1)
              k2 = ky(ptr1)
              id = brid(ptr1)
              sect = brsect(ptr1)
              type = brntyp(brtype(ptr1))
              nbr = iabs(brnch_ptr(ptr1))
              call getchr (3, own, kbrnch(3,nbr))
              bus_1c = bus(k1)
              bus_2c = bus(k2)
              base_1c = code (base(k1), 4, 0)
              base_2c = code (base(k2), 4, 0)
           else if (ptr1 .gt. 0) then
              relvlt = '(1)'
              k1 = kx(ptr1)
              k2 = ky(ptr1)
              id = brid(ptr1)
              sect = brsect(ptr1)
              type = brntyp(brtype(ptr1))
              nbr = iabs(brnch_ptr(ptr1))
              call getchr (3, own, kbrnch(3,nbr))
              bus_1c = bus(k1)
              bus_2c = bus(k2)
              base_1c = code (base(k1), 4, 0)
              base_2c = code (base(k2), 4, 0)
           else 
              relvlt = '(2)'
              k1 = okx(ptr2)
              k2 = oky(ptr2)
              id = obrid(ptr2)
              sect = obrsect(ptr2)
              type = brntyp(obrtype(ptr2))
              nbr = iabs(obrnch_ptr(ptr2))
              call getchr (3, own, okbrnch(3,nbr))
              bus_1c = oldbus(k1)
              bus_2c = oldbus(k2)
              base_1c = code (oldbase(k1), 4, 0)
              base_2c = code (oldbase(k2), 4, 0)
           endif
           write (text, 290) type, own, bus_1c, base_1c,
     &                       bus_2c, base_2c, id, sect,
     &                       quan1, quan2, quan1-quan2, relvlt
  290      format(1x, a2, 1x, a3, 1x, a8, a4, 1x, a8, a4, 1x, a1,
     &            i1, t43, f8.1, t57, f8.1, t66, f8.1, 1x, a3)
           if (o2 .lt. maxbuf_out) then
              length = apdoutbuf(o2, text(1:80), out_buffer(o2:))
              o2 = o2 + length
              loop = ix
           else if (repeat) then
              finished = .true.
           endif
           if (scrfilx .gt. 0) write (scrfilx, '(a)') text
           ix = ix + 1
        enddo

c*** remember maxbuf_out is really 400 less than the real buffer size

        if (o2 .gt. maxbuf_out) then
           write (out_buffer(o2:o2+8), 820) linefeed, null
  820      format (a, '*[MORE]', a)
           o2 = o2 + 9
        endif

  900   continue
        return
        end
