C    %W% %G%
C****************************************************************
C
C   File: areaintrpt.f
C
C   Purpose: Routine to obtain an area interchange report
c
C   Author: Walt Powell  Date: 14 December 1992
C   Called by: p_report.f
C
C****************************************************************
C
        integer function areaintrpt (in_buffer, out_buffer, scrfil)

        character in_buffer * (*)
        character out_buffer * (*)
        integer scrfil

        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/arcntl.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/sortuvov.inc'
c
	double precision pgold, pgnew, padj
c
	character null * 1, linefeed * 1, header(4) * 132
        integer o2, apdoutbuf, scrfilx, findstr, loop(2), lastloop(2)
        logical gtfltr, chkfltr, change_f, repeat, finished

        save

        out_buffer(1:1) = null
        o2 = 1
        maxbuf_out = len( out_buffer ) - 400

        null = char(0)
        linefeed = char(10)

        last = index( in_buffer, null )
        if ( last .eq. 0 ) last = len( in_buffer )
        ibuf_ful = 0

        ix = last
        if ( ix .gt. 50 ) i = 50
        scrfilx = scrfil
        repeat = (findstr( in_buffer(1:ix), 'CONTINUE') .ne. 0) 
        if (repeat) then
           scrfilx = 0
           do i = 1, 2
              lastloop(i) = loop(i)
           enddo
        else
           repeat = .false.
           do i = 1, 2
              lastloop(i) = 0
           enddo
c
c          Search and align to "WHERE" ...
c
           ix = findstr (in_buffer, 'WHERE') ! findstr is a 
c                                            ! case-insensitive
c                                            ! version of index
           if (ix .gt. 0) then
              ix = ix + len('WHERE')
              change_f = gtfltr(in_buffer(ix:))
           else
              do i = 1, 7
                 filter(i) = 0
              enddo
           endif

c          Set up the page header

           if (scrfilx. gt. 0) then
              outbuf = 'Area Interchange'
              call rpnlod
    
              write (outbuf, 90) chase1(1), chase1(34), chase1(35)
   90         format('Case: ', a10, ' Project: ', 2a10)
              call hedlod
           endif

           write (header(1), 100) cspare(30), dte
  100      format (' Area interchange summary case ', a10, ' date ', 
     &               a10)
           write (header(2), 110)
  110      format (' Area name       Slack bus               Slack bus g
     &eneration         Net       Zone Composition ')
           write (header(3), 120)
  120      format ('             Name       Base  Beginning  Change  Fin
     &al   maximum   interchange')
           write (header(4), 130)
  130      format ('                                 MW        MW      M
     &W       MW        MW')

           do i = 1, 4
              if (scrfilx .gt. 0) write (scrfilx, '(a)') header(i)
              length = apdoutbuf(o2, header(i), out_buffer(o2:))
              o2 = o2 + length
              if (scrfilx .gt. 0) then
                 outbuf = header(i)
                 call shdlod(i)
              endif
           enddo

           if (scrfilx .gt. 0) then
              outbuf = ' '
              call shdlod(5)
              call comlod(1)
              call comlod(2)
              call forbtm()
              call fortop()
              call prnt_fltr (in_buffer(ix:))
           endif
        endif

        ix = lastloop(1)
        if (ix .eq. 0) ix = 1

        finished = (.not. (jtie .gt. 0))
        do while ( ix .le. ntotc .and. .not. finished)

           if (chkfltr(arcnam(ix), '**', '***', 0.0, '**', 0)) then
              anet = arcnet(ix) * bmva
              pgold = area(7,ix)
              pgnew = area(8,ix)
              padj = pgnew - pgold
              write (outbuf, 140) arcnam(ix), arcbus(ix), arcbas(ix), 
     &           pgold, padj, pgnew, area(6,ix), anet, 
     &           (arczns(j,ix),j=1,MAXCAZR)
  140         format (1x, a10, 2x, a8, f7.1, f9.1, f8.1, f9.1, f9.1,
     1               2x, f9.1, 6x, 10(a2, 2x))
              if (scrfilx .gt. 0) call prtout (1)
              if ((ix .gt. lastloop(1) .or. 
     &            (ix .eq. lastloop(1) .and. lastloop(2) .lt. 1)) .and. 
     &            o2 .lt. maxbuf_out ) then
                 length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
                 o2 = o2 + length
                 loop(1) = ix
                 loop(2) = 1
              elseif ( repeat ) then
                 finished = .true.
                 ibuf_ful = 1
                 go to 150
              elseif ( ibuf_ful .eq. 0 ) then
                 ibuf_ful = 1
              endif
              do j = 1, 9
                 j1 = j * MAXCAZR + 1
                 j2 = (j+1) * MAXCAZR
                 if (arczns(j1,ix) .ne. ' ') then
                    write (outbuf,462) (arczns(jk,ix),jk=j1,j2)
  462               format (t81, 10(a2, 2x))
                    if (scrfilx .gt. 0) call prtout (1)
                    if ((ix .gt. lastloop(1) .or. 
     &                  (ix .eq. lastloop(1) .and. lastloop(2) .ge. j))
     &                      .and. o2 .lt. maxbuf_out ) then
                       length = apdoutbuf(o2, outbuf(1:132), 
     &                                    out_buffer(o2:))
                       o2 = o2 + length
                       loop(1) = ix
                       loop(2) = j
                    elseif ( repeat ) then
                       finished = .true.
                       ibuf_ful = 1
                       go to 150
                    elseif ( ibuf_ful .eq. 0 ) then
                       ibuf_ful = 1
                    endif
                 endif
              enddo
              if (o2 .lt. maxbuf_out) then
                 loop(1) = ix
                 loop(2) = 5
              endif
           else if (o2 .lt. maxbuf_out) then
              loop(1) = ix
              loop(2) = 5
           endif
  150      continue
           ix = ix + 1
        enddo
        areaintrpt = 0

c*** remember maxbuf_out is really 400 less than the real buffer size
        if (o2 .gt. maxbuf_out) then
           length = apdoutbuf(o2, '*[MORE]' , out_buffer(o2:))
           o2 = o2 + length
        endif

        if (ntotc .eq. 0 .or. jtie .eq. 0) then
           write (errbuf(1), 160)
  160      format(' No area interchange data in network ')
           call prterx ('W', 1)
           areaintrpt = 1
        endif

  900   continue
        return
        end
