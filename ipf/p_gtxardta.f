C    @(#)p_gtxardta.f	20.4 1/7/99
C****************************************************************
C
C       File: p_gtxardta.f
C
C       Purpose: Routine to obtain area interchange data from the
c                the alternate base case data file for input
C                area A <areaname>
C
C       Author: Walt Powell  Date: 19 August 1993
C       Called by: p_gtdata.f
C
C****************************************************************
C
        integer function p_gtxardta (in_buffer, out_buffer)

c
c       Output parameter:
c
c       in_buffer  - a character string specifying desired data
c       out_buffer - a character string for storing data
c
c       Return status: 0 - successful
c                      1 - errors
 
        include 'ipfinc/parametr.inc'

        character in_buffer * (MAXBUFFER), out_buffer * (MAXBUFFER)

        include 'ipfinc/blank.inc'
        include 'ipfinc/alt_case.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/owncom.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/sortuvov.inc'

        common /xarea_data/ gentot, lodtot, lostot, oarcact(4*MAXCAR),
     &                     oarcflg(4*MAXCAR) 
        real gentot, lodtot, lostot, oarcact
        integer oarcflg
   
        character  null * 1, linefeed * 1, text*132, zn * 2, 
     &             areaname * 10
        integer o2, oldo2, apdoutbuf, first

        p_gtxardta = 0   ! set default status "successful"
        null = char(0)
        linefeed = char(10)

        out_buffer(1:1) = null
        o2 = index (out_buffer,null)
        maxbuf_out = len( out_buffer ) - 400
c
c       Read Area data
c
        first = 1
        last = index (in_buffer, null)
        if (last .eq. 0) last = len (in_buffer)
c
c       Position in_buffer to next record if / GET_DATA
c
        if (in_buffer(first:first) .eq. '/') then
           first = nxt_term(in_buffer(first+1:)) + first
           if (in_buffer(first:first) .eq. linefeed) first = first + 1
        endif

        do while (first .lt. last)
           next = nxt_term(in_buffer(first+1:)) + first
           text = in_buffer(first:next-1)
           first = next
           if (in_buffer(first:first) .eq. linefeed) first = first + 1
           if (text(1:1) .eq. 'A') then
c
c             Obtain all data associated with type "A" record.
c
              areaname = text(4:13)

              do i = 1, ontotc
                 if (oarcnam(i) .eq. areaname) go to 120
              enddo
              write (errbuf(1), 110) areaname
  110         format (' Area ', a, ' is not in system ')
              call prterx ('W', 1)
              go to 900

  120         gentot = 0.0
              lodtot = 0.0
              lostot = 0.0
 
              export = oarcnet(i)*bmva
 
              do 130 j = 1,MAXCAZ
                 zn = oarczns(j,i)
                 if (j .eq. 1 .or. zn .ne. ' ') then
                    do k = 1, onztot
                       if (oacznam(k) .eq. zn) then
                          gentot = gentot + ozsum(1,k)
                          lodtot = lodtot + ozsum(3,k)
                          lostot = lostot + ozsum(5,k)
                          go to 130
                       endif
                    enddo
                 endif
  130         continue
C
C             Write area "A" quantities to output
C
              write (text, 140) oarcnam(i), gentot, lodtot, lostot, 
     &                          export
  140         format('A ', a10, 1x, 4e15.7)
              length = apdoutbuf(o2, text, out_buffer(o2:))
              oldo2 = o2
              o2 = o2 + length
              if (o2 .gt. maxbuf_out) then
                 o2 = oldo2
                 write (out_buffer(o2:o2+8), 820) linefeed, null
  820            format (a, '*[MORE]', a)
                 o2 = o2 + 9
                 go to 900
              endif
C
C             Write Intertie "I" quantities to output
C
              jt = 1
              do while (jt .le. ontotic .and. 
     &                 (oarcint(1,jt) .ne. areaname))
                 jt = jt + 1
              enddo
              if (jt .gt. ontotic .or.
     &           (oarcint(1,jt) .ne. areaname)) then
                 write (errbuf(1), 150) areaname
  150            format (' Intertie "I" record for area ', a, 
     &              ' is not in system ')
                 call prterx ('W', 1)
                 go to 900
              endif             
              do while (jt .le. ontotic .and. 
     &                 (oarcint(1,jt) .eq. areaname))
   
                 cfs = oarcact(jt) - oarcinp(jt)
                 write (text, 160) oarcint(1,jt), oarcint(2,jt),
     &                             oarcinp(jt), 
     &                             oarcact(jt), cfs, oarcflg(jt)
  160            format ('I ', a10, 1x, a10, 1x, 3e15.7, 1x, i1)
                 length = apdoutbuf(o2, text, out_buffer(o2:))
                 oldo2 = o2
                 o2 = o2 + length
                 if (o2 .gt. maxbuf_out) then
                    o2 = oldo2
                    write (out_buffer(o2:o2+8), 820) linefeed, null
                    o2 = o2 + 9
                    go to 900
                 endif
                 jt = jt + 1
              enddo
           else if (text(1:1) .eq. '(') then
              go to 900
           else
              write (errbuf(1), 170) text(1:40)
  170         format (' Illegal text record (', a, ')')
              call prterx ('W', 1)
           endif
        enddo

c*** remember maxbuf_out is really 400 less than the real buffer size
  900   if (o2 .gt. maxbuf_out) then
           write (out_buffer(o2:o2+8), 820) linefeed, null
           o2 = o2 + 9
        endif
        return
        end
