C    @(#)ex_bus.f	20.3 2/13/96
C****************************************************************
C
C   File: ex_bus.f
C   Purpose: Routine to inquire of a bus's status:
C            Returned value: status = 1 : bus does not exist
C                                     0 : bus exists
C
C   Author: Walt Powell  Date: 10 Nov 1992
C   Called by: p_gtdata.f
C
C****************************************************************
C
        integer function ex_bus (in_buffer, out_buffer)
        character in_buffer *(*), out_buffer *(*)
c
        include 'ipfinc/prt.inc'
 
        character word(20) * 60, capital * 60,
     &            null * 1, linefeed * 1,
     &            bus_name * 8, ljstfy * 60,
     &            tmpc * 20, tempc * 60
        logical found
        integer o2, i1, find_bus

        null = char(0)
        linefeed = char(10)

        ex_bus = 1       ! initialize return status 'unsuccessful'

        out_buffer(1:1) = null
        o2 = index (out_buffer,null)
        i1 = 1
        i2 = index (in_buffer,null)
        nb = 0

c***    call uscan (in_buffer(i1:i2), word, nwrd, '/=(',
        call uscan (in_buffer(i1:i2), word, nwrd, '=(',
     &              ', ' // linefeed // null )
c
c	Align "iwrd" after "TYPE = BUS_EXISTS""
c
	iwrd = 1
c***    if (word(iwrd)(1:1) .eq. '/') then
           found = .false.
c***       iwrd = iwrd + 1
           do while (iwrd .le. nwrd .and. .not. found)
              word(iwrd) = capital(word(iwrd))
              if (word(iwrd)(1:6) .eq. 'BUS_EX') then
                 found = .true.
              endif
              iwrd = iwrd + 1
           enddo
c***    endif
              
        do while (iwrd .le. nwrd)

           word(iwrd) = capital(word(iwrd))
           if (word(iwrd)(1:3) .eq. 'BUS') then
              iwrd = iwrd + 1
              if (word(iwrd) .eq. '=') iwrd = iwrd + 1
c
c             Remove any "..." bracketing bus name
c
              i = index ( word(iwrd), '"' )
              do while (i .ne. 0)
                 tmpc = word(iwrd)(i+1:)
                 word(iwrd)(i:) = tmpc
                 i = index ( word(iwrd), '"' )
              enddo
              bus_name = word(iwrd)(1:8)
              last = lastch(word(iwrd))
              if ( last .gt. 8 ) then
                 tmpc = word(iwrd)(9:)
                 word(iwrd) = ljstfy(tmpc)
              else
                 iwrd = iwrd + 1
              endif
              if (index(word(iwrd), '.') .eq. 0) then
                 last = lastch(word(iwrd))
                 tmpc = word(iwrd)(1:last) // '.0'
                 word(iwrd) = tmpc
              endif
              read (word(iwrd), 215, err = 220) base_kv
  215         format (f6.0)
              if (bus_name .ne. ' '  .and. base_kv .gt. 5.0)
     &            nb = find_bus (bus_name, base_kv)
              goto 230
  220         last = lastch (word(iwrd))
              write (errbuf(1), 222) word(iwrd)(1:last)
  222         format (' Illegal base kv (', a, ') ')
              call prterx ('W', 1)
  230         continue
           else if (word(iwrd)(1:1) .eq. '(') then
              iwrd = nwrd + 1
           else
              last = lastch (word(iwrd))
              write (errbuf(1), 250) word(iwrd)(1:last)
  250         format (' Meaningless keyword (', a, ') ')
              call prterx ('W', 1)
           endif
           iwrd = iwrd + 1
        enddo

        if (nb .gt. 0) ex_bus = 0   ! update return status 'successful'

        return 
        end
