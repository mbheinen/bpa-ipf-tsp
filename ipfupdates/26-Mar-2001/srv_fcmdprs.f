C    @(#)srv_fcmdprs.f	20.14 1/7/99
c****************************************************************
c File: srv_fcmdprs.f
c Called by: srv_cmdprs.c
c Purpose: Parse for commands that are handled by Fortran routines and
c          call the appropriate routine.
c          All recursive routines MUST be handled in "C" and are taken
c          care of by "srv_cmdprs.c" (/COMMAND and /CFLOW).
c****************************************************************

        integer function srv_fcmdprs (in_buffer, out_buffer)

c*** hard code size of arguments to insure valid "len" operations
c*** when this function is called by "C"

        include 'ipfinc/parametr.inc'

        character in_buffer * (MAXBUFFER)
        character out_buffer * (MAXBUFFER)

        include 'ipfinc/lfiles.inc'
        include 'ipfinc/jobctl.inc'
        include 'ipfinc/prt.inc'

        common /is_batch / is_batch

        character * 1   null, linefeed
        character * 20  ipf_cmd, tempc, word(5)
        character * 15  rname
        character * 120 text
        character scr_buf * (MAXBUFFER)
        integer  o2, cmdstat

        character * 20    capital
        integer apdoutbuf, first,
     &          p_pfinit,  p_gtbase,  p_gtnetdat, p_change, p_chgbty,
     &          p_cnvtld,  p_solton,  p_gtdata,   p_report, p_newbse,
     &          p_svfile,  p_reduct,  p_plot,     p_syscal, p_pfexit,
     &          p_cdproc,  p_initdef, p_loaddef,  p_subdef, p_ptdata,
     &          p_ctlpow,  p_fstout,  p_chgpar,   p_agc,    p_gendrp,
     &          p_cmde2,   firstxstr
        logical finished, exist, opened

        null = char(0)
        linefeed = char(10)
        rname = 'srv_fcmdprs.f'
        o2 = 1
        out_buffer(1:1) = null

        cmdstat = 0     ! default return SUCCESS state

        last = index( in_buffer, null )
        if (last .eq. 0) then
           write (errbuf(1), 70010)
           write (79, '(1x, a)') errbuf(1)
70010      format(' Improperly terminated command buffer --')
           write (errbuf(2), 70020)
           write (79, '(1x, a)') errbuf(2)
70020      format(' "NULL" terminator not found, command ignored.')
           if (is_batch .eq. 0) then
              call prterx ('E',2)
           else
              call prterx ('F',2)
           endif
           cmdstat = 1
           call apnd_stat( out_buffer(o2:), cmdstat, rname )
           srv_fcmdprs = cmdstat
           return
        endif

        ilast = min0 (last, 80)
c	write (*, 70021) in_buffer(1:ilast)
70021   format (' command [', a, ']')
	
	lflf = index( in_buffer(1:last), linefeed // linefeed )
        do while ( lflf .ne. 0 )
           scr_buf(1:last-1) = in_buffer(1:lflf) //
     &                         in_buffer(lflf+2:last)
           last = last - 1
           in_buffer(1:last) = scr_buf(1:last)
           lflf = index( in_buffer(1:last), linefeed // linefeed )
        enddo

        next = nxt_term(in_buffer) 
        finished = .false.
        do while (.not. finished)
          if (next .eq. 1 .and. in_buffer(1:1) .eq. linefeed) then
            in_buffer(1:1) = ' '
            next = nxt_term(in_buffer) 
          else if (next .le. 1) then
            write (errbuf(1), 70040)
70040       format(' Improperly formatted command buffer --')
            write (errbuf(2), 70050)
70050       format(' "/" character not found, command ignored.')
            if (is_batch .eq. 0) then
              call prterx ('E',2)
            else
              call prterx ('F',2)
            endif
            cmdstat = 1
            call apnd_stat( out_buffer(o2:), cmdstat, rname )
            srv_fcmdprs = cmdstat
            return
          else if (in_buffer(1:next-1) .eq. ' ') then
            in_buffer(next:next) = ' '
            next = nxt_term(in_buffer(next:)) 
          else if (next .ge. MAXBUFFER) then
            write (errbuf(1), 70040)
            write (errbuf(2), 70050)
            if (is_batch .eq. 0) then
              call prterx ('E',2)
            else
              call prterx ('F',2)
            endif
            cmdstat = 1
            call apnd_stat( out_buffer(o2:), cmdstat, rname )
            srv_fcmdprs = cmdstat
            return
          else
            finished = .true.
          endif
        enddo
        first = index( in_buffer(1:next), '/' )
        nextfirst = index( in_buffer(first+1:last), linefeed // '/' )
        if (nextfirst .gt. 0 .and. nextfirst .lt. last) then
          nextfirst = nextfirst + first + 1
          scr_buf = in_buffer(nextfirst:last)
          in_buffer(nextfirst:last) = null // ' ' 
        else
          nextfirst = 0
        endif
        do while (first .lt. last)
          if (first .eq. 0 .or. first .eq. next) then
            write (errbuf(1), 70060) in_buffer(1:60)
70060       format (' Invalid command record "', a, '"')
            if (is_batch .eq. 0) then
              call prterx ('E', 1)
            else
              call prterx ('F', 1)
            endif
            cmdstat = 1
            call apnd_stat( out_buffer(o2:), cmdstat, rname )
            srv_fcmdprs = cmdstat
            return
          endif
          inrcd = in_buffer(first:next-1)
          ipf_cmd = inrcd(2:21)
          call uscan(ipf_cmd, word, nwrd, '=', ' ,'//linefeed//null)
c
c         capitalize word(1) and compress out any "_"
c         put word(1) into ipf_cmd
c
          ipf_cmd = capital(word(1))
          tempc = ipf_cmd
          i = index( tempc, '_' )
          do while ( i .gt. 0 )
            ipf_cmd(i:) = tempc(i+1:)
            tempc = ipf_cmd
            i = index( tempc, '_' )
          enddo

          next = min0 (next, 60)
          next = max0 (1, next)
          if ( firstxstr(ipf_cmd, 'INITIALIZE') .ne. 0)  then
            call cpyinbfil(in_buffer, inp)
            cmdstat = p_pfinit (in_buffer, out_buffer(o2:)) 
            endfile lprt
	    rewind lprt
            endfile mfich
	    rewind mfich
            endfile dbug
	    rewind dbug

          else if ( firstxstr(ipf_cmd, 'OLDBASE') .ne. 0)  then
            inquire (unit=lprt, exist=exist)
	    inquire (unit=lprt, name=text)
	    inquire (unit=lprt, opened=opened)
	    ilast = lastch (text)
	    write (*, 70022) lprt, text(1:ilast), exist, opened
70022       format (' File ', i4, ' name ', a, ' exist ', i4, 
     &              ' opened ', i4)
            close(unit=lprt)
            open(unit=lprt, file=text(1:ilast), status='unknown', 
     &           form='formatted')
	      
            inquire (unit=mfich, exist=exist)
	    inquire (unit=mfich, name=text)
	    inquire (unit=mfich, opened=opened)
	    ilast = lastch (text)
	    write (*, 70022) mfich, text(1:ilast), exist, opened
            close(unit=mfich)
            open(unit=mfich, file=text(1:ilast), status='unknown', 
     &           form='formatted')

            inquire (unit=dbug, exist=exist)
	    inquire (unit=dbug, name=text)
	    inquire (unit=dbug, opened=opened)
	    ilast = lastch (text)
	    write (*, 70022) dbug, text(1:ilast), exist, opened
            close(unit=dbug)
            open(unit=dbug, file=text(1:ilast), status='unknown', 
     &           form='formatted')

            call cpyinbfil(in_buffer, inp)
            cmdstat = p_gtbase (in_buffer, out_buffer(o2:)) 

          else if ( firstxstr(ipf_cmd, 'NETWORKDATA') .ne. 0)  then
            call cpyinbfil(in_buffer, inp)
            cmdstat = p_gtnetdat (in_buffer, out_buffer(o2:)) 

          else if ( firstxstr(ipf_cmd, 'CHANGES') .ne. 0)  then
            call cpyinbfil(in_buffer, inp)
            cmdstat = p_change (in_buffer, out_buffer(o2:)) 

          else if ( firstxstr(ipf_cmd, 'CHANGEBUS*') .ne. 0)  then
            call cpyinbfil(in_buffer, inp)
            cmdstat = p_chgbty (in_buffer, out_buffer(o2:))

          else if ( firstxstr(ipf_cmd, '%LOAD*') .ne. 0)  then
            call cpyinbfil(in_buffer, inp)
            cmdstat = p_cnvtld (in_buffer, out_buffer(o2:))

          else if ( firstxstr(ipf_cmd, 'SOLUTION') .ne. 0)  then
            call cpyinbfil(in_buffer, inp)
            cmdstat = p_solton (in_buffer, out_buffer(o2:)) 

          else if ( firstxstr(ipf_cmd, 'CHANGEP*') .ne. 0)  then
            call cpyinbfil(in_buffer, inp)
            cmdstat = p_chgpar (in_buffer, out_buffer(o2:)) 

          else if ( firstxstr(ipf_cmd, 'GETDATA') .ne. 0)  then
            cmdstat = p_gtdata (in_buffer, out_buffer(o2:)) 

          else if ( firstxstr(ipf_cmd, 'REPORTS') .ne. 0)  then
            call cpyinbfil(in_buffer, inp)
            cmdstat = p_report (in_buffer, out_buffer(o2:)) 

          else if ( firstxstr(ipf_cmd, 'NEWBASE') .ne. 0)  then
            call cpyinbfil(in_buffer, inp)
            cmdstat = p_newbse (in_buffer, out_buffer(o2:)) 

          else if ( firstxstr(ipf_cmd, 'SAVEFILE') .ne. 0)  then
            call cpyinbfil(in_buffer, inp)
            cmdstat = p_svfile (in_buffer, out_buffer(o2:)) 

          else if ( firstxstr(ipf_cmd, 'REDUCT*') .ne. 0)  then
            call cpyinbfil(in_buffer, inp)
            cmdstat = p_reduct (in_buffer, out_buffer(o2:)) 
 
          else if ( firstxstr(ipf_cmd, 'PLOT') .ne. 0)  then
            call cpyinbfil(in_buffer, inp)
            cmdstat = p_plot (in_buffer, out_buffer(o2:)) 
 
          else if ( firstxstr(ipf_cmd, 'SYSCAL') .ne. 0)  then
            call cpyinbfil(in_buffer, inp)
            cmdstat = p_syscal (in_buffer, out_buffer(o2:)) 

          else if ( firstxstr(ipf_cmd, 'OUTAGE*') .ne. 0)  then
            call cpyinbfil(in_buffer, inp)
            cmdstat = p_fstout (in_buffer, out_buffer(o2:)) 
 
          else if (ipf_cmd .eq. 'QUIT'  .or.
     &             ipf_cmd .eq. 'EXIT' ) then
            call cpyinbfil(in_buffer, inp)
            cmdstat = p_pfexit (in_buffer, out_buffer(o2:)) 
  
          else if ( firstxstr(ipf_cmd, 'PROCESS') .ne. 0)  then
            call cpyinbfil(in_buffer, inp)
            cmdstat = p_cdproc (in_buffer, out_buffer(o2:)) 
  
          else if ( firstxstr(ipf_cmd, 'INITDEF*') .ne. 0)  then
            call cpyinbfil(in_buffer, inp)
            cmdstat = p_initdef (in_buffer, out_buffer(o2:)) 
  
          else if ( firstxstr(ipf_cmd, 'LOADDEF*') .ne. 0)  then
            call cpyinbfil(in_buffer, inp)
            cmdstat = p_loaddef (in_buffer, out_buffer(o2:)) 
  
          else if ( firstxstr(ipf_cmd, 'SUBDEF*') .ne. 0)  then
            call cpyinbfil(in_buffer, inp)
            cmdstat = p_subdef (in_buffer, out_buffer(o2:)) 
  
          else if ( firstxstr(ipf_cmd, 'PUTDATA') .ne. 0)  then
            call cpyinbfil(in_buffer, inp)
            cmdstat = p_ptdata (in_buffer, out_buffer(o2:)) 

          else if ( firstxstr(ipf_cmd, 'AGC') .ne. 0)  then
            call cpyinbfil(in_buffer, inp)
            cmdstat = p_agc (in_buffer, out_buffer(o2:)) 

          else if ( firstxstr(ipf_cmd, 'GENDROP') .ne. 0)  then
            call cpyinbfil(in_buffer, inp)
            cmdstat = p_gendrp (in_buffer, out_buffer(o2:)) 

          else if ( firstxstr(ipf_cmd, 'COMMONMODEANAL*') .ne. 0)  then
            call cpyinbfil(in_buffer, inp)
            cmdstat = p_cmde2 (in_buffer, out_buffer(o2:)) 

          else if (ipf_cmd(1:3) .eq. 'END') then

          else
            call cpyinbfil(in_buffer, inp)
            cmdstat = p_ctlpow (in_buffer, out_buffer(o2:)) 

          endif

          if (nextfirst .gt. 0) then
            first = 1
            last = index (scr_buf, null)
            if (first .lt. last) then
              o2 = index( out_buffer, null )
              if (o2 .eq. 0) then
                o2 = 1
                out_buffer(o2:o2) = null
              endif
              in_buffer = scr_buf(1:last)
              last = index (in_buffer, null)
              nextfirst = index( in_buffer(first+1:last), linefeed//'/')
              if (nextfirst .gt. 0 .and. nextfirst .lt. last) then
                nextfirst = nextfirst + first + 1
                scr_buf = in_buffer(nextfirst:last)
                in_buffer(nextfirst:last) = null // ' ' 
              else
                nextfirst = 0
              endif
            else
              first = last + 1
            endif
          else
            first = last + 1
          endif
        enddo
        srv_fcmdprs = cmdstat
        return
        end
