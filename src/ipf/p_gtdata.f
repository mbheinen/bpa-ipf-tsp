C    @(#)p_gtdata.f	20.9 1/7/99
C****************************************************************
C
C   File: p_gtdata.f
C   Purpose: IPF shell program to process /GET_DATA commands
C
C   Author: Walt Powell  Date: 20 February 1992 
C                        Modified: 1 November 1995
C   Called by: 
C
C****************************************************************
C
	integer function p_gtdata (in_buffer, out_buffer) 

        character in_buffer*(*)
        character out_buffer*(*)

        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/filnam.inc'
        include 'ipfinc/jobctl.inc'
        include 'ipfinc/dtaiop.inc'
        include 'ipfinc/changr.inc'
        include 'ipfinc/delete.inc'
        include 'ipfinc/epridc.inc'
        include 'ipfinc/errorsw.inc'
        include 'ipfinc/pfstates.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/errmsg.inc'
        include 'ipfinc/errorx.inc'
        include 'ipfinc/alt_case.inc'
c
c
c       Make "word" large enough for long filenames
c
        character*60  word(50), capital
        character null*1, linefeed*1, filename*60, text*120
c
        integer o1, o2, apdoutbuf, chkerr, ex_bus, ex_file, gtstatus,
     &          p_ldardata, p_gtardata, p_initdef, p_loaddef, p_subdef,
     &          gtcount, findstr, gtcflowi, gtcflowrefi, tempfile, 
     &          ldaltbse, status, errflg, p_ldxardta, p_gtxardta,
     &          gtconnect, loaded, file_sts, bufsize
c
        null = char(0)
        linefeed = char(10)
        bufsize = len (out_buffer)
        p_gtdata = 0     ! default return SUCCESS state
        numerr = 0       ! reinitialize error count
        out_buffer(1:1) = null

c*************** Now done by command parser ****************************
c***c
c***c       Replace "*[EOM]" with "(END)"
c***c
c***        last = index (in_buffer, null)
c***        ix = findstr (in_buffer(1:last), '*[EOM]')
c***        if (ix .gt. 0) then
c***           in_buffer(ix:ix+5) = '(END)' // null
c***        endif
c***
c**********************************************************************

        next = nxt_term(in_buffer) 
        inrcd = in_buffer(1:next-1)
        call uscan(in_buffer(2:next-1), word, nwrd, '=',' ,'//linefeed)
c
c       Capitalize all word() except <filename>
c
        iflag = 0
        do i = 1, nwrd
           if (iflag .ne. 1 .and. word(i) .ne. '=') then
              word(i) = capital(word(i))
           endif
           if (iflag .eq. 0 .and. word(i) .eq. 'FILE') then
              iflag = 1
           else if (iflag .eq. 1 .and. word(i) .ne. '=') then
              iflag = 2
           endif
        enddo
c
c       Reset error flag
c
        call setercnt (0, ' ')
        miscell = 0

        if (word(2) .eq. 'TYPE' .and.
     1      word(4) .eq. 'INPUT') then
           call setercnt (0, ' ')
           call gtinput (in_buffer, out_buffer)
           call prtime('GET_INPUT')
           if (chkerr('W')+chkerr('E')+chkerr('F') .gt. 0)
     1         p_gtdata = 1
           inrcd = buf
        else if (word(2) .eq. 'TYPE' .and.
     1           word(4) .eq. 'COUNT') then
           p_gtdata = gtcount (in_buffer, out_buffer)
        else if (word(2) .eq. 'TYPE' .and.
     1           word(4) .eq. 'NETWORK_DATA') then
c
c          Reset error flag
c
           call setercnt (0, ' ')
           call gtnetdat (in_buffer, out_buffer)
        else if (word(2) .eq. 'TYPE' .and.
     1           word(4) .eq. 'BUS_LIST') then
           call bus_list (in_buffer, out_buffer, errflg)
           call prtime('BUS_LIST')
           inrcd = buf
        else if (word(2) .eq. 'TYPE' .and.
     1           word(4) .eq. 'BSEKV_LIST') then
           call bsekvlst (in_buffer, out_buffer, errflg)
           call prtime('BASEKV_LIST')
           inrcd = buf
        else if (word(2) .eq. 'TYPE' .and.
     1           word(4) .eq. 'AREA_LIST') then
           call area_list (in_buffer, out_buffer, errflg)
           call prtime('AREA_LIST')
           inrcd = buf
        else if (word(2) .eq. 'TYPE' .and.
     1           word(4) .eq. 'OWNER_LIST') then
           call owner_list (in_buffer, out_buffer, errflg)
           call prtime('OWNER_LIST')
           inrcd = buf
        else if (word(2) .eq. 'TYPE' .and.
     1           word(4) .eq. 'ZONE_LIST') then
           call zone_list (in_buffer, out_buffer, errflg)
           call prtime('ZONE_LIST')
           inrcd = buf
        else if (word(2) .eq. 'TYPE' .and.
     1           word(4) .eq. 'RECORD_LIST') then
           call type_list (in_buffer, out_buffer, errflg)
           call prtime('RECORD_LIST')
           inrcd = buf
        else if (word(2) .eq. 'TYPE' .and.
     1           word(4) .eq. 'OUTPUT') then
c           if (ostates .le. 4) then
           if ( lskp .eq. 0 ) then
              p_gtdata = 1
              write (errbuf(1), 142)
  142         format(' Unsolved base data in residence')
              call prterx ('W', 1)
           else
              call gtoutput (in_buffer, out_buffer)
              call prtime('GET_OUTPUT')
              inrcd = buf
              if (chkerr('W')+chkerr('E')+chkerr('F') .gt. 0)
     1            p_gtdata = 1
           endif
        else if (word(2) .eq. 'TYPE' .and.
     1           word(4) .eq. 'A_DATA') then
           call a_data (in_buffer, out_buffer)
           call prtime('A_DATA')
           inrcd = buf
           if (chkerr('W')+chkerr('E')+chkerr('F') .gt. 0)
     1         p_gtdata = 1
        else if (word(2) .eq. 'TYPE' .and.
     1           word(4) .eq. 'I_DATA') then
           call i_data (in_buffer, out_buffer)
           call prtime('I_DATA')
           inrcd = buf
           if (chkerr('W')+chkerr('E')+chkerr('F') .gt. 0)
     1         p_gtdata = 1
        else if (word(2) .eq. 'TYPE' .and.
     1           word(4)(1:8) .eq. 'LINE_IMP') then
           call p_lic (in_buffer, out_buffer)
           call prtime('P_LIC')
           inrcd = buf
           if (chkerr('W')+chkerr('E')+chkerr('F') .gt. 0)
     1         p_gtdata = 1
        else if (word(2) .eq. 'TYPE' .and.
     1           word(4)(1:7) .eq. 'FILE_EX') then
           p_gtdata = ex_file (in_buffer, out_buffer)
        else if (word(2) .eq. 'TYPE' .and.
     1           word(4)(1:7) .eq. 'FILE_WRT') then
           p_gtdata = file_sts (in_buffer, out_buffer)
        else if (word(2) .eq. 'TYPE' .and.
     1           word(4)(1:6) .eq. 'BUS_EX') then
           p_gtdata = ex_bus (in_buffer, out_buffer)
        else if (word(2) .eq. 'TYPE' .and.
     1           word(4)(1:8) .eq. 'BUS_VOLT') then
           call gtbsvolt (in_buffer, out_buffer, errflg)
           call prtime('GET_OUTPUT')
           inrcd = buf
        else if (word(2) .eq. 'TYPE' .and.
     1           word(4)(1:7) .eq. 'CONNECT') then
           istatus = gtconnect (in_buffer, out_buffer)
           inrcd = buf
           p_gtdata = istatus
        else if (word(2) .eq. 'TYPE' .and.
     1           word(4)(1:7) .eq. 'OUTAGES') then
           call gtoutage (in_buffer, out_buffer)
           inrcd = buf
        else if (word(2) .eq. 'TYPE' .and.
     1           word(4)(1:6) .eq. 'STATUS') then
           p_gtdata = gtstatus (in_buffer, out_buffer)
           inrcd = buf
        else if (word(2) .eq. 'TYPE' .and.
     1           word(4)(1:8) .eq. 'INIT_DEF') then
           p_gtdata = p_initdef (in_buffer, out_buffer)
           inrcd = buf
        else if (word(2) .eq. 'TYPE' .and.
     1           word(4)(1:8) .eq. 'LOAD_DEF') then
           p_gtdata = p_loaddef (in_buffer, out_buffer)
           inrcd = buf
        else if (word(2) .eq. 'TYPE' .and.
     1           word(4)(1:7) .eq. 'SUB_DEF') then
           p_gtdata = p_subdef (in_buffer, out_buffer)
           inrcd = buf
        else if (word(2) .eq. 'TYPE' .and.
     1           word(4)(1:9) .eq. 'LOAD_AREA') then
           p_gtdata = p_ldardata (in_buffer, out_buffer)
           inrcd = buf
        else if (word(2) .eq. 'TYPE' .and.
     1           word(4)(1:9) .eq. 'AREA_DATA') then
           p_gtdata = p_gtardata (in_buffer, out_buffer)
           inrcd = buf
        else if (word(2) .eq. 'TYPE' .and.
     1           word(4)(1:13) .eq. 'LOAD_REF_AREA') then
           p_gtdata = p_ldxardta (in_buffer, out_buffer)
           inrcd = buf
        else if (word(2) .eq. 'TYPE' .and.
     1           word(4)(1:13) .eq. 'REF_AREA_DATA') then
           p_gtdata = p_gtxardta (in_buffer, out_buffer)
           inrcd = buf
        else if (word(2) .eq. 'TYPE' .and.
     1           word(4)(1:11) .eq. 'CFLOW_INPUT') then
           itext = findstr (in_buffer, 'CFLOW_INPUT')
     1           + lastch (word(4))
           p_gtdata = gtcflowi (in_buffer(itext:), out_buffer)
           inrcd = buf
        else if (word(2) .eq. 'TYPE' .and.
     1           word(4)(1:15) .eq. 'CFLOW_REF_INPUT') then
           itext = findstr (in_buffer, 'CFLOW_REF_INPUT')
     1           + lastch (word(4))
           p_gtdata = gtcflowrefi (in_buffer(itext:), out_buffer)
           inrcd = buf
        else if (word(2) .eq. 'TYPE' .and.
     1           word(4)(1:13) .eq. 'LOAD_REF_BASE') then
           itext = findstr (in_buffer, 'LOAD_REF_BASE')
     1           + lastch (word(4))
           iwrd = 5
           if (word(iwrd) .eq. 'FILE') then
              iwrd = iwrd + 1
              if (word(iwrd) .eq. '=') iwrd = iwrd + 1
              filename = word(iwrd)
              tempfile = 20           
              ierr = 99    ! Indicate that input file is binary formatted
              call opnfil (tempfile, filename, ierr)
              if (ierr .ne. 0) then
                 write (errbuf(1), 100)
  100            format(' Reference base case cannot be opened ')
                 write (errbuf(2), 110) filename
  110            format(' File ', a)
                 call prterx ('W', 2)
                 p_gtdata = 1
              else
c
c                Function ldaltbse (load alternate base) installs names 
c                "ocase" and "ofilename"
c
                 ocase = ' '
                 status = ldaltbse (tempfile, filename, ocase, loaded)
                 if (status .ne. 0) then
                    write (errbuf(1), 120)
  120               format(' Reference base case cannot be loaded ')
                    write (errbuf(2), 130) filename
  130               format(' File ', a)
                    call prterx ('W', 2)
                    p_gtdata = 1
                 endif
              endif
           else
              write (errbuf(1), 140)
  140         format(' No reference base case loaded ')
              call prterx ('W', 1)
              p_gtdata = 1
           endif
           inrcd = buf
        else if (word(2) .eq. 'TYPE' .and.
     1           word(4) .eq. 'REF_OUTPUT') then
           if (ofilename .eq. ' ' .or. ofilename(1:1) .eq. null) then
              p_gtdata = 1
              write (errbuf(1), 150)
  150         format(' Reference base case not loaded')
              call prterx ('W', 1)
           else
              call gtaltopt (in_buffer, out_buffer)
              call prtime('REF_OUTPUT')
              inrcd = buf
              if (chkerr('W')+chkerr('E')+chkerr('F') .gt. 0)
     1            p_gtdata = 1
           endif
        else if (word(2) .eq. 'TYPE' .and.
     1           word(4)(1:8) .eq. 'COMMENTS') then
           call gtcommnt (in_buffer, out_buffer)
           inrcd = buf
        else if (word(2) .eq. 'TYPE' .and.
     1           word(4)(1:7) .eq. 'SOL_PAR') then
           call gtsolpar (in_buffer, out_buffer)
           inrcd = buf
        else if (word(2) .eq. 'TYPE' .and.
     1           word(4)(1:12) .eq. 'AREA_OF_ZONE') then
           call gtarofzn (in_buffer, out_buffer)
           inrcd = buf
        else if (word(2) .eq. 'TYPE' .and.
     1           word(4)(1:7) .eq. 'SYSTEM') then
           call gtsyspar (in_buffer, out_buffer)
           inrcd = buf
        endif
c
c       Append error messages to buffer
c
        j = 1 
        length = 1
        o2 = index (out_buffer,null)
        do while (j .le. numerr .and. length .gt. 0)
           last = lastch (errm(j))
           if (last .gt. 130) last = 130
           length = apdoutbuf(o2, errm(j)(1:last), out_buffer(o2:))
           o2 = o2 + length
           j = j + 1
        enddo
c
c 	Append summary
c
        write (text, 340) 'p_gtdata.f', p_gtdata, ostates
  340   format ('/', a, ' return status: ', i2, ' IPF state: ', i2)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length
c
c       Reset error flag
c
        call setercnt (0, ' ')
c
c       Debug printout invoked with IPF command /TRACE, CHANGE = ON
c
c       if (kase1(27) .ne. 0) then
c          o1 = 1
c          do while (o1 .lt. bufsize .and. 
c    &              (out_buffer(o1:o1) .ne. null))
c             next = nxt_term (out_buffer(o1+1:)) + o1
c             o2 = min (o1+131, next-1)
c             write (dbug, 830) out_buffer(o1:o2)             
c 830         format (1x, a)
c             if (o2 .lt. next-1) then
c                write (dbug, 832) out_buffer(o2+1:next-1)
c 832            format (1x, '->', a)
c             endif
c             o1 = next
c             if (out_buffer(o1:o1) .eq. linefeed) o1 = o1 + 1
c          enddo
c       endif
        return
	end
