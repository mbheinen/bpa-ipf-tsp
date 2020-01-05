C    @(#)p_initdef.f	20.7 1/7/99
C****************************************************************
C
C       File: p_initdef.f
C       Purpose: This program processes the following command
c
c          /GET_DATA, TYPE=INIT_DEF
C
C       Author: Walt Powell  Date: 21 July 1992 
C                            Modified: 21 July 1992
C       Called by: 
C
C****************************************************************
C
	integer function p_initdef (in_buffer, out_buffer) 

        character in_buffer*(*)
        character out_buffer*(*)

        include 'ipfinc/parametr.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/filnam.inc'
        include 'ipfinc/jobctl.inc'
        include 'ipfinc/pfstates.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/errmsg.inc'
        include 'ipfinc/errorx.inc'
        include 'ipfinc/usranl.inc'

        character null * 1, linefeed * 1, text * 80
        integer apdoutbuf, o2, status, open_file, bufsize
        logical opened

        null = char(0)
        linefeed = char(10)
        bufsize = len (out_buffer)
        p_initdef = 0  ! default return SUCCESS state
        numerr = 0     ! reinitialize error count
        out_buffer(1:1) = null

        numusr = 0
        numdef(1) = 0
        numtxt(1) = 0
        usrdbg(1) = 0
        usrfil(1) = ' '
        numdef(2) = 0
        numtxt(2) = 0
        usrdbg(2) = 0
        usrfil(2) = ' '

        lunusr = 25
        inquire (unit=lunusr, opened=opened)
        if (.not. opened) then
C
C          Note: VAX/VMS limits sequential record size to 8191 words.
C          This restriction require the arrays to be written in
C          200-record segments.
C
           status = open_file(lunusr, ' ', 'U', 'W', ios)
           if (status .ne. 0) then
              write (errbuf(1), 104) ios, lunusr
  104         format (' Error No. ', i3, 
     1           ' opening scratch file on logical unit',i3)
              call prterx ('W',1)
              p_initdef = 1
              go to 130
           endif
        else
           rewind lunusr
        endif
c
c       Append error messages to buffer
c
  130   continue
        o2 = index (out_buffer, null)

        j = 1 
        do while (o2 .lt. bufsize-80 .and. j .le. numerr)
           last = lastch(errm(j)) 
           length = apdoutbuf(o2, errm(j)(1:last), out_buffer(o2:))
           o2 = o2 + length
           j = j + 1
        enddo
        if (o2 + 80 .lt. bufsize-80) then
           write (text, 340) 'p_initdef.f', p_initdef, ostates
  340      format ('/', a, ' return status: ', i2, ' IPF state: ', i2)
           last = lastch(text) 
           length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
           o2 = o2 + length
        endif
c
c       Reset error flag
c
        call setercnt (0, ' ')
        return
	end
