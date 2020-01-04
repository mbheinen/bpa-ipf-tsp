C    @(#)p_lic.f	20.5 1/7/99
C****************************************************************
C
C   File: p_lic.f
C   Purpose: Routine to obtain BCD image of INPUT Intertie "I"
C            data 
C
C   Author: Walt Powell  Date: 22 July 1992
C                        Modified: 22 July 1992
C   Called by:
C
C****************************************************************
C
        subroutine p_lic (in_buffer, out_buffer)

      	implicit real*8 (a-h, o-z),  integer*4 (i-n)

c
c       This subroutine parses the input buffer to the line impedance
c       calculation routines.
c       Output parameter:
c
c       out_buffer - a character string for storing data
c
        include 'ipfinc/parametr.inc'

        character out_buffer * (MAXBUFFER)
        character in_buffer * (MAXBUFFER)

        include 'ipfinc/lfiles.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/deck25.inc'
 
        parameter( MAXCND = 36 ) ! N

      	dimension  iphase(MAXCND), skin(MAXCND), resis(MAXCND),
     &             diam(MAXCND), horiz(MAXCND), vtower(MAXCND),
     &	           vmid(MAXCND), separ(MAXCND), alpha(MAXCND),
     &	           numbnd(MAXCND), zout(6)

        character record * 120, word(20) * 60, capital * 60,
     &            null * 1, linefeed * 1, fmt * 8,
     &            filename * 60, units * 7
        logical eof, yes, found
        integer o2, apdoutbuf, i1, findstr
        real temp(20)

        null = char(0)
        linefeed = char(10)

        num = 0
        units = 'ENGLISH'
        freq = 60.0
        basemva = 100.0
        iprsup = 0
c
c       Decode line impedance calculation input. There are two forms:
c
c       /GET_DATA, TYPE = LINE_IMPEDANCE_CALCULATION, FILE = <filename>
c	
c	and
c
c       /GET_DATA, TYPE = LINE_IMPEDANCE_CALCULATION,
c        UNITS = < ENGLISH | METRIC >, DISTANCE = < miles | km >
c        BASEKV = <basekv>, BASEMVA = <basemva>, FREQUENCY = <freq>
c        CONDUCTOR = 1 .3636 .05215 1.602 -20.75 50. 50. 0.0 0.0 0
 
        out_buffer(1:1) = null
        o2 = index (out_buffer,null)

        i1 = 1

        do while (i1 .lt. index(in_buffer, null))
           i2 = nxt_term(in_buffer(i1+1:)) + i1
           call uscan (in_buffer(i1:i2-1), word, nwrd, '=' ,
     &                 ', ' // linefeed // null )
c
c	   If parsing /GET_DATA, ..., align "iwrd" after 
c          "TYPE = LINE_IMPEDANCE_CALCULATION"
c
	   iwrd = 1
           if (findstr(word(iwrd), 'GET_DATA') .ne. 0) then
              found = .false.
              iwrd = iwrd + 1
              do while (iwrd .le. nwrd .and. .not. found)
                 word(iwrd) = capital(word(iwrd))
                 if (word(iwrd)(1:8) .eq. 'LINE_IMP') then
                    found = .true.
                 endif
                 iwrd = iwrd + 1
              enddo
           endif
              
           do while (iwrd .le. nwrd)

              word(iwrd) = capital(word(iwrd))
              if (word(iwrd)(1:4) .eq. 'FILE') then
                 i = iwrd + 1
                 if (word(i) .eq. '=') i = i + 1
                 filename = word(i)
                 last = lastch (filename)
                 inquire (file = filename, exist = yes)
                 if (yes) then
                    lun = 30
                    open (lun, file = filename(1:last), status = 'old',
     &                    form = 'formatted', err = 86)
                    eof = .false.
c
c		    append text after "/GET_DATA,..."
c
                    ix = i2
                    do while (.not. eof)
                       read (lun, 80, end=90) record
   80                  format (a)
                       last = lastch (record)
                       length = apdoutbuf(ix, record(1:last), 
     &                                    in_buffer(ix:))
                       ix = ix + length
                    enddo
                    go to 90
                 endif
   86            write (errbuf(1), 88) filename(1:last)
   88            format (' Error opening file (', a, ') ')
                 call prterx ('W', 1)
                 go to 900

   90            close (unit=lun)
                 iwrd = nwrd

              else if (word(iwrd)(1:5) .eq. 'UNITS') then
                 i = iwrd + 1
                 if (word(i) .eq. '=') i = i + 1
                 word(i) = capital(word(i))
                 if (word(i)(1:7) .eq. 'ENGLISH') then
                    units = 'ENGLISH'
                 else if (word(i)(1:6) .eq. 'METRIC') then
                    units = 'METRIC'
                 else
                    last = lastch (word(i))
                    write (errbuf(1), 100) word(i)(1:last)
  100               format (' Meaningless UNITS keyword (', a, ') ')
                    call prterx ('W', 1)
                 endif
                 iwrd = i

              else if (word(iwrd)(1:5) .eq. 'DEBUG') then
                 i = iwrd + 1
                 if (word(i) .eq. '=') i = i + 1
                 word(i) = capital(word(i))
                 if (word(i)(1:2) .eq. 'ON') then
                    iprsup = 1
                 else if (word(i)(1:7) .eq. 'FULL') then
                    iprsup = 4
                 else
                    last = lastch (word(i))
                    write (errbuf(1), 100) word(i)(1:last)
                    call prterx ('W', 1)
                 endif
                 iwrd = i

              else if (word(iwrd)(1:8) .eq. 'DISTANCE') then
                 i = iwrd + 1
                 if (word(i) .eq. '=') i = i + 1
                 last = lastch(word(i))
                 write (fmt, 102) last
  102            format ('(f', i2, '.0)')
                 read (word(i), fmt=fmt, err=110, end=110) dist
                 go to 130

  110            last = lastch (word(i))
                 write (errbuf(1), 120) 'DISTANCE', word(i)(1:last)
  120            format (' Error decoding ', a, ' keyword (', a, ') ')
                 call prterx ('W', 1)

  130            continue
                 iwrd = i

              else if (word(iwrd)(1:6) .eq. 'BASEKV') then
                 i = iwrd + 1
                 if (word(i) .eq. '=') i = i + 1
                 last = lastch(word(i))
                 write (fmt, 102) last
                 read (word(i), fmt=fmt, err=140, end=140) basekv
                 go to 150

  140            last = lastch (word(i))
                 write (errbuf(1), 120) 'BASEKV', word(i)(1:last)
                 call prterx ('W', 1)

  150            continue
                 iwrd = i

              else if (word(iwrd)(1:9) .eq. 'FREQUENCY') then
                 i = iwrd + 1
                 if (word(i) .eq. '=') i = i + 1
                 last = lastch(word(i))
                 write (fmt, 102) last
                 read (word(i), fmt=fmt, err=160, end=160) freq 
                 go to 170

  160            last = lastch (word(i))
                 write (errbuf(1), 120) 'FREQUENCY', word(i)(1:last)
                 call prterx ('W', 1)

  170            continue
                 iwrd = i

              else if (word(iwrd)(1:7) .eq. 'BASEMVA') then
                 i = iwrd + 1
                 if (word(i) .eq. '=') i = i + 1
                 last = lastch(word(i))
                 write (fmt, 102) last
                 read (word(i), fmt=fmt, err=180, end=180) basemva
                 go to 190

  180            last = lastch (word(i))
                 write (errbuf(1), 120) 'BASEMVA', word(i)(1:last)
                 call prterx ('W', 1)

  190            continue
                 iwrd = i

              else if (word(iwrd)(1:9) .eq. 'CONDUCTOR') then
                 i = iwrd + 1
                 if (word(i) .eq. '=') i = i + 1
                 do j = i, nwrd
                    last = lastch(word(j))
                    write (fmt, 102) last
                    read (word(j), fmt=fmt, err=200, end=200) 
     &                 temp(j-i+1)
                    go to 210

  200               last = lastch (word(j))
                    write (errbuf(1), 120) 'CONDUCTOR', 
     &                                     word(j)(1:last)
                    call prterx ('W', 1)
                    temp(j-i+1) = 0.0

  210               continue
                 enddo
                 if (num .lt. MAXCND) then
                    num = num + 1
         	    iphase(num) = temp(1)
                    skin(num) = temp(2)
                    resis(num) = temp(3)
                    diam(num) = temp(4)
                    horiz(num) = temp(5)
                    vtower(num) = temp(6)
                    vmid(num) = temp(7)
                    separ(num) = temp(8)
                    alpha(num) = temp(9)
                    numbnd(num) = temp(10)
                 else
                    write (errbuf(1), 220) MAXCND
  220               format (' More than ', i2, ' conductors. Overflow oc
     &curred at record ')
                    write (errbuf(2), 230) 
     &                 in_buffer(i1:i1+60)
  230               format ('(', a, ')')
                    call prterx ('W', 2)
                 endif
                 iwrd = nwrd

              else
                 last = lastch (word(iwrd))
                 write (errbuf(1), 240) word(iwrd)(1:last)
  240            format (' Meaningless keyword (', a, ') ')
                 call prterx ('W', 1)
              endif
              iwrd = iwrd + 1
           enddo
           i1 = i2
           if (in_buffer(i1:i1) .eq. linefeed) i2 = i2 + 1
        enddo
        if (dist .le. 0.0) then
           write (errbuf(1), 242) 'DISTANCE', dist
  242      format (' Illegal value of keyword (', a, ') = ', f14.6)
           call prterx ('W', 1)
           go to 900
        endif
        if (basemva .le. 0.0) then
           write (errbuf(1), 242) 'BASEMVA', basemva
           call prterx ('W', 1)
           go to 900
        endif
        if (basekv .le. 0.0) then
           write (errbuf(1), 242) 'BASEKV', basekv
           call prterx ('W', 1)
           go to 900
        endif
        if (num .le. 0) then
           write (errbuf(1), 244) 
  244      format (' No conductor data entered ')
           call prterx ('W', 1)
           go to 900
        endif
        ierr = 0
      	call linimp( num, dist, units, basemva, basekv, freq, iphase, 
     1               skin, resis, diam, horiz, vtower, vmid, separ, 
     2               alpha, numbnd, zout, ierr )
        if (ierr .eq. 0) then
           write (record, 250) zout
  250      format (' LIC = ', 6f14.8)
        endif
        length = apdoutbuf(o2, record, out_buffer(o2:))
        o2 = o2 + length
        if (length .eq. 0) then
           write (errbuf(1), 260) 
  260      format (' Output buffer overflowed in subroutine "p_lic"')
           call prterx ('W', 1)
        endif

        o2 = o2 + 1
        out_buffer(o2:o2) = null

  900   continue
        return 
        end
