C    @(#)psplot.f	20.11 3/29/99
        integer function psplot(cordfil,in_buffer)

        include 'ipfinc/parametr.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/filnam.inc'
        include 'ipfinc/jobctl.inc'
        include 'ipfinc/dtaiop.inc'
        include 'ipfinc/errorsw.inc'
        include 'ipfinc/bscords.inc'
        include 'ipfinc/postdta.inc'
        include 'ipfinc/srtdta.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/usranl.inc'
        include 'ipfinc/pasprm.inc'
        include 'ipfinc/update.inc'
        include 'ipfinc/basval.inc'
        include 'ipfinc/apcrfil.inc'
        include 'ipfinc/pfstates.inc'

***************** moved "text" to postdta.inc ********************
      character in_buffer * (MAXBUFFER), out_buffer * (MAXBUFFER)
      character null*1, linefeed*1, short_buffer*5, busnm*8

******************** merge coord and base stuff **************
      integer error, status, p_gtdata, p_gtardata, pa_odif
      character type*1, cordfil*(*)
      character longfil*80 
      integer ld_ref     !0-do not; 1-do /GET_DATA,TYPE=LOAD_REF_AREA 
******************** merge coord and base stuff **************

        character usrcmnt(10)*80, postcp*90 
        character symnam(1000) * 12, subdef * 132, defchr(100) * 40
        character dgoptxt(6)*7, broptxt(4)*2, date*9, time*8
        character flotxt*18
        character fmtpqsm*30
        character blnkrcd*160
      	real symval(1000)
      	real symvbg(1000)          ! >DEFINE values from background case
        integer ndefchbg(1000)     ! char pointer   from background case
        character defchrbg(100)*40 ! char    values from background case
      	integer symind(1000), ndefch(1000), open_file
	logical eof, opened

        data fmtpqsm(1:24) /'('' ({'',i6,'' ('',i6,'')})'')'/
        data dgoptxt / 'P-Q', 'AMP-MVA', 'LOSS', 'INTRCHG',
     1  'COORDS', '% RATNG'/
        data broptxt /'- ', ' Q', 'P ','PQ'/ 

C****** set "psplot" to no error condition *******************

        blnkrcd = ' '
        psplot = 0
        kntucmnt = -1
        maxucmnt = 10 
        maxuopt = 30
        blrplate = .true.        
        jndcmt = 0
        iapcrfil = 0
        ld_ref = 0

        null = char(0)
        linefeed = char(10)
c
c       Initialize program variables
c
******************* debug stuff ************************
*     print 9901, in_buffer(1:300) 
*9901 format(' entering psplot - in_buffer: ',a)
******************* debug stuff ************************
*****************  stuff not needed as a subroutine in powerflow  *****
*       call pfinit
*       call restop
*****************  stuff not needed as a subroutine in powerflow  *****
c
c       Define logical units
c
c*** defined in  pfinit
c      inp = 13
c      datai = 16
c      lprt = 14
c*****************************
c*** use numbers from 40 up to avoid conflicts
c*** numbers 21 - 24 are for temporary use and should not conflict
      corfil = 40
      icrdmax = 5
      icrdfls = 1
      mcrdfls(1) = 41
      mcrdfls(2) = 21
      mcrdfls(3) = 22
      mcrdfls(4) = 23
      mcrdfls(5) = 24
      lcord = mcrdfls(icrdfls)
*     lpost = 42
*     close (unit = lpost, err = 81)
*  81 close (unit = lcord, err = 82)
      close (unit = lcord, err = 82)
   82 close (unit = corfil, err = 83)
   83 continue
******************** merge coord and base stuff **************
c     kcorflg          0 = next coordinate data comes from read(corfil)
c                      1 = next coordinate data comes from nxtcord record
c                      2 = eof encountered on (corfil)

      morcord = 1   !  1 = more coordinate data to be read for this set
c                      2 = an additional COORD record was read and waiting
c                      3 = end record encountered on file lcord
******************** merge coord and base stuff **************
c
c       Disable error trapping
c
        errorsw = 1
c
**************  skip cor file, get postscript file name *************** 
      i1 = nxt_term(in_buffer(1:)) + 1
      ipstfil = nxt_term(in_buffer(i1:)) + i1 
      i2 = nxt_term(in_buffer(ipstfil:))  + ipstfil - 2 
      postdynm = in_buffer(ipstfil:i2)
************************ debug stuff *************************
*     print 9954, ipstfil,i2, postdynm, multprc, multplt 
*9954 format('in psplot ipstfil,i2, postdynm,multprc, multplt = '
*    1,2i5,5x, a, 2i5)
************************ debug stuff *************************

****************** open a postscript file for output ***********
      if (multprc .le. 1) then   
         lpost = 42
         close (unit = lpost, err = 135)

  135    last = lastch (postdynm)
         status = open_file(lpost, postdynm(1:last), 'F', 'W', ios)
         if (status .ne. 0) then
            write (errbuf(1), 136) ios, postdynm(1:last), lpost
  136       format (' Error No. ', i3,
     &              ' opening Postscript output file ', a, 
     &              ' on logical unit ', i3)
            call prterx ('W',1)
            psplot = 1
            go to 900
         endif
************************ debug stuff *************************
*     print 9952, postdynm 
*9952 format('in psplot postdynm = ',a)
************************ debug stuff *************************
***************** copy pfmaster.post file to dynamic file ******** 
***************** use lcord as temporary file ********************
         last = lastch (postmstr)
         status = open_file(lcord, postmstr(1:last), 'F', 'R', ios)
         if (status .ne. 0) then
            write (errbuf(1), 137) ios, postmstr(1:last), lcord
  137       format (' Error No. ', i3,
     1              ' opening file ', a, ' on logical unit', i3)
            call prterx ('W',1)
            psplot = 1
            go to 900
         endif
************************ debug stuff *************************
*     print 9953, postmstr
*9953 format('in psplot postmstr = ',a)
************************ debug stuff *************************
  138    read (lcord,'(a)',end=139) postcp 
         write(lpost,'(a)')postcp
         go to 138
  139    close(unit=lcord) 
c
c     Open *.cor file
c
      endif   
************************ debug stuff *************************
*     print 9951, cordfil
*9951 format('in psplot cordfil = ',a)
************************ debug stuff *************************

******************** merge coord and base stuff **************
      last = lastch(cordfil)
      longfil = cordfil(1:last)
      call opnfila(lcord, longfil,  status)
      if (status .ne. 0) then
         write (errbuf(1), 140) ios, cordfil(1:last), lcord
  140    format (' Error No. ', i3,
     &           ' opening Coordinate file ', a, 
     &           ' on logical unit', i3)
         call prterx ('W',1)
         psplot = 1
         go to 900
      endif

      status = open_file(corfil, 'scratch.1', 'F', 'W', ios)
      if (status .ne. 0) then
         write (errbuf(1), 141) ios, 'scratch.1', corfil
  141    format (' Error No. ', i3,
     &           ' opening scratch file ', a, 
     &           ' on logical unit', i3)
         call prterx ('W',1)
         psplot = 1
         go to 900
      endif
c
******  Begin-Get comments from in_buffer *************************
c
*     i1 = nxt_term(in_buffer) + 1
      i1 = ipstfil
      if (in_buffer(i1:i1) .eq. linefeed ) i1 = i1 + 1
      i2 = index (in_buffer, null)
      kntuopt = 0
      do while (i1 .lt. i2)
         next = nxt_term(in_buffer(i1+1:)) + i1
******************** debug stuff ********************************
*     print 9845, next
*9845    format(' next = ',i3)     
******************** debug stuff ********************************
         kntucmnt = kntucmnt + 1
         if (kntucmnt .gt. 0 .and. kntucmnt .le. maxucmnt)then
            usrcmnt(kntucmnt) = in_buffer(i1:next-1)
******************** debug stuff ********************************
*     print 9841, kntucmnt, usrcmnt(kntucmnt)
*9841       format(' kntucmnt, usrcmnt(kntucmnt)',i4,1x ,a)
******************** debug stuff ********************************
******* look for instructions to append another .cor file ********
             if (usrcmnt(kntucmnt)(1:1) .eq. '&') then
               apcrfil =  usrcmnt(kntucmnt)(2:)
               iapcrfil = 1
             endif 
******* look for imbeded options records ***********
             if (usrcmnt(kntucmnt)(1:1) .eq. '@') then
               if (kntuopt .lt. maxuopt) kntuopt = kntuopt + 1
               usropts(kntuopt) =  usrcmnt(kntucmnt)(2:)
               kntucmnt = kntucmnt - 1
******************** debug stuff *******************************
*     print 9847, maxuopt, kntuopt, usropts(kntuopt)
*9847       format(' maxuopt, kntuopt, usropts(kntuopt)',2i4,1x ,a)
******************** debug stuff *******************************
             endif 
******************** debug stuff *******************************
*     print 9841, kntucmnt, usrcmnt(kntucmnt)
*     print 9843, in_buffer(1:500)
*9843       format(' in_buffer:', a)
*     print 9777, i1, i2, i1+next-2
*9777       format (' psplot.f: i1, i2, i1+next-2 = ', 3i4)
******************* debug stuff **********************************
          endif
          i1 = next
          if (in_buffer(i1:i1) .eq. linefeed ) i1 = i1 + 1
      enddo
      kntucmnt = kntucmnt - 1 

******  End-Get comments from in_buffer *************************

  145   errcord = 0
        call srtcord
        if (errcord .ne. 0) then
           print 147, errbuff(1:80)
  147      format(a)
c dlc        stop
           return
        endif
        kotgflg = 0
        kcorflg = 0
********************* SETUP FOR BUBBLE PLOT******************
************* This logic is now used for area loss setup too
*       if (kntarea .gt. 0) then
           write(in_buffer(1:100), 149) linefeed, null
  149      format('/GET_DATA,TYPE=LOAD_AREA',a,'(END)',a)
           out_buffer(1:1) = null
************************* debug stuff ***********************
*     print 9831, in_buffer(1:100)
*9831      format('before call p_ldardata', a)
************************* debug stuff ***********************
           status = p_ldardata(in_buffer, out_buffer)
           ld_ref = 1
************************* debug stuff ***********************
*     print 9873, out_buffer(1:100)
*9873      format('after call p_ldardata', a)
************************* debug stuff ***********************

*       endif
******************** merge coord and base stuff **************
        go to 170
******************** merge coord and base stuff **************

  170   continue
        rewind corfil
        eof = .false.

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
           status = open_file(lunusr,'user_ana.scr', 'U', 'W', ios)
           if (status .ne. 0) then
              write (errbuf(1), 104) ios, lunusr
  104         format (' Error No. ', i3,
     1           ' opening scratch file on logical unit',i3)
              call prterx ('W',1)
           endif
        else
           rewind lunusr
        endif

        write (lpost, 171)
  171   format('gsave'/'initPost')

        do while (.not. eof)
           if (kcorflg .eq. 0) then
               read (corfil, 110, end=500) text
  110	format (a)
**************** debug stuff ***********************
*     print 9955, text(1:100)
*9955          format(' in psplot text = ',a)
**************** debug stuff ***********************
           elseif (kcorflg .eq. 1) then
               text = nxtcord
               kcorflg = 0
           else
               go to 500
           endif

           if (text(1:5) .eq. '[ID C') then
              call postcord(text)

           elseif (text(1:1) .eq. 'O') then
              call postoptn(text)

           elseif (text(1:1) .eq. 'B') then

**** issue command '/GET_DATA,TYPE=LOAD_REF_AREA' the first time thru 
**** ie when the first bus or area record is processed  
              if (vaopdif .eq. 1 .and. ld_ref .eq. 1) then
                 write(in_buffer(1:100), 401) linefeed, null
                 out_buffer(1:1) = null
                 status = p_ldxardta(in_buffer, out_buffer)
                 ld_ref = 0
              endif

              if (dgoptyp .eq. 5) then   ! begin bus procedure 
                  nxtcord = text
                  kbusyes = 1
                  call postbus(blnkrcd)
              else 
              
              write(in_buffer, 172) text(3:14)
  172         format('B', t7, a)
              last = lastch(in_buffer)
              in_buffer(last+1:) = null
              out_buffer(1:) = null

************************* debug stuff ***********************
*     print 9963, bsopvlt, bsopgen
*9963         format( ' bsopvlt, bsopgen', 2i5)
*     print 9961, in_buffer(1:100)
*9961         format(' before call gtoutput', a)
************************* debug stuff ***********************
              if (vaopdif .eq. 0) then  
                call gtoutput(in_buffer, out_buffer)
              else
                call pf_odif(in_buffer, out_buffer,error) 
              endif
************************* debug stuff ***********************
*     print 9971, out_buffer(  1: 80)
*9971         format(' after call gtoutput', /,a)
************************* debug stuff ***********************
              first = 1
              last = index (out_buffer, null)
              kbusyes = 0
******************** debug stuff **************************
*     print 9911, first, last
*9911         format(' in psplot at 172 - first,last :', 2i5)
******************** debug stuff **************************
              do while (first .lt. last)
******************** debug stuff **************************
*     print 9921
*9921            format('in do while first lt last loop')
******************** debug stuff **************************

                 next = nxt_term(out_buffer(first+1:)) + first
************************** debug stuff ************************
*     print 9180, next, out_buffer(first:next-1)
*9180            format (1x, i5, a)
************************** debug stuff ************************

                 type = out_buffer(first:first)
************* no room for code with all these *&^%$ indents ********
                 if     (type .eq. 'B') then
                      kbusyes = 1
                      read(out_buffer(first:next-1),301) buspf, basepf
  301                 format(bz, t7, a8, t15, f4.0)
                      call postbus(out_buffer(first:next-1))
                 elseif (type .eq. 'L' .or. type .eq. 'E' .or.
     1                     type .eq. 'T') then
                      read(out_buffer(first:next-1),311)
     1                buspf1, basepf1, buspf2, basepf2
  311                 format(bz, t7, a8, t15, f4.0, t20, a8, t28,f4.0)
***************** debug stuff ********************
*     print 9181, first, next
*9181 format(' in psplot at 311+ - first,next :', 2i5)
*     print 9183, type,buspf1, basepf1, buspf2, basepf2
*9183 format('type,buspf1, basepf1, buspf2, basepf2', a2,2(a12,f7.2)) 
*     print 9185, out_buffer(first:first+80)
*     print 9187, out_buffer(next-80:next-1)
*9185 format( ' out_buffer(first:first+80) ',a)
*9187 format( ' out_buffer(next-80:next-1) ',a)
***************** debug stuff ********************
*                     if ( buspf1 .lt. buspf2 .or.
*    1                    (buspf1 .eq. buspf2 .and.
*    2                     basepf1 .lt. basepf2)) then
                         call postbrn(out_buffer(first:next-1))
*                     endif
                 endif
                 first = next 
                 if (out_buffer(first:first) .eq. linefeed)
     &              first = first + 1
              enddo
              endif                    ! end (dgoptyp .eq. 5) check
************************* debug stuff ***********************
*     print 9931, kbusyes
*9931 format('after first,last do loop - kbusyes', i5)
************************* debug stuff ***********************
              if (kbusyes .eq. 1) then

****** begin: out of indent room revert back*********  
        if (bsopsum .eq. 1 .and. kdspflg .eq. 0 .and. 
     1      (abs(psumflo) .gt. 0.5 .or. abs(qsumflo) .gt. 0.5)) then 
 
             ipsumflo = sign ((abs(psumflo) + 0.5), psumflo)
             lognum = alog10(amax1((abs(psumflo)+0.5),1.0)) + 1.0
             if (psumflo .lt. 0.0) lognum = lognum + 1
             if (lognum .gt. 6) then
                 apqsum = '(******)'
                 go to 340
             endif
             write (fmtpqsm( 9: 9), 331) lognum
  331        format(i1)
 
             iqsumflo = sign ((abs(qsumflo) + 0.5), qsumflo)
             lognum = alog10(amax1((abs(qsumflo)+0.5),1.0)) + 1.0
             if (qsumflo .lt. 0.0) lognum = lognum + 1
             if (lognum .gt. 6) then
                 apqsum = '(******)'
                 go to 340
             endif
             write (fmtpqsm(17:17), 331) lognum
************ debug stuff **********************
*     print 9931, fmtpqsm
*9931 format(' fmtpqsm: ',a)
************ debug stuff **********************
             write(apqsum, fmtpqsm) ipsumflo, iqsumflo
************ debug stuff **********************
*     print 9941, psumflo, qsumflo, ipsumflo, iqsumflo, apqsum
*9941 format(' psumflo,qsumflo, ipsumflo, iqsumflo, apqsum ',
*    12f10.1, 2i10,a)
************ debug stuff **********************
 
  340        namary(53:72) = apqsum(1:20)
         endif



****** end:   out of indent room revert back*********  

                  write (lpost, 183) busary, genary, reaary,
     1                          namary, flgary
  183             format(a/a/a/a/a)
              endif
           elseif (dgoptyp .eq. 5 .and. 
     1      (text(1:1) .eq. 'L' .or. text(1:1) .eq. 'E' .or. 
     2       text(1:1) .eq. 'T')) then
             kcorflg = 1
             nxtcord = text
************************* debug stuff ***********************
*     print 9601, nxtcord
*9601         format('before call postbrn nxtcord = ', a)
************************* debug stuff ***********************
             call postbrn(blnkrcd)    ! print coordinates only 

****************** LOGIC FOR BUBBLE PLOT *************
           elseif (text(1:1) .eq. 'A') then
           if (vaopdif .eq. 1 .and. ld_ref .eq. 1) then
              write(in_buffer(1:100), 401) linefeed, null
  401         format('/GET_DATA,TYPE=LOAD_REF_AREA',
     1        a,'(END)',a)
              out_buffer(1:1) = null
              status = p_ldxardta(in_buffer, out_buffer)
              ld_ref = 0
           endif
              write(in_buffer, 403) text(3:12), null
  403         format('A', t4, a, a)
              out_buffer(1:1) = null
 
************************* debug stuff ***********************
*     print 9861, in_buffer(1:100)
*9861         format('before call p_gtardata', a)
************************* debug stuff ***********************
              if (vaopdif .eq. 0) then
                status = p_gtardata(in_buffer, out_buffer)
              else
                status = pa_odif(in_buffer, out_buffer)
              endif

************************* debug stuff ***********************
*     print 9871, out_buffer(1:100)
*9871         format('after call p_gtdata', a)
************************* debug stuff ***********************
              first = 1
              last = index (out_buffer, null)
              kbusyes = 0
******************** debug stuff **************************
*     print 9811, first, last
*9811         format(' in psplot at 403 - first,last :', 2i5)
******************** debug stuff **************************
              do while (first .lt. last)
******************** debug stuff **************************
*     print 9821
*9821            format('in do while first lt last loop')
******************** debug stuff **************************
 
                 next = nxt_term(out_buffer(first+1:)) + first
************************** debug stuff ************************
*     print 9801, out_buffer(first:next-1)
*     print 9801, out_buffer(first:first+99)
*9801            format (1x, a)
************************** debug stuff ************************
                 type = out_buffer(first:first)
************* no room for code with all these *&^%$ indents ********
                 if     (type .eq. 'A') then
                      kbusyes = 1
                      read(out_buffer(first:next-1),411) bubpf
  411                 format(bz, t3, a10)
                      bubpf1 = bubpf
                      call postara(out_buffer(first:next-1))
                 elseif (type .eq. 'I') then
                      read(out_buffer(first:next-1),421) bubpf2
  421                 format(bz, t14, a10)
                      if ( bubpf1 .lt. bubpf2 ) then
                        call postint(out_buffer(first:next-1))
                      endif
                 endif
                 first = next
                 if (out_buffer(first:first) .eq. linefeed)
     &              first = first + 1
              enddo
************************* debug stuff ***********************
*   print 9833, kbusyes
*9833 format('after first,last do loop - kbusyes', i5)
************************* debug stuff ***********************
              if (kbusyes .eq. 1)
     1        write (lpost, 431) corary, namary, araary
  431         format(a/a/a)

****************** LOGIC FOR BUBBLE PLOT *************

           else if (text(1:1) .eq. '>') then
              numdef(1) = numdef(1) + 1
              usrdef(numdef(1)) = text
              numusr = 1
           else if (text(1:1) .eq. 'C' .or. text(1:1) .eq. 'P'
     1         .or. text(1:1) .eq. 'D' ) then
              numtxt(1) = numtxt(1) + 1
              usrtxt(numtxt(1)) = text
              numusr = 1
           endif
        enddo
************************* debug stuff ***********************
*     print 9941
*9941   format(' computing values for xymbols')
************************* debug stuff ***********************
  500   continue

********** BEGIN LOGIC FOR OUTAGE BUSES AND BRANCHES **************
        write(in_buffer, 511)
  511   format('SHORT_LIST')
        last = lastch(in_buffer)
        in_buffer(last+1:last+1) = null
        out_buffer(1:1) = null

************************* debug stuff ***********************
*     print 9861, in_buffer(1:100)
*9861   format('before call outage_lst', a)
************************* debug stuff ***********************
        call outage_lst(in_buffer, out_buffer)
************************* debug stuff ***********************
*     print 9871, out_buffer(1:100)
*9871   format('after call outage_lst', a)
************************* debug stuff ***********************
        rewind corfil
        kcorflg = 0
        first = 1
        last = index (out_buffer, null)
******************** debug stuff **************************
*     print 9811, first, last
*9811   format(' in psplot at 172 - first,last :', 2i5)
******************** debug stuff **************************
        do while (first .lt. last)
******************** debug stuff **************************
*     print 9821
*9821      format('in do while first lt last loop')
******************** debug stuff **************************

           next = nxt_term(out_buffer(first+1:)) + first
************************** debug stuff ************************
*     print 9831, out_buffer(first:next-1)
*9831      format ('psplot-9831', a)
************************** debug stuff ************************
           type = out_buffer(first:first)
           read(out_buffer(first:next-1),521) buspf, basepf
  521      format(bz, t7, a8, t15, f4.0)
************************** debug stuff ************************
*          junk = (ksrchcor(buspf, basepf))
*     print 9851, buspf, basepf, junk
*9851      format('buspf, basepf, ksrchcor = ' a8, f6.1, i5)
************************** debug stuff ************************
           if (ksrchcor(buspf, basepf) .gt. 0) then
  530         if (kcorflg .eq. 0) then
                 read(corfil, 110, end = 590) text
              else
                 text = nxtcord
                 kcorflg = 0
              endif
****************** debug stuff *************************
*     print 9881, text
*9881         format(' text = ',a)
****************** debug stuff *************************
              if ( text(1:1) .eq. 'B' .or. text(1:1) .eq. 'T' .or. 
     1             text(1:1) .eq. 'L' .or. text(1:1) .eq. 'E') then
                 read(text, 541) busnm, buskv
  541            format( bz, 2x, a8, f4.0)
****************** debug stuff *************************
*     print 9701, buspf,basepf, busnm, buskv
*9701            format('buspf,basepf, busnm, buskv = ',2(a8,f6.1))
****************** debug stuff *************************
                 if (buspf .gt. busnm .or.
     1              buspf .eq. busnm .and. basepf .gt. buskv) then
                    go to 530
                 elseif (buspf .lt. busnm .or.
     1                   buspf .eq. busnm .and. basepf .lt. buskv) then
                    nxtcord = text
                    kcorflg = 1
                    go to 580
                 endif
***** match between coordinate data and powerflow data ******
****************** debug stuff *************************
*     print 9891, text
*9891            format('match text = ', a)
*     print 9893, out_buffer(first:next-1)
*9893            format('match pf data = ', a)
****************** debug stuff *************************
                 if ( type .eq. 'B') then
                    kotgflg = 1
                    call postbus(out_buffer(first:next-1))
                    write (lpost, 183) busary, genary, reaary,
     1                                 namary, flgary
***** check for non-printing buses ****************
                    if ( text(2:2) .eq. '1') go to 530
                 else
*********** are there any parallel circuits in service? *****
                    write(in_buffer, 551)out_buffer(first:next-1)
  551               format(a)
                    lastp = lastch(in_buffer)
                    in_buffer(lastp+1:) = null
                    short_buffer(1:) = null
****************** debug stuff *************************
*     print 9711, in_buffer(1:50)
*9711               format('in_buffer-9711 = ',a)
****************** debug stuff *************************
                    call service_query(in_buffer, short_buffer)
****************** debug stuff *************************
*     print 9721, short_buffer
*9721               format('short_buffer = ',a)
*     print 9731, out_buffer(1:150)
*9731               format('out_buffer = ',a)
****************** debug stuff *************************
                    kotgflg = 1
                    if (short_buffer(1:1) .eq. '1') kotgflg = 2
                    if (text(1:1) .ne. 'B') then
                       kcorflg = 1
                       nxtcord = text
                    endif
                    call postbrn(out_buffer(first:next-1))
                 endif
                 go to 580
              else
                 go to 530
              endif
           endif
  580      first = next 
           if (out_buffer(first:first) .eq. linefeed) first = first + 1
        enddo
  590   continue
********** END LOGIC FOR OUTAGE BUSES AND BRANCHES **************
********** Put ostates command on *.ps file in case of no solution   
        write (lpost, 601) ostates
  601   format( i6, ' oStates') 

C
C     	Compute new values for symbols
C
        if (numdef(1) .gt. 0) then
           lunusr = 25
           rewind lunusr

           iuser = 1
           max1 = numdef(iuser)
           do first  = 1, max1, 200
             last = min0 (first+199,max1)
             write (lunusr) (usrdef(i),i=first,last)
           enddo
           max2 = numtxt(iuser)
           do first = 1, max2, 200
             last = min0 (first+199,max2)
             write (lunusr) (usrtxt(i),i=first,last)
           enddo

C  *********** be sure losses have been calculated
********** debug stuff **************************
*     print 9981  
*9981      format(' before updzon')
********** debug stuff **************************
*          update(1) = 0  ***** now set in  updzon()
*          call updzon()
*          if (ostates .ge. 5) call updzon() ***** now called from p_ldardata
********** debug stuff **************************
*     print 9991  
*9991      format(' after updzon')
********** debug stuff **************************

           call getdef (numdef(1), usrdef, numsym, symnam, symval,
     1                  symind, nchr, ndefch, defchr)
           if (vaopdif .eq. 1) then
              call ogetdef (numdef(1), usrdef, numsym, symnam, symvbg,
     1                     symind, nchrbg, ndefchbg, defchrbg)
              do i = 1, numdef(1)
**************** debug stuff ***********************
*     print 9221, i, symval(i), symvbg(i)
*9221 format(' in psplot - i, symval(i), symvbg(i)' i5, 2f10.2)
**************** debug stuff ***********************
                 if (ndefchbg(i) .eq. 0) then
                    symval(i) = symval(i) - symvbg(i)  
                 else
                    if (symnam(i)(1:3) .eq. 'ALT') then
                       indbg = ndefchbg(i)
                       defchr(indbg) = defchrbg(indbg)
**************** debug stuff ***********************
*     print 9223, i, indbg,  ndefchbg(i), defchrbg(indbg) 
*9223 format(' i, indbg,  ndefchbg(i), defchrbg(indbg)' 3i5, a)
**************** debug stuff ***********************
                    endif
                 endif
              enddo
           endif


           do i = 1, numtxt(1)
              text = usrtxt(i)
**************** debug stuff ***********************
*     print 9231, i, text, symnam(i), symval(i)
*9231 format(' in pslpot i, text, symnam(i), symval(i)', i5, a,a,f10.2)
**************** debug stuff ***********************
              usrtxt(i)(2:) = subdef (text(2:), numsym, symnam, symval,
     1                                symind, nchr, ndefch, defchr)
           enddo
        endif
        do i = 1, numtxt(1)
************************** debug stuff ************************
*          write (*,9201) usrtxt(i)
*9201	   format (1x, a)
************************** debug stuff ************************
           if     (usrtxt(i)(1:1) .eq. 'C') then
               call postcmnt(usrtxt(i))
           elseif (usrtxt(i)(1:1) .eq. 'P') then
               call postpost(usrtxt(i))
           elseif (usrtxt(i)(1:1) .eq. 'D') then
               call postdraw(usrtxt(i))
           endif
        enddo
*********Begin-put Boilerplate data on Diagram ***************
        if (blrplate) then 
           blrplate = .false.
           call am_date(date)
           call timoda(time)
           flotxt = ' '
           if ( dgoptyp .eq. 1 .or. dgoptyp .eq. 3 .or. dgoptyp .eq. 4) 
     1        write (flotxt, 611) broptxt(broppsn*2 + bropqsn + 1),
     2        broptxt(bropprc*2 + bropqrc + 1)
  611         format( ' FLOW: S:', a2, '  R:',a2) 
           write (lpost, 651) dgoptxt(dgoptyp), flotxt, date, time, 
     &        prgvsn
  651      format('(Diagram Type: ',a7, 1x, a17, 1x, ' Plotted: ',
     &        a9, ' Time: ', a8, ' Version: ', a20, ') plotId')  

*********Begin-Put coordinate file name on diagram ************
           last = lastch(cordfil)
           write (lpost, 661) cordfil(1:last)
  661      format('(',a, ') cordTxt')
*********End-Put coordinate file name on diagram ************

*********Begin-Put user comments on diagram ****************
           kntucmnt = min0(kntucmnt, maxucmnt)
           if (kntucmnt .gt. 0) then
              do i1 = 1, kntucmnt
                 call postucmt (usrcmnt(i1))
              enddo
              kntucmnt = 0
           endif
*********End-Put user comments on diagram ****************
*********Begin-put header records and powerflow comments on diagram *** 
              write(in_buffer, 671) null
  671         format('/GET_DATA,TYPE=COMMENTS', a)
              out_buffer(1:1) = null
 
              call gtcommnt(in_buffer, out_buffer)
 
              first = 1
              last = index (out_buffer, null)
              indcmnt = 0
              do while (first .lt. last)
 
                 next = nxt_term(out_buffer(first+1:)) + first
                 indcmnt = indcmnt + 1
                 if (indcmnt .le. 5) then
************************** process header records *************
                    if     (indcmnt .le. 2) then 
                      continue   ! discard this header record
                    elseif (indcmnt .le. 3 .and. ssophdr .ne. 1) then 
                      continue   ! discard this header record
                    elseif (ssophdr .ne. 0) then
                      if (jndcmt .eq. 0) then
                        jndcmt = 1
                        write (lpost, 681) out_buffer(first+1:next-1)
*** first header record must locate the header coordinates via the pfmaster.post 
  681                   format('(', a, ') pfHdr1' )
                      else
                        write (lpost, 691) out_buffer(first+1:next-1)
*** remainder of records can be printed below first one with CordCmnt 
  691                   format('(', a, ') 0.0 0.0  CordCmnt' )
                      endif
                    else
                      continue   ! discard this header record
                    endif
                 elseif (indcmnt .le. ssoppfc + 5) then 
************************** process comment records *************
                    call postucmt (out_buffer(first+1:next-1))
                 else
                    go to 700
                 endif
                 first = next
                 if (out_buffer(first:first) .eq. linefeed)
     &              first = first + 1
              enddo
  700 continue



*********End-put header records and powerflow comments on diagram *** 
        endif
*********End-put Boilerplate data on Diagram ***************

      
        write (lpost, 711)
  711   format('grestore')
        if (morcord .lt. 3) go to 145

        psplot = 0
        write (lpost, 721)
  721   format('showpage')
        if (multprc .eq. multplt) then
           rewind lpost
           close (unit = lpost)
        endif 
        close (unit = lcord)
        close (unit = corfil)
  900   continue
        return
        end
