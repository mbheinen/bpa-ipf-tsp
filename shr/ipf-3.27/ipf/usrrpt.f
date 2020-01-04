C    @(#)usrrpt.f	20.7 8/20/98
C****************************************************************
C
C   	File: usrrpt.f
C
C   	Purpose: Writes user-defined reports to two files, the power 
C                flow output (.PFO) and OUTPUT files. The OUTPUT file 
C                is defined in the Program Control Language file with
C                the command /USER_ANALYSIS, FILE = <file_name>, 
C                                            OUTPUT = <file_name>
C
C   	Author: Walt Powell            Date: 13 November 1992
C   	Called by: analys.f
C
C****************************************************************
      subroutine usrrpt

      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/usranl.inc'
      include 'ipfinc/coment.inc'
 
      character symnam(1000) * 12, subdef * 132, defchr(100) * 40
      real symval(1000)
      integer symind(1000), ndefch(1000), first
C ***                                                                  *
C *** LOCAL VARIABLES                                                  *
C ***                                                                  *
      dimension subhdr(3)
      character header * 133, subhdr * 133, text * 132
      integer nmbrec, numsh, i
 
C ***                                                                  *
C *** Use sequential access input file already opened.                 *
C ***                                                                  *
      rewind lunusr
      do 272 iuser = 1, numusr
      lun = 0
      max1 = numdef(iuser)
      do 92 first  = 1, max1, 200
         last = min0 (first+199,max1)
         read (lunusr) (usrdef(i),i=first,last)
   92 continue
      max2 = numtxt(iuser)
      do 94 first = 1, max2, 200
         last = min0 (first+199,max2)
         read (lunusr) (usrtxt(i),i=first,last)
   94 continue
 
      if (usrfil(iuser) .ne. ' ') then
C ***                                                                  *
C ***    NUMUPX keeps track of the total number of records in the
C ***    user analysis output file.
C ***                                                                  *
         numupx(iuser) = 0
C ***                                                                  *
C ***    Open UA output file if option enabled.                        *
C ***                                                                  *
         lun = 23
         close (unit=lun, err = 100)
  100    open (unit=lun, file = usrfil(iuser), status = 'OLD',
     2      err = 103)
C *** removed to meet F77 rules *********************
C    1      RECORDTYPE = 'VARIABLE',
C ***************************************************
C
C        Read to end-of-file to emulate 'APPEND' option.
C
  101    read (lun, 102, end=130) text
  102    format (a)
         go to 101
 
  103    open (unit=lun, file = usrfil(iuser), status = 'NEW',
     2      err = 110)
C *** removed to meet F77 rules *********************
C    1      RECORDTYPE = 'VARIABLE',
C ***************************************************
         go to 130
 
  110    last = lastch (usrfil(iuser))
         write (errbuf(1),120) usrfil(iuser)(1:last)
  120    format (' User_Analysis output file cannot be opened (', a,
     1      ')')
         call prterx ('W', 1)
         usrfil(iuser) = ' '
         lun = 0
  130    continue
      endif
C ***                                                                  *
C *** Compute new values for symbols                                   *
C ***                                                                  *
      call getdef (numdef(iuser), usrdef, numsym, symnam, symval,
     1             symind, nchr, ndefch, defchr)
 
      first = 1
 
  140 do 250 ir = first, numtxt(iuser)
C ***                                                                  *
C ***    Search for "H" header record.                                 *
C ***                                                                  *
         if (usrtxt(ir)(1:1) .eq. 'H') then
C ***
C ***       Initialize counter NMBREC to zero. NMBREC is used to count
C ***       the number of records processed.  It is reset to one (1)
C ***       after 50 records have been encountered. This counter is used
C ***       to flag when it is time to write the header and subheaders
C ***       to the top of the next page.
C ***
            nmbrec = 0
C ***
C ***       If user analysis output file is enabled, store the header
C ***       record for writing to top of page when either a user defined
C ***       page break ('1' in column 2) or 50 records have been written
C ***       to the U. A. output file, and write header record to top of
C ***       user analysis file now. Also store a '1' in column one of
C ***       header record so that page breaks can be forced by the user.
C ***
            if (lun .ne. 0) then
               header = usrtxt(ir)(2:)
               header(1:1) = '1'
               write (lun,150 ) header
  150          format (a)
            endif
C ***
            call forbtm
            outbuf = usrtxt(ir)(2:)
            call shdlod(1)
C ***                                                                  *
C ***       Process any subheader "S" records.                         *
C ***                                                                  *
C ***       Initialize arrays which store subheader records to ' '
C ***                                                                  *
            outbuf = ' '
            do 160 i = 2, 5
               call shdlod(i)
  160       continue
            do 170 i = 1, 3
               subhdr(i) = ' '
  170       continue
c
c           Append case comment text if APPEND_COMMENTS flag set.
c
            if (cmnflg(iuser)) then
               do i = 1, ncom
                  outbuf = com(i)
                  call prtout (1)
                  if (lun .gt. 0) write (lun, 150) outbuf
               enddo
            endif
C ***                                                                  *
C ***       Initialize counter NUMHDR which saves the number of header &
C ***       subheader records found in the user analysis input file. It
C ***       is initialized to '1' since a header record is required.
C ***                                                                  *
            numhdr = 1
C ***                                                                  *
C ***       Initialize counter NUMSH which saves the number of
C ***       subheader records found in the user analysis input file.
C ***                                                                  *
            numsh = 0
C ***                                                                  *
            do 190 ix = ir + 1, numtxt(iuser)
C ***                                                                  *
C ***          If a subheader record is found.
C ***                                                                  *
               if (usrtxt(ix)(1:1) .eq. 'S') then
C ***                                                                  *
C ***             Increment counter NUMHDR (number of header and
C ***             subheader records).
C ***                                                                  *
                  numhdr = numhdr + 1
C ***                                                                  *
C ***             Increment counter NUMSH (number of subheader records).
C ***                                                                  *
                  numsh = numsh + 1
C ***                                                                  *
C ***             If user analysis file is enabled and if counter NUMSH
C ***             is less than or equal to three, store the subheader
C ***             record for writing to top of page when either a user
C ***             defined page break is encountered ('1' in column 2) or
C ***             50 records have been written to the UA output file,
C ***             and write subheader record to top of page (but below
C ***             header) of user analysis file now.
C ***                                                                  *
                  if (numsh .le. 3 .and. lun .ne. 0) then
                     subhdr(numsh) = usrtxt(ix)(2:)
                     write (lun,150 ) subhdr(numsh)
                  endif
C ***                                                                  *
C ***             If counter NUMHDR is less than or equal to four, store
C ***             the subheader record for writing to top of page when
C ***             fifty records have been written to the powerflow
C ***             output (.PFO) file, and write subheader record to top
C ***             of page (but below header) of .PFO file now.
C ***                                                                  *
                  if (numhdr .le. 4) then
                     write (outbuf,150) usrtxt(ix)(2:)
                     call shdlod(numhdr)
                  endif
               else
                  go to 200
               endif
  190       continue
            call fortop
            go to 202
C ***                                                                  *
C ***       Determine the number of subheader records used (note that
C ***       only 3 are allowed). Place a '#' sign in the array element
C ***       following the last used element, to force a blank line
C ***       between the last subheader and the user analysis defined
C ***       text (C records) in the .PFO file.
C ***                                                                  *
  200       if (subhdr(1) .eq. ' ') then
               outbuf = '#'
               call shdlod(2)
            elseif (subhdr(2) .eq. ' ') then
               outbuf = '#'
               call shdlod(3)
            elseif (subhdr(3) .eq. ' ') then
               outbuf = '#'
               call shdlod(4)
            elseif (subhdr(3) .ne. ' ') then
               outbuf = '#'
               call shdlod(5)
            endif
C ***                                                                  *
C ***       Process comment "C" text                                   *
C ***                                                                  *
            call fortop
C ***                                                                  *
  202       first = ix
            do 220 ix = first, numtxt(iuser)
               if (usrtxt(ix)(1:1) .eq. 'C') then
C ***                                                                  *
C ***             Substitute any symbols for their numerical value.    *
C ***                                                                  *
                  outbuf = subdef( usrtxt(ix)(2:), numsym, symnam,
     1                           symval, symind, nchr, ndefch, defchr)
C ***                                                                  *
C ***             Write output to UA file, if option enabled.          *
C ***                                                                  *
                  if (lun .ne. 0) then
C ***                                                                  *
C ***                Increment counters NUMUPX and NMBREC.
C ***                                                                  *
                     numupx(iuser) = numupx(iuser) + 1
                     nmbrec = nmbrec + 1
C ***                                                                  *
C ***                If a 'C' record is found with a '1' in column two,
C ***                the user wants to force a page break in the UA
C ***                output file. Blank out the '1', since the header
C ***                record contains a '1' in column one. Otherwise if
c ***                50 records have been written to the UA output file,
C ***                do the same. Set the counter NMBREC back to one,
C ***                since this counter keeps track of the number of
C ***                records written to the UA file. Write the header,
C ***                subheaders, and a blank line to the UA file.
C ***                                                                  *
                     if ((outbuf(1:1) .eq. '1') .or.
     1                   (nmbrec .ge. 50)) then
                        outbuf(1:1) = ' '
                        nmbrec = 1
                        write (lun,150 ) header
                        do 210 i = 1, min(numsh,3)
                           write (lun,150 ) subhdr(i)
  210                   continue
                        write (lun,150 ) ' '
                     endif
C ***                                                                  *
C ***                Write the record to the UA output file.
C ***                                                                  *
                     write (lun,150 ) outbuf
C ***                                                                  *
                  endif
C ***                                                                  *
C ***             Write output to .PFO file.  Note that column 1 is    *
C ***             restricted to " " or "0" for this file. This disables*
C ***             user forced page breaks.                             *
C ***                                                                  *
  217             if (outbuf(1:1) .eq. '1') outbuf(1:1) = ' '
                  call prtout (1)
C ***                                                                  *
               else if (usrtxt(ix)(1:1) .eq. 'H') then
                  first = ix
                  go to 140
               endif
  220       continue
            go to 252
C
         else
C
            write (errbuf(1),230 )
  230       format ('"H" record does not precede /USER_ANALYSIS text.')
            write (errbuf(2),240 ) usrtxt(ir)(1:80)
  240       format ('/USER_ANALYSIS text (', a, ') ')
            call prterx ('W', 2)
 
         endif
 
  250 continue
C ***                                                                  *
C *** Close User Analysis output file if enabled.
C ***                                                                  *
  252 if (lun .ne. 0) then
         close (unit=lun, err =260 )
  260    continue
         last = lastch (usrfil(iuser))
         write (outbuf,270 ) numupx(iuser), usrfil(iuser)(1:last)
  270    format (1x, i4,
     1      ' records written to /USER_ANALYSIS auxiliary file ', a)
         call prtout (1)
      endif
  272 continue
C ***                                                                  *
C *** Blank out all subheaders used in this module.                    *
C ***                                                                  *
  280 do 290 i = 1, 5
         outbuf = ' '
         call shdlod( i )
  290 continue
 
      return
      end
