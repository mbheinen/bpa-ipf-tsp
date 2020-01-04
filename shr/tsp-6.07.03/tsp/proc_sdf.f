C    %W% %G%
      subroutine proc_sdf (inputfile)
      character *(*) inputfile

      include 'tspinc/params.inc'
      include 'tspinc/param.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/blkcom1.inc'
      include 'tspinc/namec.inc' 
      include 'tspinc/bname.inc'
      include 'tspinc/buskv.inc'
      include 'tspinc/busnum.inc'
      include 'tspinc/igentn.inc'
      include 'tspinc/gentbla.inc'
      include 'tspinc/comn34.inc'
      include 'tspinc/bcur.inc'
      include 'tspinc/busvolt.inc'
      include 'tspinc/lnk12.inc'
      include 'tspinc/vfhistory.inc'

      common /error_code/ error_code
      integer error_code

      integer MAXLOADSHED
      parameter (MAXLOADSHED = 1000)

      common /amtrx/ numloadshed, sortloadshed(MAXLOADSHED),
     &               loadshed(MAXLOADSHED)
      integer numlodshed, sortloadshed
      character loadshed*132

      character gid*1, gname*16, text*512, word(100)*20, zone1c*2,
     &          ljstfy*60, tempfile*60
      logical finished, finished1, errorsw
      integer open_ge_file, read_ge_file, close_ge_file, firstxstr, 
     &        ftn_atoi, status
      
      save

      errorsw = .false.

      numbus = 0
      numgen = 0
      numtime = 0
      numloadshed = 0
      mxxbus = 0
      nvdlor = 0
      nfqlor = 0
      nvllor = 0
      nvdhir = 0
      nvdlof = 0
      nfqlof = 0
      nvllof = 0
      nvdhif = 0
      ibcode = 0
      iflagr = 0
      nintrvl = 100

      tempfile = ljstfy(inputfile)
      last = lastch(tempfile)
      status = open_ge_file (0, tempfile(1:last), 'r')
      if (status .ne. 0) then
	write (outbuf, 10000) inputfile(1:last)
10000   format (' Error opening Special Data File ', a)
        call prterr('W', 1)
        errorsw = .true.
        go to 900
      endif
      write (outbuf, 10010) inputfile(1:last)
10010 format (' Processing Special Data File ', a)
      call prtout(1)
        
      text = ' '
      last = read_ge_file (0, text)
      numrec = 0
      finished = (last .eq. 0)
      do while (.not. finished)
        numrec = numrec + 1
        if (text(1:1) .eq. '[') then
          call uscan (text(1:last), word, nwrd, '=',  '[] ')   
          if (firstxstr (word(1), 'DIP*') .ne. 0) then
            text = ' '
            last = read_ge_file (0, text)
            if (last .gt. 0) then
              finished1 = .not. (text(last:last) .eq. '-')
              do while (.not. finished1)
                numrec = numrec + 1
                text(last:last) = ' '
                last = read_ge_file (0, text(last:)) + last - 1
                if (last .eq. 1) then
                  finished1 = .true.
                else
                  finished1 = .not. (text(last:last) .eq. '-')
                endif
              enddo
              call uscan (text(1:last), word, nwrd, '=',  ' ')   
              num = 0
              do iwrd = 1, nwrd, 3
                if (word(iwrd+1) .ne. '=') then
                  last1 = lastch (word(iwrd))
                  last2 = lastch (word(iwrd+1))
                  last3 = lastch (word(iwrd+2))
                  write (errbuf(1), 10020) word(iwrd)(1:last1),
     &              word(iwrd+1)(1:last2), word(iwrd+2)(1:last3)
10020             format (' Syntatic error in assignent (', a, 1x, a,
     &              1x, a, ')')
                  call prterr('W', 1)
                endif
                if (firstxstr(word(iwrd), 'TSTART') .ne. 0) then
c                 tstart = time to begin scanning 
                  tstart = ftn_atof(word(iwrd+2))
                else if (firstxstr(word(iwrd), 'TARM') .ne. 0) then
c                 tarm = time to arm final report writing
                  tarm = ftn_atof(word(iwrd+2))
                else if (firstxstr(word(iwrd), 'VDLO') .ne. 0) then
c                 vdlo = low volt dev threshold
                  vdlo = ftn_atof(word(iwrd+2))
                else if (firstxstr(word(iwrd), 'FQLO') .ne. 0) then
c                 fqlo = low freq threshold
                  fqlo = ftn_atof(word(iwrd+2))
                else if (firstxstr(word(iwrd), 'VLLO') .ne. 0) then
c                 vllo = low volt threshold
                  vllo = ftn_atof(word(iwrd+2))
                else if (firstxstr(word(iwrd), 'VDHI') .ne. 0) then
c                 vdhi = high volt dev threshold
                  vdhi = ftn_atof(word(iwrd+2))
                else if (firstxstr(word(iwrd), 'VDIPLD') .ne. 0) then
c                 vdipld = low voltage dip threshold (load buses)
                  vdipld = ftn_atof(word(iwrd+2))
                else if (firstxstr(word(iwrd), 'VDIPNL') .ne. 0) then
c                 vdipnl = low voltage dip threshold (no-load buses)
                  vdipnl = ftn_atof(word(iwrd+2))
                else if (firstxstr(word(iwrd), 'FRQDIP') .ne. 0) then
c                 frqdip = low frequency dip threshold
                  frqdip = ftn_atof(word(iwrd+2))
                else if (firstxstr(word(iwrd), 'NVDLOR') .ne. 0) then
c                 nvdlor = max no. low volt dev buses reported during
c                          run
                  nvdlor = ftn_atoi(word(iwrd+2))
                else if (firstxstr(word(iwrd), 'NVDLOF') .ne. 0) then
c                 nvdlof = max no. low volt dev buses in final report
                  nvdlof = ftn_atoi(word(iwrd+2))
                else if (firstxstr(word(iwrd), 'NFQLOR') .ne. 0) then
c                 nfqlor = max no. low freq buses reported during
c                          run
                  nfqlor = ftn_atoi(word(iwrd+2))
                else if (firstxstr(word(iwrd), 'NFQLOF') .ne. 0) then
c                 nfqlof = max no. low freq buses in final report
                  nfqlof = ftn_atoi(word(iwrd+2))
                else if (firstxstr(word(iwrd), 'NVLLOR') .ne. 0) then
c                 nvllor = max no. low volt buses reported during
c                          run
                  nvllor = ftn_atoi(word(iwrd+2))
                else if (firstxstr(word(iwrd), 'NVLLOF') .ne. 0) then
c                 nvllof = max no. low volt buses in final report
                  nvllof = ftn_atoi(word(iwrd+2))
                else if (firstxstr(word(iwrd), 'NVDHIR') .ne. 0) then
c                 nvdhir = max no. hi volt dev buses reported during
c                          run
                  nvdhir = ftn_atoi(word(iwrd+2))
                else if (firstxstr(word(iwrd), 'NVDHIF') .ne. 0) then
c                 nvdhif = max no. hi volt dev buses in final report
                  nvdhif = ftn_atoi(word(iwrd+2))
                else if (firstxstr(word(iwrd), 'IBCODE') .ne. 0) then
c                 ibcode = print control switch
                  ibcode = ftn_atoi(word(iwrd+2))
                else if (firstxstr(word(iwrd), 'IFLAGR') .ne. 0) then
c                 iflagr = print final report flag
                  iflagr = ftn_atoi(word(iwrd+2))
                else if (firstxstr(word(iwrd), 'NINTRVL') .ne. 0) then
c                 nintrvl = time step interval for intermittent reports
                  nintrvl = ftn_atoi(word(iwrd+2))
                else
                  last1 = lastch (word(iwrd))
                  last2 = lastch (word(iwrd+1))
                  last3 = lastch (word(iwrd+2))
                  write (errbuf(1), 10030) word(iwrd)(1:last1),
     &              word(iwrd+1)(1:last2), word(iwrd+2)(1:last3)
10030             format (' Unrecognized keyword (', a, 1x, a,
     &              1x, a, ')')
                  call prterr('W', 1)
                endif
              enddo

              write (outbuf, 10040) tstart
10040         format (' Voltage scan parameters - TSTART  = ', f8.3)
              call prtout (1)
              write (outbuf, 10050) tarm
10050         format ('                         - TARM    = ', f8.3)
              call prtout (1)
              write (outbuf, 10060) vdlo
10060         format ('                         - VDLO    = ', f8.3)
              call prtout (1)
              write (outbuf, 10070) fqlo
10070         format ('                         - FQLO    = ', f8.3)
              call prtout (1)
              write (outbuf, 10080) vllo
10080         format ('                         - VLLO    = ', f8.3)
              call prtout (1)
              write (outbuf, 10090) vdhi
10090         format ('                         - VDHI    = ', f8.3)
              call prtout (1)
              write (outbuf, 10100) vdipld
10100         format ('                         - VDIPLD  = ', f8.3)
              call prtout (1)
              write (outbuf, 10110) vdipnl
10110         format ('                         - VDIPNL  = ', f8.3)
              call prtout (1)
              write (outbuf, 10120) frqdip
10120         format ('                         - FRQDIP  = ', f8.3)
              call prtout (1)
              write (outbuf, 10130) nvdlor
10130         format ('                         - NVDLOR  = ', i8)
              call prtout (1)
              write (outbuf, 10140) nvdlof
10140         format ('                         - NVDLOF  = ', i8)
              call prtout (1)
              write (outbuf, 10150) nfqlor
10150         format ('                         - NFQLOR  = ', i8)
              call prtout (1)
              write (outbuf, 10160) nfqlof
10160         format ('                         - NFQLOF  = ', i8)
              call prtout (1)
              write (outbuf, 10170) nvllor
10170         format ('                         - NVLLOR  = ', i8)
              call prtout (1)
              write (outbuf, 10180) nvllof
10180         format ('                         - NVLLOF  = ', i8)
              call prtout (1)
              write (outbuf, 10190) nvdhir
10190         format ('                         - NVDHIR  = ', i8)
              call prtout (1)
              write (outbuf, 10200) nvdhif
10200         format ('                         - NVDHIF  = ', i8)
              call prtout (1)
              write (outbuf, 10210) ibcode
10210         format ('                         - IBCODE  = ', i8)
              call prtout (1)
              write (outbuf, 10220) iflagr
10220         format ('                         - IFLAGR  = ', i8)
              call prtout (1)
              write (outbuf, 10230) nintrvl
10230         format ('                         - NINTRVL = ', i8)
              call prtout (1)
              text = ' '
              last = read_ge_file (0, text)
              finished = (last .eq. 0)
            else    
              finished = .true.
            endif
          else if (firstxstr (word(1), 'LOAD*') .ne. 0) then
            finished1 = .false.
            do while (.not. finished1)
              text = ' '
              last = read_ge_file (0, text)
              if (last .gt. 0) then
                numrec = numrec + 1
                if (text(1:1) .ne. 'U') then
                  finished1 = .true.
                else 
                  numloadshed = numloadshed + 1
                  loadshed(numloadshed) = text(1:last)
                  sortloadshed(numloadshed) = numloadshed
                endif
              else
                finished = .true.
                finished1 = .true.
              endif
            enddo
          else
            write (errbuf(1), 10240) text(1:40)
10240       format (' Unrecognized segment in Special Data File (', a, 
     &        ')')
            call prterr('W', 1)
            errorsw = .true.
          endif
        else
          write (errbuf(1), 10250) 
10250     format (' Record not preceded with a "[...]" segment idenitife
     &r')
          write (errbuf(2), 10260) text(1:40)
10260     format (' Record rejected (', a, ')')
          call prterr('W', 1)
          errorsw = .true.
        endif
      enddo

  900 continue
      if (errorsw) then
        error_code = 10
        call set_exit(error_code)
      endif
      return
      end
