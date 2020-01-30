C    @(#)prtusr.f	20.4 11/11/97
      subroutine prtusr (lun, numrec)
      dimension lun(*), numrec(*)
C                                                                      *
C     This subroutine prints user-defined reports.                     *
C                                                                      *
C     LUN is logical unit number of auxiliary output.                  *
C     NUMREC is count of records written to auxiliary output file.     *
C                                                                      *
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/usranl.inc'
 
      character symnam(1000) * 12, subdef * 132, defchr(100) * 40
      character dmytxt * 132
      real symval(1000)
      integer symind(1000), ndefch(1000), first, status
      logical opened
 
      logical flag
C                                                                      *
C     UA sequential access input file is already open.                 *
C                                                                      *
C     Compute new values for symbols                                   *
C                                                                      *
      rewind lunusr
      do 220 iuser = 1, numusr
      flag = (numrec(iuser) .eq. 0)     !Flag set for first record
      max1 = numdef(iuser)
      do first  = 1, max1, 200
         last = min0 (first+199,max1)
         read (lunusr) (usrdef(i),i=first,last)
      enddo
      max2 = numtxt(iuser)
      do first = 1, max2, 200
         last = min0 (first+199,max2)
         read (lunusr) (usrtxt(i),i=first,last)
      enddo
C                                                                      *
C     Open UA output file if option enabled.                           *
C                                                                      *
      call close_file (lun(iuser))
      last = lastch (usrfil(iuser))
      open (unit = lun(iuser),
     &      file = usrfil(iuser)(1:last),
     &      status = 'OLD',
     &      err = 150)
C
C     Read to end-of-file to emulate 'APPEND' option.
C
      do while (.true.)
        read (lun(iuser), fmt='(a)', end=170) dmytxt
      enddo

C ***************************************************
  150 last = lastch (usrfil(iuser))
      write (errbuf(1), 160) usrfil(iuser)(1:last)
  160 format (' User_Analysis output file cannot be opened (', a,
     1   ')')
      call prterx ('W', 1)
      usrfil(iuser) = ' '
      lun(iuser) = 0
  170 continue
 
      call getdef (numdef(iuser), usrdef, numsym, symnam, symval,
     1             symind, nchr, ndefch, defchr)
 
      do 200 ir = 1, numtxt(iuser)
C                                                                      *
C        Process only "C" records.                                     *
C                                                                      *
         if (usrtxt(ir)(1:1) .eq. 'C') then
C                                                                      *
C           Substitute any symbols for their numerical value.          *
C                                                                      *
            outbuf = subdef (usrtxt(ir)(2:), numsym, symnam, symval,
     1                        symind, nchr, ndefch, defchr)
C                                                                      *
C           Restrict column 1 to " " or "0".                           *
C                                                                      *
            if (outbuf(1:1) .eq. '#') then
               if (flag) then
                 outbuf(1:1) = ' '
                 call prtout (1)
               endif
            else if (index (' 0', outbuf(1:1)) .ne. 0) then
               outbuf(1:1) = ' '
               call prtout (1)
            endif
C                                                                      *
C           Duplicate output on auxiliary file if option enabled.      *
C                                                                      *
            if (lun(iuser) .gt. 0) then
               if (outbuf(1:1) .ne. '#') then
                  numrec(iuser) = numrec(iuser) + 1
                  write (lun(iuser), 190) outbuf
  190             format (a)
               endif
            endif
 
         endif
  200 continue
      if (lun(iuser) .gt. 0) then
         close (unit=lun(iuser),err=210)
  210    continue
      endif
  220 continue
  900 continue
      return
      end
