C    @(#)pflbeg.f	20.4 2/13/96
      subroutine pflbeg
C
C     POWERFLOW.BEGIN JOB INITIALIZATION
C
      include 'ipfinc/jobctl.inc'
      include 'ipfinc/filnam.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/pageno.inc'
      include 'ipfinc/prt.inc'
 
      common /is_batch / is_batch

      character job*20
      logical chkbch, finished

      call am_date(rdate)
      job = 'START PWRFLW '//prgvsn
      batch = chkbch(batch)
 
C     Set up the page header

      write (outbuf, 10) prgvsn
   10 format('BPA POWER FLOW PROGRAM VERSION:',a)
      call hedlod
 
C     print program version on crt or .log file

      print 15, outbuf(1:60)
   15 format(1x,a60)
 
C     Blank the report name, subheader and comments

      outbuf = ' '
      call rpnlod
      call shdlod(1)
      call shdlod(2)
      call shdlod(3)
      call shdlod(4)
      call shdlod(5)
      call comlod(1)
      call comlod(2)
 
C     Initialize parameters and open the files

      call pfinit
      call pfopen
      call prtime('START')
      endjob = .false.
 
      if( batch ) then
         outbuf='0[ BATCH RUN ]'
      else
         outbuf='0[ INTERACTIVE RUN ]'
      endif
 
      call prtout(1)
      call space(1)
 
      if (batch) then
         finished = .false.
         do while (.not. finished)
            read(inp, 25, end=30)inrcd
   25       format (a)
            if (index('([',inrcd(1:1)) .eq. 0) then
               l = lastch(inrcd)
               l = min0(l,80)
               outbuf = ' ' // inrcd(1:l) // '***COMMENT***'
               call prtout(1)
            else
               finished = .true.
            endif
         enddo
   30    if (.not. finished) then
            last = lastch (inpnm)
            write (errbuf(1), 40) inpnm(1:last)
   40       format (' No PFC commands encountered in file ', a)
            if (is_batch .eq. 0) then
               call prterx ('E',1)
               return
            else
               call prterx ('F',1)
               call erexit (1)
            endif
         endif
      endif
      return
 
      end
