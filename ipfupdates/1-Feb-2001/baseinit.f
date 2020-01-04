C    %W% %G%
C****************************************************************
C
C   File: baseinit.f
C   Purpose: IPF subroutine to initialize for a new base case
C
C   Author: Walt Powell  Date: 9 November 1992
C   Called by:
C
C****************************************************************
C
        subroutine baseinit

      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/basval.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/filnam.inc'
      include 'ipfinc/jobctl.inc'
      include 'ipfinc/dtaiop.inc'
      include 'ipfinc/errorsw.inc'
      include 'ipfinc/header.inc'
      include 'ipfinc/coment.inc'
      include 'ipfinc/pageno.inc'
      include 'ipfinc/pfstates.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/errmsg.inc'
      include 'ipfinc/errorx.inc'
      include 'ipfinc/changr.inc'
      include 'ipfinc/delete.inc'
      include 'ipfinc/oldchg.inc'
      include 'ipfinc/pqcurves.inc'
      include 'ipfinc/xdata.inc'
      include 'ipfinc/sortuvov.inc'
      include 'ipfinc/tbx.inc'
      include 'ipfinc/qsdup.inc'
      include 'ipfinc/zbdata.inc'
      include 'ipfinc/pctger.inc'
      include 'ipfinc/gendrp.inc'

        common /bld_vlimit/ update
        logical update

        common /bldtbl/ bldtbl, fltstr
        logical bldtbl
        integer fltstr

        common /case_flags/ owner_flag
        logical owner_flag
c
c       owner_flag - owner array loaded?
c
        logical chkbch

c       initialize data from the old "blkdta"
        call init_bd_all
c
c	reinitialize owner flag
c
        owner_flag = .false.    
c
c       reinitialize error count
c
        numerr = 0
c
c       reinitialize gen_drop 
c
        numdrp = 0
        numgen = 0
        itdpmx = 0
        drptot = 0.0d0
        drptol = 10.0d0
        gensum_flag = 1
c
c       Initialize program variables
c
        call pfinit
        call restop
c
c       Disable error trapping
c
        errorsw = 1
c
c       Enable bus sort and voltage limits array initialization
c
        sortsw = 0
        update = .false.

c       if (ostates .gt. 0) then
c          write (errbuf(1), 100)
c 100      format('Initializing cleared all base data in residence')
c          call prterx ('W', 1)
c       endif

        batch = chkbch(batch)
c
c       set program state to "initialized"
c
        ostates = 1
        lskp = 0  
c
c       Initialize change archive
c
        numchg = 0
        numchgfl = 0
        numold = 0
        ndelete = 0
        ipctno = 0
c
c       Reinitialize pointers for pqdata, xdata, and TBX
c
        pq_flag = .false.
        xdt_flag = .false.
        tbx_loaded = 0
c
c       Save any generated debug file
c
        write (outbuf, 10) prgvsn
   10   format('BPA power flow program version:', a)
        call hedlod

        ncom = 0
        outbuf = ' '
        call hedlod
        call rpnlod
        call shdlod(1)
        call shdlod(2)
        call shdlod(3)
        call shdlod(4)
        call shdlod(5)
        call comlod(1)
        call comlod(2)

        do i = 1, MAXCMT
           com(i) = ' '
        enddo

        kspare(17) = 1

        wscc_aname = ' '
        wscc_bname = ' '

        fltstr = -1
        bldtbl = .true.

        dupsw = .false.

        cntngn = .false.
        loadck = .false.

        call bushinit ()       ! initialize the bus hash tables...
        call tbxhinit ()       ! initialize the tbx hash tables...

        return
        end
