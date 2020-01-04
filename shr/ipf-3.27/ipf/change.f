C    @(#)change.f	20.7 3/29/99
C****************************************************************
C
C   File: change.f
C
C   Purpose: Process /CHANGE commands
C
C   Author: BPA staff       Date: 18 May 1992
C   Called by: p_gtdata.f, prodat.f
C
C****************************************************************
C
      subroutine change ()
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/basval.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/changr.inc'
      include 'ipfinc/dtaiop.inc'
      include 'ipfinc/filnam.inc'
      include 'ipfinc/jobctl.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/oldchg.inc'
      include 'ipfinc/ordsta.inc'
      include 'ipfinc/prt.inc'
C
C       Update old base values
C
        call xdate(dte)
        basval(5) = dte
        call n_time( ihr, imin, isec )
        write (basval(6), 90) ihr, imin, isec
   90   format (i2, ':', i2, ':', i2)
        basval(8) = prgvsn
        basval(9) = usrnam

C
C       Set up headers
C 
        call forbtm () 
        write (outbuf,100)
  100   format('* * * Network changes * * *')
        call rpnlod ()
        call fortop ()
        call space(2)
        call chread (numfil)
 
        if (numchg .gt. 0)   then
C
C          Convert to external order if in internal order.
C
           if (ordvlt .eq. 2) then
              ordvlt = 1
              call mvnew1d(e,opt2inp,ntot)
              call mvnew1d(f,opt2inp,ntot)
           endif
           if (ordcap .eq. 2) then
              ordcap = 1
              call mvnew2d(capcor,opt2inp,ntot)
           endif
           call chchek ()
           call chpct ()
           call chdel ()
           call chprea ()
           call chadd ()
           call chmods ()
           if (numchg .gt. 0) call sortbus ()
           call chmisc ()
           call chg_error ()
C
C          Format of oldchg()
C
C             (1:120) - change record
C
C           (121:121) - origination code
C                      ' ' indicates original record
C                      'P' inidcates pseudo-change record
C                      'T' indicates transpose record 
C                      'U' indicates transpose psuedo-change record 
c
C           (122:125) - chgcrd() record numnber, converted to an
c                       absolute index
C
C           (126:126) - process code
C                      ' ' indicates not processed
C                      'P' indicates processed without errors
C                      'E' indicates processed with errors
C
C           (127:128) - source of record code
C                      'n' indicates from imported change file no. "n"
C                      ' ' indicates interactive change 
C
           do ic = 1, numchg
              call storchgs (chgcrd(ic), numfil)
           enddo
        else
           outbuf = '0 No change records encountered'
           call prtout(1)
        endif
 
        return
        end
