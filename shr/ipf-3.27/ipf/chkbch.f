C    @(#)chkbch.f	20.3 2/13/96
        logical function chkbch(xbatch)                                  
        logical xbatch

        include 'ipfinc/jobctl.inc'
c    
c       BATCH is interpreted as running without user intervention,
c       be it launched interactively or via a command file.  The
c       mode reflects the program and not the mode.  The batch PF
c       program sets BATCH = .TRUE.; all  others set BATCH = .FALSE.

        xbatch = batch
        chkbch = batch

        return
        end
