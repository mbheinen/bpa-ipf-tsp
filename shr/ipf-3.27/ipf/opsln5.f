C    @(#)opsln5.f	20.3 2/13/96
        subroutine opsln5
C
      include 'ipfinc/blank.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
C
        write ( outbuf, 137 )
  137   format( '0 ENTER DUMMY "OPTSLN5"' )
C
        isw = crtsw
        crtsw = 1
        call prtout(1)
C
        notprint = 1
C
        lprtsv = lprtsw
        fichsx = fichsw
C
        lprtsw = 1
        if( kspare(16) .ge. 0 ) fichsw=1
C
        call forbtm
C
        lprtsw = lprtsv
        fichsw = fichsx
        crtsw = isw

        return
        end
