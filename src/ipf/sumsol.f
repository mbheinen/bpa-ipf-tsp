C    @(#)sumsol.f	20.4 8/20/98
        subroutine sumsol
 
C       SUMSOL--TO CHECK FOR OPTION SETTINGS BEFORE WRITING
C               SOLUTION OPTION SUMMARY
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/slnopt.inc'
 
        character tag(4)*1
 
        call forbtm
        outbuf='* * * SOLUTION OPTIONS * * *'
        call rpnlod
        outbuf = ' '
        do 101 i = 1, 5
        call shdlod(i)
  101   continue
        call fortop
 
        call space (2)
 
        write (outbuf,2)
    2   format (1x, 132('*'))
        call prtout (1)
 
        do 4 i = 1,2
        write (outbuf,3)
    3   format (' *', t133,'*')
    4   call prtout (1)
 
        write (outbuf,1)
    1   format (' *', t46,'Solution parameter summary', t133, '*')
        call prtout (1)
 
        do 5 i = 1,2
        write (outbuf,3)
    5   call prtout (1)
 
        write (outbuf,10) iopton(1),iopton(2),iopton(3)
   10   format(' *', t18, 'Iterations', t43, '( ', i3, ' )', 3x,
     &    'Decoupled', 5x, '( ', i3, ' )', 3x, 'Modified Newton', 5x,
     &    '( ', i3, ' )', 3x, 'Newton ', t133, '*')
        call prtout (1)
 
        write (outbuf,11)
   11   format (' *', t18, '_________', t133, '*')
        call prtout (1)
 
        write (outbuf,3)
        call prtout (1)
C
        write (outbuf,20) option(4), option(5)
   20   format(' *', t18, 'Tolerances', t43,'( ', f6.4, ' )', 3x, 
     &    'Bus', 5x, '( ', f6.4, ' )', 3x, 'Area interchange', t133,
     &    '*')
        call prtout (1)
C
        write (outbuf,30) option(6), option(7)
   30   format(' *', t18, '----------', t43, '( ', f6.4, ' )', 3x,
     &    'TX ', 5x, '( ', f6.4, ' )', 3x, 'Q (reactive)', t133, '*')
        call prtout (1)
C
        write (outbuf,3)
        call prtout (1)
 
        write (outbuf,70) option(8), option(9)
   70   format(' *', t43, '( ', f6.4, ' )', 3x, 'Q residual', 2x, '( ',
     &    f6.3, ' )', 3x, 'Phase shift', t133, '*')
        call prtout (1)
C
        write (outbuf,80) option(10), option(11)
   80   format(' *', t43, '( ', f6.4, ' )', 3x, 'Del_angle', 3x, '( ',
     &    f6.4, ' )', 3x, 'Del_volt', t133, '*')
        call prtout (1)
 
        write (outbuf,3)
        call prtout (1)
 
        if (iopton(18) .eq. 0) then
           tag(1) = ' '
        else
           tag(1) = 'X'
        endif
 
        write (outbuf,90) tag(1)
   90   format(' *', t18, 'Miscellaneous controls',
     &    t43, '( ', a, ' )', t49, 'Flat start', t133, '*')
        call prtout (1)
 
        if (option(40) .eq. 0.500) then
           tag(1) = ' '
        else
           tag(1) = 'X'
        endif
        write (outbuf,92) tag(1), option(40)
   92   format(' *', t43, '( ', a, ' )', t49, 'Starting tap ratio (', 
     &    f6.3, ' )', t133, '*')
        call prtout (1)
 
        if (kspare(24) .eq. 0) then
           tag(1) = 'X'
           tag(2) = ' '
           tag(3) = ' '
        else if (kspare(24) .eq. 1) then
           tag(1) = ' '
           tag(2) = 'X'
           tag(3) = ' '
        else if (kspare(24) .eq. 2) then
           tag(1) = ' '
           tag(2) = ' '
           tag(3) = 'X'
        endif
 
        write (outbuf,110) tag(1)
  110   format(' *', 
     &    t18, '------------- --------',
     &    t43, 'Type BX bus - (', a, ') BPA  option # 1',
     &    t133, '*')
        call prtout (1)
 
        write (outbuf,112) tag(3)
  112   format(' *',
     &    t43, '              (', a, ') BPA  option # 2 (V_max)',
     &    t133, '*')
        call prtout (1)
 
        write (outbuf,114) tag(2)
  114   format(' *',
     &    t43, '              (', a, ') WSCC option', t133, '*')
        call prtout (1)
 
        write (outbuf,3)
        call prtout (1)
C
        if (iopton(16) .eq. 0) then
           tag(1) = ' '
           tag(2) = ' '
           tag(3) = ' '
           tag(4) = ' '
        else if (iopton(16) .eq. 1) then
           tag(1) = ' '
           tag(2) = 'X'
           tag(3) = ' '
           tag(4) = 'X'
        else if (iopton(16) .eq. 2) then
           tag(1) = 'X'
           tag(2) = 'X'
           tag(3) = 'X'
           tag(4) = 'X'
        else if (iopton(16) .eq. 3) then
           tag(1) = 'X'
           tag(2) = 'X'
           tag(3) = ' '
           tag(4) = ' '
        else
           tag(1) = ' '
           tag(2) = 'X'
           tag(3) = ' '
           tag(4) = ' '
        endif
        write (outbuf,120) tag(1)
  120   format(' *', t18, 'LTC control', t43, '( ', a, ' )', 3x, 
     &    'Type R', t133,'*')
        call prtout (1)
 
        write (outbuf,130) tag(2)
  130   format(' *', t18, '--- -------', t43, '( ', a, ' )', 3x, 
     &    'Type R (d-c commutating LTC''s)', t133, '*')
        call prtout (1)
 
        write (outbuf,140) tag(3)
  140   format(' *', t43, '( ', a, ' )', 3x, 
     &    'Types RN and RQ (LTC control of reactive flow)' ,t133, '*')
        call prtout (1)
 
        write (outbuf,142) tag(4)
  142   format(' *', t43, '( ', a, ' )', 3x, 
     &    'Types RM and RP (LTC phase shifter control of active flow)',
     &    t133,'*')
        call prtout (1)
 
        write (outbuf,3)
        call prtout (1)
C
        if (iopton(17) .eq. 0) then
           tag(1) = 'X'
           tag(2) = ' '
           tag(3) = ' '
        else if (iopton(17) .eq. 1) then
           tag(1) = ' '
           tag(2) = 'X'
           tag(3) = ' '
        else
           tag(1) = ' '
           tag(2) = ' '
           tag(3) = 'X'
        endif
        write (outbuf,150) tag(1)
  150   format(' *', t18, 'AI control', t43, '( ', a, ' )', 3x, 'OFF',
     &    t133, '*')
        call prtout (1)
 
        write (outbuf,160) tag(2)
  160   format(' *', t18, '-- -------', t43, '( ', a, ' )', 3x, 
     &    'Control (default)', t133, '*')
        call prtout (1)
 
        write (outbuf,170) tag(3)
  170   format(' *', t43, '( ', a, ' )', 3x, 'Monitor', t133, '*')
        call prtout (1)
 
        write (outbuf,3)
        call prtout (1)
        write (outbuf,3)
        call prtout (1)
C
        if (iopton(12) .eq. 0) then
           tag(1) = ' '
        else
           tag(1) = 'X'
        endif
        if (iopton(14) .eq. 0) then
           tag(2) = ' '
        else
           tag(2) = 'X'
        endif
 
        write (outbuf,210) tag(1),tag(2)
  210   format(' *', t18, 'Debugs', t43, '( ', a, ' )', 3x, 'Bus', 9x, 
     &    '( ', a, ' )', 3x, 'AI', t133, '*')
        call prtout (1)
 
        if (iopton(12) .eq. 0) then
           tag(1) = ' '
        else
           tag(1) = 'X'
        endif
        if (iopton(15) .eq. 0) then
           tag(2) = ' '
        else
           tag(2) = 'X'
        endif
 
        write (outbuf,220) tag(1),tag(2)
  220   format(' *', t18, '------', t43, '( ', a, ' )', 3x, 'TX ', 9x,
     &    '( ', a, ' )', 3x, 'D-c model', t133, '*')
        call prtout (1)
 
        do 240 i = 1,5
           write (outbuf,3)
  240   call prtout (1)
        write (outbuf,2)
        call prtout (1)
 
        outbuf=' '
        call rpnlod
C
        return
        end
