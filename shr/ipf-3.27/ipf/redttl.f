C    @(#)redttl.f	20.3 2/13/96
        subroutine redttl
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
C
        common /oldcrd/ tempc(3)
        character tempc*10
 
        common /eqarea/ iarea,kadata(MAXCAR)
 
        dimension tag(6)
        character label*20, tag*1
 
        call forbtm
        call fortop
        write (outbuf,610)
  610   format(' ',132('*'))
        call prtout(1)
        write (outbuf,620)
  620   format(' *',t133,'*')
        do 622 i = 1,4
        call prtout(1)
  622   continue
        write (outbuf,630) oldcse,kbsknt,kbrknt
  630   format(' *',t10,'SYSTEM DATA FROM CASE ',a10,'--- ',i4,
     1     ' BUSSES',i4,' BRANCHES',t133,'*')
        call prtout(1)
        write (outbuf,620)
        call prtout(1)
        write (outbuf,640)
  640   format(' *',t10,'DISPOSITION OF ELIMINATED INJECTIONS ----------
     1----------------------------',t133,'*' )
        call prtout(1)
        write (outbuf,620)
        call prtout(1)
        write (outbuf,650)
  650   format(' *',t10,'TYPE',t35,'REDUCTION MODE',t69,'FINAL MODE',
     1              t133,'*' )
        call prtout(1)
        write (outbuf,620)
        call prtout(1)
C
        nsw=1
        ix=kase1(2)
        iy=kase1(3)
        label='GENERATION         '
        if (chase1(2) .eq. ' ') then
           ix=2
           kase1(2)=2
        endif
        if (chase1(3) .eq. ' ') then
           iy=1
           kase1(3)=1
        endif
  660   do 670 i=1,6
  670   tag(i)=' '
        tag(2*ix+1)='X'
        tag(2*iy+2)='X'
        write (outbuf,680) label,tag(1),tag(2)
  680   format(' *',t10,a20,'(',a1,') CONSTANT CURRENT',
     1                  t65,'(',a1,') CONSTANT POWER',t133,'*' )
        call prtout(1)
        write (outbuf,690) tag(3), tag(4)
  690   format(' *',    t30,'(',a1,') CONSTANT ADMITTANCE',
     1                  t65,'(',a1,') CONSTANT CURRENT',t133,'*' )
        call prtout(1)
        write (outbuf,700) tag(5), tag(6)
  700   format (' *',   t30,'(',a1,') REI ACQUISITION',
     1                  t65,'(',a1,') CONSTANT ADMITTANCE',t133,'*' )
        call prtout(1)
        write (outbuf,620)
        call prtout(1)
        go to (710,720,730), nsw
  710   nsw=2
        ix=kase1(4)
        iy=kase1(5)
        label='LOAD'
        go to 660
  720   nsw=3
        ix=kase1(6)
        iy=kase1(7)
        if (chase1(7) .eq. ' ') then
           iy=2
           kase1(7)=2
        endif
        label='SHUNT ADMITTANCE'
        go to 660
  730   do 740 i=1,6
  740   tag(i)=' '
        write (outbuf,780)
  780   format(' *',t10,'DEFINITION OF REDUCED NETWORK -----------------
     1----------------------------',t133,'*' )
        call prtout(1)
        write (outbuf,620)
        call prtout(1)
 
        if (jtie .eq. 0) kase1(20)=0
        if (chase1(20) .eq. ' ') then
           kase1(20) = 1
           chase1(20) = ' 1'
        endif
        if (chase1(24) .eq. ' ') kase1(24)=1
        if (chase1(20) .ne. ' ') tag(1)='X'
        if (chase1(21) .eq. ' ') tag(2)='X'
        read (chase1,750) tol
  750   format (f10.5)
        if (tol .eq. 0) then
           if (chase1(1) .eq. ' ') tol=0.02
        endif
        write (outbuf,790) tag(1)
  790   format(' *',t13,'(',a1,') RETAIN AREA INTERCHANGE SYSTEM',
     1              t133,'*')
        call prtout(1)
        tag(1) = ' '
        if (iarea .gt. 0) tag(1) = 'X'
        write (outbuf,792) tag(1)
  792   format(' *',t13,'(',a1,') Substitute areas with full equivalent'
     1                ,t133,'*')
        call prtout(1)
 
        tag(4) = ' '
        read (chase1(34),750) sang
        if (sang .eq. 0) then
           if (chase1(34) .eq. ' ') sang = 100.0
        endif
        read (chase1(35),750) pgen
        if (pgen .eq. 0) then
           if (kase1(11) .ne. 0) pgen = 100.0
        endif
        pgmax = pgen/bmva
        if (pgen .ne. 0) tag(4) = 'X'
        if (kase1(11) .ne. 0) tag(4) = 'X'
        write (outbuf,760) tag(4),pgen
  760   format (' *',t13,'(',a1,') RETAIN ALL GENERATORS ABOVE ',
     1    f6.1,' MW ',t133,'*' )
        call prtout (1)
 
        if (chase1(24) .eq. ' ') tag(3)='X'
        write (outbuf,800) tag(3)
  800   format(' *',t13,'(',a1,') OPTIMALLY EXPAND REDUCED SYSTEM',
     1              t133,'*')
        call prtout(1)
        write (outbuf,620)
        call prtout(1)
        write (outbuf,810)
  810   format(' *',t10,'TOLERANCES ------------------------------------
     1----------------------------',t133,'*' )
        call prtout(1)
        write (outbuf,620)
        call prtout(1)
        write (outbuf,820) tol
  820   format (' *',t10,'MINIMUM EQUIVALENT BRANCH ADMITTANCE  (',
     1      e10.3,')',t133,'*')
        call prtout(1)
        write (outbuf,770) sang
  770   format (' *',t10,'MINIMUM INJECTION ON REI ACTIVE NODES (',
     1      e10.3,')',t133,'*')
        call prtout (1)
        write (outbuf,620)
        call prtout(1)
        write (outbuf,610)
        call prtout(1)
C
        call forbtm
        call fortop
        return
        end
