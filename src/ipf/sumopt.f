C    @(#)sumopt.f	20.4 1/7/99
        subroutine sumopt
 
C       SUMOPT--TO PREPARE THE "FILLING" FOR A STARRED SUMMARY OF
C               OPTIONS FOR THE CURRENT CASE
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/filnam.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/oldfil.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/zonlst.inc'
 
        character*60 name
 
        call forbtm
 
C       OPTION SUMMARY
C       **************
 
        outbuf='* * * OPTION SUMMARY * * *'
        call rpnlod
        call prohed
        call fortop
 
        name = ' '
        inquire (unit=inp,name=name)
 
        write (outbuf,15)
15      format (1x,132('*'))
        call prtout (1)
           do 55 i=1,3
              write(outbuf,52)
52            format(' *',t133,'*')
              call prtout (1)
55         continue
        write (outbuf,31) name
 
C       DATA FILES
C       **********
 
31      format(' *',t18,'DATA FILES :',t43,'(X)',
     1        t49,'INPUT COMMAND FILE  :',t70,a,t133,'*')
        call prtout (1)
        write (outbuf,32)
32      format(' *',t43,'( )',t49,'OLD BASE FILE :',t133,'*')
 
        if (obasnm .ne. ' ') then
           outbuf(44:44) = 'X'
           outbuf (64:93) = obasnm
        endif
        call prtout (1)
 
        if (oldbrd .ne. ' ') then
           write (outbuf,50) oldbrd,crun1(1),krun1(2)
50        format(' *',t18,'---- ---- ------',t43,'(X)',t49,'BRANCH ',
     1   'DATA FILE :',t70,a,t93,'SELECTION DATE :',t109,a1,i2,t133,'*')
           call prtout (1)
           write (outbuf,54) oldbsd
54         format(' *',t52,'BUS DATA FILE :',t70,a,t133,'*')
           call prtout (1)
        else
 
           write (outbuf,40)
40         format(' *',t18,'---- ---- ------',t43,'( )',t49,'BRANCH ',
     1     'DATA FILE :',t93,'SELECTION DATE:',t133,'*')
           call prtout (1)
           write (outbuf,47)
47         format(' *',t52,'BUS DATA FILE : ',t133,'*')
           call prtout (1)
           do 73 i=1,3
              write(outbuf,74)
74            format(' *',t133,'*')
              call prtout (1)
73         continue
 
        endif
 
 
        if (bsbrnm .ne. ' ') then
           write (outbuf,60) bsbrnm
60         format(' *',t43,'(X)',t49,'NETWORK DATA FILE : ',t70,a,t133,
     1     '*')
           call prtout (1)
 
           do 63 i=1,3
              write(outbuf,64)
64            format(' *',t133,'*')
              call prtout (1)
63         continue
 
        endif
 
        if (chgnam .ne. ' ') then
           write (outbuf,70) chgnam
70         format(' *',t43,'(X)',t49,'CHANGE  FILE : ',t70,a,t133,'*')
           call prtout (1)
        endif
 
        if (bmva .ne. 100.0) then
           write (outbuf,90) bmva
90         format(' *',t43,'DATA ON ',f7.1,' MVA BASE',t133,'*')
           call prtout (1)
           write (outbuf,92)
92         format (' *',t133,'*')
           call prtout (1)
        else
           write (outbuf,90) 100.0
           call prtout (1)
           write (outbuf,93)
93         format (' *',t133,'*')
           call prtout (1)
        endif
 
C       REPORT SORT ORDER
C       *****************
 
        write (outbuf,71)
71      format(' *',t18,'REPORT SORT ORDER',t43,'( )',t49,'BUS-BASE',
     &         t133,'*')
        if (kspare(11) .eq. 0) then
           outbuf (44:44) = 'X'
        endif
        call prtout (1)
        write (outbuf,80)
80      format(' *',t18,'------ ---- -----',t43,'( )',t49,
     &         'ZONE-BUS-BASE',t133,'*')
        if (kspare(11) .eq. 1) then
           outbuf (44:44) =  'X'
        endif
        call prtout (1)
        write (outbuf,85)
85      format(' *',t18,'------ ---- -----',t43,'( )',t49,
     &         'OWNER-BUS-BASE',t133,'*')
        if (kspare(11) .eq. 3) then
           outbuf (44:44) =  'X'
        endif
        call prtout (1)
        write (outbuf,91)
91      format(' *',t43,'( )',t49,'AREA-BUS-BASE',t133,'*')
        if (kspare(11).eq.2) then
           outbuf (44:44) = 'X'
        endif
        call prtout (1)
           do 200 i=1,3
              write(outbuf,205)
205           format(' *',t133,'*')
              call prtout (1)
200        continue
 
 
C       LISTING REQUESTED
C       *****************
 
        write (outbuf,100)
100     format(' *',t18,'LISTINGS REQUESTED',t49,'PAPER',t61,'FICHE',
     1  t133,'*')
        call prtout (1)
        write (outbuf,110)
110     format(' *',t18,'-------- ---------',t49,'-----',t61,'-----',
     1  t133,'*')
        call prtout (1)
        write (outbuf,92)
        call prtout (1)
 
 
 
C       INPUT LISTING
C       *************
 
        write (outbuf,120)
120     format(' *',t18,'INPUT LISTING',t43,'( )',t49,'NONE',t58,'( )',
     1  t64,'NONE',t133,'*')
        if (kspare(4).eq.0) then
           outbuf (44:44) = 'X'
        endif
        if (kspare(5).eq.0) then
           outbuf (59:59) = 'X'
        endif
        call prtout (1)
        write (outbuf,130)
130     format(' *',t18,'----- -------',t43,'( )',t49,'ZONES',t58,'( )',
     1  t64,'ZONES',t133,'*')
        if (kspare(4).eq.1) then
           outbuf (44:44) = 'X'
        endif
        if (kspare(5).eq.1) then
           outbuf (59:59) = 'X'
        endif
        call prtout (1)
        write (outbuf,140)
140     format(' *',t43,'( )',t49,'FULL',t58,'( )',t64,'FULL',t133,'*')
        if (kspare(4).eq.2) then
           outbuf (44:44) = 'X'
        endif
        if (kspare(5).eq.2) then
           outbuf (59:59) = 'X'
        endif
        call prtout (1)
        write (outbuf,142)
142     format (' *',t133,'*')
        call prtout (1)
 
 
 
C       OUTPUT LISTING
C       **************
 
        write (outbuf,150)
150     format(' *',t18,'OUTPUT LISTING',t43,'( )',t49,'NONE',t58,'( )',
     1  t64,'NONE',t133,'*')
 
        if (kspare(6).eq.0) then
          outbuf(44:44) = 'X'
        endif
        if (kspare(7).eq.0) then
           outbuf(59:59) = 'X'
        endif
        call prtout (1)
        write (outbuf,160)
160     format(' *',t18,'------ -------',t43,'( )',t49,'ZONES',t58,'( )
     1  ',t64,'ZONES',t133,'*')
 
        if (kspare(6).eq.1) then
           outbuf (44:44) = 'X'
        endif
        if (kspare(7).eq.1) then
           outbuf (59:59) = 'X'
        endif
        call prtout (1)
        write (outbuf,170)
170     format(' *',t43,'( )',t49,'FULL',t58,'( )',t64,'FULL',t133,'*')
        if (kspare(6).eq.2) then
           outbuf(44:44) = 'X'
        endif
        if (kspare(7).eq.2) then
           outbuf(59:59) = 'X'
        endif
        call prtout (1)
 
        write (outbuf,172)
172     format (' *',t133,'*')
        call prtout (1)
 
 
 
C       ANALYSIS LISTING
C       ****************
 
        write (outbuf,101) kspare(8),kspare(9)
101     format(' *',t18,'ANALYSIS LISTING',t43,'( )',t49,'LEVEL ',
     1   i1,t58,'( )',t64,'LEVEL ',i1,t133,'*')
        call prtout (1)
        write (outbuf,190)
190     format(' *',t18,'-------- -------',t43,'( )',t49,'ZONES',
     1  t58,'( )',t64,'ZONES',t133,'*')
 
        if (npzanl.gt.0) then
           if (nfzanl.gt.0) then
              outbuf (44:44) = 'X'
           else
              outbuf (59:59) = 'X'
           endif
        else if (nfzanl.gt.0) then
           outbuf(59:59) = 'X'
        endif
 
        call prtout (1)
        write (outbuf,201)
201     format(' *',t43,'( )',t49,'OWNERS',t58,'( )',t64,'OWNERS',
     1  t133,'*')
 
        if (npoanl.gt.0) then
           if (nfoanl.gt.0) then
              outbuf (44:44) = 'X'
              outbuf (59:59) = 'X'
           else
              outbuf (44:44) = 'X'
           endif
        else if (nfoanl.gt.0) then
           outbuf(59:59) = 'X'
        endif
        call prtout (1)
 
        write (outbuf,203)
203     format (' *',t133,'*')
        call prtout (1)
 
 
C       AREA INTER. LISTING  - interchange matrix and/or tie flows
C       **********************************************************
 
        write (outbuf,210)
210     format(' *',t18,'AREA INTER. LISTING',t43,'( )',t49,'NONE',t133,
     1  '*')
        if (kspare(12).eq.0) then
           outbuf(44:44) = 'X'
        endif
        call prtout (1)
        write (outbuf,220)
220     format(' *',t18,'---- ------ -------',t43,'( )',t49,'TIE LINE',
     1  t133,'*')
        if (kspare(12).eq.2) then
           outbuf(44:44) = 'X'
        endif
        call prtout (1)
        write (outbuf,230)
230     format(' *',t43,'( )',t49,'MATRIX',t133,'*')
        if (kspare(12).eq.1) then
           outbuf(44:44) = 'X'
        endif
        call prtout (1)
        write (outbuf,240)
240     format(' *',t43,'( )',t49,'FULL',t133,'*')
        if (kspare(12).eq.3) then
           outbuf(44:44) = 'X'
        endif
        call prtout (1)
           do 500 i=1,3
              write(outbuf,505)
505           format(' *',t133,'*')
              call prtout (1)
500        continue
 
 
C       OWNER INTER. LISTING
C       ********************
 
        write (outbuf,250)
250     format(' *',t18,'OWNER INTER. LISTING',t43,'( )',t49,'NONE',t133
     1  ,'*')
        if (kspare(13).eq.0) then
           outbuf (44:44) = 'X'
        endif
        call prtout (1)
        write (outbuf,260)
260     format(' *',t18,'----- ------ -------',t43,'( )',t49,'TIE LINE',
     1  t133,'*')
        if (kspare(13).eq.2) then
           outbuf(44:44) = 'X'
        endif
        call prtout (1)
        write (outbuf,270)
270     format(' *',t43,'( )',t49,'MATRIX',t133,'*')
        if (kspare(13).eq.1) then
           outbuf(44:44) = 'X'
        endif
        call prtout (1)
        write (outbuf,280)
280     format(' *',t43,'( )',t49,'FULL',t133,'*')
        if (kspare(13).eq.3) then
           outbuf(44:44) = 'X'
        endif
        call prtout (1)
           do 520 i=1,3
              write(outbuf,525)
525           format(' *',t133,'*')
              call prtout (1)
520        continue
 
        if (nbasnm .ne. ' ') then
           write (outbuf,131) nbasnm
131        format(' *',t43,'NEW BASE SAVED AS : ',a20,t133,'*')
           call prtout (1)
        else
           write (outbuf,290)
290        format(' *',t43,'NEW BASE NOT SAVED',t133,'*')
           call prtout (1)
           do 297 i=1,3
              write(outbuf,298)
298           format(' *',t133,'*')
              call prtout (1)
297        continue
        endif
 
        write (outbuf,294)
294     format (1x,132('*'))
        call prtout (1)
 
        return
        end
