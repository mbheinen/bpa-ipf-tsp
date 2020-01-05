C    @(#)bd0002.f	20.8 2/28/00
        subroutine init_bd0002

        include 'ipfinc/com002.inc'

        lctl = 59
        ctldic(1)  = 'OLDBASE'
        ctldic(2)  = 'NEWBASE'
        ctldic(3)  = 'NETWORK*'
        ctldic(4)  = 'PINPUT*'
        ctldic(5)  = 'FINPUT*'
        ctldic(6)  = 'POUTPUT*'
        ctldic(7)  = 'FOUTPUT*'
        ctldic(8)  = 'PANALYSIS*'
        ctldic(9)  = 'FANALYSIS*'
        ctldic(10) = 'AILIST*'
        ctldic(11) = 'OILIST*'
        ctldic(12) = 'DEBUG*'
        ctldic(13) = 'ERRORS*'
        ctldic(14) = 'VOLTAGELIM'
        ctldic(15) = 'LTC'
        ctldic(16) = 'SOLITER*'
        ctldic(17) = 'TOLER*'
        ctldic(18) = 'AICONTROL*'
        ctldic(19) = 'BRANCHDATA'
        ctldic(20) = 'RPTSORT*'
        ctldic(21) = 'MVABASE'
        ctldic(22) = 'OVERL*'
        ctldic(23) = 'TRACE'
        ctldic(24) = 'REBUILD'
        ctldic(25) = 'LIMITS'
        ctldic(26) = 'MISC*'
        ctldic(27) = 'END'
        ctldic(28) = 'STOP'
        ctldic(29) = 'DATA'
        ctldic(30) = 'HEAD*'
        ctldic(31) = 'COMMENT*'
        ctldic(32) = 'CHANGES*'
        ctldic(33) = 'INCLUDEC*'
        ctldic(34) = 'NEXTCASE*'
        ctldic(35) = 'MERGE*'
        ctldic(36) = 'REDUCT*'
        ctldic(37) = 'OUTAGESIM*'
        ctldic(38) = 'BUSD*'
        ctldic(39) = 'SOLUTION'
        ctldic(40) = 'LINEE*'
        ctldic(41) = 'TXEFF*'
        ctldic(42) = 'CHECK*'
        ctldic(43) = 'BUSSEN*'
        ctldic(44) = 'LINESE*'
        ctldic(45) = 'EXTEND*'
        ctldic(46) = 'SORTAN*'
        ctldic(47) = 'LOSSSE*'
        ctldic(48) = 'GENDROP*'
        ctldic(49) = 'CHANGEB*'
        ctldic(50) = 'ANALY*'
        ctldic(51) = 'USERAN*'
        ctldic(52) = '%LOAD*'
        ctldic(53) = 'EPRI*'
        ctldic(54) = 'AGC*'
        ctldic(55) = 'SAVEF*'
        ctldic(56) = 'LOADP*'
        ctldic(57) = 'LOADG*'
        ctldic(58) = 'INIT*'
        ctldic(59) = 'CASEDEP*'
 
        incsw = 0

        lolb = 2
        olbdic(1) = 'CASE'
        olbdic(2) = 'REBUILD'

        lrpt = 3
        rptdic(1) = 'BUS'
        rptdic(2) = 'ZONE'
        rptdic(3) = 'AREA'

        lail = 4
        aildic(1) = 'NONE'
        aildic(2) = 'TIE*'
        aildic(3) = 'MAT*'
        aildic(4) = 'FULL'

        lltc = 3
        ltcdic(1) = 'OFF'
        ltcdic(2) = 'ON'
        ltcdic(3) = 'ONNV'

        laic = 3
        aicdic(1) = 'OFF'
        aicdic(2) = 'CON*'
        aicdic(3) = 'MON*'

        ltrac = 7
        traced(1) = 'REORDER'
        traced(2) = 'XREF'
        traced(3) = 'AUTO'
        traced(4) = 'YMATRIX'
        traced(5) = 'OUTPUT'
        traced(6) =  'MERGE'
        traced(7) = 'CHANGE'

        trapnt(1) = 37
        trapnt(2) = 40
        trapnt(3) = 39
        trapnt(4) = 38
        trapnt(5) = 28
        trapnt(6) = 33
        trapnt(7) = 27

        lopt = 5
        lstopt(1) = 'NONE*'
        lstopt(2) = 'ZONE*'
        lstopt(3) = 'FULL*'
        lstopt(4) = 'ERROR*'
        lstopt(5) = 'FAIL*'

        lan2 = 4
        anropt(1) = 'LEVE*'
        anropt(2) = 'ZONE*'
        anropt(3) = 'OWN*'
        anropt(4) = 'AREA*'

        lsoli = 3
        soldic(1) = 'DECOUP*'
        soldic(2) = 'NEWT*'
        soldic(3) = 'OPTIM*'

        itepnt(1) = 1
        itepnt(2) = 3
        itepnt(3) = 21

        ltol = 5
        toldic(1) = 'BUS*'
        toldic(2) = 'AIPOW*'
        toldic(3) = 'TX*'
        toldic(4) = 'Q*'
        toldic(5) = 'V*'
        toldic(6) = '  '

        tolpnt(1) = -4
        tolpnt(2) = -5
        tolpnt(3) = -6
        tolpnt(4) = -7
        tolpnt(5) = -24

        llim = 10
        limdic(1)  = 'QRES*'
        limdic(2)  = 'PHA*'
        limdic(3)  = 'DELVMI*'
        limdic(4)  = 'DELVMA*'
        limdic(5)  = 'DELPMI*'
        limdic(6)  = 'DELPMA*'
        limdic(7)  = 'DELTMI*'
        limdic(8)  = 'DELTMA*'
        limdic(9)  = 'DELANG*'
        limdic(10) = 'DELVOLT*'
        limdic(11) = '    '
        limdic(12) = '    '

        limpnt(1)  = -8
        limpnt(2)  = -9
        limpnt(3)  = -23
        limpnt(4)  = -24
        limpnt(5)  = -25
        limpnt(6)  = -26
        limpnt(7)  = -27
        limpnt(8)  = -28
        limpnt(9)  = -10
        limpnt(10) = -11

        ldeb = 7
        debdic(1) = 'TX*'
        debdic(2) = 'BUS*'
        debdic(3) = 'AI*'
        debdic(4) = 'DC*'
        debdic(5) = 'OPT*'
        debdic(6) = 'OPV*'
        debdic(7) = 'OPP*'
        debdic(8) = '    '

        debpnt(1) = 12
        debpnt(2) = 13
        debpnt(3) = 14
        debpnt(4) = 15
        debpnt(5) = 29
        debpnt(6) = 30
        debpnt(7) = 31

        lcon = 5
        condic(1) = 'VFLAT**'
        condic(2) = 'RELAX**'
        condic(3) = 'XRGHOST*'
        condic(4) = 'VOLTAGER**'
        condic(5) = 'SAVEBE*'

        conpnt(1) = 18
        conpnt(2) = -33
        conpnt(3) = 19
        conpnt(4) = 36
        conpnt(5) = 37

        numtxf = 3
        effdic(1) = 'TOTAL*'
        effdic(2) = 'CORE*'
        effdic(3) = 'OWN*'

        lansel = 23
        selanl(1) = 'UNSCH*'
        selanl(2) = 'LOSSOWN*'
        selanl(3) = 'SYSTEMZONE'
        selanl(4) = 'UVOV*'
        selanl(5) = 'LINELOAD*'
        selanl(6) = 'TRANLOAD*'
        selanl(7) = 'TRANEX*'
        selanl(8) = 'XSYSTEM*'
        selanl(9) = 'BPALOADS*'
        selanl(10) = 'DCSYSTEM*'
        selanl(11) = 'SHUNTSUM*'
        selanl(12) = 'SUMLTC*'
        selanl(13) = 'SUMPHASE*'
        selanl(14) = 'SUM%VAR*'
        selanl(15) = 'SUMBX*'
        selanl(16) = 'SUMRANI*'
        selanl(17) = 'SERIES*'
        selanl(18) = 'BUS*'
        selanl(19) = 'SPIN*'
        selanl(20) = 'LINEEFF*'
        selanl(21) = 'USER*'
        selanl(22) = 'TRANEFF*'
        selanl(23) = 'TRANLOSS*'
 
        return
        end

c      	block data bd0002
 
c      	include 'ipfinc:com002.inc'

c      	data lctl /55/,ctldic/'OLDBASE','NEWBASE','NETWORK*',
c     1   'PINPUT*', 'FINPUT*', 'POUTPUT*', 'FOUTPUT*', 'PANALYSIS*',
c     2   'FANALYSIS*', 'AILIST*', 'OILIST*', 'DEBUG*', 'ERRORS*',
c     3   'VOLTAGELIM', 'LTC', 'SOLITER*', 'TOLER*', 'AICONTROL*',
c     4   'BRANCHDATA', 'RPTSORT*', 'MVABASE', 'OVERL*', 'TRACE',
c     5   'REBUILD', 'LIMITS', 'MISC*', 'END', 'STOP', 'DATA', 'HEAD*',

c     6   'COMMENT*', 'CHANGES*', 'INCLUDEC*', 'NEXTCASE*',
c     7   'MERGE*', 'REDUCT*', 'OUTAGESIM*', 'BUSD*', 'SOLUTION',
c     8   'LINEE*', 'TXEFF*', 'CHECK*', 'BUSSEN*', 'LINESE*',
c     9   'EXTEND*', 'SORTAN*', 'LOSSSE*', 'GENDROP*', 'CHANGEB*',
c     *   'ANALY*', 'USERAN*', '%LOAD*', 'EPRI*', 'AGC*', 'SAVEF*' /
 
c        data incsw /0/
c        data lolb /2/,olbdic/'CASE','REBUILD'/
c        data lrpt /3/,rptdic/'BUS','ZONE','AREA'/
c        data lail /4/,aildic/'NONE','TIE*','MAT*','FULL'/
c        data lltc /3/,ltcdic/'OFF','ON','ONNV'/
c        data laic /3/,aicdic/'OFF','CON*','MON*'/
c        data ltrac /7/,traced/'REORDER','XREF','AUTO','YMATRIX',
c     1                        'OUTPUT', 'MERGE','CHANGE'/
c        data trapnt/37,40,39,38,28,33,27/
c        data lopt /5/,lstopt/'NONE*','ZONE*','FULL*','ERROR*','FAIL*'/

c        data lan2 /4/,anropt/'LEVE*','ZONE*','OWN*', 'AREA*'/
c        data lsoli/3/,soldic  /'DECOUP*','NEWT*','OPTIM*'/
c        data itepnt/1,3,21/
c        data ltol /5/,toldic/'BUS*','AIPOW*','TX*','Q*','OPCUT*','  '/

c        data tolpnt/-4,-5,-6,-7,-22/
c        data llim /10/,limdic/'QRES*','PHA*','DELVMI*','DELVMA*'
c     1        ,'DELPMI*','DELPMA*','DELTMI*','DELTMA*','DELANG*'
c     2        ,'DELVOLT*','    ','    '/
c        data limpnt/-8,-9,-23,-24,-25,-26,-27,-28,-10,-11/
c        data ldeb /7/, debdic/'TX*','BUS*','AI*','DC*','OPT*'
c     1                       ,'OPV*','OPP*','    '/
c        data debpnt/12,13,14,15,29,30,31/
c        data lcon /5/, condic/'VFLAT**','RELAX**','XRGHOST*',
c     1                        'VOLTAGER**','SAVEBE*'/
c        data conpnt/18,-33,19,36,37/
c        data numtxf /3/, effdic /'TOTAL*','CORE*','OWN*'/
c        data lansel /23/, selanl / 'UNSCH*', 'LOSSOWN*', 'SYSTEMZONE',

c     1    'UVOV*', 'LINELOAD*', 'TRANLOAD*', 'TRANEX*', 'XSYSTEM*',
c     2    'BPALOADS*', 'DCSYSTEM*', 'SHUNTSUM*', 'SUMLTC*',
c     3    'SUMPHASE*', 'SUM%VAR*', 'SUMBX*', 'SUMRANI*', 'SERIES*',
c     4    'BUS*', 'SPIN*', 'LINEEFF*', 'USER*', 'TRANEFF*',
c     5    'TRANLOSS*' /
 
c        end
