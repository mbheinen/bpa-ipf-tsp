C    @(#)bd0005.f	20.3 2/13/96
        subroutine init_bd0005
 
        include 'ipfinc/com005.inc'

        lsin = 21
        sindic(1)  =  'OPTIM*'
        sindic(2)  =  'REACT*'
        sindic(3)  =  'DISPAT*'
        sindic(4)  =  'INCLUDEB*'
        sindic(5)  =  'EXCL*'
        sindic(6)  =  'SENS*'
        sindic(7)  =  'INCLUDEC*'
        sindic(8)  =  'DEBUG*'
        sindic(9)  =  'TRACEL*'
        sindic(10) =  'SOLITER*'
        sindic(11) =  'TOLER*'
        sindic(12) =  'AICONTROL*'
        sindic(13) =  'LIMITS'
        sindic(14) =  'MISC*'
        sindic(15) =  'TRACEB*'
        sindic(16) =  'LTC'
        sindic(17) =  'BASES*'
        sindic(18) =  'GENDROP'
        sindic(19) =  'LOADSOL*'
        sindic(20) =  'BX'
        sindic(21) =  'SVC'

        lsoli = 4
        soldic(1) =  'DECOUP*'
        soldic(2) =  'NEWT*'
        soldic(3) =  'OPTIM*'
        soldic(4) =  'CURR*'

        itepnt(1) =  1
        itepnt(2) =  3
        itepnt(3) =  21
        itepnt(4) =  2

        ltol = 5
        toldic(1) = 'BUS*'
        toldic(2) = 'AIPOW*'
        toldic(3) = 'TX*'
        toldic(4) = 'Q*'
        toldic(5) = 'V*'
        toldic(6) = '  '

        tolpnt(1) =  -4
        tolpnt(2) =  -5
        tolpnt(3) =  -6
        tolpnt(4) =  -7
        tolpnt(5) =  -24

        llim = 10
        limdic(1)  =  'QRES*'
        limdic(2)  =  'PHA*'
        limdic(3)  =  ' '
        limdic(4)  =  ' '
        limdic(5)  =  ' '
        limdic(6)  =  ' '
        limdic(7)  =  ' '
        limdic(8)  =  ' '
        limdic(9)  =  'DELANG*'
        limdic(10) =  'DELVOLT*'
        limdic(11) =  '    '
        limdic(12) =  '    '

        limpnt(1)  = -8
        limpnt(2)  = -9
        limpnt(3)  = 0
        limpnt(4)  = 0
        limpnt(5)  = 0
        limpnt(6)  = 0
        limpnt(7)  = 0
        limpnt(8)  = 0
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

        debpnt(1) =  12
        debpnt(2) =  13
        debpnt(3) =  14
        debpnt(4) =  15
        debpnt(5) =  29
        debpnt(6) =  30
        debpnt(7) =  31

        lcon = 13	
        condic(1)  = 'VFLAT**'
        condic(2)  = 'RELAX**'
        condic(3)  = 'XRGHOST*'
        condic(4)  = 'VOLTAGER**'
        condic(5)  = 'SAVEBE*'
        condic(6)  = 'DC*'
        condic(7)  = 'XBUS'
        condic(8)  = 'NUMV*'
        condic(9)  = 'TSTART*'
        condic(10) = 'TCONT*'
        condic(11) = 'ITERSUM*'
        condic(12) = 'PHASE*'
        condic(13) = 'BRIDGE*'

        conpnt(1)  = 18
        conpnt(2)  = -33
        conpnt(3)  = 19
        conpnt(4)  = 36
        conpnt(5)  = 37
        conpnt(6)  = 38
        conpnt(7)  = 0
        conpnt(8)  = 39
        conpnt(9)  = 40
        conpnt(10) = 0
        conpnt(11) = 0
        conpnt(12) = 21
        conpnt(13) = 23

        rad = 57.29577951
 
        return
        end

c      block data bd0005
 
c      include 'ipfinc:com005.inc'

c      data lsin / 21 /
c      data sindic / 'OPTIM*', 'REACT*', 'DISPAT*', 'INCLUDEB*',
c     1              'EXCL*', 'SENS*', 'INCLUDEC*', 'DEBUG*',
c     2              'TRACEL*', 'SOLITER*', 'TOLER*', 'AICONTROL*',
c     3              'LIMITS', 'MISC*', 'TRACEB*', 'LTC', 'BASES*',
c     4              'GENDROP', 'LOADSOL*', 'BX', 'SVC' /
c      data lsoli / 4 /
c      data soldic / 'DECOUP*', 'NEWT*', 'OPTIM*', 'CURR*' /
c      data itepnt / 1, 3, 21, 2 /
c      data ltol / 5 /
c      data toldic / 'BUS*', 'AIPOW*', 'TX*', 'Q*', 'OPCUT*', '  ' /
c      data tolpnt / -4, -5, -6, -7, -22 /
c      data llim / 10 /
c      data limdic /'QRES*', 'PHA*', 'DELVMI*', 'DELVMA*', 'DELPMI*',
c     1             'DELPMA*', 'DELTMI*', 'DELTMA*', 'DELANG*',
c     2             'DELVOLT*', '    ', '    ' /
c      data limpnt / -8, -9, -23, -24, -25, -26, -27, -28, -10, -11 /
c      data ldeb / 7 /
c      data debdic / 'TX*', 'BUS*', 'AI*', 'DC*', 'OPT*'   , 'OPV*',
c     1              'OPP*', '    ' /
c      data debpnt / 12, 13, 14, 15, 29, 30, 31 /
c      data lcon / 12 /
c      data condic / 'VFLAT**',   'RELAX**', 'XRGHOST*', 'VOLTAGER**',
c     1              'SAVEBE*', 'DC*', 'XBUS', 'NUMV*', 'TSTART*',
c     2              'TCONT*', 'ITERSUM*', 'PHASE*' /
c      data conpnt / 18, -33, 19, 36, 37, 38, 0, 39, 40, 0, 0, 21 /
c      data rad / 57.29577951 /
 
c      end
