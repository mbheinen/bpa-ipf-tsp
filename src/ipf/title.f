C    @(#)title.f	20.3 2/13/96
      subroutine title(isw)
 
C               ROUTINE SETS UP THE REPORT TITLES IN THE
C               SUBHEADING ARRAY TO BE USED FOR THE LINE
C               PRINTER AND FICHE ANALYSIS REPORTS.
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/anlys.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/prt.inc'
 
      common /sortsw/ sortsw, vltsrt(MAXBUS)
      integer sortsw, vltsrt
 
      character x*4
      call forbtm
c
c             1   2   3   4   5   6   7   8   9   10  11  12  13  14 
  100 go to (110,150,210,250,300,340,900,390,430,510,550,600,640,680
     1  ,751,760,820,880,915,720) isw
c         15  16  17  18  19  20
 
C     ******************************************************************
 
  110 write (outbuf,120)
  120 format('0',40x,'System Generations and Loads Summary by Owner')
        call shdlod(1)
 
      write (outbuf,130)
  130 format('0', t5, 'Ownership', t40, '/-- Generation --/',
     1  '/--------------------------  Composite Loads ',
     2   '--------------------------/')
      call shdlod(2)
      write (outbuf, 132)
  132 format(t58,
     1  '   Total composite        Constant P        Constant I',
     2  '        Constant Z ')
      call shdlod(3)
      write (outbuf,140)
  140 format(t40, 5('       MW     MVAR'))
      call shdlod(4)
      outbuf= '0'
      call shdlod(5)
      call fortop
      go to 900
 
C     ******************************************************************
 
  150 write (outbuf,160)
  160 format('0',50x,'Summary of Bus Quantities')
      call shdlod(1)
 
      write (outbuf,170)
  170 format('0Bus name       Voltage  Bank taps     Generation       ',
     1 ' Load           Statics and reactors     Bus  Own  Zone   Gen ',
     2 ' Voltage/angle')
      call shdlod(2)
 
      write (outbuf,180)
  180 format(37x,' MW      MVAR    MW      MVAR    Used     Exist',
     1 '    Unsched  Type             PF       PU/DEG  ')
      call shdlod(3)
 
      write (outbuf,190)
  190 format(    70x,'MVAR    Apprvd     MVAR  ' )
      call shdlod(4)
 
      write (outbuf,200)
  200 format(    79x,'MVAR  ' )
      call shdlod(5)
 
      call fortop
      go to 900
 
C     ******************************************************************
 
  210 write (outbuf,220)
  220 format('0',44x,'Transmission System Loss Summary by Owner')
      call shdlod(1)
 
      write (outbuf,230)
  230 format ('0   Ownership                Line loss       ',
     1'Transformer loss      DC converter        Total loss           ',
     2 'Miles     MW-Miles')
      call shdlod(2)
 
      write (outbuf,240)
  240 format ('                            MW      MVAR       ',
     1'MW      MVAR          MW      MVAR       MW      MVAR ')
      call shdlod(3)
 
      outbuf= '0'
      call shdlod(4)
      outbuf=' '
      call shdlod(5)
      call fortop
      go to 900
 
C     ******************************************************************
 
  250 write (outbuf,260)
  260 format('0',40x,'Summary of Undervoltage - Overvoltage Buses ')
 
      if (sortsw .eq. 1) then
         outbuf(86:) = '(Sorted by ownerships)'
      else if (sortsw .eq. 0 .or. sortsw .eq. 2) then
         outbuf(86:) = '(Sorted by bus names)'
      else if (sortsw .eq. 3) then
         outbuf(86:) = '(Sorted by zones)'
      else if (sortsw .eq. 4) then
         outbuf(86:) = '(Sorted by areas)'
      endif
      call shdlod(1)
 
      write (outbuf,270)
  270 format (1h0,15x,'Own  Zone  Bus name  base    Type      Voltage',
     1 '          Relative      Voltage range       Violation')
      call shdlod(2)
 
      write (outbuf,280)
  280 format( t55,'KV       PU           %      minimum    maximum',
     1        t110, 'PU')
      call shdlod(3)
 
      write (outbuf,290)
  290 format( t87,'PU         PU'  )
      call shdlod(4)
 
      outbuf = '0'
      call shdlod(5)
      call fortop
      go to 900
 
C     ******************************************************************
 
  300 write (outbuf,310) ratln
  310 format ('0          Transmission Lines Loaded Above ', f5.1,
     &  '% of Ratings or with more than 30 degrees electrical angle')

      if (sortsw .eq. 1) then
         outbuf(109:) = '(Sorted by ownerships)'
      else if (sortsw .eq. 0 .or. sortsw .eq. 2) then
         outbuf(109:) = '(Sorted by bus names)'
      else if (sortsw .eq. 3) then
         outbuf(109:) = '(Sorted by zones)'
      else if (sortsw .eq. 4) then
         outbuf(109:) = '(Sorted by areas)'
      endif
      call shdlod(1)
 
      write (outbuf,320)
  320 format ('0Own Zn Bus1      Base Zn Bus2      Base  ID SC   Case',
     1        '   Thermal Bottle- Nominal     Case   Thermal Bottle- ',
     2        'Nominal  Power   Angle')
      call shdlod(2)
 
      write (outbuf,330)
  330 format ('                                                current',
     1        '  rating   neck   rating             rating   neck   ',
     2        'rating   factor  degrees')
      call shdlod(3)
 
      write (outbuf,332)
  332 format (65x,'rating                             rating' )
      call shdlod(4)
 
      write (outbuf,334)
  334 format ('                                                   amps',
     1      '  amps      amps    amps       MVA     MVA    MVA     MVA')
      call shdlod(5)
      call fortop
      go to 900
 
C     ******************************************************************
 
  340 write (outbuf,350) rattx
  350 format('0',40x,' Transformers Loaded Above ',f5.1,'% of Ratings ')
 
      if (sortsw .eq. 1) then
         outbuf(88:) = '(Sorted by ownerships)'
      else if (sortsw .eq. 0 .or. sortsw .eq. 2) then
         outbuf(88:) = '(Sorted by bus names)'
      else if (sortsw .eq. 3) then
         outbuf(88:) = '(Sorted by zones)'
      else if (sortsw .eq. 4) then
         outbuf(88:) = '(Sorted by areas)'
      endif
      call shdlod(1)
 
      write (outbuf,360)
  360 format ('0Own Zn Bus1      Base Zn Bus2      Base  ID SC   Case
     1  Thermal   %   Emergency %    Adj MVA  Bottlenk  %   Nominal   %
     2  Power')
      call shdlod(2)
 
      write (outbuf,370)
  370 format ('                                                  Loading
     1  Rating        Rating         Loading  Rating        Rating
     2  factor')
      call shdlod(3)
 
      write (outbuf,380)
  380 format ('                                                  MVA
     1  mva           mva                     mva
     2       ')
      call shdlod(4)
 
      outbuf = ' '
      call shdlod(5)
      call fortop
      go to 900
 
C     ******************************************************************
 
  390 write (outbuf,400)
  400 format('0', 50x, 'BPA Industrial Loads')
      call shdlod(1)
 
      write (outbuf, 410)
  410 format(t2, 'Bus', t17, 'Zone', t24, 'Load Type', t40,
     1   '/----------------------- BPA Load Classification -------------
     2---------/')
      call shdlod(2)
 
      write (outbuf, 420)
  420 format(t40,
     1   '/----- Firm -----//- Interruptible //-- Potential ---//-- Type
     1 Total --/')
      call shdlod(3)
 
      write (outbuf, 422)
  422 format(t40, 4('       MW     MVAR'))
      call shdlod(4)
 
      outbuf = ' '
      call shdlod(5)
      call fortop
      go to 900
 
C     ******************************************************************
 
  430 if (koptsw.le.4) then
 
              write (outbuf,440)
  440           format('0',40x,'Loss Sensitivity Summary of Buses ')
              call shdlod(1)
 
              write (outbuf,450)
  450 format ('0BUS NAME  BASE    DLOSS/DP AT PNET      DLOSS/DQ',
     1 ' AT QNET     DLOSS/DV AT VOLTAGE  BUS TYPE  COMMENTS      ')
              call shdlod(2)
 
              write (outbuf,460)
  460 format ('           (KV)     (MW/MW)    (MW)      (MW/MVAR)  ',
     1 '(MVAR)     (MW/KV)      (KV)                     '  )
              call shdlod(3)
 
  470   else
        write (outbuf,480)
  480      format('0',40x,'Cost Sensitivity Summary of Buses ')
           call shdlod(1)
 
           write (outbuf,490)
  490 format('0BUS NAME  BASE    DCOST/DP AT PNET      ',
     1'DCOST/DQ AT QNET'
     2,'  DCOST/DV AT VOLTAGE  BUS TYPE  COMMENTS            ')
           call shdlod(2)
 
           write (outbuf,500)
  500 format('           (KV)     ($/MWH)    (MW)     ($/MVARH)   ',
     1 '(MVAR)   ($/KVH)    (KV)    '  )
           call shdlod(3)
 
        endif
 
      outbuf = ' '
      call shdlod(4)
      call shdlod(5)
      call fortop
      go to 900
 
C     ******************************************************************
 
  510 x = 'LOSS'
      if (koptsw.gt.4) x = 'COST'
      write (outbuf,520) x
  520 format('0',40x,a4, ' Sensitivity Summary of LTC Transformers ')
      call shdlod(1)
 
      write (outbuf,530)
  530 format('0FIXED TAP BUS     VARIABLE TAP BUS  TYPE   TAP          '
     1,'    CONTROLLED             GRADIENT (V)       COMMENTS        ')
      call shdlod(2)
 
      write (outbuf,540)
  540 format('                                          (KV/KV)',
     1 '    QUANTITY            SENSITIVITY (Q,P)        '  )
      call shdlod(3)
 
      outbuf = ' '
      call shdlod(4)
      call shdlod(5)
      call fortop
      go to 900
 
C     ******************************************************************
 
  550 write (outbuf,560)
  560 format('0',20x,'Cost Sensitivity Summary of Dispatchable ',
     1' GENERATION ' )
      call shdlod(1)
 
      write (outbuf,570)
  570 format(1h0,28x,'GENERATION COSTS($/H)  =   A($/H)  +  ',
     1 'B($/MWH)*PGEN(MW)+  C($/MW/MWH)*PGEN(MW)**2    ')
      call shdlod(2)
 
      write (outbuf,580)
  580 format('0 GENERATOR  BASE             GENERATION    FIXED COSTS '
     1,'    DCOST/DPGEN    D(SYSTEM COSTS)/DPGEN    GENERATION COST  ')
      call shdlod(3)
 
      write (outbuf,590)
  590 format('             (KV)            ($/H)         ($/MWH)     '
     1,'      ($/MWH)        ($/MWH)                       ($/H) '  )
      call shdlod(4)
 
      outbuf = ' '
      call shdlod(5)
      call fortop
      go to 900
 
C     ******************************************************************
 
  600 write (outbuf,610)
  610 format('0',19x,'System Generations, Loads, Losses and ',
     1  'Bus Shunt Summary by Zones ')
      call shdlod(1)
 
      write (outbuf,620)
  620 format('0', t2, 'Zone', t8, '/-- Generation --/',
     1   '/--------------------------  Composite Loads -----------------
     2---------//---- Losses ----//-- Bus Shunt ---/')
      call shdlod(2)
 
      write (outbuf,630)
  630 format(t26,
     1   '   Total composite        Constant P        Constant I',  
     2   '        Constant Z ')
      call shdlod(3)
 
      write (outbuf,632)
  632 format(t8, 7('       MW     MVAR'))
      call shdlod(4)
 
      outbuf = '0'
      call shdlod(5)
      call fortop
      go to 900
 
C     ******************************************************************
 
  640 write (outbuf,650)
  650 format('0',50x,'Summary of Unscheduled Reactive')
      call shdlod(1)
 
      write (outbuf,660)
  660 format( 1h0,40x,'OWN  ZN  BUS        BASE   TYPE   UNSCHEDULED',
     1        '  KV')
      call shdlod(2)
 
      write (outbuf,670)
  670 format(  78x,'MVAR'  )
      call shdlod(3)
 
      outbuf = ' '
      call shdlod(4)
      call shdlod(5)
      call fortop
      go to 900
 
C     ******************************************************************
 
  680 write (outbuf,690)
  690 format('0',40x,'Optimal Summary of Voltage Gradients ')
      call shdlod(1)
 
      write (outbuf,700)
  700 format('0 OPTIMAL BUS        ADJACENT BUSSES    TYPE  V RELAXED  '
     1     , ' Q RELAXED   GRADIENT   ' )
      call shdlod(2)
 
      write (outbuf,710)
  710 format(   50x,'(KV)       (MVAR) '  )
      call shdlod(3)
 
      outbuf= ' '
      call shdlod(4)
      call shdlod(5)
      call fortop
      go to 900
 
C     ******************************************************************
 
  751 write (outbuf,752)
  752 format('0',45x,' Spinning Reserve Summary ')
      call shdlod(1)
 
      write (outbuf,753)
  753 format('0                ------- ACTIVE  POWER (1) -------     ',
     1'------------------------ REACTIVE  POWER (2) -----------------',
     2 '------------')
      call shdlod(2)
 
      write (outbuf,754)
  754 format('0 AREA/ZONE      SUM PMAX    SUM PGEN      RESERVE     ',
     1       'SUM QMAX    SUM QMIN  SUM Q BOOST  SUM Q BUCK   ',
     2       'BOOST RESERVE  BUCK RESERVE')
      call shdlod(3)
 
      write (outbuf,755)
  755 format('                    (MW)        (MW)        (MW)       ',
     1    'AVAILABLE   AVALIABLE     USED        USED          (MVAR)',
     2    '        (MVAR)')
      call shdlod(4)
 
      write (outbuf,756)
  756 format('                                                        ',
     1 '(MVAR)       (MVAR)     (MVAR)      (MVAR) ')
      call shdlod(5)
      call fortop
      goto 900
 
C     ******************************************************************
 
  760 write (outbuf,770)
  770 format('0',45x,' Shunt Reactive Summary ')
      call shdlod(1)
 
      write (outbuf,780)
  780 format ('0              ------------- CAPACITORS (MVAR) ----------
     1-------         ',
     2'---------------- REACTORS (MVAR) --------------- ')
      call shdlod(2)
 
      write (outbuf,790)
  790 format('0 AREA/ZONE    EXISTING         EXISTING    RESERVE  ',
     1'UNSCHEDULED         EXISTING        EXISTING     RESERVE  ',
     2 'UNSCHEDULED ')
      call shdlod(3)
 
      write (outbuf,800)
  800 format('               APPROVED         APPROVED    UNUSED    ',
     1'               APPROVED        APPROVED     UNUSED ')
      call shdlod(4)
      write (outbuf,810)
  810 format('               AVAILABLE          USED',20x,
     1       '               AVAILABLE         USED  ')
      call shdlod(5)
      call fortop
      go to 900
 
C     ******************************************************************
 
  820 write (outbuf,830)rateff
  830 format ('0',t21,' Transmission Line Efficiency Analysis -- ',
     1        'Lines loaded above ',f5.1,' % of minimum ratings')
      call shdlod(1)
 
      write (outbuf,840)
  840 format ('0',70x,'|',5x, 'Load',4x,'|',14x,'Losses',13x,'|',2x,
     1        'Loading')
      call shdlod(2)
 
      write (outbuf,850)
  850 format('________________________________________________________',
     1'______________________________________________________________',
     2'____________')
      call shdlod(3)
 
      write (outbuf,860)
  860 format (' OWN  ZN  BUS1      BASE   ZN  BUS2      BASE   ID SC  ',
     1'LENGTH  COND.   | MVA    AMPS |      MW        MW/MILE    ',
     2 '%  OF | % OF LN')
      call shdlod(4)
 
      write (outbuf,870)
  870 format ('                                                       ',
     1'MILES    AMPS   |             |                         LN FLOW',
     2' |FLOW/100 MI')
      call shdlod(5)
      call fortop
      go to 900
 
C     ******************************************************************
 
  880 write (outbuf,885) ratxef
  885 format('0',t21,' Transformer Efficiency Analysis -- Total Losses a
     1bove',f5.2,' % of minimum ratings')
      call shdlod(1)
 
      write (outbuf,890)
  890 format ('0                                                      ',
     1'                |        Losses (MW)     |       Percent losses')
      call shdlod(2)
 
      write (outbuf,895)
  895 format('______________________________________________________',
     1'_____________________________________________________________',
     2 '______')
      call shdlod(3)
 
      write (outbuf,905)
  905 format (' OWN  ZN  BUS1      BASE   ZN  BUS2      BASE   ID',
     1'  SC  MVA LD  MVA RT | CORE |  CU  |   TOTAL  |CORE    CU',
     2 '     TOTAL')
      call shdlod(4)
      outbuf = '0'
      call shdlod(5)
      call fortop
      go to 900
 
C     ******************************************************************
 
  915 write (outbuf,920) ratcef
  920 format ('0',t21,' Transformer Efficiency Analysis -- Core ',
     1        'Losses Above',f5.2,' % of minimum ratings')
      call shdlod(1)
      write (outbuf,890)
      call shdlod(2)
      write (outbuf,895)
      call shdlod(3)
      write (outbuf,905)
      call shdlod(4)
      outbuf = '0'
      call shdlod(5)
      call fortop
      go to 900
 
C     ******************************************************************
 
  720 write (outbuf,730)
  730 format('0',40x,'Optimal Summary of LTC Transformer Gradients ' )
      call shdlod(1)
 
      write (outbuf,740)
  740 format('0TRANSFORMER    VARIABLE  TAP     GRADIENT   ADJACENT ',
     1       'BUSSES   TYPE  V RELAXED   Q RELAXED  GRADIENT ')
      call shdlod(2)
 
      write (outbuf,750)
  750 format(    73x,'(KV)       (MVAR) '  )
      call shdlod(3)
 
      outbuf= ' '
      call shdlod(4)
      call shdlod(5)
      call fortop
      go to 900
 
C     ******************************************************************
 
  900 return
 
      end
