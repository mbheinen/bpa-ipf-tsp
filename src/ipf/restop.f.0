C    %W% %G%
        subroutine restop
 
C       RESeT OPtions at the begining of each Powerflow
C       case to the standard DEFAULT VALUES..
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/addata.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/coment.inc'
      include 'ipfinc/dtaiop.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/header.inc'
      include 'ipfinc/jobctl.inc'
      include 'ipfinc/pageno.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/timmsg.inc'
      include 'ipfinc/zonlst.inc'

C       Set up job control variables
 
        jobreq(3) = 'BUILD_BASE'
 
        do 100 i = 1,40
           if (i .ne. 27 .and. i .ne. 28 .and. i .ne. 33 .and.
     &         i .ne. 37 .and. i .ne. 38 .and. i .ne. 39 .and.
     &         i .ne. 40) kase1(i) = 0
           chase1(i) =  ' '
           if (i .ne. 1  .and. i .ne. 11 .and. i .ne. 16 .and. 
     &         i .ne. 17 .and. i .ne. 22 .and. i .ne. 24 .and.
     &         i .ne. 33) kspare(i) = 0
           cspare(i) = ' '
 100    continue
 
        kchgsw = 1
        call xdate(cspare(31))
 
        do 120 i = 1,20
           krun1(i) = 0
           crun1(i) = ' '
 120    continue
 
        do 140 i = 1,100
           count(i) = 0
           kount(i) = 0
 140    continue
 
        clabl1  = ' '
        clabl2  = ' '
 
C       Define standard user options
 
        do 150 i = 1,40
  150   option(i) = 0.0
C
C       Fiche input data list
C
        kspare(5) = 0
C
C       Fiche output listing
C
        kspare(7) = 0
C
C       Analysis listing level on paper
C
        kspare(8) = 2
C
C       Analysis listing level on fiche
C
        kspare(9) = 0
C
C       Failed solution output listing
C
        kspare(10) = 11
C
C       Area_Interchange listing
C
        kspare(12) = 3
C
C       XBUS=VMAX solution option
C
        kspare(24) = 2
C
C       Decoupled solution iteration count.
C
        iopton(1) = 2
C
C       Current solution iteration count.
C
        iopton(2) = 0
C
C       Newton solution iteration count.
C
        iopton(3) = 40
C
C       Bus solution Tolerance
C
        option(4) = 0.001
C
C       Area Interchange Tolerance
C
        option(5) = 0.001
C
C       Auto Transformer Tolerance
C
        option(6) = 0.001
C
C       Q (reactive) Tolerance
C
        option(7) = 0.001
C
C       Q residual Limit
C
        option(8) = 0.100
C
C       Phase shift limit
C
        option(9) = 10.001
C
C       Maximum angle excursion limit
C
        option(10) = 1.000
C
C       Maximum voltage excursion limit
C
        option(11) = 0.150
C
C       Auto Tx control
C
        iopton(16) = 2
C
C       Area Interchange control
C
        iopton(17) = 3
 
C       ( 3 as default means that presence of data determines
C         default of control)
C
C       Force flat start
C
        iopton(18) = 1
C
C       XR control
C
        iopton(19) = 1
C
C       Voltage adjustment tolerance
C
        option(24) = 0.001
C
C       TSTART500 factor
C
        option(35) = 0.50
C
C       Relax_V control
C
        iopton(36) = 0
C
C       Save 'BE'_BUS control
C
        iopton(37) = 0
C
C       NUMVSTEPS (in NRQLIM)
C
        iopton(39) = 4
C
C       TSTART factor
C
        option(40) = 0.50
 
        bmva = 100.0
 
        return
        end
