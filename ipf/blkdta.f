C    @(#)blkdta.f	20.35 5/3/00
      subroutine init_blkdta

c     previously      block data blkdta

      include 'ipfinc/parametr.inc'

      include 'ipfinc/addata.inc'
      include 'ipfinc/addtbx.inc'
      include 'ipfinc/itrhis.inc'
      include 'ipfinc/com010.inc'
      include 'ipfinc/comdrx.inc'
      include 'ipfinc/bxlock.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/agc.inc'
      include 'ipfinc/jobctl.inc'
      include 'ipfinc/ltcsln.inc'
      include 'ipfinc/mrgtxt.inc'
      include 'ipfinc/oldbus.inc'
      include 'ipfinc/loscom.inc'
      include 'ipfinc/oldfil.inc'
      include 'ipfinc/svc.inc'
      include 'ipfinc/usranl.inc'
      include 'ipfinc/xtran.inc'
      include 'ipfinc/owncom.inc'
      include 'ipfinc/pfstates.inc'
      include 'ipfinc/bsekvhsh.inc'
      include 'ipfinc/ownhash.inc'
      include 'ipfinc/zbo.inc'
      include 'ipfinc/update.inc'
      include 'ipfinc/datainit.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/lndpcp.inc'

      common /ownflg/ ownflg
      logical ownflg

      common /init/ init  
C                               PRTPKG-BLKDTA   
      common /prtdbg/ dbsw
      integer dbsw
C                               BLKDTA-BLD2DC   
      common /iswap/ iswap(2,9)   
C                               BLKDTA-BRCHEK   
      common /ran001/ range(9,10) 
      common /ran002/ brtypec(9) 
      character brtypec*2
C                               BLKDTA-CHKBCH   
      common  /jpi/   
     4         lenpnm , locpnm , adrpnm , artnpn , 
     9         alldon  
      integer*2 lenpnm,  locpnm   
      integer*4 adrpnm,  artnpn, alldon   
C                               BLKDTA-CHKBRN   
      common /itot/ itot,ioerr
C                               BLKDTA-CHREAD   
      common /skpdic/ skpdic(2)   
      character*10 skpdic 
C                               BLKDTA-PRESLN   
      common /pre001/ iq(13)  
C                               BLKDTA-VLTLIM   
      common /vlt001/ ratio3, ratio4  
C                               BLKDTA-RDTYP9   
      common /rdt001/ start   
      character start * 10  

      common /bcdflg/ bldtbx, lowx, xlinmn, npctq, pctq(MAXBUS)
      logical bldtbx, lowx
 
c     Note: This variable can be changed with a symbolic debugger
      common /term_debug/ iterm


      prgvsn = 'IPF  327'    ! Current Program version
      kspare(33) = 9         ! Current Program/base case version code

      call blkdta_alt

      brnch_nxt(0) = 0
      brtype(0) = 0

      last_out1 = 0
      last_out2 = 0
      ymod_cmde_flag = .false.
      defvlt = 'NOMINAL'

      update(1) = 1          ! Force updating ZSUM arrays
      do i = 2, 10
         update(i) = 0
      enddo
      ostates = 0
      iterm = 0
      bldtbx = .false.
      lowx = .false.
      npctq = 0
      xlinmn = 0.0
      do i = 1 , MAXBUS
         pctq(i) = 100.0
      enddo
      ntbxad = 0
      numagc = 0
      numlck = 0
      itrtot = 0
      oldrop = -9.0e10
      pctpku = 0.0d0
      do i = 1, 1000
         pold(i) = 0.0d0
      enddo
      do i = 1, MAXCAR
         slkgen(i) = 0.0
      enddo
      itrtyp = 2
      itrhis(1) = 0
      itrhis(2) = 0
      numlsz = 0
      numlsa = 0
      do i = 1, MAXLTC
         ltcsln(i) = 0
      enddo
      numobs = 0
      numsvc = 0
      xtrflg = 0
      numusr = 0
      do i = 1, 20
         numdef(i) = 0
         numtxt(i) = 0
         usrdbg(i) = 0
         numupx(i) = 0
         usrfil(i) = ' '
      enddo
      ownflg = .false.
      nbsmrg = 0
      nbrmrg = 0
      nifmrg = 0
      oldbrd =' '
      oldbsd =' '
      srtlst = '~~~~~~~~'
      nvlim = 25

      vlimit(1,1)  =    0.0
      vlimit(2,1)  =    6.5
      vlimit(3,1)  =    0.950
      vlimit(4,1)  =    1.052
      vlimit(5,1)  =    0.333   
      vlimit(1,2)  =    6.6
      vlimit(2,2)  =    6.6
      vlimit(3,2)  =    0.950
      vlimit(4,2)  =    1.065
      vlimit(5,2)  =    0.333   
      vlimit(1,3)  =    6.6
      vlimit(2,3)  =   49.9
      vlimit(3,3)  =    0.950
      vlimit(4,3)  =    1.052
      vlimit(5,3)  =    0.333   
      vlimit(1,4)  =   50.0
      vlimit(2,4)  =   50.0
      vlimit(3,4)  =    0.950
      vlimit(4,4)  =    1.050
      vlimit(5,4)  =    0.333   
      vlimit(1,5)  =   50.0
      vlimit(2,5)  =   59.9
      vlimit(3,5)  =    0.950
      vlimit(4,5)  =    1.052
      vlimit(5,5)  =    0.333   
      vlimit(1,6)  =   60.0
      vlimit(2,6)  =   60.0
      vlimit(3,6)  =    0.950
      vlimit(4,6)  =    1.050
      vlimit(5,6)  =    0.333   
      vlimit(1,7)  =   60.0
      vlimit(2,7)  =   62.9
      vlimit(3,7)  =    0.950
      vlimit(4,7)  =    1.052
      vlimit(5,7)  =    0.333   
      vlimit(1,8)  =   63.0
      vlimit(2,8)  =   63.0
      vlimit(3,8)  =    0.930
      vlimit(4,8)  =    1.080
      vlimit(5,8)  =    0.333   
      vlimit(1,9)  =   63.0
      vlimit(2,9)  =   99.9
      vlimit(3,9)  =    0.950
      vlimit(4,9)  =    1.052
      vlimit(5,9)  =    0.333   
      vlimit(1,10) =  100.0
      vlimit(2,10) =  100.0
      vlimit(3,10) =    0.950
      vlimit(4,10) =    1.050
      vlimit(5,10) =    0.333   
      vlimit(1,11) =  100.0
      vlimit(2,11) =  100.0
      vlimit(3,11) =    0.950
      vlimit(4,11) =    1.070
      vlimit(5,11) =    0.333   
      vlimit(1,12) =  100.0
      vlimit(2,12) =  114.9
      vlimit(3,12) =    0.950
      vlimit(4,12) =    1.052
      vlimit(5,12) =    0.333   
      vlimit(1,13) =  115.0
      vlimit(2,13) =  115.0
      vlimit(3,13) =    0.950
      vlimit(4,13) =    1.070
      vlimit(5,13) =    0.333   
      vlimit(1,14) =  115.0
      vlimit(2,14) =  131.9
      vlimit(3,14) =    0.950
      vlimit(4,14) =    1.052
      vlimit(5,14) =    0.333   
      vlimit(1,15) =  132.0
      vlimit(2,15) =  161.0
      vlimit(3,15) =    0.950
      vlimit(4,15) =    1.090
      vlimit(5,15) =    0.333   
      vlimit(1,16) =  132.0
      vlimit(2,16) =  199.9
      vlimit(3,16) =    0.950
      vlimit(4,16) =    1.052
      vlimit(5,16) =    0.333   
      vlimit(1,17) =  200.0
      vlimit(2,17) =  200.0
      vlimit(3,17) =    0.950
      vlimit(4,17) =    1.050
      vlimit(5,17) =    0.333   
      vlimit(1,18) =  200.0
      vlimit(2,18) =  229.9
      vlimit(3,18) =    0.950
      vlimit(4,18) =    1.052
      vlimit(5,18) =    0.333   
      vlimit(1,19) =  230.0
      vlimit(2,19) =  230.0
      vlimit(3,19) =    0.950
      vlimit(4,19) =    1.070
      vlimit(5,19) =    0.333   
      vlimit(1,20) =  230.0
      vlimit(2,20) =  499.9
      vlimit(3,20) =    0.950
      vlimit(4,20) =    1.052
      vlimit(5,20) =    0.333   
      vlimit(1,21) =  500.0
      vlimit(2,21) =  500.0
      vlimit(3,21) =    1.000
      vlimit(4,21) =    1.100
      vlimit(5,21) =    0.333   
      vlimit(1,22) =  500.1
      vlimit(2,22) = 1099.9
      vlimit(3,22) =    0.950
      vlimit(4,22) =    1.052
      vlimit(5,22) =    0.333   
      vlimit(1,23) = 1100.0
      vlimit(2,23) = 1100.0
      vlimit(3,23) =    1.000
      vlimit(4,23) =    1.100
      vlimit(5,23) =    0.333   
      vlimit(1,24) = 1100.1
      vlimit(2,24) = 9999.9
      vlimit(3,24) =    0.950
      vlimit(4,24) =    1.052
      vlimit(5,24) =    0.333   
      vlimit(1,25) =    0.0
      vlimit(2,25) = 9999.9
      vlimit(3,25) =    0.950
      vlimit(4,25) =    1.052
      vlimit(5,25) =    0.333

      do i = 1, 10
         do j = 1, 25
            vlimzn(i,j) = ' '
         enddo
      enddo
      vlimzn(1,2) = 'M1'
      vlimzn(1,4) = '16'
      vlimzn(1,6) = '17'
      vlimzn(1,8) = '20'
c     vlimzn(1,10) = '16'
      vlimzn(2,10) = 'M5'
      vlimzn(1,11) = 'M4'
      vlimzn(1,13) = 'M4'
      vlimzn(1,15) = '17'
      vlimzn(2,15) = '20'
c     vlimzn(1,17) = '16'
      vlimzn(1,19) = '17'
      vlimzn(2,19) = '20'

      ntot = 0
      ltot = 0
      jtie = 0
      ktchge = 0
      mtdcbs = 0
      mtdcln = 0
      ntota = 0
      ntotb = 0
      ntotc = 0
      kxtot = 0
      kdtot = 0
      ksy = 0
      ntot2 = 0
      nbsadd = 0
      nbradd = 0
      nbslck = 0
      natot = 0
      jphno = 0
      init = 0
      numldc = 0
C                          PRTPKG-BLKDTA
      dbsw = 0
C                          ANALYS-BSANRP-BLKDTA 
      off = ' '
      fnct(1) = 'RECTIFIER '
      fnct(2) = 'INVERTER  '
C                          BLKDTA-BLD2DC
      iswap(1,1) = 3
      iswap(2,1) = 1
      iswap(1,2) = 4
      iswap(2,2) = 2
      iswap(1,3) = 15
      iswap(2,3) = 12
      iswap(1,4) = 16
      iswap(2,4) = 13
      iswap(1,5) = 17
      iswap(2,5) = 14
      iswap(1,6) = 23
      iswap(2,6) = 18
      iswap(1,7) = 24
      iswap(2,7) = 19
      iswap(1,8) = 27
      iswap(2,8) = 28
      iswap(1,9) = 34
      iswap(2,9) = 33
C                          BLKDTA-BRCHEK
      range(1,1) = 0.0
      range(2,1) = 0.33
      range(3,1) = 3.33
      range(4,1) =  0.25
      range(5,1) =  2.50
      range(6,1) =  0.65
      range(7,1) =  0.98
      range(8,1) =  2.0
      range(9,1) =  3.3   
      range(1,2) =  69.0
      range(2,2) = 0.25
      range(3,2) = 1.42
      range(4,2) =  0.25
      range(5,2) =  1.70
      range(6,2) =  0.60
      range(7,2) =  0.98
      range(8,2) =  2.0
      range(9,2) =  3.3   
      range(1,3) = 100.0
      range(2,3) = 0.10
      range(3,3) = 1.42
      range(4,3) =  0.06
      range(5,3) =  0.70
      range(6,3) =  0.60
      range(7,3) =  0.98
      range(8,3) =  2.3
      range(9,3) =  3.3   
      range(1,4) = 115.0
      range(2,4) = 0.10
      range(3,4) = 0.71
      range(4,4) =  0.06
      range(5,4) =  0.70
      range(6,4) =  0.60
      range(7,4) =  0.98
      range(8,4) =  2.3
      range(9,4) =  3.3   
      range(1,5) = 138.0
      range(2,5) = 0.05
      range(3,5) = 0.71
      range(4,5) =  0.06
      range(5,5) =  0.50
      range(6,5) =  0.50
      range(7,5) =  0.98
      range(8,5) =  2.3
      range(9,5) =  3.3   
      range(1,6) = 161.0
      range(2,6) = 0.03
      range(3,6) = 0.50
      range(4,6) =  0.04
      range(5,6) =  0.50
      range(6,6) =  0.50
      range(7,6) =  0.98
      range(8,6) =  2.3
      range(9,6) =  3.3   
      range(1,7) = 230.0
      range(2,7) = 0.03
      range(3,7) = 0.50
      range(4,7) =  0.02
      range(5,7) =  0.18
      range(6,7) =  0.50
      range(7,7) =  0.95
      range(8,7) =  2.4
      range(9,7) =  4.5   
      range(1,8) = 300.0
      range(2,8) = 0.03
      range(3,8) = 0.10
      range(4,8) =  0.02
      range(5,8) =  0.09
      range(6,8) =  0.50
      range(7,8) =  0.85
      range(8,8) =  2.4
      range(9,8) =  4.5   
      range(1,9) = 345.0
      range(2,9) = 0.03
      range(3,9) = 0.10
      range(4,9) =  0.02
      range(5,9) =  0.09
      range(6,9) =  0.50
      range(7,9) =  0.85
      range(8,9) =  2.4
      range(9,9) =  4.5   
      range(1,10) = 500.0
      range(2,10) = 0.03
      range(3,10) = 0.07
      range(4,10) =  0.02
      range(5,10) =  0.05
      range(6,10) =  0.50
      range(7,10) =  0.80
      range(8,10) =  2.7
      range(9,10) =  4.5
c
      brtypec(1) = 'EP'
      brtypec(2) = 'LM'
      brtypec(3) = 'L '
      brtypec(4) = 'R '
      brtypec(5) = 'T '
      brtypec(6) = 'TP'
      brtypec(7) = 'LD'
      brtypec(8) = 'E '
      brtypec(9) = 'RZ'
C
C       BLKDTA-CHKBCH
C
      itot = 0
      ioerr = 0
C                          BLKDTA-CHREAD
      skpdic(1) = 'CHANGES*'
      skpdic(2) = 'COM*'
C                          BLKDTA-PRESLN
      iq(1) = 1
      iq(2) = 2
      iq(3) = 2
      iq(4) = 1
      iq(5) = 1
      iq(6) = 0
      iq(7) = 0
      iq(8) = 0
      iq(9) = 0
      iq(10) = 1
      iq(11) = 0
      iq(12) = 1
      iq(13) = 0
C                          BLKDTA-VLTLIM
      ratio3 = 0.2
      ratio4 = 0.40
C                          BLKDTA-RDTYP9
      start = 'NEW SYSTEM'

      num_zbo = 0

c***  owners(i) 

      i = 0

      i = i + 1
      owners(i) =
     1 'AAC    ANACONDA ALUMINUM COMPANY                            '  

      i = i + 1
      owners(i) =
     1 'AEC    ATOMIC ENERGY COMMISSION                             '  

      i = i + 1
      owners(i) =
     1 'AEP    ARIZONA ELECTRIC POWER COOPERATION                   '  

      i = i + 1
      owners(i) =
     1 'ALA    ALABAMA POWER COMPANY                                '  

      i = i + 1
      owners(i) =
     1 'ALC    ALUMINUM COMPANY OF AMERICA                          '  

      i = i + 1
      owners(i) =
     1 'APS    ARIZONA PUBLIC SERVICE COMPANY                       '  

      i = i + 1
      owners(i) =
     1 'ARL    ARKANSAS POWER AND LIGHT COMPANY                     '  

      i = i + 1
      owners(i) =
     1 'ARR    ARROWHEAD ELECTRIC COOPERATIVE INC.                  '

      i = i + 1
      owners(i) =
     1 'AVA    AVISTA CORP.                                         '  

      i = i + 1
      owners(i) =
     1 'AVC    AMARGOSA VALLEY COOPERATIVE INC.                     '  

      i = i + 1
      owners(i) =
     1 'BBE    BIG BEND ELECT. COOP                                 '

      i = i + 1
      owners(i) =
     1 'BCH    BRITISH COLUMBIA HYDRO AND POWER AUTHORITY           '  

      i = i + 1
      owners(i) =
     1 'BEC    BASIN ELECTRIC POWER COOP.                           '  

      i = i + 1
      owners(i) =
     1 'BEP    BASIN ELECTRIC COOPERATIVE                           '  

      i = i + 1
      owners(i) =
     1 'BHP    BLACK HILLS POWER AND LIGHT COMPANY                  '  

      i = i + 1
      owners(i) =
     1 'BLC    BLACHLY-LANE COOP.                                   '  

      i = i + 1
      owners(i) =
     1 'BPA    BONNEVILLE POWER ADMINISTRATION                      '  

      i = i + 1
      owners(i) =
     1 'BPD    BENTON CO. PUD                                       '  

      i = i + 1
      owners(i) =
     1 'BRE    BENTON REA                                           '  

      i = i + 1
      owners(i) =
     1 'CAL    CALIFORNIA DEPARTMENT OF WATER RESOURCES             '  

      i = i + 1
      owners(i) =
     1 'CCC    COOS CURRY ELECTRIC COOP                             '  

      i = i + 1
      owners(i) =
     1 'CCN    CENTRALIA, CITY OF                                   '

      i = i + 1
      owners(i) =
     1 'CCP    COWLITZ COUNTY PUBLIC UTILITY DISTRICT NO.1          '

      i = i + 1
      owners(i) =
     1 'CCS    CITY OF COLORADO SPRINGS                             '  

      i = i + 1
      owners(i) =
     1 'CE1    DEP. OF ARMY CORPS OF ENGINEER (REGION 1 AREA)       '  

      i = i + 1
      owners(i) =
     1 'CE2    DEP. OF ARMY CORPS OF ENGINEER (REGION 2 AREA)       '  

      i = i + 1
      owners(i) =
     1 'CE3    DEP. OF ARMY CORPS OF ENGINEER (REGION 3 AREA)       '  

      i = i + 1
      owners(i) =
     1 'CE4    DEP. OF ARMY CORPS OF ENGINEER (REGION 4 AREA)       '  

      i = i + 1
      owners(i) =
     1 'CE5    DEP. OF ARMY CORPS OF ENGINEER (REGION 5 AREA)       '  

      i = i + 1
      owners(i) =
     1 'CE6    DEP. OF ARMY CORPS OF ENGINEER (REGION 6 AREA)       '  

      i = i + 1
      owners(i) =
     1 'CE7    DEP. OF ARMY CORPS OF ENGINEER (REGION 7 AREA)       '  

      i = i + 1
      owners(i) =
     1 'CED    COMMONWEALTH EDISON COMPANY OF INDIANA,  INC.        '

      i = i + 1
      owners(i) =
     1 'CEN    CENTRAL POWER ELECTRIC COOP., INC. (N. DAKOTA)       '

      i = i + 1
      owners(i) =
     1 'CHW    CHEWELAH, CITY OF                                    '

      i = i + 1
      owners(i) =
     1 'CIP    CENTRAL IOWA POWER COOPERATIVE                       '  

      i = i + 1
      owners(i) =
     1 'CIS    CENTRAL ILLINOIS PUBLIC SERVICE COMPANY              '  

      i = i + 1
      owners(i) =
     1 'CLA    CLALLAM PUD                                          '  

      i = i + 1
      owners(i) =
     1 'CLK    CLARK COUNTY PUBLIC UTILITY DISTRICT NO. 1           '  

      i = i + 1
      owners(i) =
     1 'CLP    CENTRAL LINCOLN PUD                                  '  

      i = i + 1
      owners(i) =
     1 'CLR    CLEARWATER POWER                                     '  

      i = i + 1
      owners(i) =
     1 'CLT    CLATSKANIE PUD                                       '  

      i = i + 1
      owners(i) =
     1 'CMS    CHICAGO, MILWAUKEE, ST.PAUL AND PACIFIC R.R. CO.     '

      i = i + 1
      owners(i) =
     1 'CNE    CENTRAL ELEC. COOP.                                  '

      i = i + 1
      owners(i) =
     1 'CNP    CENTRAL NEBRASKA PUBLIC POWER AND IRRIGATION DIST.   '  

      i = i + 1
      owners(i) =
     1 'COB    CORN BELT POWER COOPERATIVE, INC.                    '

      i = i + 1
      owners(i) =
     1 'COE    DEP. OF ARMY CORPS OF ENGINEERS                      '  

      i = i + 1
      owners(i) =
     1 'COL    COLUMBIA REA                                         '  

      i = i + 1
      owners(i) =
     1 'COR    CITY OF RICHLAND                                     '  

      i = i + 1
      owners(i) =
     1 'CPA    COOPERATIVE POWER ADMINISTRATION                     '  

      i = i + 1
      owners(i) =
     1 'CPD    CHELAN COUNTY PUBLIC UTILITY DISTRICT NO. 1          '  

      i = i + 1
      owners(i) =
     1 'CPI    CONSUMERS POWER INC.                                 '  

      i = i + 1
      owners(i) =
     1 'CPL    CALGARY POWER LIMITED                                '  

      i = i + 1
      owners(i) =
     1 'CPN    C P NATIONAL                                         '  

      i = i + 1
      owners(i) =
     1 'CPP    CONSUMERS PUBLIC POWER DISTRICT (NEBRASKA)           '  

      i = i + 1
      owners(i) =
     1 'CPS    COMMUNITY PUBLIC SERVICE CO.                         '  

      i = i + 1
      owners(i) =
     1 'CPU    CALIFORNIA PACIFIC UTILITIES COMPANY                 '

      i = i + 1
      owners(i) =
     1 'CRP    COLUMBIA RIVER PUD                                   '  

      i = i + 1
      owners(i) =
     1 'CU     COLORADO-UTE ELECTRIC ASSOCIATION                    '  

      i = i + 1
      owners(i) =
     1 'DEC    DOUGLAS ELECTRIC COOP.                               '

      i = i + 1
      owners(i) =
     1 'DOE    DOE_ RICHLAND                                        '

      i = i + 1
      owners(i) =
     1 'DPC    DAIRYLAND POWER COOPERATIVE (WISC., MINN.)           '

      i = i + 1
      owners(i) =
     1 'DPD    DOUGLAS COUNTY PUBLIC UTILITIES DISTRICT             '  

      i = i + 1
      owners(i) =
     1 'EEQ    EASTERN EQUIVALENT                                   '  

      i = i + 1
      owners(i) =
     1 'EIL    EASTERN IOWA LIGHT AND POWER COOPERATIVE             '  

      i = i + 1
      owners(i) =
     1 'ELE    EL PASO ELECTRIC COMPANY                             '  

      i = i + 1
      owners(i) =
     1 'ELM    ELMHURST MUTUAL                                      '  

      i = i + 1
      owners(i) =
     1 'ELP    EL PASO ELECTRIC COMPANY                             '  

      i = i + 1
      owners(i) =
     1 'EMP    EMERALD PUD                                          '  

      i = i + 1
      owners(i) =
     1 'EPE    EL PASO ELECTRIC                                     '

      i = i + 1
      owners(i) =
     1 'ERP    EAST RIVER ELECTRIC POWER COOP.,INC.(S. DAKOTA)      '

      i = i + 1
      owners(i) =
     1 'EWE    EUGENE WATER AND ELECTRIC BOARD (OREGON)             '  

      i = i + 1
      owners(i) =
     1 'FEC    FLATHEAD ELECTRIC COOP.                              '  

      i = i + 1
      owners(i) =
     1 'FOR    FOREST GROVE                                         '  

      i = i + 1
      owners(i) =
     1 'FRC    FALL RIVER ELEC. COOP                                '  

      i = i + 1
      owners(i) =
     1 'FRK    FRANKLIN CO. PUD                                     '  

      i = i + 1
      owners(i) =
     1 'GH     GRAYS HARBOR COUNTY PUBLIC UTILTIY DISTRICT          '  

      i = i + 1
      owners(i) =
     1 'GPD    GRANT COUNTY PUD NO.2 (WASHINGTON)                   '  

      i = i + 1
      owners(i) =
     1 'GSU    GULF STATE UTILITIES COMPANY (TEXAS, LOUISIANA)      '

      i = i + 1
      owners(i) =
     1 'HAR    HARNEY ELECTRIC COOP                                 '  

      i = i + 1
      owners(i) =
     1 'HEA    HIGHLINE ELECTRIC ASSOCIATION                        '  

      i = i + 1
      owners(i) =
     1 'HPL    HOUSTON POWER AND LIGHT COMPANY                      '

      i = i + 1
      owners(i) =
     1 'IDP    IDAHO POWER COMPANY                                  '  

      i = i + 1
      owners(i) =
     1 'IEL    IOWA ELECTRIC LIGHT AND POWER                        '  

      i = i + 1
      owners(i) =
     1 'IGE    IOWA ILLINOIS GAS AND ELECTRIC COMPANY               '  

      i = i + 1
      owners(i) =
     1 'IID    IMPERIAL IRRIGATION DISTRICT (CALIFORNIA)            '  

      i = i + 1
      owners(i) =
     1 'IIG    IOWA-ILLINOIS GAS & ELECTRIC CO.                     '  

      i = i + 1
      owners(i) =
     1 'ILL    ILLINOIS POWER COMPANY                               '  

      i = i + 1
      owners(i) =
     1 'ILM    ILLINOIS AND EASTERN MISSOURI                        '  

      i = i + 1
      owners(i) =
     1 'IME    INDIANA AND MICHIGAN ELECTRIC COMPANY                '  

      i = i + 1
      owners(i) =
     1 'INL    INLAND POWER AND LIGHT                               '  

      i = i + 1
      owners(i) =
     1 'INP    INLAND POWER AND LIGHT COMPANY                       '

      i = i + 1
      owners(i) =
     1 'IPC    IDAHO POWER COMPANY                                  '  

      i = i + 1
      owners(i) =
     1 'IPL    IOWA POWER AND LIGHT COMPANY                         '  

      i = i + 1
      owners(i) =
     1 'IPS    IOWA PUBLIC SERVICE COMPANY                          '  

      i = i + 1
      owners(i) =
     1 'IPU    IOWA SOUTHERN UTILITIES CO.                          '  

      i = i + 1
      owners(i) =
     1 'ISP    INTERSTATE POWER COMPANY                             '  

      i = i + 1
      owners(i) =
     1 'ISU    IOWA SOUTHERN UTILITIES COMPANY                      '  

      i = i + 1
      owners(i) =
     1 'KCP    KANSAS CITY POWER AND LIGHT COMPANY                  '  

      i = i + 1
      owners(i) =
     1 'KEC    KOOTENAI COOP.                                       '  

      i = i + 1
      owners(i) =
     1 'KGE    KANSAS GAS AND ELECTRIC COMPANY                      '  

      i = i + 1
      owners(i) =
     1 'KLI    KLICKITAT COUNTY PUD                                 '  

      i = i + 1
      owners(i) =
     1 'KPL    KANSAS POWER AND LIGHT COMPANY                       '  

      i = i + 1
      owners(i) =
     1 'LA     CITY OF LOS ANGELES DEPARTMENT OF WATER AND POWER    '

      i = i + 1
      owners(i) =
     1 'LCR    LOWER COLORADO REGION WESTERN AREA POWER ADMIN.      '  

      i = i + 1
      owners(i) =
     1 'LEC    LANE CO. ELEC.COOP.                                  '  

      i = i + 1
      owners(i) =
     1 'LES    LINCOLN ELECTRIC SYSTEM                              '  

      i = i + 1
      owners(i) =
     1 'LEW    LEWIS CO. PUD.                                       '  

      i = i + 1
      owners(i) =
     1 'LKV    LAKEVIEW L&P                                         '  

      i = i + 1
      owners(i) =
     1 'LPL    LOUISIANA POWER AND LIGHT COMPANY                    '  

      i = i + 1
      owners(i) =
     1 'LRE    LOST RIVER ELECTRIC                                  '  

      i = i + 1
      owners(i) =
     1 'LSD    LAKE SUPERIOR DISTRICT POWER COMPANY                 '  

      i = i + 1
      owners(i) =
     1 'LVP    LOWER VALLEY POWER AND LIGHT                         '  

      i = i + 1
      owners(i) =
     1 'MAI    MAIN-MID-AMERICA INTERPOOL NETWORK                   '  

      i = i + 1
      owners(i) =
     1 'MCM    MCMINNVILLE, CITY OF                                 '

      i = i + 1
      owners(i) =
     1 'MDR    MODERN ELECTRIC COOP                                 '

      i = i + 1
      owners(i) =
     1 'MDU    MONTANA-DAKOTA UTILITIES COMPANY                     '

      i = i + 1
      owners(i) =
     1 'MEC    MIDSTATE ELECTRIC COOP.                              '

      i = i + 1
      owners(i) =
     1 'MFR    MILTON-FREEWATER                                     '  

      i = i + 1
      owners(i) =
     1 'MH     MANITOVA HYDRO ELECTRIC BOARD                        '  

      i = i + 1
      owners(i) =
     1 'MIN    MINNKOTA POWER COOPERATIVE, INC.                     '

      i = i + 1
      owners(i) =
     1 'MLC    MISSOURI POWER AND LIGHT COMPANY                     '  

      i = i + 1
      owners(i) =
     1 'MLE    MOON LAKE ELECTRIC ASSOCIATION, INC.                 '

      i = i + 1
      owners(i) =
     1 'MN1    MASON COUNTY PUD #1                                  '  

      i = i + 1
      owners(i) =
     1 'MN3    MASON COUNTY PUD #3                                  '  

      i = i + 1
      owners(i) =
     1 'MPC    MONTANA POWER COMPANY                                '  

      i = i + 1
      owners(i) =
     1 'MPL    MINNESOTA POWER AND LIGHT COMPANY                    '  

      i = i + 1
      owners(i) =
     1 'MPO    MISSISSIPPI POWER AND LIGHT COMPANY                  '

      i = i + 1
      owners(i) =
     1 'MPR    MID PACIFIC REGION - USBR                            '  

      i = i + 1
      owners(i) =
     1 'MPS    MISSOURI PUBLIC SERVICE COMPANY                      '  

      i = i + 1
      owners(i) =
     1 'MPW    MUSCATINE POWER AND WATER                            '  

      i = i + 1
      owners(i) =
     1 'MSV    MISSION VALLEY                                       '  

      i = i + 1
      owners(i) =
     1 'MWD    METROPOLITAN WATER DISTRICT OF SOUTHERN CALIFORNIA   '  

      i = i + 1
      owners(i) =
     1 'NEP    N.W. ELECTRIC POWER COOP., INC. (MISSOURI,ARK.)      '

      i = i + 1
      owners(i) =
     1 'NGT    NEBRASKA ELECTRIC GENERATING AND TRANSMISSION COOP   '  

      i = i + 1
      owners(i) =
     1 'NIP    NORTHWEST IOWA POWER COOPERATIVE                     '  

      i = i + 1
      owners(i) =
     1 'NLI    NORTHERN LIGHTS, INC.                                '

      i = i + 1
      owners(i) =
     1 'NPC    NEVADA POWER COMPANY                                 '  

      i = i + 1
      owners(i) =
     1 'NPP    NEBRASKA PUBLIC POWER SYSTEM                         '

      i = i + 1
      owners(i) =
     1 'NPR    NORTH PACIFIC REGION - USBR                          '  

      i = i + 1
      owners(i) =
     1 'NSC    NORTHERN STATES POWER COMPANY  (WISCONSIN)           '

      i = i + 1
      owners(i) =
     1 'NSP    NORTHERN STATES POWER COMPANY  (MINN.,N.D.,S.D.)     '

      i = i + 1
      owners(i) =
     1 'NWA    NORTHERN WASCO PUD                                   '  

      i = i + 1
      owners(i) =
     1 'NWP    NORTHWESTERN PUBLIC SERVICE COMPANY (S. DAKOTA)      '  

      i = i + 1
      owners(i) =
     1 'OGE    OKLAHOMA GAS AND ELECTRIC COMPANY                    '  

      i = i + 1
      owners(i) =
     1 'OKP    OKANOGAN CO. PUD                                     '  

      i = i + 1
      owners(i) =
     1 'OPD    OMAHA PUBLIC POWER DISTRICT                          '  

      i = i + 1
      owners(i) =
     1 'OPL    ORCAS POWER AND LIGHT                                '

      i = i + 1
      owners(i) =
     1 'OPP    OMAHA PUBLIC POWER DISTRICT                          '  

      i = i + 1
      owners(i) =
     1 'OTC    OREGON TRAIL COOP                                    '

      i = i + 1
      owners(i) =
     1 'OTP    OTTER TAIL POWER COMPANY                             '  

      i = i + 1
      owners(i) =
     1 'OWI    OROVILLE-WYANDOTTE IRRIGATION DISTRICT  (CALIF.)     '

      i = i + 1
      owners(i) =
     1 'PAC    PACIFIC PUD                                          '  

      i = i + 1
      owners(i) =
     1 'PAN    PORT ANGELES                                         '  

      i = i + 1
      owners(i) =
     1 'PDO    PEND OREILLE PUD                                     '  

      i = i + 1
      owners(i) =
     1 'PEG    PLAINS ELECTRIC G AND T COOP  (NEW MEXICO)           '  

      i = i + 1
      owners(i) =
     1 'PEN    PENINSULA LT. CO.                                    '  

      i = i + 1
      owners(i) =
     1 'PG     PORTLAND GENERAL ELECTRIC COMPANY                    '  

      i = i + 1
      owners(i) =
     1 'PGE    PACIFIC GAS AND ELECTRIC COMPANY                     '  

      i = i + 1
      owners(i) =
     1 'PGT    PLAINS ELECTRIC G AND T COOP.  (NEW MEXICO)          '

      i = i + 1
      owners(i) =
     1 'PLM    PLUMMER, CITY OF                                     '

      i = i + 1
      owners(i) =
     1 'PNM    PUBLIC SERVICE COMPANY OF NEW MEXICO                 '

      i = i + 1
      owners(i) =
     1 'PPL    PACIFIC POWER AND LIGHT COMPANY                      '  

      i = i + 1
      owners(i) =
     1 'PPW    PACIFIC POWER AND LIGHT - WYOMING                    '  

      i = i + 1
      owners(i) =
     1 'PRP    PLATTE RIVER POWER AUTHORITY                         '  

      i = i + 1
      owners(i) =
     1 'PSC    PUBLIC SERVICE COMPANY OF COLORADO                   '  

      i = i + 1
      owners(i) =
     1 'PSE    PUGET SOUND ENERGY                                   '  

      i = i + 1
      owners(i) =
     1 'PSI    PUBLIC SERVICE COMPANY OF INDIANA                    '  

      i = i + 1
      owners(i) =
     1 'PSO    PUBLIC SERVICE COMPANY OF OKLAHOMA                   '  

      i = i + 1
      owners(i) =
     1 'PSP    PUGET SOUND ENERGY                                   '  

      i = i + 1
      owners(i) =
     1 'R1     WESTERN AREA POWER ADMIN. REGION 1                   '  

      i = i + 1
      owners(i) =
     1 'R2     WESTERN AREA POWER ADMIN. SACRAMENTO AREA            '  

      i = i + 1
      owners(i) =
     1 'R3     WESTERN AREA POWER ADMIN. REGION 3                   '

      i = i + 1
      owners(i) =
     1 'R4     WESTERN AREA POWER ADMIN. SALT LAKE CITY AREA        '  

      i = i + 1
      owners(i) =
     1 'R5     WESTERN AREA POWER ADMIN. REGION 5                   '  

      i = i + 1
      owners(i) =
     1 'R6     WESTERN AREA POWER ADMIN. BILLINGS AREA              '  

      i = i + 1
      owners(i) =
     1 'R7     WESTERN AREA POWER ADMIN. DENVER AREA                '  

      i = i + 1
      owners(i) =
     1 'RCP    RURAL COOPERATIVE POWER ASSOCIATION  (MINNESOTA)     '

      i = i + 1
      owners(i) =
     1 'RFT    RAFT RIVER RURAL ELECTRIC COOP                       '  

      i = i + 1
      owners(i) =
     1 'SC     SOUTHERN CALIFORNIA EDISON COMPANY                   '  

      i = i + 1
      owners(i) =
     1 'SCE    SOUTHERN CALIFORNIA EDISON COMPANY                   '  

      i = i + 1
      owners(i) =
     1 'SCL    SEATTLE CITY LIGHT COMPANY                           '  

      i = i + 1
      owners(i) =
     1 'SCP    SOUTHEAST COLORADO POWER ASSOCIATION                 '

      i = i + 1
      owners(i) =
     1 'SDG    SAN DIEGO GAS AND ELECTRIC COMPANY                   '  

      i = i + 1
      owners(i) =
     1 'SEC    SALEM ELECTRIC COOP.                                 '  

      i = i + 1
      owners(i) =
     1 'SJL    SAINT JOSEPH LIGHT AND POWER COMPANY                 '  

      i = i + 1
      owners(i) =
     1 'SMD    SACRAMENTO MUNICIPAL UTILITIES DISTRICT              '  

      i = i + 1
      owners(i) =
     1 'SPA    SOUTHWESTERN POWER ADMIN.                            '  

      i = i + 1
      owners(i) =
     1 'SPC    SASKATCHAWAN POWER COMPANY                           '  

      i = i + 1
      owners(i) =
     1 'SPD    SNOHOMISH COUNTY PUBLIC UTILITIES DISTRICT           '  

      i = i + 1
      owners(i) =
     1 'SPP    SIERRA PACIFIC POWER COMPANY                         '  

      i = i + 1
      owners(i) =
     1 'SPS    SOUTHWESTERN PUBLIC SERVICE COMPANY                  '  

      i = i + 1
      owners(i) =
     1 'SRE    SALMON RIVER ELECTRIC                                '  

      i = i + 1
      owners(i) =
     1 'SRP    SALT RIVER POWER DISTRICT                            '  

      i = i + 1
      owners(i) =
     1 'SUB    SPRINGFIELD UTILITY BOARD                            '

      i = i + 1
      owners(i) =
     1 'SVE    SURPRISE VALLEY                                      '

      i = i + 1
      owners(i) =
     1 'SWP    SOUTHWESTERN POWER ADMINISTRATION                    '  

      i = i + 1
      owners(i) =
     1 'SWR    SOUTHWEST REGION - USBR                              '  

      i = i + 1
      owners(i) =
     1 'TCE    TRI-COUNTY ELECTRIC ASSOCIATION, INC. (WYOMING)      '

      i = i + 1
      owners(i) =
     1 'TCL    TACOMA CITY LIGHT COMPANY                            '  

      i = i + 1
      owners(i) =
     1 'TEP    TUSCON ELECTRIC POWER COMPANY                        '  

      i = i + 1
      owners(i) =
     1 'TES    TEXAS ELECTRIC SERVICE COMPANY                       '  

      i = i + 1
      owners(i) =
     1 'TGE    TUCSON GAS AND ELECTRIC COMPANY                      '  

      i = i + 1
      owners(i) =
     1 'TIL    TILLAMOOK PUD                                        '  

      i = i + 1
      owners(i) =
     1 'TPL    TEXAS POWER AND LIGHT COMPANY                        '  

      i = i + 1
      owners(i) =
     1 'TRI    TRI-STATE GENERATION AND TRANSMISSION ASSOC.         '

      i = i + 1
      owners(i) =
     1 'TSG    TRI-STATE GENERATION AND TRANSMISSION ASSOC.         '  

      i = i + 1
      owners(i) =
     1 'TVA    TENNESEE VALLEY AUTHORITY                            '  

      i = i + 1
      owners(i) =
     1 'UEC    UNION ELECTRIC COMPANY (IOWA,MISSOURI,ILLINOIS)      '

      i = i + 1
      owners(i) =
     1 'UMT    UMATILLA COOP.                                       '

      i = i + 1
      owners(i) =
     1 'UPA    UNITED POWER ASSOCIATION, INC. (NORTH DAAKOTA)       '

      i = i + 1
      owners(i) =
     1 'UPL    UTAH POWER AND LIGHT COMPANY                         '  

      i = i + 1
      owners(i) =
     1 'USN    U.S.NAVY                                             '  

      i = i + 1
      owners(i) =
     1 'VID    VERA IRRIGATION DISTRICT                             '  

      i = i + 1
      owners(i) =
     1 'WAP    WESTERN AREA POWER ADMINISTRATION-BILLINGS AREA      '  

      i = i + 1
      owners(i) =
     1 'WEC    WASCO ELECTRIC COOP.                                 '  

      i = i + 1
      owners(i) =
     1 'WEP    WISCONSIN ELECTRIC POWER COMPANY                     '  

      i = i + 1
      owners(i) =
     1 'WIS    WISCONSIN PUBLIC SERVICE CORP.                       '  

      i = i + 1
      owners(i) =
     1 'WKP    WEST KOOTENAY POWER AND LIGHT COMPANY, LTD.          '

      i = i + 1
      owners(i) =
     1 'WMP    WISCONSIN MICHIGAN POWER COMPANY                     '  

      i = i + 1
      owners(i) =
     1 'WPD    WHATCOM COUNTY PUD                                   '  

      i = i + 1
      owners(i) =
     1 'WPS    WASHINGTON PUBLIC POWER SUPPLY SYSTEM                '  

      i = i + 1
      owners(i) =
     1 'WRE    WELLS RURAL ELECTRIC CO.                             '  

      i = i + 1
      owners(i) =
     1 'WRP    WISCONSIN RIVER POWER COMPANY                        '  

      i = i + 1
      owners(i) =
     1 'WST    WESTERN POWER AND GAS COMPANY  (COLORADO)            '  

      i = i + 1
      owners(i) =
     1 'WWP    WASHINGTON WATER POWER COMPANY                       '  

      i = i + 1
      owners(i) =
     1 'YWE    YUMA WRAY ELEC.ASSN., INC.                           '

      i = i + 1
      do j = i, MAX_OWNERS
         owners(j) = ' '
      enddo

      return
      end 
