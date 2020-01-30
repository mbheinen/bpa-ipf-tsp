C    @(#)probeg.f	20.3 2/13/96
      subroutine probeg
 
C *** PROCESS-BEGIN                                                    *
C ***     (1)                                                          *
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/addata.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/changr.inc'
      include 'ipfinc/coment.inc'
      include 'ipfinc/dtaiop.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/errmsg.inc'
      include 'ipfinc/errorx.inc'
      include 'ipfinc/filnam.inc'
      include 'ipfinc/header.inc'
      include 'ipfinc/jobctl.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/mrgsys.inc'
      include 'ipfinc/mrgtxt.inc'
      include 'ipfinc/optim.inc'
      include 'ipfinc/pctger.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/tempbsnm.inc'
      include 'ipfinc/timmsg.inc'
      include 'ipfinc/zonlst.inc'


      dimension icount(8)
      equivalence (icount,nfzdta )
 
C *** RESET-CASE-OPTIONS-TO-STANDARD-DEFAULTS ONLY FOR (POWERFLOW)     *
 
      if ( jobreq(1) .ne. 'NEXTCASE' ) call restop
 
C *** RESeT OPtions at the begining of eachPowerfloworNextcase         *
C *** case to the standard DEFAULT VALUES..                            *
 
      obasnm = '                    '
      brdnam = obasnm
      bsdnam = obasnm
      chgnam = obasnm
      bsbrnm = obasnm
 
C *** BLANK OUT ERROR ARRAYS                                           *
 
      do 100 i = 1, 100
         errm(i) = ' '
  100 continue
      numerr = 0
      do 110 i = 1, 5
  110 errcnt(i) = 0
 
C *** ZERO OUT PRINT & FICHE ZONE COUNTERS                             *
 
      do 120 i = 1, 8
         icount(i) = 0
  120 continue
 
C *** BLANK THE REPORT NAME, SUB-HEADING, HEADER COMMENTS..            *
 
      outbuf = ' '
      call rpnlod
      call shdlod(1)
      call shdlod(2)
      call shdlod(3)
      call shdlod(4)
      call shdlod(5)
      call comlod(1)
 
C *** CASE COMMENT COUNTER                                             *
 
      ncom = 0
 
C *** PRTIME MESSAGE COUNTER                                           *
 
      msgnum = 0
 
C *** NO OF % CHANGES                                                  *
      ipctno = 0
 
C *** PAPER INPUT LISTING OFF                                          *
      kspare(4) = 0
 
C *** PAPER OUTPUT LISTING OFF                                         *
      kspare(6) = 0
 
C        SET UP "FICHE ON" DEFAULTS
C            FICHE INPUT DATA LIST
         kspare(5) = 2
C            FICHE OUTPUT LISTING
         kspare(7) = 2
C            ANALYSIS LISTING LEVEL ON FICHE
         kspare(9) = 4
 
      knew = 2
      knum = 2
      kabort = 0
      minerr = 0
      lskp = 0
      koptsw = 0
      ntotcs = 0
      noch = 0
      nbsadd = 0
      nbradd = 0
      kdchg = 0
      nbsys1 = 0
      nbsys2 = 0
      nbsmrg = 0
      nbrmrg = 0
      nifmrg = 0
      lnct1 = 0
      lnct2 = 0
      lnct3 = 0
      lnct4 = 0
      lnct5 = 0
      lnct6 = 0
      ibuscb = 0
 
C *** FIND-FIRST-CONTROL-RECORD                                        *
 
      if (index('([',inrcd(1:1)) .ne. 0) go to 150
  130    read (inp,140,end=152) inrcd
  140    format (a)
  150 continue
      go to 154
  152 inrcd = '( END )'
  154 continue 

      ityp = index('([',inrcd(1:1))
 
      outbuf = ' ' / /inrcd
      if (ityp .eq. 0) then
         l = len(inrcd)
         l = min0(l,80)
         outbuf = ' ' / /inrcd(1:l) / /'***COMMENT***'
      endif
 
      call prtout(1)
      if (ityp .eq. 0) go to 130
 
      call ctlcom
 
      if ( kspare(16) .lt. 0 ) then
C               TURN OF ALL FICHE LISTS
C                    FICHE INPUT DATA LIST
         kspare(5) = 0
C                    FICHE OUTPUT LISTING
         kspare(7) = 0
C                    FICHE ANALYSIS LISTING LEVEL
         kspare(9) = 0
         fichsw = 0
      endif
 
      return
      end
