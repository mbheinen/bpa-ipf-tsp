C    %W% %G%
        subroutine swingm 

C                   Bonneville Power Administration
C                         Portland, Oregon
  
C                       NOTICE TO NON-BPA USERS
C                       -----------------------
  
C       Bonneville Power Administration (BPA) releases
C       BPA-developed computer programs under the following
C       conditions:
  
C       1.  BPA does not charge for program development costs;
C           however a fee to cover costs incurred in answering
C           the request is assessed against the organization
C           receiving the material. This fee typically includes
C           costs for manpower, computer time, reproduction,
C           packaging, and mailing.
  
C       2.  BPA cannot provide assistance with conversion to
C           other computers, and cannot provide consulting
C           service for the program users.
  
C       3.  In consideration of receipt and acceptance of
C           the programs and related documentation, you
C           and your organization agree to advise any
C           third-party recipients in writing that the
C           programs and/or documentation are in the public
C           domain and available from BPA in the event that
C           these programs and documentation or portions
C           thereof are sold, assigned, or transferred to
C           other organizations. The intent of this agreement
C           is to assure that BPA-developed or BPA-supplied
C           programs and documentation, whether in whole or
C           in part, which are in the public domain are
C           identified as such to the recipients.
  
C                       "LEGAL NOTICE"
C                        ------------
  
C       Neither BPA nor any person acting on behalf of BPA:
C 
C       1.  makes any warranty or representation, expressed
C           or implied, with respect to the accuracy,
C           completeness, or usefulness of the programs or
C           related documentation; or
C 
C       2. assumes any liabilities with respect to the use
C          of these materials or for damages resulting from
C          their use.

c     Revisions:
c       May/20/92 - DEM:
c         Added feature to have free-format input in portions of the
c           control file (L5).
c       Jun/11/92 - DEM:
c         Added command line argument for a file containing list of all
c           other permanent i/o files.
c         Closed files via CLOSTS using file num variables instead
c           of constant nums
c         For history file (L8), changed writing of e-o-f marker to
c           once at end of simulation instead of at end of every
c           time step.

      include 'tspinc/params.inc'
      include 'tspinc/blkcom1.inc'
      include 'tspinc/titles.inc'                                              
      include 'tspinc/files.inc'                                               
      include 'tspinc/prt.inc'
      include 'tspinc/pageno.inc'
      include 'tspinc/topbot.inc'
      include 'tspinc/reread.inc'
      common /linksw/ lnksw1
      include 'tspinc/fltopt.inc'
      include 'tspinc/newton.inc'
      include 'tspinc/vfhistory.inc'
      common /xtimex/ time1(10),time2(10)

c     Local variables

      character case*8, db*8, aux*8, au*8, plott*8, pcas*10, job*30,
     &          text*80, symbol*40, tempfile*60
      integer status, proc_sdf, keys(36)
      logical go_on, qa, debug, dbgfind, dbghere
c dlc
      integer rd_rtn, mv_rpt, mv_sol

c     Begin     Begin     Begin     Begin     Begin     Begin
c     Force BLOCK DATA unit to be linked in
      external stabdat                                                  

      save

      data job /'                              '/
      data tempfile / ' ' /
      data mv_sol / 1 /
      data mv_rpt / 2 / 

      rd_rtn = 0
c
c     Initialize Special Machine Data File
c
      numgen = 0
      numtime = 0
      mxxbus = 0               ! VFHIST counter
C      
C     Initialize logical units according to VAX or Unix
c
      if (is_it_vms() .ne. 0) then
         l1 = 1
         l2 = 2
         l3 = 3
         l4 = 4
         l5 = 5
         l6 = 6
         l7 = 7
         l8 = 8
         l9 = 9
         l10 = 10
         l11 = 11
         l12 = 12
         l13 = 13
         l15 = 15
         l22 = 22
         l23 = 23
      else
         l1 = 31
         l2 = 32
         l3 = 33
         l4 = 34
         l5 = 25
         l6 = 26
         l7 = 7
         l8 = 8
         l9 = 9
         l10 = 10
         l11 = 11
         l12 = 12
         l13 = 13
         l15 = 15
         l22 = 22
         l23 = 23
      endif

c     Open file list file & get file names
      call getfnam                                                      

c     Determine if debugging of subs will be done  

      qa = dbgfind (dbgifl,dbgofl)                                      
      if (qa) write (*,'(a)') ' SWINGM - will be debugging'             
      debug = dbghere ('SWINGM  ')                                        

C     Debug section

      if (debug) then                                                   
        call dbgeko ('SWINGM - after fetching file names')              
        call dbgwrc ('  CTRLFL = ',ctrlfl)                              
        call dbgwrc ('  BSEFL =  ',bsefl)                               
        call dbgwrc ('  SAVIFL = ',savifl)                              
        call dbgwrc ('  SOLFL = ',solfl)                                
        call dbgwrc ('  PRTFL = ',prtfl)                                
        call dbgwrc ('  PLTFL = ',pltfl)                                
        call dbgwrc ('  AUXFL = ',auxfl)                                
        call dbgwrc ('  SAVOFL = ',savofl)                              
      endif                                                             
C      
C     Open swing data input file (*.SWI) file (L5)
C      
      call opents(1)

c     Open printout file by name

      lprt = l6                                                         
      call opents(7)                                                    
 183  continue
      ctlfmt = 'OLD'                                                    
 184  call target                                                       
      rewind (l5)                                                       
C 
C     Set pagination logical file units and switches
C 
      kpltsw = 0
      mfich = 55
      dbug  = 0
      lprtsw = 1
      fichsw = 0
      crtsw = 0
      kef = 0                                                           
      swcas = ' '
      scase = ' '

c     In future SWCASE will be only storage of case ID

      swcase = 'UNTITLED'

c     Establish case ID's for Stab, PF, SDI-IN & -OUT
C     Derive case ID's from file names

      call getids                                                       

C     Debug section

      if (debug) then                                                   
        call dbgeko ('SWINGM - after fetching case IDs:')               
        call dbgwrc ('  SWCASE = ',swcase)                              
        call dbgwrc ('  PFCASE = ',pfcase)                              
        call dbgwrc ('  SAVIN =  ',savin)                               
        call dbgwrc ('  SAVOUT = ',savout)                              
      endif                                                             
C 
C     Initialization pagination parameters..
C 
      outbuf = ' '
      header = ' '
      repnam = ' '
      do 100 i = 1,10
        errbuf(i ) = ' '
100   continue
      do 110 i = 1,5
110   call shdlod(i)
      write (outbuf,120)
120   format ('    * * * INPUT AND INITIALIZATION * * *         ')
      call rpnlod
      coment(1) = ' '
      coment(2) = ' '
      pageno = -1
      lineno = 0
      maxlin = 59
      fichpg = -1
      fichln = 0
      fichmx = 59
      lstdnl = 0
      lstdnf = 0
      call date(rdate)

C     Temporary addition to get fiche ID on listing.  (OK, no data
C     read from control file yet.)

      read (55,130,err=140,end=140) buffer
130   format (a80)
140   continue
C      
C     Initialize cpu timers
C      
      call xtime(0)
      call stime('START')

      lnksw1=1
      nend8=0
      lcomp = 2
      ifltsw = 2
      inewts = 1
      link=3
      call mpost('SWINGM')
C      
C     WRTVER writes program version to output file
C      
      call wrtver
C      
C     Obtain swing identification SWCASE from name of .SWI file
C      
      job(1:19) = 'START SWING '//ver
      job(21:30) = swcase                                               
      call prgmon(job,0)

C     Do a preread of control file, since two cards saved in
C     BUFFER/BUFNXT string pair.

      call readin
      go to 260
C      
C     LINK directs processing:
C
C       1 = Solution
C       2 = Y-matrix modification and reduction
C       3 = Input
C       4 = Initializaton
C       5 = Output
C       6 = Plotting
C      
  220 go to (420,440,240,400,560,600,225),link
  225 link = 1
      go to 220

C     Here if LINK == 3

  240 continue
  260 call readin
  265 read (buffer,280) aux
  280 format (a5)
C      
C     Read solution method switches from SOL card
C      
      if (aux .eq. 'SOL') then
         read (buffer,285) lcomp, ifltsw, inewts
 285     format (bz, 5x, 3(i1,2x))
         if (lcomp .eq. 0) lcomp = 2
         if (ifltsw .eq. 0) ifltsw = 2
         if (inewts .eq. 0) inewts = 1
         go to 260
      endif
      if (aux .eq. 'DEBUG') then
C      
C        Initialize debug switches
C      
         read (buffer,320) keys
  320    format (bz,6x,36i1)
         write (outbuf,340) buffer
         call prtout (1)
  340    format ('0',a)
         call krddbg (keys)
         go to 260
      endif

C     FICHE card used only by SWGINIT proc file builder

      if (aux .eq. 'FICHE') go to 260

      if (aux .eq. 'CASE') go to 360

C     Check if plotting with no prior simulation

      if (debug) then                                                   
        call dbgeko ('SWINGM - before sn345: Testing for "90" card.')   
        call dbgwrc ('  AUX /card type/ = ', aux)                       
      endif                                                             
      if (aux .eq. '90') go to 480

  345 write (errbuf(1),350)
  350 format ('0 Data card discrepancy. The following card is ignored.')
      write (errbuf(2),355) buffer
  355 format('0', 5x, a80)
      call prterr ('W', 2)
      go to 260
C      
C     Start of input and initialization
C 
C     Write identifier to FICHE
C 
c     Here if just found CASE card
c     Must modify later to show indiv field instead of whole CASE
c     card

  360 write (outbuf,380) buffer
  380 format('0', a)
      call prtout(1)
      write (outbuf,381)
  381 format(' ---- ----------    -    --------- --------')
      call prtout (1)
C      
C     Open solution history file L8 = 8 (.SOL file)
C     Open power flow data file L3 = 3 (.BSE file)
C     Open scratch files needed in INPUT and INITIALIZATION
C      
      call opents(2)
      write(outbuf,10420) swcase,pfcase                                 
10420 format('SWING CASE: ',a,' POWERFLOW CASE: ',a)
      call hedlod
      if (lcomp .eq. 1) then
         write(outbuf,390)
 390     format('0',5x,'LCOMP  = 1--compensated lines are not',
     1   ' relocated to the bottom of the matrix.')
         call prtout(1)
      else
         write(outbuf,391)
 391     format('0',5x,'LCOMP  = 2--compensated lines are relocated to',
     1   ' the bottom of the matrix.')
         call prtout(1)
      endif
      if (ifltsw .eq. 1) then
         write(outbuf,392)
 392     format('0',5x,'IFLTSW = 1--faulted bus is  relocated to',
     1   ' the bottom of the matrix.')
         call prtout(1)
      else
         write(outbuf,393)
 393      format('0',5x,'IFLTSW = 2--faulted bus is not relocated to',
     1   ' the bottom of the matrix')
         call prtout(1)
      endif
      if (inewts .eq. 1) then
         write(outbuf,394)
 394     format('0',5x,'INEWTS = 1--network solution uses the',
     1   ' Y MATRIX.')
         call prtout(1)
      else
         write(outbuf,395)
 395     format('0',5x,'INEWTS = 2--network solution uses ',
     1   ' NEWTONS method.')
         call prtout(1)
      endif
C      
C     Start of INPUT
C      
      call xtime(1)
c dlc
      call rdbse(rd_rtn, mv_sol)
      call tapewk

      call input1
      call input2
      call input3

      call xtime(-1)
      go to 220
C      
C     Start of initialization
C      
c     Here if LINK == 4

 400  call xtime(2)
      call initl1
      call initl2
      call initl3
      call initl4
      call xtime(-2)
C      
C     Close files no longer needed
C
c     dlc file not opened and bombs unix !      
c     call closts (l1,1)                                              
      call closts (l2,1)                                              
      call closts (l3,0)                                              
      call stime('INITIALIZATION')
      call forbtm
      write (outbuf,410)
  410 format ('          * * *  SOLUTION  * * *                 ')
      call rpnlod
      call fortop
      
      go to 220
C      
C     Start of solution portion of the program.  
C     If LNKSW1 = 1, then subroutine SOLN must be called to obtain 
C     initial matrix reduction and obtain initial network solution as
c     well as initializing program constants.  
C     If lnksw1 = 2, then SOLN is finished and the program starts 
C     the solution by calling subroutine cntrl
C      
c     Here if LINK == 1
 420  if (lnksw1 .eq. 2) then
        call xtime(3)

c       CNTRL is called at the + side of t=0 and the + side of all
c       disturbances.

        call cntrl
        call xtime(-3)
      else
        call soln

c       Set flag to indicate first write to history file

        kef = 1                                                         
      endif

c     Look for a card following the 'FF' card.

      if (link .eq. 3) then
        call readin
 450    format (a80)
        if (debug) then                                                 
          call dbgeko ('SWINGM - before sn440: Testing for "90" card.') 
          call dbgwrc ('  BUFFER[1:5] /card type/ = ', buffer(1:5))     
        endif                                                           
        aux = buffer(1:2)                                               
        if (aux .eq. '90') goto 480                                     
        go to 265                                                       
      endif
      go to 220
C      
C     Y-matrix modification and/or reduction
C      
c     Here if LINK == 2

440   call xtime(5)
      call reduce
      call xtime(-5)
      go to 420
C      
C     Start of output and plotting portion of the program
C      
  480 continue
      link=5
      if (kef .eq. 1) then                                              
        call closts (l8,0)                                              
      endif
      read (buffer, 490) tempfile
  490 format (bz, t10, a)

c     Here if LINK == 5

  560 call forbtm
      write (outbuf,580)
  580 format ('        * * *  OUTPUT LISTING  * * *             ')
      call rpnlod
      call fortop
C      
C     Open files L8, L3, and L11 for output subroutine
C      
      call opents(5)
      call xtime(8)
      call rdbse(rd_rtn,mv_rpt)
c
c     Process Optional Special Data File after Powerflow base case 
c     loaded
c
      if (tempfile .ne. ' ') then
        status = proc_sdf (tempfile)
      endif

      call nout1
      call nout2
      call closts (l3,0)                                                
      if (link.ne.6) go to 620
C      
C     Call plotting routine
C      
c     Here if LINK == 6

c     Must connect to a unit number

 600  if (kpltsw .eq. 0) then                                           
         call vtrinit (22,' ')                                          
         call plots(1,0,0)                                         
      endif                                                             
      kpltsw = 1
      call calplt
C      
C     End of job - close files and exit
C      
620   call xtime(-8)
      job(1:19) = 'FINIS SWING '//ver
      job(21:30) = swcase                                             
      call prgmon(job,0)
      call stime('COMPLETE RUN')
C      
C     Output cpu times
C      
      write(outbuf,700)
 700  format('0',7x,'INPUT',3x,'INIT',4x,'CNTRL',3x,'DERIV',3x,'REDUC'
     1  ,3x,'DWNBK',3x,'DC',6x,'OUT')
      call prtout(1)
      write(outbuf,710)( time1(itrr),itrr= 1,8)
 710  format('0',5x,8(f7.1,1x))
      call prtout(1)

      call closts (l2,1)                                              
      call closts (l8,0)                                              
      call closts (l9,0)                                              
      call proerr
      call closts( mfich,0)
      return
      end
