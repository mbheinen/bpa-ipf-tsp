C    %W% %G%
    	program sds
C
C	[SWING DATA SUMMARY]
C
C	Purpose:
C       --------
C       The Swing Data Summary Program (SDS) summarizes
C       machine MVA and machine MVA/PSS in service by areas. It
C       also summarizes the Inertia in MW-Seconds and the total
C       Generated Power and Pumping Load in MW's.
C
C	Title:	
C	------
C	SDS
C
C	Calling Sequence:
C	-----------------
C	The user must be prepared to supply the values for the
C	following variables:
C
C	PFNAME -- Powerflow history file. The program looks for the
C	          file in the users directory, BASECASE_DIR and
C		  WSCCBASE_DIR. A file type of .BSE is defaulted. 
C		  Any other file type will have to be specified.
C
C	SWNAME -- Swing data file. Program looks for the file in the
C		  users directory and WSCCBASE_DIR. A file type of 
C		  .SDI is defaulted. Any other file type will have to
C		  be specified.
C
C	The program is selected from the Standard Power System Analysis
C	Program (PSAP) menu by entering
C
C	   PSAP 15 (return) 
C	   on the terminal keyboard and making appropriate
C	   selection from the displayed menu.
C
C	Alternatively, the program may be selected by entering
C
C	   RUN/NODEBUG SDS_DIR:SDS (return)
C	   on the terminal keyboard.
C
C	Called By:
C       ----------
C	PF_DIR:PSAP.COM
C
C	Calls To:
C	---------
C	RDDTAI (Case,Loaded)   To open and load base case
C	READEC (PNET,KK(1,KT),12) To get PNET value for bus KT.
C
C	Variable & Parameter Description
C       --------------------------------
C	Name		Common Block	Description
C	----		------------    -----------
C	BMVA		BLANK		Base MVA used for the case.
C					Default = 100.0 MVA
C
C	BSBASE				KV of swing bus (KB)
C
C	BSNAME				Name of swing bus (KB)
C
C	CASE				Powerflow case called by RDDTAI
C					(Defaulted to Blank)
C
C	DATAI		LFILES		Logical unit for Data input in
C					saved powerflow cases
C
C	DATAO		LFILES		Data output for saved powerflow cases
C
C	DATE				Today's Date
C
C	DESC1				Old basecase description (word 1)
C
C	DESC2				Old basecase description (word 2)
C
C	Emws				Inertia (MW)
C
C	GMVAR0				MVA of 'M ' records
C
C	GMVAR1				MVA of MC,MF,MS,MI records
C
C	IFM				Logical variable (If 'M ' card) .true.
C					means 'M ' present
C
C	JUNK				
C
C	KA				Area index number for each bus
C
C	KB				External Number of bus
C
C	KK				Pointer to PNET for bus (*) stored
C					in /ECS/
C
C	KT				Internal number of bus
C
C	LOADED				Called from RDDTAI. A return code that
C					signifies the success or failure of
C					RDDTAI. If loaded = 1 you have good 
C					PF case.
C
C	INP2OPT				External-to-internal order array
C
C	NTOTC		BLANK		Number of control areas for area 
C					interchange
C
C	OUTFIL				Output file name of run, eg.'SWNAME.SDS
C
C	PFNAME				Oldbase File Name
C
C	PGEN				Bus generation in MW
C
C	PLOAD				Bus load in MW.
C
C	PNET				Bus net injection (generation - load)
C
C	RECORD				Character variable to transfer data
C					one file to another
C
C	SWNAME				Name of swing .SDI file
C
C	PSSTOT				Total MVA with PSS
C
C	MVATOT				Total MVA
C
C	EMWSTOT				Total Inertia (MWS)
C
C	GENTOT				Total generation (MW)	
C
C	PUMPTOT				Total Pumped Storage load (MW)
C
C	YES				Logical true or false. .true. means
C					oldbase	or swing file name opened
C
C	Written by:
C	-----------
C	Laura G. Larson
C
C	Documented by:
C	--------------
C	Laura G. Larson
C
C*******************************************
C	Include all necessary common blocks & type declaration
C
	include 'ipfinc/parametr.inc'	
	include 'ipfinc/lfiles.inc'
	include 'ipfinc/blank.inc'
	include 'ipfinc/dtaiop.inc'

        parameter (MAXKV     = MAXBUS*2/100) !
	include 'tspinc/pointr.inc'
	include 'ipfinc/area.inc'
	include 'ipfinc/arcntl.inc'
	include 'ipfinc/alpha.inc'
	include 'ipfinc/bus.inc'
C	
	common/machic/machic(3000)
	character*80 machic
C
	character*8 bsname

        common/swdata/swdata,junk,dat,remotf,relayf,serief,capcpf,
     1   loadf,reprf,areaf,busbas,shedf,machnf,jblnk,swcas,swdate,
     2   zones,loadn,nettf 
        character*10  swdata,junk,dat,remotf,relayf,serief,capcpf,
     1   loadf,reprf,areaf,busbas,shedf,machnf,jblnk,swcas,swdate,
     2   zones,loadn,nettf 

	character*60 outfil
	character*60 pfname,swname
	character*132 record
C
	dimension totmva(5,60)
	integer find_bus
	real mvatot
	logical ifm

	call pfinit()

        dbug = 9
        lprt = 6
C
C	                                 Get Powerflow Filename
C
        pfname = ' '
10      call getpfn(pfname)
C
C                                        Open PF File
C	
        call openpf(pfname,*10)
C
C	                                 Get Swing Filename
C
135     call getswn(swname)
C
C                                        Open SW File
C
	open (unit=4,file=swname,status='OLD',readonly,form='UNFORMATTED',
     &  shared,err=135)
C
C                                        Get Swing Data
C
        call gtswdta

	do 300 i=1,60
	do 300 j=1,3
300	totmva(j,i) = 0
C	
C	                                 Initialize Summary Totals
C
	pumptot = 0
	gentot = 0
	emwstot = 0
	mvatot = 0
	psstot = 0

	ifm = .false.

C **********************************************************************
C	Code block jay_1 moved to here by Jay Coleman  10DEC91
C **********************************************************************

C
C                                  Set up output file 'SWNAME".SDS and open it. 
C

	if (swname .ne. ' ') then
	   j1 = index(swname,']')
	   if (j1.eq.0) j1 = index(swname,':')
	   j = index(swname(j1+1:),'.')

	   if (j.eq.0) then
	      k = index(swname(j1+1:),' ')

	      if (k.eq.0) then
	         k = len (swname(j1+1:))+1
	      endif

	      outfil = swname(j1+1:j1+k-1) // '.SDS'
	   else
	      outfil = swname(j1+1:j1+j-1) // '.SDS'
	   endif

	endif

C **********************************************************************
C	End code block Jay_1
C **********************************************************************


	open(unit=1,type='NEW',name=outfil,carriagecontrol='FORTRAN')
	open(unit=2,type='SCRATCH')
        write (1,425)
425     format(' LIST OF BUSES THAT HAVE DIFFERENT MVA VALUES ')	
	write (1,427)
427     format(' --------------------------------------------'/)

C **********************************************************************
C	Code block jay_2 moved to here by Jay Coleman  10DEC91
C **********************************************************************

	write (2,305)
305	format(// ' LIST OF BUSES THAT ARE NOT IN THE POWERFLOW ')
	write (2,307)
307	format(   ' -------------------------------------------'/)

C **********************************************************************
C	End code block Jay_2
C **********************************************************************

        do 500 i = 1,mac 
C
C                                        Search for M,MI,MC,MS, or MF
C
	if (index('M $MC$MF$MS$MI',machic(i)(1:2)).ne.0) then

	   if (machic(i)(1:2) .eq. 'M ') then
              read (machic(i),310 )gmvar0
310	      format(t17,f5.1)
	      ifm = .true.

	      if (gmvar0 .eq. 0) then   ! If MVA of 'M ' is zero use default
	         gmvar0 = 100           ! of 100 MVA
              endif

              go to 500
	   else
	      read (machic(i),315 )bsname,bsbase,emws,gmvar1
315           format(3x,a8,f4.0,1x,f6.0,t29,f4.0)
	   endif

	   if (gmvar1.eq.0) then   ! If MVA of MS,MF,MC,MI is zero use default
	      gmvar1 = 100         ! of 100 MVA
	   endif

C
C	                                 Find Bus Number
C
	   kb = find_bus(bsname,bsbase)

	   if (kb.le.0) then
	      write (2,400) bsname,bsbase
	      write (*,400) bsname,bsbase
400	      format (' The SDI bus ',a8,f6.1,' is not in the powerflow.'/)
	   else

	      ka = jarzn(kb)	
C
C	      If there is more than 10% difference in the MVA value between
C	      the 'M ' record and the MC,MF,MS or MI record print out the 
C	      Bus, Area	and MVA values in the output report. 

	      if (ifm .and. (abs(gmvar1-gmvar0)/gmvar1.gt. 0.10)) then
	         write (1,450)bsname,bsbase,arcnam(ka),gmvar0,gmvar1
450		 format(' THE MVA FOR ',a8,f6.1,' AREA ',a10,
     &          ' HAS TWO DIFFERENT VALUES ',f7.1,5x,f7.1/)
	      endif
C
C	      If the MVA = 100 in the MC,MF,MI,MS record use the MVA
C	      from the 'M ' record for the area totals.
C	
	      if (ifm) then                               
	         if (gmvar1 .eq. 100) gmvar1 = gmvar0
	      endif


	      ifm = .false.
	      totmva(2,ka) = totmva(2,ka) + gmvar1
              mvatot = mvatot + gmvar1             ! Total MVA
	      totmva(3,ka) = totmva(3,ka) + emws
	      emwstot = emwstot + emws             ! Total Inertia in MWS
	      kt = inp2opt(kb)
C
C	                             GET (PNET) OF BUS DATA FOR BUS KT	
C
	      pgen = (pnetu(kt) + ploadu(kt)) * bmva

	      if (pgen .gt. 0) then
	         totmva(4,ka) = totmva(4,ka) + pgen
	         gentot = gentot + pgen            ! Total generation in MW
 	      else
	         totmva(5,ka) = totmva(5,ka) + pgen
	         pumptot = pumptot + pgen          ! Total Pumping Load in MW
	      endif		
           endif
	else if (index('SS$SP$SF',machic(i)(1:2)).ne.0) then
	   totmva(1,ka) = totmva(1,ka) + gmvar1
	   psstot = psstot + gmvar1                ! Total MVA with PSS
	endif


C **********************************************************************
C	Code block jay_1 used to be here, moved by Jay Coleman  10DEC91
C **********************************************************************

C **********************************************************************
C	Code block jay_2 used to be here, moved by Jay Coleman  10DEC91
C **********************************************************************

C **********************************************************************
C			This line commented out by Jay Coleman  10DEC91
C	DO 500 K=1,MAC
C **********************************************************************

500	continue
C
C                                         Write output file 'SWNAME.SDS'
C
	rewind 2
502     read (2,503,end=504) record
	write (1,503) record
503     format (a)
	goto 502 
504	continue

	write (1,510)
510	format('1', 50x,'SWING DATA SUMMARY'/)
	write (1,520)swname
520	format ( 35x,' FOR ',a25,' STABILITY DATA ')
	write (1,530)
530	format ( 35x,'-----------------------------------------------'/)
	write (1,540)
540	format ( 5x,'AREA',14x,'MVA WITH PSS',15x,'MVA',t70,'
     &  emws ',T81,'generation (mw)',T99,'pumping load (mw)')
	write (1,550)
550	format ( 5x,'----',14x,'------------',15x,'---',t70,'
     &  ---- ',T81,'---------------',T99,'-----------------')	


	do 650 i = 1,ntotc
	write (1,610) arcnam(i),totmva(1,i),totmva(2,i),totmva(3,i),
     &  totmva(4,i),totmva(5,i)
610	format (5x,a10,10x,f10.1,10x,f10.1,10x,f10.1,10x,f10.1,10x,f10.1/)
650	continue

	write (1,660)
660	format (25x,'----------',10x,'----------',10x,'----------',10x,'
     &  ----------',10x,'----------')
	write (1,670)psstot,mvatot,emwstot,gentot,pumptot
670     format(10x,'TOTAL',10x,f10.1,10x,f10.1,10x,f10.1,10x,f10.1,10x,
     &  f10.1)

	write (*,700)swname,outfil
700	format ('0YOUR OUTPUT FILE NAME FOR CASE ',a25,' IS: ',a)

        end
