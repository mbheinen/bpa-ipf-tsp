C    @(#)pfinit.f	20.6 11/12/98
        subroutine pfinit
 
C       INITIALIZE POWERFLOW JOB VARIBLES
C       ( done only on job initialization )
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/basval.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/epridc.inc'
      include 'ipfinc/filnam.inc'
      include 'ipfinc/jobctl.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/mrgtxt.inc'
      include 'ipfinc/optim.inc'
      include 'ipfinc/pageno.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/topbot.inc'
      include 'ipfinc/usranl.inc'
      include 'ipfinc/zonlst.inc'
      include 'ipfinc/wsccbase.inc'
      include 'ipfinc/miscfile.inc'
      include 'ipfinc/lndpcp.inc'
c
c*** now done with initialization routine
C
C       Force linkage of   Block   Data   modules
C
        call init_bd_all
 
C       Logical file variables

        logmbs = 10
        logmbr = 11
        wscfil = 12
        wscc_aname = ' '
        wscc_bname = ' '
c
c       Caution: the following logical assignment must be coordinated
c       with initlz.f
c
        inp = 13
        lprt = 14
        mfich = 15
        datai = 16
        obasnm = ' '
        datao = 17
        nbasnm = ' '
        brndta  = 18
        brdnam  = ' '   
        dbug  = 19 

        busbrn = 20 
        bsbrnm  = ' '   
        busfil = 21 
        bsdnam = ' '
        chgdta = 22 
        chgnam = ' '
        lun = 23            ! Used in analys for user_analysis
        svchfl = 24
        lunusr = 25         ! Used in getusr for user_analysis
        lunscr = 27         ! Used in solton for best solution
        lunscr1 = 28        ! Used as a scratch file within a module
        lunscr2 = 29        ! Used as a scratch file within a module
        newchgfil = ' '
        newnetfil = ' '
c
c       lun = 30            ! Reserved for scratch 
c       lun = 40            ! Reserved for plotting
c       lun = 41            ! Reserved for plotting
c       lun = 42            ! Reserved for plotting
        arcvfile = 51
c
c       luns 56-65          ! reserved for command files
c
c       Print file switches - 1 = on, 0 = off

        lprtsw = 1  
        fichsw = 0  
        crtsw = 1   
        if( batch ) crtsw = 0   

c       Set counters

        nocase = 0  
        nobus = 0   

c       Set pagination parameters..  

        pageno = -1 
        lineno = 0  
        maxlin = 59 
        fichpg = -1 
        fichln = 0  
        fichmx = 59 
        lstdnl = 0  
        lstdnf = 0  

c       Set up job variables

        bmva = 100.0
        sang = 0.0  
        ksyuu = 0   
        yptr = 0
        nbsmrg=0
        nbrmrg=0
        nifmrg=0
        ntot_alf = 0
        ntot = 0
        ntot2 = 0   
        ltot = 0
        ltot2 = 0
        ntota = 0   
        ntotb = 0   
        ntotc = 0   
        numldc = 0
        wsccflag = .false.
        do i = 1, MAXBUS
          wsccbase(i) = ' '
        enddo
c
c       Initialize EPRI RP1964 counters
c
        nepbus = 0
        nepctl = 0
        mepbus = 0
        mepctl = 0
        ntotcs = 0  
        jtie = 0
        nshift = 0  
        jphno = 0   
        kdtot = 0   
        kxtot = 0   
        nztot = 0   
        mtdcbs = 0  
        mtdcln = 0  
        nbslck = 0  
        kbsknt = 0  
        kbrknt = 0  
        natot = 0   
        nfzdta = 0  
        npzdta = 0  
        nfzout = 0  
        npzout = 0  
        nfzanl = 0  
        npzanl = 0  
        nfoanl = 0  
        npoanl = 0  
        leffan = 0  
        txefan = 0  
        lncnt1 = 0  
        lncnt2 = 0  
        lncnt3 = 0  
        lncnt4 = 0  
        lncnt5 = 0  
        lncnt6 = 0  
        do i = 1,40  
           if (i .ne. 33) kspare(i)= 0 
           cspare(i)= ' '   
        enddo
        numlrt = 0  

C       Don't save the Fiche file unless asked for...   
C       with a [ FICHE,COPIES=n ] record... 

        kspare(16) = -1 
        kchgsw = 1  
        call xdate(dte) 
        cspare(31) = dte
c
c       Initialize BASVAL() values
c
        do i = 1, 9
           basval(i) = ' '
        enddo
        basval(5) = dte
        basval(8) = prgvsn

        return  
        end 

