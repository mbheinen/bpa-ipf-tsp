C    %W% %G%
      subroutine dfnbse
C
C     define-base-system-inputs
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/filnam.inc'
      include 'ipfinc/jobctl.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/mrgtxt.inc'
      include 'ipfinc/prt.inc'
c
        logical baseok, reset_fatal_error
        integer error, crtsv, fichsv, status, flatstart
 
        nbsmrg = 0
        nbrmrg = 0
        nifmrg = 0
        reset_fatal_error = .false.
        flatstart = 1
        open (logmbs,status='scratch')
        open (logmbr,status='scratch')
        endfile logmbs
        endfile logmbr
        rewind logmbs
        rewind logmbr
C
C       MERGE_BASE
C         (0,1)
C
        if ( jobreq(1)(1:9) .eq . 'MERGEOLDB') then
C
C          Merge 1st old base
C
           call mrgbse (1)
           if (jobreq(1) .eq. 'QUIT') goto 900
           inrcd = buf
           call ctlpow
           buf = inrcd
C
C          MERGE 2ND BASE
C
           if (jobreq(1)(1:9) .eq. 'MERGEOLDB') then
              call mrgbse (2)
              inrcd = buf
              call ctlpow
              buf = inrcd
              if (jobreq(1) .eq. 'QUIT') goto 900
              call intfce
              call prtime('MERG_INTERFACE_1')
              card = buf(1:1)
              rewind logmbs
              call bsread
              call cmpltbus(error)
              call prtime('BUS_READ')
              call sortbus
              call sortcbus
              call sortpqcv
              call prtime('BUS_SORT')
              rewind logmbr
              call brread
              call prtime('BRANCH_READ_AND_SORT')
              call btable (flatstart, .false.)
              call prtime('BUILD_TABLE')

C             Block any data existing on merge scratch 
c             files which has been processed.  

              rewind logmbs 
              endfile logmbs
              rewind logmbs 
              rewind logmbr 
              endfile logmbr
              rewind logmbr 

           else if (jobreq(1)(1:9) .eq. 'MERGENEWB') then   
C       
C             Merging a bus/base file poses a subtle problem: The   
C             branch data file contains some interface candidates, i.e.,
C             branches with one missing terminal bus record. In order to
C             properly process these provisional branches, the first
C             subsystem is appended to the second but the interface 
C             branches from the first are omitted.  
C       
C             During the interface selection, the present interface bran
C             from the second subsystem are evaluated against interface
C             branches from the first and the best candidate is selected
C       
              read (inp,110,end=130) buf
  110         format (a)
              inrcd = buf
              call ctlpow
              buf = inrcd
              card = buf(1:1)
              kspare(1) = 0
              rewind logmbs
              call bsread
              call cmpltbus(error)
              call prtime ('BUS_READ')
              call sortbus
              call sortcbus
              call sortpqcv
              call prtime ('BUS_SORT')
              rewind logmbr
              call brread
              call prtime ('BRANCH_READ_AND_SORT')
              call btable (flatstart, .false.)
              call prtime ('BUILD_TABLE')
C
C             Block any data existing on merge scratch files which has
C             been processed.
C
              rewind logmbs
              endfile logmbs
              rewind logmbs 
              rewind logmbr 
              endfile logmbr
              rewind logmbr 
              if (nifmrg.gt.0) then 
                 rewind logmbr  
                 call mergi2
                 call prtime('MERG_INTERFACE_2')
                 rewind logmbr  
                 call brread
                 call prtime('BRANCH_READ_AND_SORT')
                 call btable (flatstart, .false.)
                 call prtime('BUILD_TABLE') 
C       
C                Block any data existing on merge scratch files which
C                has been processed.
C       
                 rewind logmbr  
                 endfile logmbr 
                 rewind logmbr  
              endif 
           else 
              write (errbuf(1),100) 
  100         format (' INCORRECT MERGE CONTROL SEQUENCE ') 
              call prterx ('E',1)   
           endif
           if (jobreq(1) .eq. 'QUIT') go to 900 
C       
C          All base data ready  
C       
           nifmrg = 0   
           nbsmrg = 0   
           nbrmrg = 0   
           close (logmbs)   
           close (logmbr)   
           kspare(1) = 2
           jobreq(2) = ' '  
           jobreq(3) = ' '  
           inrcd = buf  
           call ctlpow  
           buf = inrcd  
           if( index('/(H',buf(1:1)) .gt. 0 ) then  
              inrcd = buf   
              call ctlpow   
              buf = inrcd   
           endif
C       
           if (jobreq(1).eq.'QUIT') go to 900   
        endif   
C       
C       Print case summary  
C       
        lprtsv=lprtsw   
        fichsv=fichsw   
        crtsv=crtsw 
        
        lprtsw=1
        if(kspare(16).ge.0) fichsw=1
        crtsw=0 
        
        call sumopt 
        
        lprtsw=lprtsv   
        fichsw=fichsv   
        crtsw=crtsv 
C       
        if ( jobreq(2).eq.'OLD_BASE' ) then  
           call gtbase(obasnm,crun1(3),baseok)  
           if (.not.baseok) then
              jobreq(1) = 'QUIT'
              go to 900 
           else 
C       
C             Store Q_NET in CAPCOR(2,*)
C       
              do 120 nb = 1, ntot   
                 kt = inp2opt(nb) 
                 capcor(2,kt) = qnetu(kt)          
  120         continue  
        
              if (krun1(5).eq.0) then   
                jobreq(3) = ' ' 
                if (kspare(1).ne.1) kspare(1) = 2   
              else  
                jobreq(3) = 'BUILD_BASE'
                kspare(1) = 1   
              endif 
           endif
        endif
        if (jobreq(4) .eq. 'LOAD_PTI' ) then  
           status = load_pti(error)
           if (status .ne. 0) then
              jobreq(1) = 'QUIT'
              go to 900 
           endif
           call sortbus
           call sortcbus
           call srtbrnch
           kspare(1) = 1   
           call ctlpow
           buf = inrcd
           jobreq(4) = ' '  
           flatstart = 0
        else if (jobreq(4) .eq. 'LOAD_GE' ) then  
           status = load_ge(error)
           reset_fatal_error = .true.
           if (status .ne. 0) then
              jobreq(1) = 'QUIT'
              go to 900 
           endif
           call sortbus
           call sortcbus
           call srtbrnch
           kspare(1) = 1   
           call ctlpow
           buf = inrcd
           jobreq(4) = ' '  
           flatstart = 0
        endif
C       
C       EXAMINE CRITERIA TO BUILD_BASE  
C       
        if (jobreq(1).eq.'OUTAGE_SIM') kspare(1) = 2
        if (jobreq(1) .eq. 'REDUCTION') then
        else
           if (jobreq(3).ne.'BUILD_BASE') then  
              if (kspare(1).ne.2) jobreq(3) = 'BUILD_BASE'  
              if (brdnam.ne.' ') jobreq(3) = 'BUILD_BASE'   
              if (bsbrnm .ne.' ') jobreq(3) = 'BUILD_BASE'  
           endif
        endif
C
C       BUILD_BASE
C         (0,1)
C
        if (jobreq(3).eq.'BUILD_BASE') then
           card = buf(1:1)
           call bsread
           call cmpltbus(error)
           call prtime('BUS_READ')
           call sortbus
           call sortcbus
           call sortpqcv
           call prtime('BUS_SORT')
           rewind logmbr
           call brread  
           call prtime('BRANCH_READ_AND_SORT')  
           call btable (flatstart, reset_fatal_error)  
           call prtime('BUILD_TABLE')   
C       
C          All base data ready  
C       
           kspare(1) = 2
           jobreq(3) = ' '  
           inrcd = buf  
           call ctlpow  
           buf = inrcd  
        endif   
C       
 900    continue
        return  
C       
 130    continue
        jobreq(1) = 'STOP'  
        goto 900
C       
        end 
