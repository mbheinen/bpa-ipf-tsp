C    @(#)rddtai.f	20.11 8/19/99
        subroutine rddtai (caseid, loaded)
        character*(*) caseid
 
c       Search for caseid and load it from the old_base file..
c
c             loaded = 0 when caseid was not found
c             loaded = 1 when caseid was located and loaded
c             loaded = 2 when the file is not a "pf data" file
c
c       search an open DATAI file for a given case and, if found,
c       read the network data into memory.
 
C       The calling program must first open DATAI file with proper
C       parameters:
C
C          CASEID = ID of case in the file: if blank, first
C                   case found in file is assumed to be the desired 
c                   case
C          STATUS = interger a return value that signifies success 
c                   failure.
c
C       Called by: GTBASE (OBNAME, CASEID, BASEOK)
c
C       Example call:
c
C          CALL RDDTAI (CASEID, STATUS)
c
C       Calls To:
c
C          RDDAC (datai, bus, ntot)
C          RDDAT (datai, base, ntot)
C          RDDATX (datai, base, ntot, error_status)
C          PRTERX (number, number)
c
C       Technical Description
C        --------- -----------
C                ----
C               | Find case and set found switch
C               |      (1)
C               |
C               | Load case if found
C     [RDDTAI] <<      (0,1)
C    Read DATAI |                            ----
C               |       (+)                  | Non PF file
C               |                            |   (0,1)
C               | Action if case not found  <<    (+)
C               |      (0,1)                 | End-of-file sensed
C               ----                         |   (0,1)
C                                            ----
C               ----
C               | Read array counter record
C               |
C               | Read Bus Names, Base KV, Zones, Ownerships
C               |
C               | Read BUSDTA array in blocks of 1600
C               |
C     Load Case<< Read BCTBL array in blockes of 1100 for bus
C               | continuation data
C               |
C               | Read interchange control records if any
C               |
C               | Read switched reactive data if any
C               |
C               | Read BRNCH array in blocks of 1800
C               ----
C
C       Written by W. L. Powell

C       Documented by E.C. Ogbusbiri
C                --------------
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/anlys.inc'
      include 'ipfinc/arcntl.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/basval.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/busanl.inc'
      include 'ipfinc/bushasht.inc'
      include 'ipfinc/cbus.inc'
      include 'ipfinc/com008.inc'
      include 'ipfinc/coment.inc'
      include 'ipfinc/dc2t.inc'
      include 'ipfinc/dcmt.inc'
      include 'ipfinc/delete.inc'
      include 'ipfinc/dtaiop.inc'
      include 'ipfinc/epridc.inc'
      include 'ipfinc/filnam.inc'
      include 'ipfinc/header.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/losanl.inc'
      include 'ipfinc/oldbus.inc'
      include 'ipfinc/oldtbx.inc'
      include 'ipfinc/ordsta.inc'
      include 'ipfinc/pfdata.inc'
      include 'ipfinc/phase.inc'
      include 'ipfinc/pqcurves.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/tbx.inc'
      include 'ipfinc/tran.inc'
      include 'ipfinc/xdata.inc'
      include 'ipfinc/ycomp.inc'
      include 'ipfinc/wsccbase.inc'
      include 'ipfinc/owner_cm.inc'
      include 'ipfinc/tx_misc.inc'
 
        common /scratch/ xcomments(MAXCMT), oldarczns(MAXCAZR,MAXCAR)
        character xcomments * 120, oldarczns * 2, 
     &            oldarczns2(50,MAXCAR) * 2
        real temp(MAXYE), x22_temp(22,MAXXDT), x2_temp(2,MAXBUS), 
     &       x8_temp(8,MAXTBX), x10_temp(10,MAXTIE), 
     &       x45_temp(45,MAX2DC), x36_temp(36,MAXMDC)
        integer kx22_temp(22,MAXXDT), kx8_temp(8,MAXTBX),
     &          kx10_temp(10,MAXTIE), kx45_temp(45,MAX2DC),
     &          kx36_temp(36,MAXMDC), ftn_atoi
        equivalence (oldarczns, temp, x22_temp, kx22_temp, x2_temp, 
     &               x8_temp, kx8_temp, x10_temp, kx10_temp,
     &               x45_temp, kx45_temp, x36_temp, kx36_temp,
     &               oldarczns2)
        
        common / is_batch / is_batch

        common / is_bpf / is_bpf

        character type*10, case*10, bdate*10, casetx(100)*10, 
     &            name * 80, tname * 80, backslash * 1, dc_code * 1,
     &            xheaders(2)*133
        logical sort, stab, rcflag, ldflow, found
        integer histin

c***    external bdpfdt
        call init_bdpfdt

        backslash = char(92)

c                         initialize case table 
c***    ncase=0 
        casetx(1) = ' ' 
C                         Initialize order switches 
        ordtbx = 1  
        ordltc = 1  
        ordtie = 1  
        orddc = 1   
        ordcap = 2  
        ordvlt = 2  
c                         initialize solution state
        lskp = 0
c                         case label record 
c*** 10     ncase = ncase + 1   
        ncase = 1   

        read (datai, end=200, err=300) type, case, bdate, clabl1,
     1      clabl2, pversn, usrnam, count  

        casetx(ncase) = case

        if (type .ne. 'PF DATA' ) go to 300  ! not a "pf data" file  

        if (case .eq. 'END') go to 300    ! e-o-i encountered,
C                                         ! case not found. 
        if ( caseid .eq. ' ' ) caseid = case 
c***        if ( caseid .ne. case ) then   
c***c                         skip to next case after end of file.  
c***c                         then go back and read case label. 
c***  20       read (datai,end=10,err=300) junk 
c***           goto 20  
c***        endif   

c                         case found-load it

        lskp = 1  ! solved case
        if ( count(1) .ne. 1 ) lskp = 3  ! diverged case
        cspare(30) = case 
        cspare(31) = bdate

        kspare(15) = count(3) 
        kspare(22) = count(2) ! This is case version
        icase_vers = count(2)
c
c       Determine if case version is compatible with prog vers 
c
        if (icase_vers .lt. 4) then
           write (errbuf(1),21) pversn
  21       format(' Base case is old format, version ', a, 
     &        ' and will be internally converted ')
           call prterx ('W', 1)
           call ldoldbse (case, bdate, loaded)
           if (loaded .eq.2) go to 300
c***
c***       set "lskp" to "OLD PF6022 case", but leave "count(1)" as is
c***
c***          lskp values:
c***                zero  =  no case loaded
c***                   1  =  solved case
c***                   2  =  old PF6022 case loaded, not re-solved
c***                   3  =  diverged case, including old PF6022 cases
c***
           if ( lskp .eq. 1 ) lskp = 2
           go to 160
        endif

        loaded = 1
C
c       Description of COUNT array 
C
c       COUNT(&)   Description
C
C         1        Solution   -   0 = diverged
C                                 1 = converged   
c
C         2        Program version when file was created  
C                  Version  Description
c
C                     0     Original Version
c
C                     1     Sorted Area Interchange records
c
C                     2     Skipped!
c
C                     3     Extended branch ratings 
C                           0 = none
C                           1 = winter ratings
C                           2 = winter ratings
C
C                     4     Revised for HASHING
c
c                     5     Expanded number of zones/area from 10 to 50
c
c                     6     skipped
c                           !!! Stored original tap1, tap2
c                           !!! values for flat start
c
c                     7     Expanded "zsum" array from 2 to 26 columns
c
c                     8     8000 bus, modify so "ntot" is used so future
c                           version numbers not needed for changing the
c                           number of buses
c
c                     9     Expanded number of zones/area from 50 to 100
c                             
C         3        Extended branch ratings - 0 = none 
C                                            1 = winter ratings
C                                            2 = summer ratings
C         4        New IPF format
C
C         5        Extended ARCZNS from 10 to 50 zones/area
C
C       Description of KOUNT array   
c
C       KOUNT(*)   Counter   Description  
c
C         1        NZTOTX    Counter of ownerships in array ZSUM
c
C         2        JOWNER    Cnt of ownerships in arrays OWNNAM 
C                            and OWNLOS  
c
C         3        NTOT      Counter of buses in array VSTART   
c
C         4        NTOTIC    Counter of area intertie "I" records 
C                            in arrays ARCINT and ARCINP  
c
C         5        NYCOMP    Cnt of type "RZ" line compensation   
C                            branches in array YCOMP
c
C         6        (not used)
c
C         7        TIME      Base case time:
C                            10000 * hr + 100*min + sec
C
C         8        NEPBUS    Number of EPRI RP1964 d-c buses in case
C
C         9        NEPCTL    Number of EPRI RP1964 d-c control
C                            records in case
c
C        10        LTOT2     Number of double-entry branch records
c
C        11        NDELETE   Number of data deletion records
c
C        12        NCOM      Number of case comment records
c
C        13        NUMCURV   Number of buses with P-Q capability
C
C        14        NTOT_ALF  Number of actual buses (none deleted)
C
C        15        NUMHDR    Number of header records
C
C        16        NTOT      Number of WSCC-ENCODED entities
C
C        17        NTOT      Number of PTI-bus numbers
c
C        18        NTOTC     Number of PTI-area numbers
C
C        19        NZTOT     Number of PTI-zone numbers
C
C        20        NUM_OWNERS Number of PTI-owner numbers
C
C        21        NUM_3TERM Number of 3-terminal Tx entities
C
C        22        NUM_ZDATA Number of Tx ZDATA entities
C
c       array counter record 

        read (datai) ntot, ntot2, ltot, ntota, ntotb, ntotc, kdtot,
     &               mtdcbs, mtdcln, kxtot, bmva, jtie, yptr, kbsknt,
     &               kbrknt, jphno, nztot, nspare, ntotcs, nbslck, 
     &               nslkxx, kount

        kdchg = 1 
        ntotcs = 0
        ltot2 = kount(10)
        ntot_alf = kount(14)

c       bus names, kv, zone code, ownership records  

        call rddac (datai, bus, ntot) 
        call rddat (datai, base, ntot)
        call rddac (datai, zone, ntot)
        call rddac (datai, owner, ntot)   

c       bus data records   

        call rddat (datai, busdta, ntot * 16)   
        call rddat (datai, htable_b, BUS_HASHSIZE)    ! Bus hash tables
        if ( icase_vers .lt. 8 ) then
           call rddat (datai, nextptr_b, 6000)         ! Bus hash tables
        else
           call rddat (datai, nextptr_b, ntot)         ! Bus hash tables
           do i = ntot+1, MAXBUS
             nextptr_b(i) = 0
           enddo
        endif

c       [conditional] continuation bus 

        if (ntot2.gt.0) then  
           call rddat (datai, bctbl, ntot2 * 12) 
           call rddat (datai, bctbl_nxt(1), ntot2) ! +bus pointer 
C                                                   ! information
        endif 

c       [conditional] interchange controls records 

        if (ntotc.gt.0) then  
           call rddac (datai, arcnam, ntotc)  
           call rddac (datai, arcbus, ntotc)  
           call rddat (datai, arcbas, ntotc)  
           call rddat (datai, arcnet, ntotc)  
           if (icase_vers .eq. 4) then
              call rddac (datai, oldarczns, ntotc * MAXCAZR)   
              do i = 1, ntotc
                 do j = 1, MAXCAZR
                    arczns(j,i) = oldarczns(j,i)
                 enddo
                 do j = MAXCAZR+1, MAXCAZ
                    arczns(j,i) = ' '
                 enddo
              enddo
           else if ( icase_vers .lt. 9 ) then
              call rddac (datai, oldarczns2, ntotc * 50)   
              do i = 1, ntotc
                 do j = 1, 50
                    arczns(j,i) = oldarczns2(j,i)
                 enddo
                 do j = 51, MAXCAZ
                    arczns(j,i) = ' '
                 enddo
              enddo
           else
              call rddac (datai, arczns, ntotc * MAXCAZ)   
           endif
           call rddat (datai, acznum, nztot)  
           call rddac (datai, acznam, nztot)  
           call rddat (datai, jarzn, ntot)
           call rddat (datai, area, ntotc * 8)  
        endif 

c       [conditional] switched reactive data record  

        if (kxtot.gt.0) then  
           call rddat (datai, x22_temp, kxtot * 22)
           do i = 1, kxtot
              xdata(1,i) = kx22_temp(1,i)
              xdata(2,i) = kx22_temp(2,i)
              do j = 3, 22
                 xdata(j,i) = x22_temp(j,i)
              enddo
           enddo
        endif 

c       branch data record

        call rddat (datai, brnch, ltot * 18)  
        call rddat (datai, rateln, ltot * 3)  
        call rddat (datai, kx(1), ltot2)  
        call rddat (datai, ky(1), ltot2)  
        call rddac (datai, brid(1), ltot2)  
        call rddat (datai, brsect(1), ltot2)  
        call rddat (datai, brtype(1), ltot2)  
        call rddat (datai, brnch_nxt(1), ltot2)  
        call rddat (datai, brnch_ptr(1), ltot2)  
C
c       internal, external bus numbers e + Jf voltage tables 
c       capacitor correction tables   
C
        call rddat (datai, alf2inp, ntot)   
        call rddat (datai, inp2alf, ntot)   
        call rddat (datai, opt2inp, ntot)   
        call rddat (datai, inp2opt, ntot)   
        call rddat (datai, temp, ntot)
        do i = 1, ntot
           e(i) = temp(i)
        enddo
        call rddat (datai, temp, ntot)
        do i = 1, ntot
           f(i) = temp(i)
        enddo
        call rddat (datai, x2_temp, ntot * 2)
        do i = 1, ntot
           capcor(1,i) = x2_temp(1,i)
           capcor(2,i) = x2_temp(2,i)
        enddo

C       Store old bus names, voltages in alpha order  

        do i = 1, ntot
           oldbus(i) = bus(i) 
           oldbas(i) = base(i)
           olde(i) = e(inp2opt(i))  
           oldf(i) = f(inp2opt(i))  
        enddo
        numobs = ntot 
C
c       conditional] phase shifter data record   

        if (jphno.gt.0) then  
           call rddat (datai, jphid, jphno * 8) 
        endif 

C       Y-matrix and solution arrays
c
        call rddat (datai, km, ntot)          
        call rddat (datai, kmlen, ntot)       
        call rddat (datai, temp, ntot)
        do i = 1, ntot
           pnetu(i) = temp(i)
        enddo
        call rddat (datai, temp, ntot)
        do i = 1, ntot
           qnetu(i) = temp(i)
        enddo
        call rddat (datai, temp, ntot)
        do i = 1, ntot
           inetr(i) = temp(i)
        enddo
        call rddat (datai, temp, ntot)
        do i = 1, ntot
           ineti(i) = temp(i)
        enddo
        call rddat (datai, temp, ntot)
        do i = 1, ntot
           ploadu(i) = temp(i)
        enddo
        call rddat (datai, temp, ntot)
        do i = 1, ntot
           qloadu(i) = temp(i)
        enddo
        call rddat (datai, vlimn, ntot)
        call rddat (datai, vlimx, ntot)
        call rddat (datai, temp, ntot)
        do i = 1, ntot
           gkku(i) = temp(i)
        enddo
        call rddat (datai, temp, ntot)
        do i = 1, ntot
           bkku(i) = temp(i)
        enddo
        call rddat (datai, ntypu, ntot)        
        call rddat (datai, nspar, ntot)       
        call rddat (datai, ikmu, yptr)        
        call rddat (datai, temp, yptr)
        do i = 1, yptr
           gkmu(i) = temp(i)
        enddo
        call rddat (datai, temp, yptr)
        do i = 1, yptr
           bkmu(i) = temp(i)
        enddo
C
c       conditional] special bus data record blocked by 50
C
        if (ntotb .gt. 0) then  
           call rddat (datai, x8_temp, ntotb * 8)
           do i = 1, ntotb
              ltyp = kx8_temp(1,i)
              tbx(1,i) = ltyp
              tbx(2,i) = kx8_temp(2,i)
              tbx(3,i) = x8_temp(3,i)
              tbx(4,i) = x8_temp(4,i)
              if (ltyp .eq. 5) then
                 tbx(5,i) = kx8_temp(5,i)
              else
                 tbx(5,i) = x8_temp(5,i)
              endif
              tbx(6,i) = x8_temp(6,i)
              tbx(7,i) = kx8_temp(7,i)
              tbx(8,i) = kx8_temp(8,i)
           enddo
c
C          Convert any distantly remote controlled buses;  
C          store original TBX array.   
c
           do j = 1, ntotb   
              ltyp = tbx(1,j)
              if (ltyp .eq. 3 .or. ltyp .eq. 5) then  
                 if (tbx(8,j) .lt. 0) then  
                    m = tbx(8,j)
                    m = iabs(m)
                    tbx(8,j) = opt2inp(m) 
                 endif   
              endif  
              oldtbx(1,j) = tbx(1,j)
              oldtbx(2,j) = tbx(2,j)
              if (ltyp .eq. 5) then  
                 oldtbx(3,j) = tbx(3,j)
              else
                 oldtbx(3,j) = 0
              endif
              do k = 4, 6
                 oldtbx(k,j) = 0
              enddo
              oldtbx(7,j) = tbx(7,j)
              oldtbx(8,j) = tbx(8,j)
           enddo
           numtbx = ntotb 
           call tbxhinit ()    ! initialize the oldtbx hash table
        endif 
C
c       [conditional] automatic transformer  
C
        if (ntota.gt.0) then  
           call rddat (datai, tran, ntota * 12)  
           call rddat (datai, temp, ntota)
           do i = 1, ntota
              tap(i) = temp(i)
           enddo
        endif 
C
c       [conditional] area interchange   
C
        if (jtie .gt. 0) then   
           call rddat (datai, x10_temp, jtie * 10)
           do i = 1, jtie
              tie(1,i) = kx10_temp(1,i)
              tie(2,i) = kx10_temp(2,i)
              do k = 3, 6
                 tie(k,i) = x10_temp(k,i)
              enddo
              do k = 7, 10
                 tie(k,i) = kx10_temp(k,i)
              enddo
           enddo
        endif 
C
c       [conditional] dc line modeling records   
C
        if (kdtot .gt. 0) then
           call rddat (datai, x45_temp, kdtot * 45)
           do i = 1, kdtot
              dc2t(1,i) = kx45_temp(1,i)
              dc2t(2,i) = x45_temp(2,i)
              dc2t(3,i) = kx45_temp(3,i)
              do k = 4, 6
                 dc2t(k,i) = x45_temp(k,i)
              enddo
              dc2t(7,i) = kx45_temp(7,i)
              do k = 8, 19
                 dc2t(k,i) = x45_temp(k,i)
              enddo
              dc2t(20,i) = kx45_temp(20,i)
              do k = 21, 32
                 dc2t(k,i) = x45_temp(k,i)
              enddo
              dc2t(33,i) = kx45_temp(33,i)
              dc2t(34,i) = kx45_temp(34,i)
              do k = 35, 36
                 dc2t(k,i) = x45_temp(k,i)
              enddo
              dc2t(37,i) = kx45_temp(37,i)
              dc2t(38,i) = kx45_temp(38,i)
              do k = 39, 45
                 dc2t(k,i) = x45_temp(k,i)
              enddo
           enddo
        endif 
        if (mtdcbs.gt.0) then 
           call rddat (datai, x36_temp, mtdcbs * 36)
           do i = 1, mtdcbs
              dcmtbs(1,i) = kx36_temp(1,i)
              dcmtbs(2,i) = x36_temp(2,i)
              dcmtbs(3,i) = kx36_temp(3,i)
              call getchr (1, dc_code, kx36_temp(4,i))
              call putchr_8 (1, dc_code, dcmtbs(4,i))
              do k = 5, 14
                 dcmtbs(k,i) = x36_temp(k,i)
              enddo
              dcmtbs(15,i) = kx36_temp(15,i)
              do k = 16, 20
                 dcmtbs(k,i) = x36_temp(k,i)
              enddo
              dcmtbs(21,i) = kx36_temp(21,i)
              dcmtbs(22,i) = kx36_temp(22,i)
              do k = 23, 26
                 dcmtbs(k,i) = x36_temp(k,i)
              enddo
              dcmtbs(27,i) = kx36_temp(27,i)
              dcmtbs(28,i) = kx36_temp(28,i)
              do k = 29, 30
                 dcmtbs(k,i) = x36_temp(k,i)
              enddo
              do k = 31, 36
                 dcmtbs(k,i) = kx36_temp(k,i)
              enddo
           enddo
        endif 
        if (mtdcln.gt.0) then 
           call rddat (datai, x10_temp, mtdcln * 10)
           do i = 1, mtdcln
              do k = 1, 3
                 dcmtln(k,i) = kx10_temp(k,i)
              enddo
              do k = 4, 6
                 dcmtln(k,i) = x10_temp(k,i)
              enddo
              do k = 7, 10
                 dcmtln(k,i) = kx10_temp(k,i)
              enddo
           enddo
        endif 

        nztotx = kount (1)
        jowner = kount (2)
        if (nztotx.gt.0) then
           if (icase_vers .le. 6) then
              read (datai) (zsum(5,i),zsum(6,i) ,i=1, nztotx)   
           else
              call rddat (datai, zsum, nztotx * 26)   
           endif
        endif 
        if (jowner.gt.0) then 
           call rddac (datai, ownnam, jowner) 
           call rddat (datai, ownlos, jowner * 2)   
        endif 

        i = kount (3) 
        if (i.gt.0) call rddat (datai, vstart, i) 

        ntotic = kount(4) 
        if (ntotic.gt.0) then 
           call rddat (datai, arcinp, ntotic)   
           call rddac (datai, arcint, 2 * ntotic) 
        endif 

        nycomp = kount(5) 
        if (nycomp .gt. 0) then   
           call rddat (datai, kycomp, 48 * nycomp) 
        endif 
C
C       Retrieve any EPRI RP1964 d-c data.
C
        nepbus = kount(8)
        nepctl = kount(9)
C
C       EPBUS = character array containing BD, BZ, LD, MD records.
C       EPCTL = character array containing CC, CD, CN, CR records.

        if (nepbus .gt. 0) then
           call rddac (datai, epbus, nepbus)
           if (nepctl .gt. 0) call rddac (datai, epctl, nepctl)

           iver = 5
           histin = datai    ! RCFLAG -  recover saved case flag.
           jtape = lprt
           stab = .false.
           ldflow = .true.
           rcflag = .true.   ! RCFLAG -  recover saved case flag.
C
C          IVER = 5 corresponds with highest version in EPRI
C          powerflow.

           call loaddc (iver, histin, rcflag, jtape, ldflow, stab)
           call loadcc (histin)
        endif

        ndelete = kount(11)
        if (ndelete .gt. 0) then
           call rddac(datai, delete, ndelete)
        endif

        ncomx = kount(12)
        if (ncom .gt. 0  .or.  is_bpf .eq. 1 ) then
c          use comments from control file, dummy read
           if (ncomx .gt. 0) then
              call rddac(datai, xcomments, ncomx)
           endif
        else
c          use comments from case file
           ncom = ncomx
           if (ncom .gt. 0) then
              call rddac(datai, com, ncom)
           endif
        endif
c
c       Added Oct 1992 - A. H. Schmidt
c
        numcurv = kount(13)
        if (numcurv .gt. 0) then

c          [conditional] P/Q curve data

           call rddat (datai, pqpgen,    17*numcurv)
           call rddat (datai, pqqmin,    17*numcurv)
           call rddat (datai, pqqmax,    17*numcurv)
           call rddat (datai, pqbusptr,  numcurv)
           call rddac (datai, pqid,      numcurv)
           call rddat (datai, pqactive,  numcurv)
           call rddat (datai, pqnumunit, numcurv)
           call rddat (datai, pg_sched,  numcurv)

        endif

        i = kount (15) 
        if (i. gt. 0) then
           if ( coment(1) .ne. ' '  .or.  coment(2) .ne. ' ' 
     &          .or.  is_bpf .eq. 1 ) then
c             dummy read
              call rddac (datai, xheaders(1)(1:132), 1) 
              call rddac (datai, xheaders(2)(1:132), 1) 
           else
c             no headers in control file, or not BPF
c             use headers from case file
              call rddac (datai, coment(1)(1:132), 1) 
              call rddac (datai, coment(2)(1:132), 1) 
           endif
        endif
c
c       IPF versions earlier than 319 cannot read past this!
c
        iver = ftn_atoi (pversn(4:8))
        if (pversn(1:3) .ne. 'IPF' .or. iver .lt. 319) go to 160
c
c       Read WSCC-encoded base Kv's (ASCII)
c
        i = kount (16) 
        if (i. gt. 0) then
           wsccflag = .true.
           call rddac (datai, wsccbase, ntot) 
        else
           wsccflag = .false.
        endif
c
c       Read PTI bus numbers
c
        i = kount (17) 
        if (i. gt. 0) then
           call rddat (datai, bus_number, i) 
        endif
c
c       Read PTI area numbers
c
        i = kount (18) 
        if (i. gt. 0) then
           call rddat (datai, area_number, i) 
        endif
c
c       Read PTI zone data
c
        i = kount (19) 
        if (i. gt. 0) then
           call rddat (datai, zone_number, i) 
           call rddac (datai, zone_name, i) 
        endif
c
c       Read PTI owner data
c
        i = kount (20) 
        if (i. gt. 0) then
           num_owners = i
           call rddat (datai, owner_number, i) 
           call rddac (datai, owner_code, i) 
           call rddac (datai, owner_name, i) 
        endif
c
c       Read 3-terminal TX data
c
        i = kount (21) 
        if (i. gt. 0) then
           num_3term = i
           call rddat (datai, tx_3term, 6*i) 
        endif
c
c       Read Tx ZDATA
c
        i = kount (22) 
        if (i. gt. 0) then
           num_zdata = i
           call rddat (datai, tx_zdata, 26*i) 
        endif
c
c       Set input/optimal order switches of data read in.
c
  160   ordcap = 2  
        orddc  = 1   
        ordltc = 1  
        ordtbx = 1  
        ordtie = 1  
        ordvlt = 2  
        ordymx = 2 

        if ( is_bpf .eq. 1 ) then
c          ! Force to (powerflow) record values, if specified
           if ( chase1(1) .ne. '  ' ) then
              case = chase1(1)
           endif
           if ( chase1(34) .ne. '  ' ) then
              clabl1 = chase1(34)
              clabl2 = chase1(35)
           endif
        else
c          ! Redefine these variables only for non-BPF  runs.
c          ! In BPF, they are already defined in CTLCOM.
           chase1(1) = case
           chase1(34) = clabl1
           chase1(35) = clabl2
        endif
C
C       Update old base values
C
        name = ' '
        basval(2) = ' '
        basval(3) = ' '
        inquire(unit=datai,name=name) 
        i = index (name, ':') 
        if (i .gt. 1) then
           basval(2) = name(1:i-1)
           tname = name(i+1:)
           name = tname
        endif 
        if ( is_it_vms() .eq. 1 ) then
           i = index (name, ']') 
           if (i .gt. 1) then
              basval(3) = name(1:i)  
              tname = name(i+1:)
              name = tname
           endif 
        else
           i = len(name)
           found = .false.
           do while ( i .gt. 1  .and.  .not. found )
              i = i - 1
              if ( name(i:i) .eq. '/'  .or.
     &             name(i:i) .eq. backslash ) found = .true.
           enddo
           if ( found ) then
              basval(3) = name(1:i)
              tname = name(i+1:)
              name = tname
           endif
        endif 
        basval(1) = name  

        basval(4) = case  
        basval(5) = bdate 

        ihr = kount(7)/10000  
        imin = mod (kount(7)/100, 100)
        isec = mod (kount(7), 100)
        write (basval(6), 22) ihr, imin, isec 
   22   format (i2, ':', i2, ':', i2)  

        basval(7) = clabl1 // clabl2  
        basval(8) = pversn
        basval(9) = usrnam

c
C       Determine whether case version are necessary
c
        if (icase_vers .ne. kspare(33)) then
           if (icase_vers .eq. 0 .and. kspare(33) .ge. 1) then  

C             Version 1 - check sort order of area interchange array.

              sort = .true. 
              do i = 1, ntotc-1 
                 if (arcnam(i) .gt. arcnam(i+1)) sort = .false. 
              enddo
c                         icase_vers = 1
              if ( sort ) kspare(22) = 1 
           endif
        endif   
        go to 900   
C
c       case not found, or end-of-file 
C
  200   loaded = 0  
        lskp = 0
        ierr = 0
        write (errbuf(1),210) caseid,ierr   
  210   format(' Case ',a10,' not found. (Error no. ',i3,'.) ',  
     1     ' List of cases readin follows:')   
        errbuf(2) = ' ' 
        k = 2   
        do i = 1, ncase, 5
           k = k + 1
           il = min0 (i+4,ncase)
           write (errbuf(k),220) (j,casetx(j),j=i,il)
  220      format (5(2x,i2,' - (',a10,')'))
        enddo
        if (is_batch .eq. 0) then
           call prterx ('E',k)
        else
           call prterx ('F',k)
        endif
        go to 900
c
c       data file incompatible with pf
c
  300   loaded = 2
        lskp = 0
        inquire (unit=datai, iostat=ierr)
        name = ' '
        inquire(unit=datai,name=name)
        last = lastch(name)
        write (errbuf(1),310) name(1:last), ierr
  310   format(' Requested base file ',a,
     1         ' is not valid. (Error no. ',i3,')')
        if (is_batch .eq. 0) then
           call prterx ('E',1)
        else
           call prterx ('F',1)
        endif
  900   return
        end
