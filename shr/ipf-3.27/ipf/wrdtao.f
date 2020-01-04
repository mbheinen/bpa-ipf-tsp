C    @(#)wrdtao.f	20.11 8/19/99
        subroutine wrdtao
 
C       Put all data on DATAO file
 
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
        include 'ipfinc/coment.inc'
        include 'ipfinc/dc2t.inc'
        include 'ipfinc/dcmt.inc'
        include 'ipfinc/delete.inc'
        include 'ipfinc/dtaiop.inc'
        include 'ipfinc/epridc.inc'
        include 'ipfinc/header.inc'
        include 'ipfinc/jobctl.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/losanl.inc'
        include 'ipfinc/pfdata.inc'
        include 'ipfinc/phase.inc'
        include 'ipfinc/pqcurves.inc'
        include 'ipfinc/rp1964_1.inc'
        include 'ipfinc/tbx.inc'
        include 'ipfinc/tran.inc'
        include 'ipfinc/xdata.inc'
        include 'ipfinc/ycomp.inc'
        include 'ipfinc/wsccbase.inc'
        include 'ipfinc/owner_cm.inc'
        include 'ipfinc/tx_misc.inc'
c
        common /scratch/ xcomments(MAXCMT), oldarczns(MAXCAZR,MAXCAR)
        character xcomments * 120, oldarczns * 2
        real temp(MAXYE), x22_temp(22,MAXXDT), x2_temp(2,MAXBUS), 
     &       x8_temp(8,MAXTBX), x10_temp(10,MAXTIE), 
     &       x45_temp(45,MAX2DC), x36_temp(36,MAXMDC)
        integer kx22_temp(22,MAXXDT), kx8_temp(8,MAXTBX),
     &          kx10_temp(10,MAXTIE), kx45_temp(45,MAX2DC),
     &          kx36_temp(36,MAXMDC)
        equivalence (oldarczns, temp, x22_temp, kx22_temp, x2_temp, 
     &               x8_temp, kx8_temp, x10_temp, kx10_temp,
     &               x45_temp, kx45_temp, x36_temp, kx36_temp)

        character code * 4, prgvsn10 * 10, name * 80, tname * 80, 
     &            backslash * 1, dc_code * 1
        logical epdc, found
c
        backslash = char(92)
        
        count(1) = 0
        if ( lskp .eq. 1 ) count(1) = 1
        count(2) = kspare(33)   
        count(3) = kspare(15)   
C
C       Update old base values
C
        name = ' '
        basval(2) = ' '
        basval(3) = ' '
        inquire(unit=datao,name=name)
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
           end do
           if ( found ) then
              basval(3) = name(1:i)
              tname = name(i+1:)
              name = tname
           endif
        endif
        basval(1) = name

        basval(4) = chase1(1)
        call xdate(dte)
        basval(5) = dte
        call n_time( ihr, imin, isec )
        write (basval(6), 90) ihr, imin, isec
   90   format (i2, ':', i2, ':', i2)
        basval(7) = chase1(34) // chase1(35)
        basval(8) = prgvsn
        basval(9) = usrnam

        clabl1 = chase1(34)
        clabl2 = chase1(35)
        pversn = prgvsn

c
c****  Write the first record ********************************
c
        prgvsn10 = prgvsn   ! Note 10 characters needed for 
C                           ! compatibility with old base cases

        write(datao) pfdata, chase1(1), dte, chase1(34), chase1(35),
     &               prgvsn10, user, count   
c
C       NTOTCS is swing flag switch 
c
c           0 -- normal 
c           1 -- dc data errors in datao file are unacceptable for  
c                transient stability studies  (defined in "dcdata") 
c
        nshift = 0
        kdtots = 0
        nspare = 0
C
c       Description of COUNT array 
C
c       COUNT(*)   Description
C
C         1        Solution   -   0 = diverged
C                                 1 = converged   
c
C         2        Program version when file was created  
C                  Version  Date         Description
c
C                     0     -----------  Original Version
c
C                     1     13-JUL-1987  Sorted Area Interchange records
c
C                     2                  Skipped!
c
C                     3                  Extended branch ratings 
C                                        0 = none
C                                        1 = winter ratings
C                                        2 = winter ratings
C
C                     4                  Revised for HASHING
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
c
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
        kount(1) = nztot
        kount(2) = jowner   
        kount(3) = ntot 
        kount(4) = ntotic   
        kount(5) = nycomp   
        kount(6) = 0
        kount(7) = 0

        call n_time(ihr,imin,isec) 
        kount(7) = 10000*ihr + 100*imin + isec  
C
C       Revised September 1991, to add RP1964 d-c data to case.
C
        kount(8) = nepbus
        kount(9) = nepctl
C
C       Revised September 1992, to case comments and deleted data
c       case.
C
        kount(10) = ltot2
        kount(11) = ndelete
        kount(12) = ncom
c
c       Revised October 20 1992, to add P/Q curve data  - Bert Schmidt
c
        kount(13) = numcurv
        kount(14) = ntot_alf
        kount(15) = 2  ! hard code number of headers saved in case
c
c       Revised July 17, 1997, to add WSCC encoded basekvs.
c
        if (wsccflag) kount(16) = ntot
c
c       Revised October 20, 1997, to add PTI and GE data.
c
        if (bus_number(alf2inp(1)) .gt. 0) then
          kount(17) = ntot
        else
          kount(17) = 0
        endif
        if (area_number(1) .gt. 0) then
          kount(18) = ntotc
        else
          kount(18) = 0
        endif
        if (zone_number(1) .gt. 0) then
          kount(19) = nztot
        else
          kount(19) = 0
        endif
        if (owner_number(1) .gt. 0) then
          kount(20) = num_owners
        else
          kount(20) = 0
        endif
        if (num_3term .gt. 0) then
          kount(21) = num_3term
        else
          kount(21) = 0
        endif
        if (num_zdata .gt. 0) then
          kount(22) = num_zdata
        else
          kount(22) = 0
        endif

        write (datao) ntot, ntot2, ltot, ntota, ntotb, ntotc, kdtot,
     &                mtdcbs, mtdcln, kxtot, bmva, jtie, yptr, kbsknt,
     &                kbrknt, jphno, nztot, nspare, ntotcs, nbslck,
     &                nslkxx, kount
        ntotcs = 0

        call wrdac (datao, bus, ntot) 
        call wrdat (datao, base, ntot)
        call wrdac (datao, zone, ntot)
        call wrdac (datao, owner, ntot)   

c       bus data records

        call wrdat (datao, busdta, ntot*16)   
        call wrdat (datao, htable_b, BUS_HASHSIZE)    ! Bus hash tables
        call wrdat (datao, nextptr_b, ntot)           ! Bus hash tables


        if (ntot2.gt.0) then  
c          [conditional] continuation bus
           call wrdat (datao, bctbl, ntot2*12) 
           call wrdat (datao, bctbl_nxt(1), ntot2) ! +bus pointer 
C                                               ! information
        endif 

        if (ntotc.gt.0) then  

c          [conditional] interchange control records

           call wrdac (datao, arcnam, ntotc)  
           call wrdac (datao, arcbus, ntotc)  
           call wrdat (datao, arcbas, ntotc)  
           call wrdat (datao, arcnet, ntotc)  
           call wrdac (datao, arczns, ntotc * MAXCAZ)   
           call wrdat (datao, acznum, nztot)  
           call wrdac (datao, acznam, nztot)  
           call wrdat (datao, jarzn, ntot)
           call wrdat (datao, area, ntotc * 8)  
        endif 


        if (kxtot.gt.0) then  

c          [conditional] switched reactive data records

           do i = 1, kxtot
              kx22_temp(1,i) = xdata(1,i)
              kx22_temp(2,i) = xdata(2,i)
              do j = 3, 22
                 x22_temp(j,i) = xdata(j,i)
              enddo
           enddo
           call wrdat (datao, x22_temp, kxtot * 22)
        endif 
c
c       branch data records
c
        call wrdat (datao, brnch, ltot * 18)  
        call wrdat (datao, rateln, ltot * 3)  
        call wrdat (datao, kx(1), ltot2)  
        call wrdat (datao, ky(1), ltot2)  
        call wrdac (datao, brid(1), ltot2)  
        call wrdat (datao, brsect(1), ltot2)  
        call wrdat (datao, brtype(1), ltot2)  
        call wrdat (datao, brnch_nxt(1), ltot2)  
        call wrdat (datao, brnch_ptr(1), ltot2)  

c       internal,external bus numbers  
c       e + j f voltage tables 
c       capacitor correction tables  
c
        call wrdat (datao, alf2inp, ntot)   
        call wrdat (datao, inp2alf, ntot)   
        call wrdat (datao, opt2inp, ntot)   
        call wrdat (datao, inp2opt, ntot)   
        do i = 1, ntot
           temp(i) = e(i)
        enddo
        call wrdat (datao, temp, ntot)
        do i = 1, ntot
           temp(i) = f(i)
        enddo
        call wrdat (datao, temp, ntot)
        do i = 1, ntot
           x2_temp(1,i) = capcor(1,i)
           x2_temp(2,i) = capcor(2,i)
        enddo
        call wrdat (datao, x2_temp, ntot * 2)

        if (jphno.gt.0) then  
c          [conditional] phase shifter data records
           call wrdat (datao, jphid, jphno * 8) 
        endif 

C       Y-matrix and solution arrays
c
        call wrdat (datao, km, ntot)          
        call wrdat (datao, kmlen, ntot)       
        do i = 1, ntot
           temp(i) = pnetu(i)
        enddo
        call wrdat (datao, temp, ntot)
        do i = 1, ntot
           temp(i) = qnetu(i)
        enddo
        call wrdat (datao, temp, ntot)
        do i = 1, ntot
           temp(i) = inetr(i)
        enddo
        call wrdat (datao, temp, ntot)
        do i = 1, ntot
           temp(i) = ineti(i)
        enddo
        call wrdat (datao, temp, ntot)
        do i = 1, ntot
           temp(i) = ploadu(i)
        enddo
        call wrdat (datao, temp, ntot)
        do i = 1, ntot
           temp(i) = qloadu(i)
        enddo
        call wrdat (datao, temp, ntot)
        call wrdat (datao, vlimn, ntot)       
        call wrdat (datao, vlimx, ntot)       
        do i = 1, ntot
           temp(i) = gkku(i)
        enddo
        call wrdat (datao, temp, ntot)
        do i = 1, ntot
           temp(i) = bkku(i)
        enddo
        call wrdat (datao, temp, ntot)
        call wrdat (datao, ntypu, ntot)        
        call wrdat (datao, nspar, ntot)       
        call wrdat (datao, ikmu, yptr)        
        do i = 1, yptr
           temp(i) = gkmu(i)
        enddo
        call wrdat (datao, temp, yptr)
        do i = 1, yptr
           temp(i) = bkmu(i)
        enddo
        call wrdat (datao, temp, yptr)

        if (ntotb.gt.0) then  

c          [conditional] special bus data records 

           do i = 1, ntotb
              ltyp = tbx(1,i)
              kx8_temp(1,i) = ltyp 
              kx8_temp(2,i) = tbx(2,i)
              x8_temp(3,i) = tbx(3,i)
              x8_temp(4,i) = tbx(4,i)
              if (ltyp .eq. 5) then
                 kx8_temp(5,i) = tbx(5,i)
              else
                 x8_temp(5,i) = tbx(5,i)
              endif
              x8_temp(6,i) = tbx(6,i)
              kx8_temp(7,i) = tbx(7,i)
              kx8_temp(8,i) = tbx(8,i)
           enddo
           call wrdat (datao, x8_temp, ntotb * 8)
        endif 

        if (ntota.gt.0) then  

c          [conditional] automatic transformer data records

           call wrdat (datao, tran, ntota * 12)  
           do i = 1, ntota
              temp(i) = tap(i)
           enddo
           call wrdat (datao, temp, ntota)
        endif 

        if (jtie.gt.0) then   

c          [conditional] area interchange data records

           do i = 1, jtie
              kx10_temp(1,i) = tie(1,i)
              kx10_temp(2,i) = tie(2,i) 
              do k = 3, 6
                 x10_temp(k,i) = tie(k,i)
              enddo
              do k = 7, 10
                 kx10_temp(k,i) = tie(k,i)
              enddo
           enddo
           call wrdat (datao, x10_temp, jtie * 10)
        endif 

        if (kdtot .gt. 0) then

c          [conditional] d-c line modeling records

           do i = 1, kdtot
              kx45_temp(1,i) = dc2t(1,i)
              x45_temp(2,i) = dc2t(2,i)
              kx45_temp(3,i) = dc2t(3,i)
              do k = 4, 6
                 x45_temp(k,i) = dc2t(k,i)
              enddo
              kx45_temp(7,i) = dc2t(7,i)
              do k = 8, 19
                 x45_temp(k,i) = dc2t(k,i)
              enddo
              kx45_temp(20,i) = dc2t(20,i)
              do k = 21, 32
                 x45_temp(k,i) = dc2t(k,i)
              enddo
              kx45_temp(33,i) = dc2t(33,i)
              kx45_temp(34,i) = dc2t(34,i)
              do k = 35, 36
                 x45_temp(k,i) = dc2t(k,i)
              enddo
              kx45_temp(37,i) = dc2t(37,i)
              kx45_temp(38,i) = dc2t(38,i)
              do k = 39, 45
                 x45_temp(k,i) = dc2t(k,i)
              enddo
           enddo
           call wrdat (datao, x45_temp, kdtot * 45)
        endif 

        if (mtdcbs.gt.0) then 

c          [conditional] d-c line modeling records

           do i = 1, mtdcbs
              kx36_temp(1,i) = dcmtbs(1,i)
              x36_temp(2,i) = dcmtbs(2,i) 
              kx36_temp(3,i) = dcmtbs(3,i) 
              call getchr_8 (1, dc_code, dcmtbs(4,i))
              call putchr (1, dc_code, kx36_temp(4,i))
              do k = 5, 14
                 x36_temp(k,i) = dcmtbs(k,i)
              enddo
              kx36_temp(15,i) = dcmtbs(15,i)
              do k = 16, 20
                 x36_temp(k,i) = dcmtbs(k,i)
              enddo
              kx36_temp(21,i) = dcmtbs(21,i)
              kx36_temp(22,i) = dcmtbs(22,i)
              do k = 23, 26
                 x36_temp(k,i) = dcmtbs(k,i)
              enddo
              kx36_temp(27,i) = dcmtbs(27,i)
              kx36_temp(28,i) = dcmtbs(28,i) 
              do k = 29, 30
                 x36_temp(k,i) = dcmtbs(k,i)
              enddo
              do k = 31, 36
                 kx36_temp(k,i) = dcmtbs(k,i)
              enddo
           enddo
           call wrdat (datao, x36_temp, mtdcbs * 36)
        endif 

        if (mtdcln.gt.0) then 

c          [conditional] d-c line modeling records

           do i = 1, mtdcln
              do k = 1, 3
                 kx10_temp(k,i) = dcmtln(k,i)
              enddo
              do k = 4, 6
                 x10_temp(k,i) = dcmtln(k,i)
              enddo
              do k = 7, 10
                 kx10_temp(k,i) = dcmtln(k,i)
              enddo
           enddo
           call wrdat (datao, x10_temp, mtdcln * 10)
        endif 

c       added AUG 1980
c       revised SEP 1993 (version 7 - zsum saved in its entirety)

        if (nztot.gt.0)  then 

c          [conditional]

           call wrdat (datao, zsum, 26 * nztot)   
        endif 

        if (jowner.gt.0) then 

c          [conditional]

           call wrdac (datao, ownnam, jowner) 
           call wrdat (datao, ownlos, jowner * 2)   
        endif 
c
C       added Feb 1981   

        call wrdat (datao, vstart, ntot)  
c
c       added May 1983   
C
        if (ntotic.gt.0) then 

c          [conditional]

           call wrdat (datao,arcinp,ntotic)
           call wrdac (datao,arcint,2 * ntotic)  
        endif 

C       Added July 1986  

        if (nycomp .gt. 0) then   

c          [conditional]

           call wrdat (datao,kycomp,48 * nycomp) 
        endif 
C
C       Added September 1991
C
        epdc = (nepbus .gt. 0)
C
C       EPBUS = character array containing BD, BZ, LD, MD records.
C       EPCTL = character array containing CC, CD, CN, CR records.

        if (epdc) then

c          [conditional]

           call wrdac (datao, epbus, nepbus)
           if (nepctl .gt. 0) call wrdac (datao, epctl, nepctl)
C
C          Retrieve a-c loads from d-c system. SETPQX expects the
C          loads to be in the order of BSNAME.
C
           do i = 1, ntot
              j = inp2opt(i)
              bsname(i,1) = bus(j)(1:4)
              bsname(i,2) = bus(j)(5:8)
              bsname(i,3) = code (base(j), 4, 0)
              ixcros(i) = i
           enddo
           call setpqx
           call savedc
           call savecc
        endif
c
c       Added Sept 1992
c
        if (ndelete .gt. 0) then

c          [conditional]

           call wrdac(datao, delete, ndelete)
        endif

        if (ncom .gt. 0) then

c          [conditional]

           call wrdac(datao,com,ncom)
        endif
c
c       Added Oct 1992 - A. H. Schmidt
c
        if (numcurv .gt. 0) then

c          [conditional] P/Q curve data

           call wrdat (datao, pqpgen,    17*numcurv)
           call wrdat (datao, pqqmin,    17*numcurv)
           call wrdat (datao, pqqmax,    17*numcurv)
           call wrdat (datao, pqbusptr,  numcurv)
           call wrdac (datao, pqid,      numcurv)
           call wrdat (datao, pqactive,  numcurv)
           call wrdat (datao, pqnumunit, numcurv)
           call wrdat (datao, pg_sched,  numcurv)
  
        endif
c
c       added Nov 1993   

        call wrdac (datao, coment(1)(1:132), 1) 
        call wrdac (datao, coment(2)(1:132), 1) 
c
c       Added Nov 1997
c
        if (wsccflag) then

c          [conditional] WSCC encoded basekvs

           call wrdac (datao, wsccbase, ntot) 
        endif
c
c       Write PTI bus numbers
c
        i = kount (17) 
        if (i. gt. 0) then
           call wrdat (datao, bus_number, i) 
        endif
c
c       Write PTI area numbers
c
        i = kount (18) 
        if (i. gt. 0) then
           call wrdat (datao, area_number, i) 
        endif
c
c       Write PTI zone data
c
        i = kount (19) 
        if (i. gt. 0) then
           call wrdat (datao, zone_number, i) 
           call wrdac (datao, zone_name, i) 
        endif
c
c       Write PTI owner data
c
        i = kount (20) 
        if (i. gt. 0) then
           call wrdat (datao, owner_number, i) 
           call wrdac (datao, owner_code, i) 
           call wrdac (datao, owner_name, i) 
        endif
c
c       Write 3-terminal TX data
c
        i = kount (21) 
        if (i. gt. 0) then
           call wrdat (datao, tx_3term, 6*i) 
        endif
c
c       Write Tx ZDATA
c
        i = kount (22) 
        if (i. gt. 0) then
           call wrdat (datao, tx_zdata, 26*i) 
        endif
c
        return  
        end   
