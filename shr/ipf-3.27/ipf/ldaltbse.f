C    @(#)ldaltbse.f	20.10 3/29/99
C****************************************************************
C
C   File: ldaltbse.f
C   Purpose: IPF program to real alternate base case history 
C            file for bus voltages and line flow comparisons.
C
C   Author: Walt Powell  Date: 20 December 1992 
C   Called by: vltdifrpt.f, lfodifrpt.f
C
C****************************************************************
C
        integer function ldaltbse (tempfile, filename, caseid, loaded)
        character*(*) caseid, filename
        integer tempfile, loaded
c 
C       Input parameters:
C
C          FILENAME : The file name which has been opened
C
C          CASEID : ID of case in the file. "Blank" defaults to
C                   the first case.
C
c          LOADED : 0 when caseid was not found
c                   1 when caseid was located and loaded
c                   2 when the file is not a "PF DATA" file
C
C          Return status : 0 = success
C                          1 = failure
C

        include 'ipfinc/parametr.inc'
  
        include 'ipfinc/prt.inc'
        include 'ipfinc/alt_case.inc'
        include 'ipfinc/alt_flag.inc'

        common /scratch/ scratch(22*MAXBUS)
        character oldarczns(MAXCAZR,MAXCAR) * 2, junk_c8(MAXBUS) * 8,
     &            oldarczns2(50,MAXCAR) * 2
        integer junk_i4(2*MAXBRN)
        real junk_r4(2*MAXBRN)
        double precision temp(MAXYE), x22_temp(22,MAXXDT), 
     &       x2_temp(2,MAXBUS), 
     &       x8_temp(8,MAXTBX), x10_temp(10,MAXTIE), 
     &       x45_temp(45,MAX2DC), x36_temp(36,MAXMDC)
        equivalence (scratch(MAXBUS+1), oldarczns, temp, x22_temp, 
     &               x2_temp, x8_temp, x10_temp, x45_temp, x36_temp,
     &               junk_r4, junk_i4, junk_c8, oldarczns2)

        common /is_batch / is_batch

        integer count(100), yptr, kount(100), nslkxx(4,10)
        character type * 10, case * 10, bdate * 10, clabel(2) * 10,
     &            pversn * 10, usrnam * 10
        character name * 80, tname * 80, backslash * 1
        logical found

        backslash = char(92)

        ldaltbse = 0
        alt_base_loaded = 1
        tbx_flag = .false.

C       binary file (oldbase)

        read (tempfile, end=200, err=300) type, case, bdate, clabel,
     &        pversn, usrnam, count  

        if (type .ne. 'PF DATA' ) go to 300  ! not a "pf data" file  
        if (case .eq. 'END') go to 300    ! e-o-i encountered.
        if ( caseid .eq. ' ' ) caseid = case 
        if ( caseid .ne. case ) go to 200 ! case not fount.
c
c       Determine if case version is compatible with prog vers 
c
        icase_vers = count(2)
c
        if (icase_vers .lt. 4) then
           write (errbuf(1),21) pversn
  21       format(' Base case is old format, version ', a)
           call prterx ('W', 1)
           go to 320
        endif

        loaded = 1

        read (tempfile) ontot, ontot2, oltot, ntota, ontotb, 
     &                  ontotc, okdtot, omtdcbs, omtdcln, okxtot, 
     &                  bmva, ojtie, yptr, kbsknt, kbrknt, 
     &                  jphno, onztot, nspare, ntotcs, nbslck, 
     &                  nslkxx, kount

        oltot2 = kount(10)
        ontot_alf = kount(14)
        name = ' '
        obasval(2) = ' '
        obasval(3) = ' '
        inquire(unit=tempfile,name=name) 
        i = index (name, ':')
        if (i .gt. 1) then
           obasval(2) = name(1:i-1)
           tname = name(i+1:)
           name = tname
        endif
        if ( is_it_vms() .eq. 1 ) then
           i = index (name, ']')
           if (i .gt. 1) then
              obasval(3) = name(1:i)
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
              obasval(3) = name(1:i)
              tname = name(i+1:)
              name = tname
           endif
        endif
        obasval(1) = name  

        obasval(4) = case  
        obasval(5) = bdate 

        ihr = kount(7)/10000  
        imin = mod (kount(7)/100, 100)
        isec = mod (kount(7), 100)
        write (obasval(6), 22) ihr, imin, isec 
   22   format (i2, ':', i2, ':', i2)  

        obasval(7) = clabel(1) // clabel(2)
        obasval(8) = pversn
        obasval(9) = usrnam

c       bus names, kv, zone code, ownership records  

        call rddac (tempfile, oldbus, ontot) 
        call rddat (tempfile, oldbase, ontot)
        call rddac (tempfile, oldzone, ontot)
        call rddac (tempfile, oldowner, ontot)

c       bus data records   

        call rddat (tempfile, okbsdta, ontot * 16)   
        call rddat (tempfile, ohtable_b, BUS_HASHSIZE)   
        if ( icase_vers .lt. 8 ) then
           call rddat (tempfile, onextptr_b, 6000)    ! Bus hash tables
        else
           call rddat (tempfile, onextptr_b, ontot)   ! Bus hash tables
        endif

c       [conditional] continuation bus 

        if (ontot2 .gt. 0) then  
           call rddat (tempfile, obctbl, ontot2 * 12) 
           call rddat (tempfile, obctbl_nxt(1), ontot2) 
        endif 

c       [conditional] interchange controls records 

        if (ontotc .gt. 0) then  
           call rddac (tempfile, oarcnam, ontotc)  
           call rddac (tempfile, junk_c8, ontotc)  
           call rddat (tempfile, junk_r4, ontotc)  
           call rddat (tempfile, oarcnet, ontotc)  
           if (icase_vers .eq. 4) then
              call rddac (tempfile, oldarczns, ontotc * MAXCAZR)   
              do i = 1, ontotc
                 do j = 1, MAXCAZR
                    oarczns(j,i) = oldarczns(j,i)
                 enddo
                 do j = MAXCAZR+1, MAXCAZ
                    oarczns(j,i) = ' '
                 enddo
              enddo
           else if ( icase_vers .lt. 9 ) then
              call rddac (tempfile, oldarczns2, ontotc * 50)   
              do i = 1, ontotc
                 do j = 1, 50
                    oarczns(j,i) = oldarczns2(j,i)
                 enddo
                 do j = 51, MAXCAZ
                    oarczns(j,i) = ' '
                 enddo
              enddo
           else
              call rddac (tempfile, oarczns, ontotc * MAXCAZ)   
           endif
           call rddat (tempfile, junk_i4, onztot)
           call rddac (tempfile, oacznam, onztot)  
           call rddat (tempfile, oarzn, ontot)
           call rddat (tempfile, junk_r4, 8*ontotc)
        endif 

c       [conditional] switched reactive data record  

        if (okxtot .gt. 0) then  
           call rddat (tempfile, oxdata, okxtot * 22)
        endif 

c       branch data record

        call rddat (tempfile, obrnch, oltot * 18)  
        call rddat (tempfile, orateln, oltot * 3)  
        call rddat (tempfile, okx(1), oltot2)  
        call rddat (tempfile, oky(1), oltot2)  
        call rddac (tempfile, obrid(1), oltot2)  
        call rddat (tempfile, obrsect(1), oltot2)  
        call rddat (tempfile, obrtype(1), oltot2)  
        call rddat (tempfile, obrnch_nxt(1), oltot2)  
        call rddat (tempfile, obrnch_ptr(1), oltot2)  
C
c       internal, external bus numbers e + jf voltage tables 
c       capacitor correction tables   
C
        call rddat (tempfile, oalf2inp, ontot)   
        call rddat (tempfile, oinp2alf, ontot)   
        call rddat (tempfile, junk_i4, ontot)   
        call rddat (tempfile, oinp2opt, ontot)   
        call rddat (tempfile, olde, ontot)   
        call rddat (tempfile, oldf, ontot)   
        call rddat (tempfile, junk_r4, 2*ontot)   
C
c       conditional] phase shifter data record   

        if (jphno.gt.0) then  
           call rddat (tempfile, junk_r4, 8*jphno)   
        endif 

C       Y-matrix and solution arrays
c
        call rddat (tempfile, junk_i4, ontot)   
        call rddat (tempfile, junk_i4, ontot)   
        call rddat (tempfile, opnetu, ontot)        
        call rddat (tempfile, oqnetu, ontot)        
        call rddat (tempfile, oinetr, ontot)   
        call rddat (tempfile, oineti, ontot)   
        call rddat (tempfile, oploadu, ontot)       
        call rddat (tempfile, oqloadu, ontot)       
        call rddat (tempfile, junk_r4, ontot)   
        call rddat (tempfile, junk_r4, ontot)   
        call rddat (tempfile, junk_r4, ontot)   
        call rddat (tempfile, junk_r4, ontot)   
        call rddat (tempfile, ontypu, ontot)        
        call rddat (tempfile, junk_i4, ontot)   
        call rddat (tempfile, junk_i4, yptr)   
        call rddat (tempfile, junk_r4, yptr)   
        call rddat (tempfile, junk_r4, yptr)   
C
c       conditional] special bus data 
C
        if (ontotb.gt.0) then  
           call rddat (tempfile, otbx, ontotb * 8)
        endif 
C
c       [conditional] automatic transformer  
C
        if (ntota.gt.0) then  
           call rddat (tempfile, junk_r4, 12*ntota)   
           call rddat (tempfile, junk_r4, ntota)   
        endif 
C
c       [conditional] area interchange   
C
        if (ojtie .gt. 0) then
           call rddat (tempfile, otie, ojtie * 10)   
        endif 
C
c       [conditional] dc line modeling records   
C
        if (okdtot .gt. 0) then
           call rddat (tempfile, odc2t, okdtot * 45) 
        endif 
        if (omtdcbs.gt.0) then 
           call rddat (tempfile, odcmtbs, omtdcbs * 36)  
        endif 
        if (omtdcln.gt.0) then 
           call rddat (tempfile, odcmtln, omtdcln * 10)  
        endif 

        nztotx = kount(1)
        ojowner = kount(2)
        if (nztotx. gt. 0)  then
           if (icase_vers .le. 6) then
              read (tempfile) (ozsum(5,i),ozsum(6,i) ,i=1, nztotx)
           else
              call rddat (tempfile, ozsum, nztotx * 26)
           endif
        endif 
        if (ojowner.gt.0) then 
           call rddac (tempfile, oownnam, ojowner) 
           call rddat (tempfile, oownlos, ojowner * 2)   
        endif 

        i = kount(3) 
        if (i.gt.0) call rddat (tempfile, junk_r4, i)   

        ontotic = kount(4) 
        if (ontotic.gt.0) then 
           call rddat (tempfile, oarcinp, ontotic)   
           call rddac (tempfile, oarcint, 2 * ontotic) 
        endif 

        ocase = case
        ofilename = filename
        close(tempfile)
        go to 900   
C
c       case not found, or end-of-file 
C
  200   loaded = 0  
        ldaltbse = 1
        alt_base_loaded = 0
        ofilename = ' '
        ocase = ' '
        ontot = 0
        ierr = 0  
        write (errbuf(1),210) caseid, ierr   
  210   format(' Case ', a10, ' not found. (Error no. ',i3,'.) ')
        if (is_batch .eq. 0) then
           call prterx ('E',1)
        else
           call prterx ('F',1)
        endif
        go to 900
c
c       data file incompatible with pf
c
  300   loaded = 2
        ldaltbse = 1
        alt_base_loaded = 0
        ofilename = ' '
        ocase = ' '
        ontot = 0
        inquire (unit=tempfile, iostat=ierr)
        name = ' '
        inquire(unit=tempfile,name=name)
        last = lastch(name)
        write (errbuf(1),310) name(1:last), ierr
  310   format(' Requested base file ',a,
     1         ' is not valid. (Error no. ',i3,')')
        if (is_batch .eq. 0) then
           call prterx ('E',1)
        else
           call prterx ('F',1)
        endif
        go to 900
c
c       Old formatted data file not acceptable for alternate base
c
  320   loaded = 2
        ldaltbse = 1
        alt_base_loaded = 0
        ofilename = ' '
        ocase = ' '
        ontot = 0
        inquire (unit=tempfile, iostat=ierr)
        name = ' '
        inquire(unit=tempfile,name=name)
        last = lastch(name)
        write (errbuf(1),330) name(1:last)
  330   format(' Requested alternate base file ', a,
     1         ' is not an IPF base file.')
        if (is_batch .eq. 0) then
           call prterx ('E',1)
        else
           call prterx ('F',1)
        endif

  900   return
        end
