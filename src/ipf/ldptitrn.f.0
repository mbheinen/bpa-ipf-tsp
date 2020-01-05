C    @(#)ldptitrn.f	20.4 5/27/98 
C**************************************************************** 
C 
C     File: ldptitrn.f 
C 
C     Purpose: Routine to load PTI translation file 
C 
C     Author: Walt Powell  Date: 21 May 1996 
C     Called by: net_data_sub.f 
C 
C**************************************************************** 
        integer function ldptitrn (scrfil, filename, numver, error) 
        integer scrfil, numver, error 
        character *(*) filename 
 
      include 'ipfinc/parametr.inc'
 
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/arcntl.inc'
      include 'ipfinc/pti_data.inc'
      include 'ipfinc/qksrt.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/basval.inc'
      include 'ipfinc/owner_cm.inc'
  
        integer MAXPTIRECORDS
        parameter (MAXPTIRECORDS = 16000)
        common /scratch/ count, array(4,MAXBUS), htable_2(MAXBUS), 
     &                   nextptr_2(MAXBUS), count_newbus,  
     &                   newbusno(MAXBUS), count_newzone,  
     &                   newzoneno(MAXCZN), count_newown,  
     &                   newownno(MAXOWN), tempc(MAXPTIRECORDS),
     &                   sort_tempc(MAXPTIRECORDS)
        integer array, count, htable_2, count_newbus, newbusno, 
     &          count_newzone, newzoneno, count_newown, newownno,
     &          sort_tempc
        character tempc*80
 
        character xbuf*120, busname*8, areaname*10, zonename*2, bell*1, 
     &            ownercode*3, ownername*32, longname*32 
        logical finished 
        integer status, add_ptim, add_ptia, add_ptiz, find_bus,  
     &          chk_ptib, hsh_ptib, find_zon, numowner, add_ptio, 
     &          ownsch, buildzbo 
        external kmp_ptib, swp_ptib 
 
        error = 0           ! initialize error count 
        numrec = 0 
        ldptitrn = 0 
        bell = char(7) 
        num_found = 0 
        num_notfound = 0 
        count_newbus = 0 
        count_newzone = 0 
        count_newown = 0 
c 
c       Initialize PTI hash arrays 
c 
        do i = 1, PTI_MAXBUS 
          nextptr_n(i) = 0 
          nextptr_b(i) = 0 
        enddo 
        do i = 1, MAXCZN 
          htable_y(i) = 0 
          htable_z(i) = 0 
          nextptr_y(i) = 0 
          nextptr_z(i) = 0 
        enddo 
        do i = 1, MAXCAR 
          htable_a(i) = 0 
          htable_c(i) = 0 
          nextptr_a(i) = 0 
          nextptr_c(i) = 0 
        enddo 
        do i = 1, MAXOWN 
          htable_o(i) = 0 
          htable_q(i) = 0 
          nextptr_o(i) = 0 
          nextptr_q(i) = 0 
        enddo 
        status = buildzbo(status) 
c 
c       Read in and hash bus data in *.TRN file 
C 
        numrec = 0 
        if (filename .ne. ' ') then 
          write (*, 10000) bell 
10000     format(1x, a, '* Loading PTI Translation File - this will take 
     & a few minutes') 
          finished = .false. 
          do while (.not. finished) 
            read (scrfil, fmt='(a)', end=140) xbuf 
            read (xbuf, 10010, err=120) numbus, busname, basekv,  
     &         numarea, numzone 
10010       format (1x, i5, 2x, a8, f4.0, i4, 20x, i4) 
            numrec = numrec + 1 
            num = add_ptim (numbus, busname, basekv, numarea) 
            if (num .gt. 0) then 
              pti_name(num) = busname 
              pti_base(num) = basekv 
              pti_zone(num) = numzone 
              pti_area(num) = numarea 
            endif 
            kold = find_bus(busname, basekv) 
            if (kold .gt. 0) then 
              num_found = num_found + 1 
              if (num .lt. 0) then 
                num = -num 
                write (errbuf(1), 10020) numbus 
10020           format ('Duplicate bus numbers (', i5,  
     &            ') in *.TRN file') 
                last = min0 (60, lastch(xbuf)) 
                write (errbuf(2), 10030) xbuf(1:last) 
10030           format ('Subsequent records ignored [', a, ']') 
                call prterx ('W', 2) 
                error = error + 1 
              endif 
 
            else if (busname .eq. ' ') then 
              finished = .true. 
            else 
              num_notfound = num_notfound  + 1 
            endif 
 
            go to 130 
 
  120       write (errbuf(1), 10040) xbuf(1:60) 
10040       format ('Error decoding bus record in *.TRN file ', a) 
            call prterx ('W', 1) 
            error = error + 1 
 
  130       continue 
          enddo 
          go to 150 
 
  140     write (errbuf(1), 10050)  
10050     format ('E-O-F encountered processing bus records in *.TRN fil 
     &e ') 
          call prterx ('W', 1) 
 
  150     continue 
c 
c         Read in and hash area data in *.TRN file 
C 
          finished = .false. 
          do while (.not. finished) 
            read (scrfil, fmt='(a)', end=200) xbuf 
            read (xbuf, 10060, err=180) numarea, areaname 
10060       format (i4, 2x, a10) 
            numrec = numrec + 1 
            num = add_ptia (numarea, areaname) 
            if (num .eq. 0) then 
              finished = .true. 
            else if (num .lt. 0) then 
              error = error + 1 
              write (errbuf(1), 10070) numarea, areaname 
10070         format ('Duplicate areas (', i4, 1x, a10, ') in *.TRN file 
     & ') 
              last = min0 (60, lastch(xbuf)) 
              write (errbuf(2), 10030) xbuf(1:last) 
              call prterx ('W', 2) 
            else 
            endif 
            go to 190 
 
  180       write (errbuf(1), 10080) xbuf(1:20) 
10080       format ('Error decoding area records in *.TRN file ', a) 
            call prterx ('W', 1) 
            error = error + 1 
 
  190       continue 
          enddo 
          go to 210 
 
  200     write (errbuf(1), 10090)  
10090     format ('E-O-F encountered processing area records in *.TRN fi 
     &le ') 
          call prterx ('W', 1) 
          error = error + 1 
 
  210     continue 
c 
c         Read in and hash zone data in *.TRN file 
C 
          finished = .false. 
          do while (.not. finished) 
            read (scrfil, fmt='(a)', end=260) xbuf 
            read (xbuf, 10100, err=240) numzone, zonename, longname 
10100       format (i4, 2x, a2, 2x, a32) 
            numrec = numrec + 1 
            if (numzone .eq. 0) then 
              finished = .true. 
            else 
              num = find_zon(zonename) 
              if (num .le. 0) then 
                error = error + 1 
                write (errbuf(1), 10110) numzone, zonename 
10110           format ('Pti zone number (', i3, '), name (', a,  
     & ') in *.TRN file is not in system. Zone number is reserved.') 
                call prterx ('W', 1) 
              else 
                num = add_ptiz (numzone, zonename) 
                if (num .eq. 0) then 
                  finished = .true. 
                else if (num .lt. 0) then 
                  error = error + 1 
                  write (errbuf(1), 10120) numzone, zonename 
10120             format ('Duplicate zones (', i4, 1x, a,  
     &              ') in *.TRN file') 
                  last = min0 (60, lastch(xbuf)) 
                  write (errbuf(2), 10030) xbuf(1:last) 
                  call prterx ('W', 2) 
                else 
                  zone_name(num) = longname 
                endif 
              endif 
            endif 
            go to 250 
 
  240       write (errbuf(1), 10130) xbuf(1:20) 
10130       format ('Error decoding zone records in *.TRN file ', a) 
            call prterx ('W', 1) 
            error = error + 1 
 
  250       continue 
 
          enddo 
          go to 270 
 
  260     write (errbuf(1), 10140)  
10140     format ('E-O-F encountered processing zone records in *.TRN fi 
     &le ') 
          call prterx ('W', 1) 
          error = error + 1 
 
  270     continue 
c 
c         Read in and hash onwer data in *.TRN file 
C 
          finished = .false. 
          do while (.not. finished) 
            read (scrfil, fmt='(a)', end=300) xbuf 
            read (xbuf, 10150, err=280) numowner, ownercode, ownername 
10150       format (i4, 2x, a3, 2x, a32) 
            numrec = numrec + 1 
            if (numowner .eq. 0) then 
              finished = .true. 
            else 
              num = ownsch(ownercode) 
              if (num .le. 0) then 
                error = error + 1 
                write (errbuf(1), 10160) numowner, ownercode 
10160           format ('Pti owner number (', i3, '), name (', a,  
     & ') in *.TRN file is not in system. Owner number is reserved.') 
                call prterx ('W', 1) 
              else 
                num = add_ptio (numowner, ownercode) 
                if (num .eq. 0) then 
                  finished = .true. 
                else if (num .lt. 0) then 
                  error = error + 1 
                  write (errbuf(1), 10170) numowner, ownercode 
10170             format ('Duplicate owners (', i4, 1x, a,  
     &              ') in *.TRN file') 
                  last = min0 (60, lastch(xbuf)) 
                  write (errbuf(2), 10030) xbuf(1:last) 
                  call prterx ('W', 2) 
                else 
                  owner_number(num) = numowner 
                  owner_code(num) = ownercode 
                  owner_name(num) = ownername 
                endif 
              endif 
            endif 
            go to 290 
 
  280       write (errbuf(1), 10180) xbuf(1:20) 
10180       format ('Error decoding owner records in *.TRN file ', a) 
            call prterx ('W', 1) 
            error = error + 1 
 
  290       continue 
 
          enddo 
          go to 310 
 
  300     write (errbuf(1), 10190)  
10190     format ('E-O-F encountered processing owner records in *.TRN f 
     &ile ') 
          call prterx ('W', 1) 
          error = error + 1 
 
  310     continue 
c 
c         Hash the bus names, identify any duplicates, and generate  
c         an alpha cross reference 
c 
          status = chk_ptib() 
          status = hsh_ptib() 
 
          last = lastch (filename) 
          write (outbuf, 10200) numrec, error, filename(1:last) 
10200     format (1x, i5, ' records and ', i5,  
     &' errors were encountered in file ', a) 
          call prtout (1) 
          write (outbuf, 10210) num_found+num_notfound, num_found 
10210     format (' Translation file bus entities: ', i5, ' matched ',  
     &      i5) 
          call prtout (1) 
        endif 
 
        return 
        end 
