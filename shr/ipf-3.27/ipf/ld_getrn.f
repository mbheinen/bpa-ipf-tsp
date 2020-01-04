C    @(#)ld_getrn.f	20.13 5/3/00
C**************************************************************** 
C 
C     File: ld_getrn.f 
C 
C     Purpose: Routine to load GE translation file 
C 
C     Author: Walt Powell  Date: 20 May 1999 
C     Called by: net_data_sub.f 
C 
C**************************************************************** 
        integer function ld_getrn (scrfil, filename, numver, error) 
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
 
        common /bpa_num / user_rule, num_area_rule, num_zone_rule, 
     &                    num_owner_rule, num_default_rule, 
     &                    area_rule(3,MAXCAR), zone_rule(3,MAXZON), 
     &                    owner_rule(3,MAXOWN), default_rule(2,100), 
     &                    owner_code_ge(MAXOWN)
        integer user_rule, num_area_rule, num_zone_rule, num_owner_rule,
     &          num_default_rule, area_rule, zone_rule, owner_rule, 
     &          default_rule
        character owner_code_ge*4

        integer   MAXOWNERS
        parameter (MAXOWNERS = 250)    !max # local symbols

        character xbuf*120, busname*8, areaname*10, zonename*2, bell*1, 
     &            ownercode*3, ownername*32, longname*32, word(10)*60
        logical finished 
        integer status, add_ptim, add_ptia, add_ptiz, find_bus,  
     &          chk_ptib, hsh_ptib, find_zon, numowner, add_ptio, 
     &          ownsch, buildzbo, ftn_atoi
        external kmp_ptib, swp_ptib 
 
        error = 0           ! initialize error count 
        numrec = 0 
        ld_getrn = 0 
        bell = char(7) 
        num_found = 0 
        num_notfound = 0 
        count_newbus = 0 
        count_newzone = 0 
        count_newown = 0 
        num_area_rule = 0
        num_zone_rule = 0
        num_owner_rule = 0

        if (filename .ne. ' ') then 
          write (*, 10000) 
10000     format(1x, '* Loading GE Translation File - this will take a f
     &ew minutes') 
        else
          write (*, 10010) 
10010     format(1x, '* No translation file specified')
          ld_getrn = 1
          go to 900
        endif
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
c       Read in and hash area data in *.TRN file 
c 
        finished = .false. 
        do while (.not. finished) 
          read (scrfil, fmt='(a)', end=100) xbuf 
          if (xbuf(1:1) .eq. '#' .or. xbuf(1:2) .eq. ' #') then
          else
            last = lastch (xbuf)
            call uscan (xbuf(1:last), word, nwrd, '=',  ' ,')   
            numarea = ftn_atoi (word(1))
            areaname = word(2)
            numrec = numrec + 1 
            num = add_ptia (numarea, areaname) 
            if (num .eq. 0) then 
              finished = .true. 
            else if (num .lt. 0) then 
              error = error + 1 
              write (errbuf(1), 10020) numarea, areaname 
10020         format ('Duplicate areas (', i4, 1x, a10, ') in *.TRN file
     &') 
              last = min0 (60, lastch(xbuf)) 
              write (errbuf(2), 10030) xbuf(1:last) 
10030         format ('Subsequent records ignored [', a, ']') 
              call prterx ('W', 2) 
            endif 
          endif
        enddo 
        go to 110 
 
  100   write (errbuf(1), 10040)  
10040   format ('E-O-F encountered processing area records in *.TRN file
     & ') 
        call prterx ('W', 1) 
        error = error + 1 
 
  110   continue 
c 
c       Read in and hash zone data in *.TRN file 
C 
        finished = .false. 
        do while (.not. finished) 
          read (scrfil, fmt='(a)', end=120) xbuf 
          if (xbuf(1:1) .eq. '#' .or. xbuf(1:2) .eq. ' #') then
          else
            last = lastch (xbuf)
            call uscan (xbuf(1:last), word, nwrd, '=',  ' ,')   
            numzone = ftn_atoi (word(1))
            zonename = word(2)
            longname = word(3)
            numrec = numrec + 1 
            if (numzone .eq. 0) then 
              finished = .true. 
            else 
              num = add_ptiz (numzone, zonename) 
              if (num .eq. 0) then 
                finished = .true. 
              else if (num .lt. 0) then 
                error = error + 1 
                write (errbuf(1), 10060) numzone, zonename 
10060           format ('Duplicate zones (', i4, 1x, a,  
     &            ') in *.TRN file') 
                last = min0 (60, lastch(xbuf)) 
                write (errbuf(2), 10030) xbuf(1:last) 
                call prterx ('W', 2) 
              else 
                zone_name(num) = longname 
              endif 
            endif 
          endif
        enddo 

        go to 130 
 
  120   write (errbuf(1), 10070)  
10070   format ('E-O-F encountered processing zone records in *.TRN file
     & ') 
        call prterx ('W', 1) 
        error = error + 1 
 
  130   continue 
c 
c       Read in and hash onwer data in *.TRN file 
C 
        finished = .false. 
        do while (.not. finished) 
          read (scrfil, fmt='(a)', end=140) xbuf 
          if (xbuf(1:1) .eq. '#' .or. xbuf(1:2) .eq. ' #') then
          else
            last = lastch (xbuf)
            call uscan (xbuf(1:last), word, nwrd, '=',  ' ,')   
            numowner = ftn_atoi (word(1))
            ownercode = word(2)
            ownername = word(3)
            longname = word(4)
            numrec = numrec + 1 
            if (numowner .eq. 0) then 
              finished = .true. 
            else 
              num = add_ptio (numowner, ownercode) 
              if (num .eq. 0) then 
                finished = .true. 
              else if (num .lt. 0) then 
                error = error + 1 
                write (errbuf(1), 10090) numowner, ownercode 
10090           format ('Duplicate owners (', i4, 1x, a,  
     &            ') in *.TRN file') 
                last = min0 (60, lastch(xbuf)) 
                write (errbuf(2), 10030) xbuf(1:last) 
                call prterx ('W', 2) 
              else 
                owner_number(num) = numowner 
                owner_code(num) = ownercode 
                owner_code_ge(num) = ownername
                owner_name(num) = longname 
              endif 
            endif 
          endif
        enddo 
        go to 150 
 
  140   write (errbuf(1), 10100)  
10100   format ('E-O-F encountered processing owner records in *.TRN fil
     &e ') 
        call prterx ('W', 1) 
        error = error + 1 
 
  150   continue 
        do i = num_onam+1, MAXOWNERS
          owner_number(i) = 0
          owner_code(i) = char (0)
          owner_code_ge(i) = char (0)
          owner_name(i) = char (0)
        enddo
c 
c       Read in and hash bus data in *.TRN file 
C 
        finished = .false. 
        do while (.not. finished) 
          read (scrfil, fmt='(a)', end=160) xbuf 
          if (xbuf(1:1) .eq. '#' .or. xbuf(1:2) .eq. ' #') then
          else
            last = lastch (xbuf)
            call uscan (xbuf(1:last), word, nwrd, '=',  ' ,')   
            numbus = ftn_atoi (word(1))
            if (numbus .eq. 0) then 
              finished = .true.
            else
              busname = word(2)
              basekv = ftn_atof (word(3))
              numarea = ftn_atoi (word(4))
              numzone = ftn_atoi (word(5))
              numowner = ftn_atoi (word(6))
              numrec = numrec + 1 
              num = add_ptim (numbus, busname, basekv, numarea) 
              if (num .gt. 0) then 
                pti_name(num) = busname 
                pti_base(num) = basekv 
                pti_zone(num) = numzone 
                pti_area(num) = numarea 
                pti_owner(num) = numowner
              else
                write (errbuf(1), 10110) numbus 
10110           format ('Duplicate bus numbers (', i5,  
     &            ') in *.TRN file') 
                last = min0 (60, lastch(xbuf)) 
                write (errbuf(2), 10030) xbuf(1:last) 
                call prterx ('W', 2) 
                error = error + 1 
              endif 
            endif 
          endif
        enddo 
        go to 170 
 
  160   write (errbuf(1), 10120)  
10120   format ('E-O-F encountered processing bus records in *.TRN file 
     &') 
        call prterx ('W', 1) 
 
  170   continue 
c 
c       Read in area rules in *.TRN file 
C 
        finished = .false. 
        do while (.not. finished) 
          read (scrfil, fmt='(a)', end=180) xbuf 
          if (xbuf(1:1) .eq. '#' .or. xbuf(1:2) .eq. ' #') then
          else
            last = lastch (xbuf)
            call uscan (xbuf(1:last), word, nwrd, '=',  ' ,')   
            num = ftn_atoi (word(1))
            if (num_area_rule .ge. MAXCAR) then
              write (errbuf(1), 10130) MAXCAR, num
10130         format ('More than ', i4, ' area rules. Overflow at area '
     &, i4)
              call prterx ('W', 1) 
              error = error + 1 
            else
              if (num .eq. 0) then
                finished = .true.
              else
                num_area_rule = num_area_rule + 1
                area_rule(1,num_area_rule) = num
                area_rule(2,num_area_rule) = ftn_atoi(word(2))
                area_rule(3,num_area_rule) = ftn_atoi(word(3))
              endif
            endif
          endif
        enddo 
        go to 190 
 
  180   write (errbuf(1), 10140)  
10140   format ('E-O-F encountered processing area rules in *.TRN file 
     &') 
        call prterx ('W', 1) 
 
  190   continue 
c 
c       Read in zone rules in *.TRN file 
C 
        finished = .false. 
        do while (.not. finished) 
          read (scrfil, fmt='(a)', end=200) xbuf 
          if (xbuf(1:1) .eq. '#' .or. xbuf(1:2) .eq. ' #') then
          else
            last = lastch (xbuf)
            call uscan (xbuf(1:last), word, nwrd, '=',  ' ,')   
            num = ftn_atoi (word(1))
            if (num_zone_rule .ge. MAXZON) then
              write (errbuf(1), 10150) MAXZON, num
10150         format ('More than ', i4, ' zone rules. Overflow at zone '
     &, i4)
              call prterx ('W', 1) 
              error = error + 1 
            else
              if (num .eq. 0) then
                finished = .true.
              else
                num_zone_rule = num_zone_rule + 1
                zone_rule(1,num_zone_rule) = num
                zone_rule(2,num_zone_rule) = ftn_atoi(word(2))
                zone_rule(3,num_zone_rule) = ftn_atoi(word(3))
              endif
            endif
          endif
        enddo 
        go to 210 
 
  200   write (errbuf(1), 10160)  
10160   format ('E-O-F encountered processing zone rules in *.TRN file 
     &') 
        call prterx ('W', 1) 
 
  210   continue 
c 
c       Read in owner rules in *.TRN file 
C 
        finished = .false. 
        do while (.not. finished) 
          read (scrfil, fmt='(a)', end=220) xbuf 
          if (xbuf(1:1) .eq. '#' .or. xbuf(1:2) .eq. ' #') then
          else
            last = lastch (xbuf)
            call uscan (xbuf(1:last), word, nwrd, '=',  ' ,')   
            num = ftn_atoi (word(1))
            if (num_owner_rule .ge. MAXOWN) then
              write (errbuf(1), 10170) MAXOWN, num
10170         format ('More than ', i4, ' owner rules. Overflow at owner
     & ', i4)
              call prterx ('W', 1) 
              error = error + 1 
            else
              if (num .eq. 0) then
                finished = .true.
              else
                num_owner_rule = num_owner_rule + 1
                owner_rule(1,num_owner_rule) = num
                owner_rule(2,num_owner_rule) = ftn_atoi(word(2))
                owner_rule(3,num_owner_rule) = ftn_atoi(word(3))
              endif
            endif
          endif
        enddo 
        go to 230 
 
  220   write (errbuf(1), 10180)  
10180   format ('E-O-F encountered processing owner rules in *.TRN file 
     &') 
        call prterx ('W', 1) 
 
  230   continue 
c 
c       Read in default rules in *.TRN file 
C 
        finished = .false. 
        do while (.not. finished) 
          read (scrfil, fmt='(a)', end=240) xbuf 
          if (xbuf(1:1) .eq. '#' .or. xbuf(1:2) .eq. ' #') then
          else
            last = lastch (xbuf)
            call uscan (xbuf(1:last), word, nwrd, '=',  ' ,')   
            num = ftn_atoi (word(1))
            if (num_default_rule .ge. 100) then
              write (errbuf(1), 10190) 100, num
10190         format ('More than ', i4, ' default rules. Overflow at def
     &ault ', i4)
              call prterx ('W', 1) 
              error = error + 1 
            else
              if (num .eq. 0) then
                finished = .true.
              else
                num_default_rule = num_default_rule + 1
                default_rule(1,num_default_rule) = ftn_atoi(word(1))
                default_rule(2,num_default_rule) = ftn_atoi(word(2))
              endif
            endif
          endif
        enddo 
        go to 250 
 
  240   write (errbuf(1), 10200)  
10200   format ('E-O-F encountered processing default rules in *.TRN fil
     &e ') 
        call prterx ('W', 1) 
 
  250   continue 
c 
c       Hash the bus names, identify any duplicates, and generate  
c       an alpha cross reference 
c 
        status = chk_ptib() 
        status = hsh_ptib() 
 
        last = lastch (filename) 
        write (outbuf, 10210) numrec, error, filename(1:last) 
10210   format (1x, i5, ' records and ', i5,  
     &' errors were encountered in file ', a) 
        call prtout (1) 
        write (outbuf, 10220) num_found+num_notfound, num_found 
10220   format (' Translation file bus entities: ', i5, ' matched ',  
     &      i5) 
        call prtout (1) 
 
  900   continue
        return 
        end 
