C    @(#)save_get.f	20.4 5/3/00
C****************************************************************
C
C     File: save_get.f
C
C     Purpose: Routine to save GE translation file
C
C     Author: Walt Powell  Date: 25 May 1999
C     Called by: net_data_sub.f
C
C****************************************************************
        integer function save_get (scrfil, filename, newfil, 
     &                             newfilename)
        integer scrfil, newfil, numver
        character *(*) filename, newfilename

        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
        include 'ipfinc/pti_data.inc'
        include 'ipfinc/qksrt.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/basval.inc'
        include 'ipfinc/owner_cm.inc' 
        include 'ipfinc/arcntl.inc' 
 
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

        character xbuf*120, busname*8, areaname*10, zonename*2, 
     &            bell*1, apostrophe*1, word(10)*32, longname*32, 
     &            ownercode*3, ownercode_ge*4, month(12)*3, 
     &            weekday(7)*3
        logical finished, eof, opened, inc_loop1, inc_loop2
        integer count0, count1, count2, count3, count4, ftn_atoi,
     &          error, fnd_ptian, fnd_ptiz, fnd_ptio
        external kmp_ptib, swp_ptib

        data month / 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul',
     &             'Aug', 'Sep', 'Oct', 'Nov', 'Dec' /
        data weekday / 'Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat' /

        error = 0           ! initialize error count
        bell = char(7)
        apostrophe = ''''
        eof = .false.
        save_get = 0
c
c       Create an updated Translation File
c
        if (newfilename .ne. ' ') then
          write (*, 10000) bell
10000     format(1x, a, '* Creating a new GE Translation File')

          call nx_date (imon, idate, iyear, iwk_day)
          if (iyear .lt. 40) then
            iyear = iyear + 2000
          else
            iyear = iyear + 1900
          endif
          call n_time (ihour, imin, isec)

          inquire (unit=scrfil, name=xbuf)
          last = lastch (xbuf)

          write (newfil, 10010) xbuf(1:last)
10010     format ('# Translation file created in ', a)

          write (newfil, 10012) weekday(iwk_day), month(imon), idate, 
     &                          ihour, imin, isec, iyear
10012     format ('#                  Date       ', a, 1x, a, 1x, 
     &      i2, 1x, i2.2, ':', i2.2, ':', i2.2, 1x, i4)

          write (newfil, 10020) '11.1'
10020     format ('#                  Version    ', a)

          last1 = lastch(filename)

          write (newfil, 10030) filename(1:last1)
10030     format ('# Source - Translation file:   ', a)

          last1 = lastch(basval(1))
          last2 = lastch(basval(2))
          last3 = lastch(basval(3))

          write (newfil, 10040) basval(2)(1:last2) // ':', 
     &      basval(3)(1:last3), basval(1)(1:last1)
10040     format ('#          IPF history file:   ', a, a, a)

          last1 = lastch(basval(4))
          last2 = lastch(basval(5))
          last3 = lastch(basval(6))
          last4 = lastch(basval(7))

          write (newfil, 10050) basval(4)(1:last1)
10050     format ('#          Case:               ', a)

          write (newfil, 10060) basval(7)(1:last4)
10060     format ('#          Description:        ', a)

          write (newfil, 10070) basval(5)(1:last2),
     &                          basval(6)(1:last3)
10070     format ('#          Date:               ', a, 1x, a)

          last1 = lastch(basval(9))
          last2 = lastch(basval(8))

          write (newfil, 10080) basval(9)(1:last1)
10080     format ('#          User:               ', a)

          write (newfil, 10090) basval(8)(1:last2)
10090     format ('#          PF Version:         ', a)

          inquire (unit=scrfil, opened=opened)
          eof = (.not. opened)
          rewind scrfil
c
c         **************************************************************
c         Process area data
c
c         Sort new PTI area numbers
c
          do i = 1, num_anam
            sort(i) = i
          enddo

          key = 8
          if (num_anam .gt. 1) then
            call qiksrt (1, num_anam, kmp_ptib, swp_ptib)
          endif
c
c         Read in old area number-name
c
          finished = .false.
          numptirec = 0
          
          inquire (unit=scrfil, opened=opened)
          eof = (.not. opened)
          do while (.not. finished .and. .not. eof)
            read (scrfil, fmt='(a)', end=310) xbuf
            if (xbuf(1:1) .eq. '#') then
            else
              last = lastch (xbuf)
              call uscan (xbuf(1:last), word, nwrd, '=',  ' ,')   
              numarea = ftn_atoi (word(1))
              if (numarea .eq. 0) then
                finished = .true.
              else 
                numptirec = numptirec + 1
                tempc(numptirec) = xbuf
                sort_tempc(numptirec) = numptirec
              endif
            endif
          enddo

          go to 320
  310     write (errbuf(1), 10110) xbuf(1:60)
10110     format ('E-O-F encountered reading area record in *.TRN file '
     &, a)
          call prterx ('W', 1)
          error = error + 1
  320     continue
c
c         Sort old PTI area numbers
c
          key = 202

          if (numptirec .gt. 0) then
            call qiksrt (1, numptirec, kmp_ptib, swp_ptib)
          endif
c
c         Merge old and new area number-name
c
          write (newfil, fmt='(a)') '#'
          write (newfil, fmt='(a)') '# - areas (1)'
          write (newfil, fmt='(a)') '#'
          write (newfil, fmt='(a)') '# num  name'
          write (newfil, fmt='(a)') '#'

          inc_loop1 = .true.
          inc_loop2 = .true.
          count1 = 0
          count0 = 0
          lsw = 1        ! 1 - Normal
c                        ! 2 - EOD old number-name
c                        ! 3 - EOD new number-name
c                        ! 4 - EOD old, new number-name

          do while (lsw .ne. 4)
c
c           Increment old area number-name data
c
            if (inc_loop1) then
              count1 = count1 + 1
              is1 = sort_tempc(count1)
              inc_loop1 = .false.
              if (count1 .gt. numptirec) then
                if (lsw .eq. 1 .or. lsw .eq. 3) lsw = lsw + 1
              endif
            endif
c
c           Increment new area number-name data
c
            if (inc_loop2) then
              count0 = count0 + 1
              is0 = sort(count0)
              inc_loop2 = .false.
              if (count0 .gt. num_anam) then
                if (lsw .eq. 1 .or. lsw .eq. 2) lsw = lsw + 2
              endif
            endif

            if (lsw .eq. 1) then
              last = lastch (tempc(is1))
              call uscan (tempc(is1)(1:last), word, nwrd, '=',  ' ,')   
              numarea = ftn_atoi (word(1))
              areaname = word(2)
              nextarea = pti_anum(is0) 
              kompar = numarea - nextarea
              if (kompar .eq. 0) then
                kompar = kompr (areaname, pti_anam(is0), junk)
              endif
              if (kompar .lt. 0) then
                write (newfil, 10120) numarea, areaname
10120           format (1x, i3, 2x, '"', a, 22x, '"')
                inc_loop1 = .true.
              else if (kompar .gt. 0) then
                write (newfil, 10120) nextarea, pti_anam(is0)
                inc_loop2 = .true.
              else
                write (newfil, 10120) numarea, areaname
                inc_loop1 = .true.
                inc_loop2 = .true.
              endif
            else if (lsw .eq. 2) then
              write (newfil, 10120) nextarea, pti_anam(is0)
              inc_loop2 = .true.
            else if (lsw .eq. 3) then
              last = lastch (tempc(is1))
              call uscan (tempc(is1)(1:last), word, nwrd, '=',  ' ,')   
              numarea = ftn_atoi (word(1))
              areaname = word(2)
              write (newfil, 10120) numarea, areaname
              inc_loop1 = .true.
            endif
          enddo

          write (newfil, fmt='(a)') ' 0'
c
c         **************************************************************
c         Process zone data
c
c         Sort new PTI zone numbers
c
          key = 6
          do i = 1, num_znam
            sort(i) = i
          enddo

          if (num_znam .gt. 1) then
            call qiksrt (1, num_znam, kmp_ptib, swp_ptib)
          endif
c
c         Read in zone number-name
c
          finished = .false.
          numptirec = 0
          
          inquire (unit=scrfil, opened=opened)
          eof = (.not. opened)
          do while (.not. finished .and. .not. eof)
            read (scrfil, fmt='(a)', end=330) xbuf
            if (xbuf(1:1) .eq. '#') then
            else
              last = lastch (xbuf)
              call uscan (xbuf(1:last), word, nwrd, '=',  ' ,')   
              numzone = ftn_atoi (word(1))
              if (numzone .eq. 0) then
                finished = .true.
              else 
                numptirec = numptirec + 1
                tempc(numptirec) = xbuf
                sort_tempc(numptirec) = numptirec
              endif
            endif
          enddo

          go to 340
  330     write (errbuf(1), 10130) xbuf(1:60)
10130     format ('E-O-F encountered reading zone record in *.TRN file '
     &, a)
          call prterx ('W', 1)
          error = error + 1
  340     continue
c
c         Sort old PTI zone numbers
c
          key = 203

          if (numptirec .gt. 0) then
            call qiksrt (1, numptirec, kmp_ptib, swp_ptib)
          endif
c
c         Merge old and new zone number-name
c
          write (newfil, fmt='(a)') '#'
          write (newfil, fmt='(a)') '# - zones (2)'
          write (newfil, fmt='(a)') '#'
          write (newfil, fmt='(a)') '# num  bpa_name  GE_name'
          write (newfil, fmt='(a)') '#'

          inc_loop1 = .true.
          inc_loop2 = .true.
          count2 = 0
          count0 = 0
          lsw = 1        ! 1 - Normal
c                        ! 2 - EOD old number-name
c                        ! 3 - EOD new number-name
c                        ! 4 - EOD old, new number-name

          do while (lsw .ne. 4)
c
c           Increment old area number-name data
c
            if (inc_loop1) then
              count2 = count2 + 1
              is2 = sort_tempc(count2)
              inc_loop1 = .false.
              if (count2 .gt. numptirec) then
                if (lsw .eq. 1 .or. lsw .eq. 3) lsw = lsw + 1
              endif
            endif
c
c           Increment new area number-name data
c
            if (inc_loop2) then
              count0 = count0 + 1
              is0 = sort(count0)
              inc_loop2 = .false.
              if (count0 .gt. num_znam) then
                if (lsw .eq. 1 .or. lsw .eq. 2) lsw = lsw + 2
              endif
            endif

            if (lsw .eq. 1) then
              last = lastch (tempc(is2))
              call uscan (tempc(is2)(1:last), word, nwrd, '=',  ' ,')   
              numzone = ftn_atoi (word(1))
              zonename = word(2)
              longname = word(3)
              nextzone = pti_znum(is0) 
              kompar = numzone - nextzone
              if (kompar .eq. 0) then
                kompar = kompr (zonename, pti_znam(is0), junk)
              endif
              if (kompar .lt. 0) then
                write (newfil, 10140) numzone, zonename, longname
10140           format (1x, i3, 2x, '"', a, '"  "', a, '"')
                inc_loop1 = .true.
              else if (kompar .gt. 0) then
                write (newfil, 10140) nextzone, pti_znam(is0),
     &            zone_name(is0)
                inc_loop2 = .true.
              else
                write (newfil, 10140) numzone, zonename, longname
                inc_loop1 = .true.
                inc_loop2 = .true.
              endif
            else if (lsw .eq. 2) then
              write (newfil, 10140) nextzone, pti_znam(is0),
     &            zone_name(is0)
              inc_loop2 = .true.
            else if (lsw .eq. 3) then
              last = lastch (tempc(is2))
              call uscan (tempc(is2)(1:last), word, nwrd, '=',  ' ,')   
              numzone = ftn_atoi (word(1))
              zonename = word(2)
              longname = word(3)
              write (newfil, 10140) numzone, zonename, longname
              inc_loop1 = .true.
            endif
          enddo

          write (newfil, fmt='(a)') ' 0'
c
c         **************************************************************
c         Process owner data
c
c         Sort new PTI owner numbers
c
          key = 7
          do i = 1, num_onam
            sort(i) = i
          enddo

          if (num_onam .gt. 1) then
            call qiksrt (1, num_onam, kmp_ptib, swp_ptib)
          endif
c
c         Read in owner number-name
c
          finished = .false.
          numptirec = 0
          
          inquire (unit=scrfil, opened=opened)
          eof = (.not. opened)
          do while (.not. finished .and. .not. eof)
            read (scrfil, fmt='(a)', end=350) xbuf
            if (xbuf(1:1) .eq. '#') then
            else
              last = lastch (xbuf)
              call uscan (xbuf(1:last), word, nwrd, '=',  ' ,')   
              numowner = ftn_atoi (word(1))
              if (numowner .eq. 0) then
                finished = .true.
              else 
                numptirec = numptirec + 1
                tempc(numptirec) = xbuf
                sort_tempc(numptirec) = numptirec
              endif
            endif
          enddo

          go to 360
  350     write (errbuf(1), 10150) xbuf(1:60)
10150     format ('E-O-F encountered reading owner record in *.TRN file 
     &', a)
          call prterx ('W', 1)
          error = error + 1
  360     continue
c
c         Sort old PTI owner numbers
c
          key = 204

          if (numptirec .gt. 0) then
            call qiksrt (1, numptirec, kmp_ptib, swp_ptib)
          endif
c
c         Merge old and new owner number-name
c
          write (newfil, fmt='(a)') '#'
          write (newfil, fmt='(a)') '# - owners (3)'
          write (newfil, fmt='(a)') '# num       ge_short     long_name'
          write (newfil, fmt='(a)') '#    bpa_name'
          write (newfil, fmt='(a)') '#'

          inc_loop1 = .true.
          inc_loop2 = .true.
          count3 = 0
          count0 = 0
          lsw = 1        ! 1 - Normal
c                        ! 2 - EOD old number-name
c                        ! 3 - EOD new number-name
c                        ! 4 - EOD old, new number-name

          do while (lsw .ne. 4)
c
c           Increment old area number-name data
c
            if (inc_loop1) then
              count3 = count3 + 1
              is3 = sort_tempc(count3)
              inc_loop1 = .false.
              if (count3 .gt. numptirec) then
                if (lsw .eq. 1 .or. lsw .eq. 3) lsw = lsw + 1
              endif
            endif
c
c           Increment new owner number-name data
c
            if (inc_loop2) then
              count0 = count0 + 1
              is0 = sort(count0)
              inc_loop2 = .false.
              if (count0 .gt. num_onam) then
                if (lsw .eq. 1 .or. lsw .eq. 2) lsw = lsw + 2
              endif
            endif

            if (lsw .eq. 1) then
              last = lastch (tempc(is3))
              call uscan (tempc(is3)(1:last), word, nwrd, '=',  ' ,')   
              numowner = ftn_atoi (word(1))
              ownercode = word(2)
              ownercode_ge = word(3)
              longname = word(4)
              nextowner = pti_onum(is0) 
              kompar = numowner - nextowner
              if (kompar .eq. 0) then
                kompar = kompr (ownercode, pti_onam(is0), junk)
              endif
              if (kompar .lt. 0) then
                write (newfil, 10160) numowner, ownercode, ownercode_ge,
     &            longname
10160           format (1x, i3, 2x, '"', a, '"  "', a, '"  "', a, '"')
                inc_loop1 = .true.
              else if (kompar .gt. 0) then
                write (newfil, 10160) nextowner, pti_onam(is0),
     &            owner_code_ge(is0), owner_name(is0)
                inc_loop2 = .true.
              else
                write (newfil, 10160) numowner, ownercode, ownercode_ge,
     &            longname
                inc_loop1 = .true.
                inc_loop2 = .true.
              endif
            else if (lsw .eq. 2) then
              write (newfil, 10160) nextowner, pti_onam(is0),
     &          owner_code_ge(is0), owner_name(is0)
              inc_loop2 = .true.
            else if (lsw .eq. 3) then
              last = lastch (tempc(is3))
              call uscan (tempc(is3)(1:last), word, nwrd, '=',  ' ,')   
              numowner = ftn_atoi (word(1))
              ownercode = word(2)
              ownercode_ge = word(3)
              longname = word(4)
              write (newfil, 10160) numowner, ownercode, ownercode_ge,
     &          longname
              inc_loop1 = .true.
            endif
          enddo

          write (newfil, fmt='(a)') ' 0'
c
c         **************************************************************
c         Process bus data
c
c         Sort new PTI bus numbers
c
          do i = 1, num_hashn
            sort(i) = i
          enddo

          key = 1

          if (num_hashn .gt. 1) then
            call qiksrt (1, num_hashn, kmp_ptib, swp_ptib)
          endif
c
c         Read in bus-area number-name
c
          finished = .false.
          numptirec = 0
          
          inquire (unit=scrfil, opened=opened)
          eof = (.not. opened)
          do while (.not. finished .and. .not. eof)
            read (scrfil, fmt='(a)', end=370) xbuf
            if (xbuf(1:1) .eq. '#') then
            else
              last = lastch (xbuf)
              call uscan (xbuf(1:last), word, nwrd, '=',  ' ,')   
              numbus = ftn_atoi (word(1))
              if (numbus .eq. 0) then
                finished = .true.
              else 
                numptirec = numptirec + 1
                tempc(numptirec) = xbuf
                sort_tempc(numptirec) = numptirec
              endif
            endif
          enddo

          go to 380
  370     write (errbuf(1), 10170) xbuf(1:60)
10170     format ('E-O-F encountered reading bus-area record in *.TRN fi
     &le ', a)
          call prterx ('W', 1)
          error = error + 1
  380     continue
c
c         Sort old PTI bus-area numbers
c
          key = 201

          if (numptirec .gt. 0) then
            call qiksrt (1, numptirec, kmp_ptib, swp_ptib)
          endif
c
c         Merge old and new bus-area number-name
c
          write (newfil, fmt='(a)') '#'
          write (newfil, fmt='(a)') '# - buses (4)'
          write (newfil, fmt='(a)') 
     &      '# num    name       kv   ge_area ge_zone ge_owner'
          write (newfil, fmt='(a)') '#'

          inc_loop1 = .true.
          inc_loop2 = .true.
          count4 = 0
          count0 = 0
          lsw = 1        ! 1 - Normal
c                        ! 2 - EOD old number-name
c                        ! 3 - EOD new number-name
c                        ! 4 - EOD old, new number-name

          do while (lsw .ne. 4)
c
c           Increment old area number-name data
c
            if (inc_loop1) then
              count4 = count4 + 1
              is4 = sort_tempc(count4)
              inc_loop1 = .false.
              if (count4 .gt. numptirec) then
                if (lsw .eq. 1 .or. lsw .eq. 3) lsw = lsw + 1
              endif
            endif
c
c           Increment new area number-name data
c
            if (inc_loop2) then
              count0 = count0 + 1
              is0 = sort(count0)
              inc_loop2 = .false.
              if (count0 .gt. num_hashn) then
                if (lsw .eq. 1 .or. lsw .eq. 2) lsw = lsw + 2
              endif
            endif

            if (lsw .eq. 1) then
              last = lastch (tempc(is4))
              call uscan (tempc(is4)(1:last), word, nwrd, '=',  ' ,')   
              numbus = ftn_atoi (word(1))
              busname = word(2)
              basekv = ftn_atof (word(3))
              numarea = ftn_atoi (word(4))
              numzone = ftn_atoi (word(5))
              numowner = ftn_atoi (word(6))
              nextbus = pti_num(is0) 
              kompar = numbus - nextbus
              if (kompar .eq. 0) then
                kompar = kompr (busname, pti_name(is0), junk)
              endif
              if (kompar .eq. 0) then
                kompar = 1000.0 * (basekv - pti_base(is0))
              endif
              if (kompar .eq. 0) then
                kompar = numarea - pti_area(is0)
              endif
              if (kompar .lt. 0) then
                write (newfil, 10180) numbus, busname, basekv, 
     &            numarea, numzone, numowner
10180           format (1x, i5, 2x, '"', a, '"', f7.2, 1x, i3, 1x, 
     &            i4, 1x, i3)
                inc_loop1 = .true.
              else if (kompar .gt. 0) then
                write (newfil, 10180) nextbus, pti_name(is0),
     &            pti_base(is0), pti_area(is0), pti_zone(is0), 
     &            pti_owner(is0)
                inc_loop2 = .true.
              else
                write (newfil, 10180) numbus, busname, basekv, 
     &            numarea, numzone, numowner
                inc_loop1 = .true.
                inc_loop2 = .true.
              endif
            else if (lsw .eq. 2) then
              write (newfil, 10180) nextbus, pti_name(is0),
     &          pti_base(is0), pti_area(is0), pti_zone(is0), 
     &          pti_owner(is0)
              inc_loop2 = .true.
            else if (lsw .eq. 3) then
              last = lastch (tempc(is4))
              call uscan (tempc(is4)(1:last), word, nwrd, '=',  ' ,')   
              numbus = ftn_atoi (word(1))
              busname = word(2)
              basekv = ftn_atof (word(3))
              numarea = ftn_atoi (word(4))
              numzone = ftn_atoi (word(5))
              numowner = ftn_atoi (word(6))
              write (newfil, 10180) numbus, busname, basekv, 
     &          numarea, numzone, numowner
              inc_loop1 = .true.
            endif
          enddo

          write (newfil, fmt='(a)') ' 0'
c
c         **************************************************************
c         Process bus renumbering area default data
c
c         Sort area default data
c
          key = 205
          do i = 1, num_area_rule
            sort(i) = i
          enddo

          if (num_area_rule .gt. 1) then
            call qiksrt (1, num_area_rule, kmp_ptib, swp_ptib)
          endif
c
c         Write bus renumbering area default data
c
          write (newfil, fmt='(a)') '#'
          write (newfil, fmt='(a)') 
     &       '# - new bus numbering rule sections'
          write (newfil, fmt='(a)')
     &       '# - area min_bus_num max_bus_num  (5)'
          write (newfil, fmt='(a)') '#'

          do count0 = 1, num_area_rule
            is0 = sort(count0)
            iptia = fnd_ptian (area_rule(1,is0))
            if (iptia .gt. 0) then
              areaname = pti_anam(iptia)
            else
              areaname = 'Non-existant'
            endif
            write (newfil, 10190) (area_rule(i,is0), i = 1,3),
     &        areaname
10190       format (1x, i3, 2x, i5, 2x, i5, 2x, '"', a, '"')
          enddo

          write (newfil, fmt='(a)') ' 0'
c
c         **************************************************************
c         Process bus renumbering zone default data
c
c         Sort zone default data
c
          key = 206
          do i = 1, num_zone_rule
            sort(i) = i
          enddo

          if (num_zone_rule .gt. 1) then
            call qiksrt (1, num_zone_rule, kmp_ptib, swp_ptib)
          endif
c
c         Write bus renumbering zone default data
c
          write (newfil, fmt='(a)') '#'
          write (newfil, fmt='(a)')
     &      '# - zone min_bus_num max_bus_num (6)'
          write (newfil, fmt='(a)') 
          write (newfil, fmt='(a)') '#'

          do count0 = 1, num_zone_rule
            is0 = sort(count0)
            iptiz = fnd_ptiz (zone_rule(1,is0))
            if (iptiz .gt. 0) then
              longname = zone_name(iptiz)
            else
              longname = 'Non-existant'
            endif
            write (newfil, 10200) (zone_rule(i,is0), i = 1,3),
     &        longname
10200       format (1x, i3, 2x, i5, 2x, i5, 2x, '"', a, '"')
          enddo

          write (newfil, fmt='(a)') ' 0'
c
c         **************************************************************
c         Process bus renumbering owner default data
c
c         Sort zone default data
c
          key = 207
          do i = 1, num_owner_rule
            sort(i) = i
          enddo

          if (num_owner_rule .gt. 1) then
            call qiksrt (1, num_owner_rule, kmp_ptib, swp_ptib)
          endif
c
c         Write bus renumbering owner default data
c
          write (newfil, fmt='(a)') '#'
          write (newfil, fmt='(a)')
     &      '# - owner min_bus_num max_bus_num  (7)'
          write (newfil, fmt='(a)') '#'

          do count0 = 1, num_owner_rule
            is0 = sort(count0)
            iptio = fnd_ptio (owner_rule(1,is0))
            if (iptio .gt. 0) then
              longname = owner_name(iptio)
            else
              longname = 'Non-existant'
            endif
            write (newfil, 10210) (owner_rule(i,is0), i = 1,3),
     &        longname
10210       format (1x, i3, 2x, i5, 2x, i5, 2x, '"', a, '"')
          enddo

          write (newfil, fmt='(a)') ' 0'
c
c         Write bus renumbering default data
c
          write (newfil, fmt='(a)') '#'
          write (newfil, fmt='(a)')
     &      '# - (default) min_bus_num max_bus_num (8)'
          write (newfil, fmt='(a)') '#'

          write (newfil, 10220) (default_rule(i,1), i = 1,2)
10220     format (1x, i5, 2x, i5)

          write (newfil, fmt='(a)') ' 0'

          write (newfil, fmt='(a)') '#'
          write (newfil, fmt='(a)') '# End-of-file (8)'
          write (newfil, fmt='(a)') '#'
          write (newfil, fmt='(a)') ' 0'
c
        endif
        return
        end
