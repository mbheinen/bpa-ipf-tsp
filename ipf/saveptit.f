C    @(#)saveptit.f	20.6 2/28/00
C****************************************************************
C
C     File: saveptit.f
C
C     Purpose: Routine to save PTI translation file
C
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: net_data_sub.f
C
C****************************************************************
        integer function saveptit (scrfil, filename, newfil, 
     &                             newfilename)
        integer scrfil, newfil, numver
        character *(*) filename, newfilename

        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
        include 'ipfinc/pti_data.inc'
        include 'ipfinc/qksrt.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/basval.inc'
 
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

        character xbuf*120, busname*8, areaname*10, zonename*2, 
     &            code*4, base1c*4, bell*1, apostrophe*1
        logical finished, eof, opened, inc_loop1, inc_loop2
        integer count0, count1, count2, count3, count4, status,
     &          bldnewown
        external kmp_ptib, swp_ptib

        error = 0           ! initialize error count
        bell = char(7)
        apostrophe = ''''
        eof = .false.
        saveptit = 0
c
c       Create an updated Translation File
c
        if (newfilename .ne. ' ') then
          write (*, 10000) bell
10000     format(1x, a, '* Creating a new PTI Translation File')
          rewind scrfil
c
c         Sort new PTI bus numbers
c
          key = 2

          if (count_newbus .gt. 0) then
            call qiksrt (1, count_newbus, kmp_ptib, swp_ptib)
          endif
c
c         Read in and sort bus number-name
c
          finished = .false.
          numptirec = 0
          
          inquire (unit=scrfil, opened=opened)
          eof = (.not. opened)
          do while (.not. finished .and. .not. eof)
            read (scrfil, fmt='(a)', end=250) 
     &        tempc(numptirec+1)
            read (tempc(numptirec+1), 10010, err=230) numbus
10010       format (1x, i5, 2x, a8, f4.0, i4, 20x, i4)
            if (numbus .eq. 0) then
              finished = .true.
            else 
              numptirec = numptirec + 1
              sort_tempc(numptirec) = numptirec
            endif
            go to 240

  230       write (errbuf(1), 10020) tempc(numptirec+1)(1:60)
10020       format ('Error decoding bus record in *.TRN file ', a)
            call prterx ('W', 1)
            error = error + 1

  240       continue
          enddo

          go to 260
  250     write (errbuf(1), 10030) tempc(numptirec+1)(1:60)
10030     format ('E-O-F encountered reading bus record in *.TRN file '
     &, a)
          call prterx ('W', 1)
          error = error + 1
  260     continue
c
c         Sort old PTI bus numbers
c
          key = 101

          if (numptirec .gt. 0) then
            call qiksrt (1, numptirec, kmp_ptib, swp_ptib)
          endif
c
c         Merge sorted bus number-name
c
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
c           Increment old bus number-name data
c
            if (inc_loop1) then
              count1 = count1 + 1
              inc_loop1 = .false.
              if (count1 .gt. numptirec) then
                if (lsw .eq. 1 .or. lsw .eq. 3) lsw = lsw + 1
              endif
            endif
c
c           Increment new bus number-name data
c
            if (inc_loop2) then
              count0 = count0 + 1
              inc_loop2 = .false.
              if (count0 .gt. count_newbus) then
                if (lsw .eq. 1 .or. lsw .eq. 2) lsw = lsw + 2
              endif
            endif

            if (lsw .eq. 1) then
              read (tempc(sort_tempc(count1)), 10010, err=270) numbus, 
     &          busname, basekv, numarea, numzone
              nextbus = pti_num(newbusno(count0)) 
              kompar = numbus - nextbus
              if (kompar .eq. 0) then
                kompar = kompr (busname, pti_name(newbusno(count0)),
     &                          junk)
              endif
              if (kompar .eq. 0) then
                kompar = 100.0 * (basekv - pti_base(newbusno(count0)))
              endif
              if (kompar .eq. 0) then
                kompar = numarea - pti_area(newbusno(count0))
              endif
              if (kompar .eq. 0) then
                kompar = numzone - pti_zone(newbusno(count0))
              endif
              if (kompar .lt. 0) then
                last = lastch (tempc(sort_tempc(count1)))
                write (newfil, '(a)') tempc(sort_tempc(count1))(1:last)
                inc_loop1 = .true.
              else if (kompar .gt. 0) then
                iptib = newbusno(count0)
                base1c = code (pti_base(iptib), 4, 0)
                write (newfil, 10040) nextbus, pti_name(iptib),
     &            base1c, pti_area(iptib), 0, basval(4)(1:10), 99
10040           format (1x, i5, 2x, a8, a4, i4, i6, 2x, '''', a, '''',
     &            i4, '    /')
                inc_loop2 = .true.
              else
                last = lastch (tempc(sort_tempc(count1)))
                write (newfil, '(a)') tempc(sort_tempc(count1))(1:last)
                inc_loop1 = .true.
                inc_loop2 = .true.
              endif
            else if (lsw .eq. 2) then
              iptib = newbusno(count0)
              base1c = code (pti_base(iptib), 4, 0)
              write (newfil, 10040) nextbus, pti_name(iptib),
     &          base1c, pti_area(iptib), 0, basval(4)(1:10), 99
              inc_loop2 = .true.
            else if (lsw .eq. 3) then
              last = lastch (tempc(sort_tempc(count1)))
              write (newfil, '(a)') tempc(sort_tempc(count1))(1:last)
              inc_loop1 = .true.
            endif
            go to 280

  270       write (errbuf(1), 10020) tempc(sort_tempc(count1))(1:60)
            call prterx ('W', 1)
            error = error + 1

  280       continue
          enddo

          xbuf = ' 0'
          last = lastch (xbuf)
          write (newfil, '(a)') xbuf(1:last)
c
c         Sort new PTI area numbers
c
          key = 5
          if (num_anam .gt. 1) then
            call qiksrt (1, num_anam, kmp_ptib, swp_ptib)
          endif
c
c         Read in and sort area number-name
c
          finished = .false.
          numptirec = 0
          
          inquire (unit=scrfil, opened=opened)
          eof = (.not. opened)
          do while (.not. finished .and. .not. eof)
            read (scrfil, fmt='(a)', end=310) tempc(numptirec+1)
            read (tempc(numptirec+1), 10050, err=290) numarea
10050       format (1x, i3, 2x, a8)
            if (numarea .eq. 0) then
              finished = .true.
            else 
              numptirec = numptirec + 1
              sort_tempc(numptirec) = numptirec
            endif
            go to 300

  290       write (errbuf(1), 10060) tempc(numptirec+1)(1:60)
10060       format ('Error decoding area record in *.TRN file ', a)
            call prterx ('W', 1)
            error = error + 1

  300       continue
          enddo

          go to 320
  310     write (errbuf(1), 10070) tempc(numptirec+1)(1:60)
10070     format ('E-O-F encountered reading area record in *.TRN file '
     &, a)
          call prterx ('W', 1)
          error = error + 1
  320     continue
c
c         Sort old PTI area numbers
c
          key = 102

          if (numptirec .gt. 0) then
            call qiksrt (1, numptirec, kmp_ptib, swp_ptib)
          endif
c
c         Merge sorted area number-name
c
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
              inc_loop2 = .false.
              if (count0 .gt. num_anam) then
                if (lsw .eq. 1 .or. lsw .eq. 2) lsw = lsw + 2
              endif
            endif

            if (lsw .eq. 1) then
              read (tempc(sort_tempc(count2)), 10050, err=340) numarea, 
     &          areaname
              nextarea = pti_anum(count0) 
              kompar = numarea - nextarea
              if (kompar .eq. 0) then
                kompar = kompr (areaname, pti_anam(count0), junk)
              endif
              if (kompar .lt. 0) then
                last = lastch (tempc(sort_tempc(count2)))
                write (newfil, '(a)') tempc(sort_tempc(count2))(1:last)
                inc_loop1 = .true.
              else if (kompar .gt. 0) then
                iptia = count0
                write (newfil, 10050) nextarea, pti_anam(count0)
                inc_loop2 = .true.
              else
                last = lastch (tempc(sort_tempc(count2)))
                write (newfil, '(a)') tempc(sort_tempc(count2))(1:last)
                inc_loop1 = .true.
                inc_loop2 = .true.
              endif
            else if (lsw .eq. 2) then
              iptia = count0
              write (newfil, 10050) nextarea, pti_anam(count0)
              inc_loop2 = .true.
            else if (lsw .eq. 3) then
              last = lastch (tempc(sort_tempc(count2)))
              write (newfil, '(a)') tempc(sort_tempc(count2))(1:last)
              inc_loop1 = .true.
            endif
            go to 350

  340       write (errbuf(1), 10060) tempc(sort_tempc(count2))(1:60)
            call prterx ('W', 1)
            error = error + 1

  350       continue
          enddo

          xbuf = ' 0'
          last = lastch (xbuf)
          write (newfil, '(a)') xbuf(1:last)
c
c         Sort new PTI zone numbers
c
          key = 3
          if (count_newzone .gt. 1) then
            call qiksrt (1, count_newzone, kmp_ptib, swp_ptib)
          endif
c
c         Read in and sort old zone number-name
c
          finished = .false.
          numptirec = 0
          
          inquire (unit=scrfil, opened=opened)
          eof = (.not. opened)
          do while (.not. finished .and. .not. eof)
            read (scrfil, fmt='(a)', end=380) tempc(numptirec+1)
            read (tempc(numptirec+1), 10090, err=360) numzone
10090       format (1x, i3, 2x, a2)
            if (numzone .eq. 0) then
              finished = .true.
            else 
              numptirec = numptirec + 1
              sort_tempc(numptirec) = numptirec
            endif
            go to 370

  360       write (errbuf(1), 10100) tempc(numptirec+1)(1:60)
10100       format ('Error decoding zone record in *.TRN file ', a)
            call prterx ('W', 1)
            error = error + 1

  370       continue
          enddo

          go to 390
  380     write (errbuf(1), 10110) tempc(numptirec+1)(1:60)
10110     format ('E-O-F encountered reading zone record in *.TRN file '
     &, a)
          call prterx ('W', 1)
          error = error + 1
  390     continue
c
c         Sort old PTI zone numbers
c
          key = 103

          if (numptirec .gt. 0) then
            call qiksrt (1, numptirec, kmp_ptib, swp_ptib)
          endif
c
c         Merge sorted zone number-name
c
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
c           Increment old zone number-name data
c
            if (inc_loop1) then
              count3 = count3 + 1
              inc_loop1 = .false.
              if (count3 .gt. numptirec) then
                if (lsw .eq. 1 .or. lsw .eq. 3) lsw = lsw + 1
              endif
            endif
c
c           Increment new zone number-name data
c
            if (inc_loop2) then
              count0 = count0 + 1
              inc_loop2 = .false.
              if (count0 .gt. count_newzone) then
                if (lsw .eq. 1 .or. lsw .eq. 2) lsw = lsw + 2
              endif
            endif

            if (lsw .eq. 1) then
              read (tempc(sort_tempc(count3)), 10090, err=400) numzone, 
     &          zonename
              nextzone = pti_znum(newzoneno(count0)) 
              kompar = numzone - nextzone
              if (kompar .eq. 0) then
                kompar = kompr (zonename, pti_znam(newzoneno(count0)),
     &                          junk)
              endif
              if (kompar .lt. 0) then
                last = lastch (tempc(sort_tempc(count3)))
                write (newfil, '(a)') tempc(sort_tempc(count3))(1:last)
                inc_loop1 = .true.
              else if (kompar .gt. 0) then
                iptiz = newzoneno(count0)
                write (newfil, 10120) nextzone, 
     &            pti_znam(newzoneno(count0))
10120           format (1x, i3, 2x, a2)
                inc_loop2 = .true.
              else
                last = lastch (tempc(sort_tempc(count3)))
                write (newfil, '(a)') tempc(sort_tempc(count3))(1:last)
                inc_loop1 = .true.
                inc_loop2 = .true.
              endif
            else if (lsw .eq. 2) then
              iptiz = newzoneno(count0)
              write (newfil, 10120) nextzone, 
     &          pti_znam(newzoneno(count0))
              inc_loop2 = .true.
            else if (lsw .eq. 3) then
              last = lastch (tempc(sort_tempc(count3)))
              write (newfil, '(a)') tempc(sort_tempc(count3))(1:last)
              inc_loop1 = .true.
            endif
            go to 410

  400       write (errbuf(1), 10110) tempc(sort_tempc(count3))(1:60)
            call prterx ('W', 1)
            error = error + 1

  410       continue
          enddo

          xbuf = ' 0'
          last = lastch (xbuf)
          write (newfil, '(a)') xbuf(1:last)
c
c         Rebuild PTI owner arrays if not already loaded
c
          if (count_newown .eq. 0) then
            status = bldnewown()
          endif
c
c         Sort new PTI owner numbers
c
          key = 4
          if (count_newown .gt. 1) then
            call qiksrt (1, count_newown, kmp_ptib, swp_ptib)
          endif
c
c         Read in and sort old owner number-name
c
          finished = .false.
          numptirec = 0
          
          inquire (unit=scrfil, opened=opened)
          eof = (.not. opened)
          do while (.not. finished .and. .not. eof)
            read (scrfil, fmt='(a)', end=440) tempc(numptirec+1)
            read (tempc(numptirec+1), 10130, err=420) numowner
10130       format (1x, i3, 2x, a3)
            if (numowner .eq. 0) then
              finished = .true.
            else 
              numptirec = numptirec + 1
              sort_tempc(numptirec) = numptirec
            endif
            go to 430

  420       write (errbuf(1), 10140) tempc(numptirec+1)(1:60)
10140       format ('Error decoding owner record in *.TRN file ', a)
            call prterx ('W', 1)
            error = error + 1

  430       continue
          enddo

          go to 450
  440     write (errbuf(1), 10150) tempc(numptirec+1)(1:60)
10150     format ('E-O-F encountered reading owner record in *.TRN file 
     &', a)
          call prterx ('W', 1)
          error = error + 1
  450     continue
c
c         Sort old PTI owner numbers
c
          key = 104

          if (numptirec .gt. 0) then
            call qiksrt (1, numptirec, kmp_ptib, swp_ptib)
          endif
c
c         Merge sorted owner number-name
c
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
c           Increment old owner number-name data
c
            if (inc_loop1) then
              count4 = count4 + 1
              inc_loop1 = .false.
              if (count4 .gt. numptirec) then
                if (lsw .eq. 1 .or. lsw .eq. 3) lsw = lsw + 1
              endif
            endif
c
c           Increment new owner number-name data
c
            if (inc_loop2) then
              count0 = count0 + 1
              inc_loop2 = .false.
              if (count0 .gt. num_onam) then
                if (lsw .eq. 1 .or. lsw .eq. 2) lsw = lsw + 2
              endif
            endif

            if (lsw .eq. 1) then
              read (tempc(sort_tempc(count4)), 10130, err=460) 
     &          numownerer, ownername
              nextowner = pti_onum(newownno(count0)) 
              kompar = numowner - nextowner
              if (kompar .eq. 0) then
                kompar = kompr (ownername, pti_onam(newownno(count0)),
     &                          junk)
              endif
              if (kompar .lt. 0) then
                last = lastch (tempc(sort_tempc(count4)))
                write (newfil, '(a)') tempc(sort_tempc(count4))(1:last)
                inc_loop1 = .true.
              else if (kompar .gt. 0) then
                iptio = newownno(count0)
                write (newfil, 10160) nextowner,
     &             pti_onam(newownno(count0))
10160           format (1x, i3, 2x, a3)
                inc_loop2 = .true.
              else
                last = lastch (tempc(sort_tempc(count4)))
                write (newfil, '(a)') tempc(sort_tempc(count4))(1:last)
                inc_loop1 = .true.
                inc_loop2 = .true.
              endif
            else if (lsw .eq. 2) then
              iptio = newownno(count0)
              write (newfil, 10160) nextowner, 
     &          pti_onam(newownno(count0))
              inc_loop2 = .true.
            else if (lsw .eq. 3) then
              last = lastch (tempc(sort_tempc(count4)))
              write (newfil, '(a)') tempc(sort_tempc(count4))(1:last)
              inc_loop1 = .true.
            endif
            go to 470

  460       write (errbuf(1), 10140) tempc(sort_tempc(count4))(1:60)
            call prterx ('W', 1)
            error = error + 1

  470       continue
          enddo

          xbuf = ' 0'
          last = lastch (xbuf)
          write (newfil, '(a)') xbuf(1:last)

          numrec = count1 + count_newbus + count2 + count3 + count4
     &           + count_newzone + count_newown
          last = lastch (newfilename)
          write (outbuf, 10190) numrec, newfilename(1:last)
10190     format (1x, i5, ' name records written to translation file ',
     & a)
          call prtout (1)
          write (*, 10200) count1, count_newbus
10200     format (1x, ' Bus records:   original ', i5, ' inserted ', i5)
          write (*, 10210) count2, num_anam
10210     format (1x, ' Area records:  original ', i5, ' inserted ', i5)
          write (*, 10220) count3, count_newzone
10220     format (1x, ' Zone records:  original ', i5, ' inserted ', i5)
          write (*, 10230) count4, count_newown
10230     format (1x, ' Owner records: original ', i5, ' inserted ', i5)

        endif

        return
        end
