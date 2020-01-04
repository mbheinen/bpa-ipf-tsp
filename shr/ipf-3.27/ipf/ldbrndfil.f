C    @(#)ldbrndfil.f	20.17 8/30/00
C**************************************************************** 
C 
C     File: ldbrndfil.f 
C 
C     Purpose: Routine to load BPA branch data file 
C 
c     Input parameters:
c
c        scrfil      -  logical unit of opened branch data file
c        filename    -  file name of opened branch data file
c        datebr      -  date to select valid branches 
c                       energization < datebr < de-ennergization
c        option      -  User-selected options
c
c     Output parameters:
c
c        error       -  0/1 (normal/error)
c
C     Author: Walt Powell  Date: 09 Feb 2000
C     Called by: net_data_sub.f 
C 
C**************************************************************** 
        integer function ldbrndfil (scrfil, filename, datebr, 
     &                              option, error) 
        integer scrfil, error 
        character filename*(*), datebr*(*), option(10)*1
 
        include 'ipfinc/parametr.inc' 
 
        include 'ipfinc/blank.inc' 
        include 'ipfinc/bus.inc' 
        include 'ipfinc/branch.inc' 
        include 'ipfinc/prt.inc' 

        common /is_batch / is_batch

        common /branch_ratings/ ratings(8,MAXBRN), 
     &                          ext_ratings(15,MAXBRN),
     &                          ext_date(2,MAXBRN),
     &                          numrat
        real ratings, ext_ratings
        character ext_date*6
        integer numrat

        integer numrec, find_bus, k1, k2, ptr, ftn_atoi,
     &          winter_index(3), winter_type
        character type*1, subtyp*1, bus1*8, bus2*8, id*1, datein*3,
     &            dateot*3, xbuf*132
        real base1, base2, yearbr, temp(15)
        logical finished

        data winter_index / 1, 10, 7 /

        datein = datebr(1:1) // datebr(4:5)
        yearbr = energd (datein)
c 
c       Read in and hash branch data in *.TRN file 
C 
        numrec = 0 
        error = 0
        ldbrndfil = 0
c
c       Use season [option(6)] and winter_type as filters for
c       extended ratings
c
c       winter_type -  1 = Normal winter
c                      2 = Moderate winter ratings
c                      3 = Extra Heavy Ratings

        winter_type = index ('NME', option(7))
        if (option(6) .eq. '1') then 
          iseason = 2
        else if (option(6) .eq. '2') then
          if (winter_type .eq. 1) then
            iseason = 1
          else if (winter_type .eq. 2) then
            iseason = 4
          else
            iseason = 3
          endif
        else if (option(6) .eq. '3') then
          iseason = 5
        else
          iseason = 0
        endif

        if (filename .ne. ' ') then 
          write (*, 10000) 
10000     format(1x, '* Loading Branch Data File - this will take a few
     & moments') 
          finished = .false. 
          do while (.not. finished) 
            read (scrfil, fmt='(a)', end=130) xbuf 
            numrec = numrec + 1
            if (xbuf(1:1) .eq. '.') go to 100
            datein = xbuf(75:77)        
            dateot = xbuf(78:80)        
            yearin = energd (datein)       
            if (yearin .eq. -9999.0) then                                   
              write (errbuf(1), 10010) datein, numrec
10010         format ('Illegal date-in (', a, 
     &           ') on branch data record No. ', i5)
              write (errbuf(2), 10020) xbuf(1:32), xbuf(75:80)
10020         format (' [', a, ' ... ', a, ']')
              if (is_batch .eq. 0) then
                call prterx ('E', 2)
                return
              else
                call prterx ('F', 2)
                call erexit (0)                                   
              endif
            endif                           

            yearot = denerg (dateot)    
            if (yearot .eq. -9999.0) then     
              write (errbuf(1), 10030) dateot, numrec
10030         format ('Illegal date-out (', a, 
     &           ') on branch data record No. ', i5) 
              write (errbuf(2), 10020) xbuf(1:32), xbuf(75:80)
              if (is_batch .eq. 0) then
                call prterx ('E', 2)
                return
              else
                call prterx ('F', 2)
                call erexit (0)                                   
              endif
            endif                           

            if (yearbr .lt. yearin .or. yearbr .ge. yearot) go to 100

            read (xbuf, 10040, err=110) type, subtyp, bus1, base1,  
     &           bus2, base2, id, ksect                        
10040       format (bz, a1, a1, 4x, a8, f4.0, 1x, a8, f4.0, a1, i1)

            k1 = find_bus(bus1, base1) 
            k2 = find_bus(bus2, base2) 
            if (k1 .le. 0 .or. k2 .le. 0) go to 100
            ptr = numbrn (k1, k2, id, ksect)
            if (ptr .le. 0) go to 100
            if (brtype(ptr) .eq. 4) ptr = brnch_nxt(ptr)
            nbr = iabs (brnch_ptr(ptr))
            if (numrat .lt. MAXBRN) then
              numrat = numrat + 1
            else
              write (errbuf(1), 10050) numrat
10050         format ('More than ', i5, 
     &                ' records in branch data file')
              if (is_batch .eq. 0) then
                call prterx ('E', 1)
                return
              else
                call prterx ('F', 1)
                call erexit (0)                                   
              endif
            endif
            do i = 1, 8
              ratings(i,numrat) = 0.0
            enddo
            do i = 1, 15
              temp(i) = 0.0
              ext_ratings(i,numrat) = 0.0
            enddo
            ext_date(1,numrat) = ' '
            ext_date(2,numrat) = ' '
c                                                  
c           Season             Rating      Temp()
c           Winter peak        Thermal       1
c                              Emergency     2
c                              Bottleneck    3
c           Summer peak        Thermal       4
c                              Emergency     5
c                              Bottleneck    6
c           Extra Heavy Winter Thermal       7
c                              Emergency     8
c                              Bottleneck    9
c           Moderate Winter    Thermal      10
c                              Emergency    11
c                              Bottleneck   12
c           Spring peak        Thermal      13
c                              Emergency    14
c                              Bottleneck   15
c                                                                       
            read (xbuf(81:), fmt='(bz,13f4.0)') (temp(i),i=1,7),
     &        (temp(i),i=9,10), (temp(i),i=12,15)

            do i = 1, 15
              ext_ratings(i,numrat) = temp(i)  
            enddo
            do i = 1, iseason - 1
              do j = 3*i-2, 3*i
                ext_ratings(j,numrat) = 0.0
              enddo
            enddo
            do i = iseason + 1, 5
              do j = 3*i-2, 3*i
                ext_ratings(j,numrat) = 0.0
              enddo
            enddo

            temp(8) = temp(2)
            temp(11) = temp(2)
            ratmax = 0.0
c
c           If all ratings are zero, replace with nominal ratings
c
            do i = 1, 15
              ratmax = amax1 (ratmax, temp(i))
            enddo
            if (ratmax .eq. 0.0) then
              ratnom = ftn_atof (xbuf(34:37))
              do i = 1, 15
                temp(i) = ratnom
              enddo
            endif           
            if (temp(7) .eq. 0.0) temp(7) = temp(10)
            if (temp(8) .eq. 0.0) temp(8) = temp(11)
            if (temp(9) .eq. 0.0) temp(9) = temp(12)
            if (temp(7) .eq. 0.0) temp(7) = temp(1)
            if (temp(8) .eq. 0.0) temp(8) = temp(2)
            if (temp(9) .eq. 0.0) temp(9) = temp(3)
            if (temp(10) .eq. 0.0) temp(10) = temp(1)
            if (temp(11) .eq. 0.0) temp(11) = temp(2)
            if (temp(12) .eq. 0.0) temp(12) = temp(3)
c
c           If a transformer has only thermal ratings, use them
c           for emergency ratings as well.
c
            if (index ('LE', xbuf(1:1)) .eq. 0) then
              do i = 1, 13, 3
                if (temp(i) .gt. 0.0 .and. temp(i+1) .eq. 0.0)
     &            temp(i+1) = temp(i)
              enddo
            endif

            if (datein .eq. '  0') then
              ext_date(1,numrat) = datein
            else
              iyear = ftn_atoi (datein(2:3))
              imonth = index ('123456789OND', datein(1:1))
              write (ext_date(1,numrat),  10052) iyear, imonth, 01
10052         format (3i2.2)
            endif
            if (dateot .eq. '  0') then
              ext_date(2,numrat) = dateot
            else
              iyear = ftn_atoi (dateot(2:3))
              imonth = index ('123456789OND', dateot(1:1))
              write (ext_date(2,numrat),  10052) iyear, imonth, 01
            endif
            do i = 1, 15
              if (temp(i) .eq. 0.0) temp(i) = 9999.0
            enddo
            iw = winter_index(winter_type)
            if (index ('LE', xbuf(1:1)) .ne. 0) then  
              ratings(1,numrat) = amin1 (temp(4), temp(5))
              ratings(2,numrat) = ratings(1,numrat)
              ratings(3,numrat) = amin1 (temp(iw), temp(iw+1))
              ratings(4,numrat) = ratings(3,numrat)
              ratings(5,numrat) = amin1 (temp(13), temp(14))
              ratings(6,numrat) = ratings(5,numrat)
              ratings(7,numrat) = ratings(5,numrat) 
              ratings(8,numrat) = ratings(6,numrat)
              if (ratings(5,numrat) .eq. 9999.0) 
     &          ratings(5,numrat) = ratings(1,numrat)
              if (ratings(6,numrat) .eq. 9999.0) 
     &          ratings(6,numrat) = ratings(2,numrat)
              if (ratings(7,numrat) .eq. 9999.0) 
     &          ratings(7,numrat) = ratings(5,numrat)
              if (ratings(8,numrat) .eq. 9999.0) 
     &          ratings(8,numrat) = ratings(6,numrat)
              rateln(1,nbr) = -numrat
            else if (xbuf(1:1) .eq. 'T') then        
              ratings(1,numrat) = amin1 (temp(4), temp(6))
              ratings(2,numrat) = amin1 (temp(5), temp(6))
              ratings(3,numrat) = amin1 (temp(iw), temp(iw+2))
              ratings(4,numrat) = amin1 (temp(iw+1), temp(iw+2))
              ratings(5,numrat) = amin1 (temp(13), temp(15))
              ratings(6,numrat) = amin1 (temp(14), temp(15))
              ratings(7,numrat) = ratings(5,numrat)
              ratings(8,numrat) = ratings(6,numrat) 
              if (ratings(5,numrat) .eq. 9999.0) 
     &          ratings(5,numrat) = ratings(1,numrat)
              if (ratings(6,numrat) .eq. 9999.0) 
     &          ratings(6,numrat) = ratings(2,numrat)
              if (ratings(7,numrat) .eq. 9999.0) 
     &          ratings(7,numrat) = ratings(5,numrat)
              if (ratings(8,numrat) .eq. 9999.0) 
     &          ratings(8,numrat) = ratings(6,numrat)
              rateln(1,nbr) = -numrat
            else if (xbuf(1:1) .eq. 'R') then
            else
              write (errbuf(1), 10060) numrec
10060         format ('Illegal record No. ', i5, 
     &                ' in branch data file.')
              write (errbuf(2), 10020) xbuf(1:32), xbuf(75:80)
              if (is_batch .eq. 0) then
                call prterx ('E', 2)
                return
              else
                call prterx ('F', 2)
                call erexit (0)                                   
              endif
            endif 
            do i = 1, 8
              if (ratings(i,numrat) .eq. 9999.0) ratings(i,numrat) = 0.0
            enddo
  100       continue 
            go to 120

  110       write (errbuf(1), 10070) numrec
10070       format ('Error decoding record No. ', i5, 
     &                ' in branch data file.')
            write (errbuf(2), 10020) xbuf(1:32), xbuf(75:80)
            if (is_batch .eq. 0) then
              call prterx ('E', 2)
              return
            else
              call prterx ('F', 2)
              call erexit (0)                                   
            endif

  120       continue
          enddo

          go to 140 

  130     write (*, 10080) numrec, filename
10080     format (1x, i5, ' records processed in branch data file ', a)
 
  140     continue
        endif
        return
        end
