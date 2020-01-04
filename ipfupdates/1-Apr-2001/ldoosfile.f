C    @(#)ldoosfile.f	20.8 11/14/00
C**************************************************************** 
C 
C     File: ldoosfile.f 
C 
C     Purpose: Routine to load Out of Service branch data file 
C 
c     Input parameters:
c
c        scrfil      -  logical unit of opened OOS branch data file
c        filename    -  file name of opened OOS branch data file
c        option      -  User-selected options
c
c     Output parameters:
c
c        error       -  0/1 (normal/error)
c
C     Author: Walt Powell  Date: 19 Jul 2000
C     Called by: net_data_sub.f 
C 
C**************************************************************** 
        integer function ldoosfile (scrfil, filename, option, error) 
        integer scrfil, error 
        character filename*(*), option(10)*1
 
        include 'ipfinc/parametr.inc' 
 
        include 'ipfinc/blank.inc' 
        include 'ipfinc/bus.inc' 
        include 'ipfinc/branch.inc' 
        include 'ipfinc/prt.inc' 
        include 'ipfinc/brtype.inc'

        common /is_batch / is_batch

        integer max_out_of_service
        parameter (MAX_OUT_OF_SERVICE = 200)
        common /out_of_service/ numoossh, shunt_status(MAXCBS),
     &                          shunt_value(16,MAX_OUT_OF_SERVICE), 
     &                          branch_status(MAXBRN)
        integer numoossh, shunt_status, branch_status
        real shunt_value

        common /branch_ratings/ ratings(8,MAXBRN),
     &                          ext_ratings(15,MAXBRN),
     &                          ext_date(2,MAXBRN),
     &                          numrat
        real ratings, ext_ratings
        character ext_date*6
        integer numrat

        integer numrec, find_bus, k1, k2, ptr, bptr1, bptr2, p,
     &          winter_index(3), winter_type, season, ftn_atoi
        character type*1, subtyp*1, bus1*8, bus2*8, id*1, xbuf*132,
     &            datein*3, dateot*3, busc*8, loc*1
        real base1, base2, temp(15)
        logical finished, first_br

        data winter_index / 1, 10, 7 /
c
c       winter_type -  1 = Normal winter
c                      2 = Moderate winter ratings
c                      3 = Extra Heavy Ratings

        winter_type = index ('NME', option(7))
        season = ftn_atoi(option(6))
c 
c       Read in and hash OOS branch data
C 
        numrec = 0 
        error = 0
        ldoosfile = 0

        if (filename .eq. ' ') go to 130
        write (*, 10000) 
10000   format(1x, '* Loading Out-of-Service Branch Data File - this wil
     &l take a few moments') 
        finished = .false. 
        do while (.not. finished) 
          read (scrfil, fmt='(a)', end=120) xbuf 
          numrec = numrec + 1
          if (xbuf(1:1) .eq. '.') go to 110

          read (xbuf, 10040, err=100) type, subtyp, kown,
     &     bus1, base1, intovr, bus2, base2, id, ksect
10040     format (bz, a1, a1, 1x, a3, a8, f4.0, i1, a8, f4.0, a1, i1)

          k1 = find_bus(bus1, base1) 
          k2 = find_bus(bus2, base2) 
          if (k1 .le. 0) then
            write (errbuf(1), 10050) xbuf(1:33)
10050       format ('Bus1 on Out-of-service record [', a, '] is not in s
     &ystem')
            call prterx ('W', 1)                
            error = 1
            go to 110
          else if (k2 .le. 0) then
            write (errbuf(1), 10060) xbuf(1:33)
10060       format ('Bus2 on Out-of-service record [', a, '] is not in s
     &ystem')
            call prterx ('W', 1)                
            error = 1
            go to 110
          endif
          ptr = numbrn (k1, k2, id, ksect)
          if (ptr .gt. 0) then
            write (errbuf(1), 10070) xbuf(1:33)
10070       format ('Out-of-service record [', a, '] is already in syste
     &m')
            call prterx ('W', 1)                
            error = 1
            go to 110
          endif
          if (ltot+1 .ge. MAXBRN) then
            write (errbuf(1), 10080) MAXBRN
10080       format ('More than ',i5,
     &           ' branch records. Overflow occurred at branch:') 
            write (errbuf(2), 10090) xbuf(1:80)   
10090       format(1x, '[', a, ']')         
            call prterx ('W', 2)                
            ltot = 1                           
          endif
          ltot = ltot + 1
          do i = 1, 18
            kbrnch(i,ltot) = 0
          enddo
          kbrnch(3,ltot) = kown
          if (ksect .eq. 0) then
            branch_status(ltot) = 0
          else
            branch_status(ltot) = 2
          endif
          if (xbuf(1:2) .eq. 'LD') then        
            datein = ' ' // xbuf(79:80) 
            dateot = ' '                
          else                           
            datein = xbuf(75:77)        
            dateot = xbuf(78:80)        
          endif                          

          read (datein, 10100) kdin   
10100     format (bz,1x,i2)           
                                    
          kdin = intdte(datein(1:1),kdin) 

c         Decode the data that is dependend upon branch type..

          if (xbuf(1:2) .eq. 'L ') then
C
C           "L" DATA         
C                           
            read (xbuf, 10110, err=100) brnch(4,ltot), brnch(16,ltot),
     &        (brnch(k,ltot),k=5,9) 
10110       format (bz,33x,f4.0,f1.0,4f6.5,f4.1) 
            if (brnch(16,ltot) .eq. 1.0) brnch(16,ltot) = 0.0 
            mcard = BRTYP_L
            kbrnch(11,ltot) = kdin      

          else if (xbuf(1:1) .eq. 'T') then
C
C           "T" DATA                           
C
            read (xbuf, 10120, err=100) brnch(4,ltot), brnch(16,ltot),
     &        (brnch(k,ltot),k=5,10)          
10120       format (bz,33x,f4.0,f1.0,4f6.5,2f5.2)         
            kbrnch(11,ltot) = kdin      
            if (subtyp.eq. 'P') mcard = BRTYP_TP
            if (brnch(10,ltot) .eq. 0.0) mcard = BRTYP_TP
            if (brnch(16,ltot) .eq. 1.0) brnch(16,ltot) = 0.0 
c
c           Store original taps in brnch(13,*) and brnch(14,*) 
c
            brnch(13,ltot) = brnch(9,ltot)
            brnch(14,ltot) = brnch(10,ltot)

          else if (xbuf(1:2) .eq. 'LD') then
c
c           "LD" DATA
c
            read (xbuf, 10130, err=100) (brnch(k,ltot),k=4,7), loc,
     &        (brnch(k,ltot),k=8,10), brnch(18,ltot), brnch(16,ltot)
10130       format (bz,33x,f4.0,3f6.2,a1,2f5.1,2f4.1,f4.0)
            mcard = BRTYP_LD
            if (loc .eq. 'J') then            
              loc = 'I'                      
              brnch(8,ltot) = -brnch(8,ltot) 
            endif                             
            ksect=0                           
            if (loc .eq. 'I') ksect = 2         
            if (loc .eq. 'R') ksect = 1         
            id= ' '                           

          else if (xbuf(1:1) .eq. 'E') then
C                                            
C           "E" DATA                          
C                                            
            read (xbuf, 10140, err=100) brnch(4,ltot), brnch(16,ltot),
     &        (brnch(k,ltot),k=5,10)         
10140       format (bz,33x,f4.0,f1.0,6f6.5)      
            mcard = BRTYP_E
            if (brnch(16,ltot).eq. 1.0) brnch(16,ltot) = 0.0 

          else if (xbuf(1:2) .eq. 'RZ') then
C                                             
C           "R" regulator or "RZ" variable series compensator Data
C
            read (xbuf, 10150, err=100)  (brnch(k,ltot),k=4,10),
     &        brnch(18,ltot)
10150       format (bz,33x,f1.0,2f5.0,f4.0,4f6.5)          
            mcard = BRTYP_RZ
            call putchr(1, subtyp, kbrnch(3,ltot))        

          else if (xbuf(1:1) .eq. 'R') then
            read (xbuf, 10160, err=100) busc, basec, (brnch(k,ltot),
     &        k=6,10)
10160       format(bz,33x,a8,f4.0,2f5.2,f2.0,2f5.0)    
            ksect = 0                               
            id = ' '                                
            mcard = BRTYP_R
            call putchr(1, subtyp, kbrnch(3,ltot))    

          else if (xbuf(1:2) .eq. 'LM') then
C                                                  
C           "LM" DATA                               
C                                                  
            read (xbuf, 10170, err=100) (brnch(k,ltot),k=4,7),
     &        brnch(16,ltot) 
10170       format(bz,33x,f4.0,3f6.2,15x,f4.0)         
            mcard = BRTYP_LM
            ksect = 0                                 
            id= ' '                                 
          endif
C                                                  
C         Test for extended ratings.                 
C                                                  
          do i = 1, 3
            rateln(i,ltot) = 0.0                    
          enddo

          if (numrat .lt. MAXBRN) then
            numrat = numrat + 1
          else
            write (errbuf(1), 10172) numrat
10172       format ('More than ', i5,
     &              ' records in branch data file')
            call prterx ('W', 1)                
            error = 1
            go to 100
          endif

          do i = 1, 8
            ratings(i,numrat) = 0.0
          enddo

          if (xbuf(81:) .ne. ' ') then
            read (xbuf(81:), fmt='(bz,13f4.0)') (temp(i),i=1,7),
     &        (temp(i),i=9,10), (temp(i),i=12,15)

            temp(8) = temp(2)
            temp(11) = temp(2)
            ratmax = 0.0
            ratnom = ftn_atof (xbuf(34:37))
c
c           If all ratings are zero, replace with nominal ratings
c
            do i = 1, 15
              ratmax = amax1 (ratmax, temp(i))
            enddo
            if (ratmax .eq. 0.0) then
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

            do i = 1, 15
              if (temp(i) .eq. 0.0) temp(i) = 9999.0
            enddo

            iw = winter_index(winter_type)
            if (option(10) .eq. 'G') then

c             ----------------------------------------------------------
c             GE conversion
c             ----------------------------------------------------------
c
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
     &            ratings(5,numrat) = ratings(1,numrat)
                if (ratings(6,numrat) .eq. 9999.0)
     &            ratings(6,numrat) = ratings(2,numrat)
                if (ratings(7,numrat) .eq. 9999.0)
     &            ratings(7,numrat) = ratings(5,numrat)
                if (ratings(8,numrat) .eq. 9999.0)
     &            ratings(8,numrat) = ratings(6,numrat)
                rateln(1,ltot) = -numrat
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
     &            ratings(5,numrat) = ratings(1,numrat)
                if (ratings(6,numrat) .eq. 9999.0)
     &            ratings(6,numrat) = ratings(2,numrat)
                if (ratings(7,numrat) .eq. 9999.0)
     &            ratings(7,numrat) = ratings(5,numrat)
                if (ratings(8,numrat) .eq. 9999.0)
     &            ratings(8,numrat) = ratings(6,numrat)
                rateln(1,ltot) = -numrat
              else if (xbuf(1:1) .eq. 'R') then
              else
                write (errbuf(1), 10174) numrec
10174           format ('Illegal record No. ', i5,
     &                  ' in branch data file.')
                write (errbuf(2), 10176) xbuf(1:32), xbuf(75:80)
10176           format (' [', a, ' ... ', a, ']')
                call prterx ('W', 2)                
                error = 1
                go to 100
              endif
            else
        
c           ----------------------------------------------------------
c           PTI conversion
c           ----------------------------------------------------------

              if (index ('LE', xbuf(1:1)) .ne. 0) then
                if (season .eq. 1) then
                  ratings(1,numrat) = temp(4)
                  ratings(2,numrat) = temp(4)
                  ratings(3,numrat) = temp(5)
                else if (season .eq. 3 .or. season .eq. 4) then
                  ratings(1,numrat) = temp(13)
                  ratings(2,numrat) = temp(13)
                  ratings(3,numrat) = temp(14)
                else 
                  ratings(1,numrat) = temp(iw)
                  ratings(2,numrat) = temp(iw)
                  ratings(3,numrat) = temp(iw+1)
                endif
                if (ratings(1,numrat) .eq. 9999.0)
     &            ratings(1,numrat) = ratnom
                if (ratings(2,numrat) .eq. 9999.0)
     &            ratings(2,numrat) = ratings(1,numrat)
                if (ratings(3,numrat) .eq. 9999.0)
     &            ratings(3,numrat) = ratings(1,numrat)
                rateln(1,ltot) = -numrat
              else if (xbuf(1:1) .eq. 'T') then
                if (season .eq. 1) then
                  ratings(1,numrat) = temp(4)
                  ratings(2,numrat) = temp(5)
                  ratings(3,numrat) = temp(6)
                else if (season .eq. 3 .or. season .eq. 4) then
                  ratings(1,numrat) = temp(13)
                  ratings(2,numrat) = temp(14)
                  ratings(3,numrat) = temp(15)
                else 
                  ratings(1,numrat) = temp(iw)
                  ratings(2,numrat) = temp(iw+1)
                  ratings(3,numrat) = temp(iw+3)
                endif
                if (ratings(1,numrat) .eq. 9999.0)
     &            ratings(1,numrat) = ratnom
                if (ratings(2,numrat) .eq. 9999.0)
     &            ratings(2,numrat) = ratings(1,numrat)
                if (ratings(3,numrat) .eq. 9999.0)
     &            ratings(3,numrat) = ratings(1,numrat)
                rateln(1,ltot) = -numrat
              else if (xbuf(1:1) .eq. 'R') then
              else
                write (errbuf(1), 10174) numrec
                write (errbuf(2), 10176) xbuf(1:32), xbuf(75:80)
                call prterx ('W', 2)
                error = 1
                go to 100
              endif
            endif
            do i = 1, 8
              if (ratings(i,numrat) .eq. 9999.0) ratings(i,numrat) = 0.0
            enddo
          endif

c         Link up branch

          if (ksect .gt. 0) then                     
            if (mcard .eq. BRTYP_LM .or. mcard .eq. BRTYP_R .or. 
     &          mcard .eq. BRTYP_LD) then
              msect = 0           
            else
              msect = ksect
            endif
          else
            msect = 0
          endif                                      

          if (msect .ne. 0) then

c           Search the branches for this bus to see if this is the
c           first section encountered on this circuit...

            first_br = .true.
            p = kbsdta(16,k1)
            do while (p .gt. 0)
              if (ky(p).eq.k2 .and. brid(p).eq.id) first_br = .false.
              p = brnch_nxt(p)
            enddo
            if ( first_br ) then
c
c             When this is the first time this sectioned branch has been
c             encountered, Add an entry for the pi-equivalent section
C
              call lksrtbrnd (ltot, k1, k2, BRTYP_PEQ, id, 0, bptr1, 
     &                        bptr2, error)
              kx(bptr1) = k1                 
              ky(bptr1) = k2
              brid(bptr1) = id
              brsect(bptr1) = 0
              brtype(bptr1) = BRTYP_PEQ

              kx(bptr2) = k2                 
              ky(bptr2) = k1
              brid(bptr2) = id
              brsect(bptr2) = 0
              brtype(bptr2) = BRTYP_PEQ

              ltot = ltot + 1
              do i = 1, 18
                 kbrnch(i,ltot) = kbrnch(i,ltot-1)
              enddo
              do i = 1, 3
                rateln(i,ltot) = rateln(i,ltot-1)
                rateln(i,ltot-1) = 0.0
              enddo
              branch_status(ltot) = 0
            endif
          endif
          call lksrtbrnd (ltot, k1, k2, mcard, id, ksect, bptr1, 
     &                    bptr2, error)
          kx(bptr1) = k1
          ky(bptr1) = k2
          brid(bptr1) = id
          brsect(bptr1) = ksect
          brtype(bptr1) = mcard

          kx(bptr2) = k2
          ky(bptr2) = k1
          brid(bptr2) = id
          brsect(bptr2) = ksect
          brtype(bptr2) = mcard

          go to 110

  100     ltot = ltot - 1
          error = 1
          write (errbuf(1), 10190) xbuf(1:80)                
10190     format (' illegal data in field :(',a80,')')     
          call prterx ('W', 1)                

  110     continue
        enddo
        go to 130 

  120   write (*, 10200) numrec, filename  
10200   format (1x, i4, ' Out-Of-Service records processed in file ',
     & a)

  130   continue

        return
        end
