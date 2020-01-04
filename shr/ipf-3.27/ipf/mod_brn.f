C    @(#)mod_brn.f	20.5 8/30/00
        subroutine mod_brn (ic, ptr)
        integer ic, ptr
c
c       Perform branch modification changes from chgcrd(ic) onto
c       kx(ptr), etc.
c
        include 'ipfinc/parametr.inc'

        include 'ipfinc/branch.inc'
        include 'ipfinc/changr.inc'
        include 'ipfinc/prt.inc'

        common /is_batch / is_batch
c
        character bus2*8, oldsbtyp*2, newsbtyp*2,
     &            fieldfmt(57)*10, brn_type(9)*2, datein*3, 
     &            dateot*3
     
        integer find_bus, fieldptr(10), field(4,57), oldtype,
     &          newtype, gtbrtype
        logical extrat 

        data brn_type /'E*','LM','L ','R ','T ','TP','LD','E ','RZ'/
        data fieldptr / 1, 1, 6, 13, 18, 26, 34, 43, 51, 58 /
        data field /
c
c          Type E* (1)  (NULL)
c
c          Type LM (2)
c
     &      1, 34, 37,  4, 1, 38, 43,  5, 1, 44, 49,  6, 1, 50, 55,  7,
     &      1, 71, 74, 18,
c
c          Type L (3)
c
     &      1, 34, 37,  4, 1, 38, 38, 16, 1, 39, 44,  5, 1, 45, 50,  6,
     &      1, 51, 56,  7, 1, 57, 62,  8, 1, 63, 66,  9,
c
c          Type R (4)
c
     &      1, 46, 50,  6, 1, 51, 55,  7, 1, 56, 57,  8, 1, 58, 62,  9,
     &      1, 63, 67, 10,
c
c          Type T (5)
c
     &      1, 34, 37,  4, 1, 38, 38, 16, 1, 39, 44,  5, 1, 45, 50,  6,
     &      1, 51, 56,  7, 1, 57, 62,  8, 1, 63, 67,  9, 1, 68, 72, 10,
c
c          Type TP (6)
c
     &      1, 34, 37,  4, 1, 38, 38, 16, 1, 39, 44,  5, 1, 45, 50,  6,
     &      1, 51, 56,  7, 1, 57, 62,  8, 1, 63, 67,  9, 1, 68, 72, 10,
c
c          Type LD (7)
c
     &      1, 34, 37,  4, 1, 38, 43,  5, 1, 44, 49,  6, 1, 50, 55,  7,
     &      1, 57, 61,  8, 1, 62, 66,  9, 1, 67, 70, 10, 1, 71, 74, 18,
     &      1, 75, 78, 16,
c
c          Type E (8)
c
     &      1, 34, 37,  4, 1, 38, 38, 16, 1, 39, 44,  5, 1, 45, 50,  6,
     &      1, 51, 56,  7, 1, 57, 62,  8, 1, 63, 68,  9, 1, 69, 74, 10,
c
c          Type RZ (9) 
c
     &      1, 35, 39,  5, 1, 40, 44,  6, 1, 45, 48,  7, 1, 49, 54,  8, 
     &      1, 55, 60,  9, 1, 61, 66, 10, 1, 67, 72, 18 /

        data fieldfmt /
c
c          Type E* (1)  (NULL)
c
c          Type LM (2)
c
     &      '(bz, f4.0)', '(bz, f6.2)', '(bz, f6.2)', '(bz, f6.2)', 
     &      '(bz, f4.0)',
c
c          Type L (3)
c
     &      '(bz, f4.0)', '(bz, f1.0)', '(bz, f6.5)', '(bz, f6.5)', 
     &      '(bz, f6.5)', '(bz, f6.5)', '(bz, f4.1)', 
c
c          Type R (4)
c
     &      '(bz, f5.2)', '(bz, f5.2)', '(bz, f2.0)', '(bz, f5.0)', 
     &      '(bz, f5.0)',
c
c          Type T (5)
c
     &      '(bz, f4.0)', '(bz, f1.0)', '(bz, f6.5)', '(bz, f6.5)', 
     &      '(bz, f6.5)', '(bz, f6.5)', '(bz, f5.2)', '(bz, f5.2)',
c
c          Type TP (6)
c
     &      '(bz, f4.0)', '(bz, f1.0)', '(bz, f6.5)', '(bz, f6.5)', 
     &      '(bz, f6.5)', '(bz, f6.5)', '(bz, f5.2)', '(bz, f5.2)',
c
c          Type LD (7)
c
     &      '(bz, f4.0)', '(bz, f6.2)', '(bz, f6.2)', '(bz, f6.2)', 
     &      '(bz, f5.1)', '(bz, f5.1)', '(bz, f4.1)', '(bz, f4.1)', 
     &      '(bz, f4.0)',
c
c          Type E (8)
c
     &      '(bz, f4.0)', '(bz, f1.0)', '(bz, f6.5)', '(bz, f6.5)', 
     &      '(bz, f6.5)', '(bz, f6.5)', '(bz, f6.5)', '(bz, f6.5)',
c
c          Type RZ (9) 
c
     &      '(bz, f5.0)', '(bz, f5.0)', '(bz, f4.0)', '(bz, f6.5)', 
     &      '(bz, f6.5)', '(bz, f6.5)', '(bz, f6.5)'  /

        read (chgcrd(ic)(122:125), '(i4)') nc
        oldtype = brtype(ptr)
        oldsbtyp = brn_type(oldtype)
        newsbtyp = chgcrd(ic)(1:2)
        newtype = gtbrtype(newsbtyp)

        if (oldtype .ne. newtype) then
           write (errbuf(1), 100) oldsbtyp, newsbtyp
  100      format(' Illegal branch type change (', a, ') -> (',
     &         a, ')')
           write (errbuf(2),110) chgcrd(ic)(1:31), nc
  110      format(' Change record (', a, ') No. ', i5)
           if (is_batch .eq. 0) then
              call prterx ('E',2)
           else
              call prterx ('F',2)
           endif
           go to 920
        endif

        nbr = brnch_ptr(ptr)
        if (nbr .gt. 0) then
c
c          Perform table driven change modifications
c
           do i = fieldptr(oldtype), fieldptr(oldtype+1)-1

              i1 = field(1,i)
              i2 = field(2,i)
              i3 = field(3,i)
              i4 = field(4,i)

              if (chgcrd(ic)(i2:i3) .ne. ' ') then
                 if (index(fieldfmt(i), 'f') .ne. 0) then
c
c                   Floating point decoding
c
                    read (chgcrd(ic)(i2:i3), fieldfmt(i), err=900)
     &                 brnch(i4,nbr)
                 else
c
c                   Integer or character decoding
c  
                    read (chgcrd(ic)(i2:i3), fieldfmt(i), err=900)
     &                 kbrnch(i4,nbr)
                 endif
             endif
           enddo
c                                                                       
c          Implement any date in ownership
c                                                                       
           if (chgcrd(ic)(4:6) .ne. ' ' .and. chgcrd(ic)(1:1) .ne. 'R') 
     &        read (chgcrd(ic)(4:6), '(a3)') kbrnch(3,nbr)
c                                                                       
c          Implement any date in metering point
c                                                                       
           if (chgcrd(ic)(19:19) .ne. ' ') 
     &        read (chgcrd(ic)(19:19), '(i1)', err=900) kbrnch(15,nbr)
c                                                                       
c          Implement any date in changes
c                                                                       
           if (oldtype .ne. 1) then
              datein = chgcrd(ic)(75:77)        
              dateot = chgcrd(ic)(78:80)        
              yearin = energd (datein)       
              if (yearin .eq. -9999.0) then                                   
                 write (errbuf(1), 120) datein
  120            format ('Illegal "date-in" data (', a, 
     &             ') ignored ')
                 write (errbuf(2),110) chgcrd(ic)(1:31), nc
                 if (is_batch .eq. 0) then
                    call prterx ('E',2)
                 else
                    call prterx ('F',2)
                 endif
                 yearin = 0.0          
              endif                           
              yearot = denerg (dateot)    
              if (yearot .eq. -9999.0) then     
                 write (errbuf(1), 130) dateot    
  130            format ('Illegal "date-out" (', a, 
     &              ') ignored ') 
                 write (errbuf(2),110) chgcrd(ic)(1:31), nc
                 call prterx('W', 2)    
                 yearot = 0.0          
              endif                    
              read (datein, 140) kdin   
  140         format (1x,i2)           
                                    
              kdin = intdte(datein(1:1),kdin) 
              kbrnch(11,nbr) = kdin

           endif
c                                                                       
c          Implement any remote bus name changes
c                                                                       
           if (oldtype .eq. 4) then
              read (chgcrd(ic), 150, err=900) bus2, base2 
  150         format (t34, a8, f4.0)                   
              if (bus2 .ne. ' ') then    
                 ix = find_bus (bus2, base2)
                 if (ix .gt. 0) then
                    kbrnch(4,nbr) = ix
                 else
                    write (errbuf(1), 160) bus2, base2
  160               format(' Remotely controlled bus (', a8, f6.1, 
     &                 ') is not in system.')
                    write (errbuf(2),110) chgcrd(ic)(1:31), nc
                    if (is_batch .eq. 0) then
                       call prterx ('E',2)
                    else
                       call prterx ('F',2)
                    endif
                    go to 920
                 endif
              endif
c
c             Process special features not captured in table driver
c
c             1. Examine consistency of R - types
 
              call getchr (1, oldsbtyp(2:2), kbrnch(3,nbr))
 
              if ((index(' OVQN',oldsbtyp(2:2)) .ne. 0 .and.
     &             index(' OVQN', newsbtyp(2:2)) .eq. 0) .or.
     &            (index('PM',oldsbtyp(2:2)) .ne. 0 .and.
     &             index('PM', newsbtyp(2:2)) .eq. 0)) then
                 write (errbuf(1), 180) oldsbtyp, newsbtyp
  180            format(' Illegal branch subtype change (', a, 
     &              ') -> (', a, ')')
                 if (is_batch .eq. 0) then
                    call prterx ('E',1)
                 else
                    call prterx ('F',1)
                 endif
                 go to 920
              endif
           endif
c
c          Implement any change in control point to "LD" branch
c 
           if (oldtype .eq. 7) then
              if (chgcrd(ic)(56:56) .ne. ' ') then
                 if (chgcrd(ic)(56:56) .eq. 'I') then
                   kbrnch(14,nbr) = 2
                 else if (chgcrd(ic)(56:56) .eq. 'R') then
                   kbrnch(14,nbr) = 1
                 else if (chgcrd(ic)(56:56) .eq. 'J') then
                   kbrnch(14,nbr) = 2
                   brnch(8,nbr) = -brnch(8,nbr)
                 else if (chgcrd(ic)(56:56) .eq. 'S') then
                   kbrnch(14,nbr) = 1
                   brnch(8,nbr) = -brnch(8,nbr)
                 endif
              endif
           endif
C 
C          Processing extended branch ratings
C 
           extrat = .false.
           if (oldtype .eq. 2 .or. oldtype .eq. 3 .or.
     &         oldtype .eq. 7 .or. oldtype .eq. 8) then
              if (chgcrd(ic)(81:84) .ne. ' ' .or.
     &            chgcrd(ic)(85:88) .ne. ' ') then
                 extrat = .true.
              endif
           else if (oldtype .eq. 5 .or. oldtype .eq. 6) then
              if (chgcrd(ic)(81:84) .ne. ' ' .or.
     &            chgcrd(ic)(85:88) .ne. ' ' .or.
     &            chgcrd(ic)(89:92) .ne. ' ') then
                 extrat = .true.
              endif
           endif
           if (extrat) then
              do i = 1, 3
                 j1 = 77 + 4*i
                 j2 = j1 + 3
                 if (chgcrd(ic)(j1:j2) .ne. ' ') then
                    read (chgcrd(ic)(j1:j2), '(bz, f4.0)', err=900) 
     &                 rateln(i,nbr) 
                 endif
              enddo
           endif 
 
        endif
        go to 920

  900   write (errbuf(1), 910) chgcrd(ic)(1:80)                                  
  910   format (' Illegal data in field : (',a80,')')                      
        call prterx ('W',1)                                               
        chgcrd(ic)(126:126) = 'E'

  920   continue

        return
        end
