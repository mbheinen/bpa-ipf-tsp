C    @(#)brn_dif.f	20.4 2/13/96
        character *(*) function brn_dif (text1, text2)
        character text1 *(*), text2 *(*)
c
c       Determine branch changes between text1 and text2
c
        include 'ipfinc/parametr.inc'

        include 'ipfinc/prt.inc'
c
        common /is_batch / is_batch
 
        character bus1*8, bus2*8
        integer find_bus, fieldptr(10), field(4,57), oldtype, 
     &          gtbrtype
        logical extrat 

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

        brn_dif = text1
        brn_dif(3:3) = 'M'
        brn_dif(4:6) = ' '
        brn_dif(19:19) = ' '
        brn_dif(34:) = ' '

        bus1 = text1(7:14)
        read (text1(15:18), '(bz, f4.0)', err=900) base1
        i1 = find_bus (bus1, base1)
        if (i1 .eq. 0) then
           write (errbuf(1), 100) text1(1:18)
  100      format (' Modified bus is not in system ', a)
           call prterx ('W', 1)
           go to 900
        endif

        bus2 = text2(7:14)
        read (text2(15:18), '(bz, f4.0)', err=920) base2
        i2 = find_bus (bus2, base2)
        if (i2 .eq. 0) then
           write (errbuf(1), 100) text2(1:18)
           call prterx ('W', 1)
           go to 920
        endif

        if (i1 .ne. i2) then
           write (errbuf(1), 110) text1(1:18)
  110      format (' Modified records do not match ', a)
           write (errbuf(2), 120) text2(1:18)
  120      format ('                               ', a)
           call prterx ('W', 2)
           go to 920
        endif

        bus1 = text1(20:27)
        read (text1(28:31), '(bz, f4.0)', err=900) base1
        i1 = find_bus (bus1, base1)
        if (i1 .eq. 0) then
           write (errbuf(1), 100) text1(1:18)
           call prterx ('W', 1)
           go to 900
        endif

        bus2 = text2(20:27)
        read (text2(28:31), '(bz, f4.0)', err=920) base2
        i2 = find_bus (bus2, base2)
        if (i2 .eq. 0) then
           write (errbuf(1), 100) text2(1:18)
           call prterx ('W', 1)
           go to 920
        endif

        if (i1 .ne. i2) then
           write (errbuf(1), 110) text1(1:18)
           write (errbuf(1), 120) text2(1:18)
           call prterx ('W', 2)
           go to 920
        endif

        if (text1(32:32) .ne. text2(32:32)) then
           write (errbuf(1), 110) text1(1:33)
           write (errbuf(2), 110) text2(1:33)
           call prterx ('W',2)
           go to 920
        endif
        
        if (text1(33:33) .ne. text2(33:33)) then
           write (errbuf(1), 110) text1(1:33)
           write (errbuf(2), 110) text2(1:33)
           call prterx ('W',2)
           go to 920
        endif

        if (text1(4:6) .ne. text2(4:6)) brn_dif(4:6) = text2(4:6)
        if (text1(19:19) .ne. text2(19:19)) 
     &      brn_dif(19:19) = text2(19:19)
        if (text1(75:77) .ne. text2(75:77)) 
     &      brn_dif(75:77) = text2(75:77)
        if (text1(78:80) .ne. text2(78:80)) 
     &      brn_dif(78:80) = text2(78:80)

        oldtype = gtbrtype(text2(1:2))
        if (oldtype .eq. 0) then
           write (errbuf(1), 110) text1(1:18)
           call prterx ('W', 2)
           go to 900
        endif
c                                      
c       Implement any remote or commutator bus changes
c                                                                       
        if (oldtype .eq. 4 .and.
     &      text1(34:45) .ne. text2(34:45)) then
           read (text2, 150, err=920) bus2, base2                
  150      format (bz, t34, a8, f4.0)                   
           if (bus2 .ne. ' ') then    
              ix = find_bus (bus2, base2)
              if (ix .gt. 0) then
                 brn_dif(21:32) = text2(21:32)
              else
                 write (errbuf(1), 160) text2(1:18), bus2, base2
  160            format(' Modification ', a, ' has a non-existant',
     &                  ' remotely controlled bus (', a8, f6.1)
                 call prterx ('W', 1)                       
              endif
           endif
        endif
c
c       Examine consistency of R - types
c 
        if (oldtype .eq. 4) then

           if ((index(' OVQN',text1(2:2)) .ne. 0 .and.
     &          index(' OVQN', text2(2:2)) .eq. 0) .or.
     &         (index('PM',text1(2:2)) .ne. 0 .and.
     &          index('PM', text2(2:2)) .eq. 0)) then
              write (errbuf(1), 180) text1(2:2), text2(2:2)
  180         format(' Illegal branch subtype change (', a, 
     &              ') -> (', a, ')')
              if (is_batch .eq. 0) then
                 call prterx ('E',2)
              else
                 call prterx ('F',2)
              endif
              go to 920
           endif
c*JGC   else
c*JGC      write (errbuf(1), 110) text1(1:33)
c*JGC      write (errbuf(2), 110) text2(1:33)
c*JGC      call prterx ('W',2)
c*JGC      go to 920
        endif
c
c       Perform table driven change modifications
c
        do i = fieldptr(oldtype), fieldptr(oldtype+1)-1

           i1 = field(1,i)
           i2 = field(2,i)
           i3 = field(3,i)
           i4 = field(4,i)

           if (text1(i2:i3) .ne. text2(i2:i3)) then
              if (text2(i2:i3) .ne. ' ') then
                 brn_dif(i2:i3) = text2(i2:i3)
              else
                 brn_dif(i2:i3) = '000000000'
              endif
           endif
        enddo
C 
C       Process extended branch ratings
C 
        extrat = .false.
        if (oldtype .eq. 2 .or. oldtype .eq. 3 .or.
     &      oldtype .eq. 7 .or. oldtype .eq. 8) then
           if (text1(81:84) .ne. ' ' .or.
     &         text1(85:88) .ne. ' ' .or.
     &         text2(81:84) .ne. ' ' .or.
     &         text2(85:88) .ne. ' ') then
              extrat = .true.
           endif
        else if (oldtype .eq. 5 .or. oldtype .eq. 6) then
           if (text1(81:84) .ne. ' ' .or.
     &         text1(85:88) .ne. ' ' .or.
     &         text1(89:92) .ne. ' ' .or.
     &         text2(81:84) .ne. ' ' .or.
     &         text2(85:88) .ne. ' ' .or.
     &         text2(89:92) .ne. ' ') then
              extrat = .true.
           endif
        endif
        if (extrat) then
           do i = 1, 3
              i2 = 77 + 4*i
              i3 = i2 + 3
              if (text1(i2:i3) .ne. text2(i2:i3)) then
                 if (text2(i2:i3) .ne. ' ') then
                    brn_dif(i2:i3) = text2(i2:i3)
                 else
                    brn_dif(i2:i3) = '000000000'
                 endif
              endif
           enddo
        endif 
 
        go to 940

  900   write (errbuf(1), 910) text1(1:80)                                  
  910   format (' Illegal data in text1 field : (',a80,')')                      
        call prterx ('W',1)                                               
        go to 940

  920   write (errbuf(1), 910) text2(1:80)                                  
  930   format (' Illegal data in text2 field : (',a80,')')                      
        call prterx ('W',1)                                               

  940   continue

        return
        end
