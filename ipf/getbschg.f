C    @(#)getbschg.f	20.4 11/12/98
        subroutine getbschg (ic, outbuffer)
        integer ic
        character outbuffer *(*)
c
c       Parse bus changes in chgcrd(ic) into outbuffer
c
        include 'ipfinc/parametr.inc'

        include 'ipfinc/changr.inc'
        include 'ipfinc/prt.inc'
c
        character xc(20)*10, yc(20)*10, subtype*1
        real xf(20)     
        integer xi(20), bustype
        equivalence (xf, xi)

        read (chgcrd(ic)(122:125), '(bz, i4)', err=900) nc

        do i = 1, 17
           yc(i) = ' '
        enddo

        if (chgcrd(ic)(1:1) .eq. 'B') then

           subtype = chgcrd(ic)(2:2)
           if (subtype .eq. '0') subtype = ' '
           call typnam (subtype, bustype)
           read (chgcrd(ic)(15:18), '(bz, f4.0)', err=900) base1
           write (outbuffer,100) nc, chgcrd(ic)(1:2), chgcrd(ic)(3:3), 

     &                           chgcrd(ic)(4:6), chgcrd(ic)(7:14), 
     &                           base1, chgcrd(ic)(19:20)

  100      format (1x, i4, 1x, a2, 1x, a1, 1x, a3, 1x, a8, f7.1, 1x, 
     &             a2)

           if (bustype .eq. 5) then
c
c             Decode "BD" record
c
              read (chgcrd(ic), 110, err=900) (xc(i),i=1,8)
  110         format(t24, a2, 5a5, a8, a4)

              read (chgcrd(ic), 120, err=900) (xf(i),i=1,6), yc(7),
     &           xf(8)
  120         format (bz, t24, f2.0, 5f5.1, a8, f4.0)
              if (xc(1) .ne. ' ') then
                 write (yc(1), '(f4.0)') xf(1)
              endif
              do i = 2, 6
                 if (xc(i) .ne. ' ') then
                    write (yc(i), '(f7.1)') xf(i)
                 endif
              enddo
              if (xc(8) .ne. ' ') then
                 write (yc(8), '(f6.1)') xf(8)
              endif
              write (outbuffer(34:), 130) (yc(i),i=1,8)
  130         format (a4, 5a7, a9, a6)

           else if (bustype .eq. 12) then
c
c             Decode "BM " record
c
              read (chgcrd(ic), 140, err=900) (xc(i),i=1,13)
  140         format(t24, a2, 5a5, a8, a4, a1, 2a3, a6, a5)

              read (chgcrd(ic), 150, err=900) (xf(i),i=1,6), yc(7),
     &           xf(8), yc(9), (xf(i),i=10,13)
  150         format (bz, t24, f2.0, 5f5.1, a8, f4.0, a1, 2f3.1, f6.1, 

     &           f5.1)
              if (xc(1) .ne. ' ') then
                 write (yc(1), '(f4.0)') xf(1)
              endif
              do i = 2, 6
                 if (xc(i) .ne. ' ') then
                    write (yc(i), '(f7.1)') xf(i)
                 endif
              enddo
              if (xc(8) .ne. ' ') then
                 write (yc(8), '(f6.1)') xf(8)
              endif
              do i = 10, 11
                 if (xc(i) .ne. ' ') then
                    write (yc(i), '(f5.1)') xf(i)
                 endif
              enddo
              if (xc(12) .ne. ' ') then
                 write (yc(12), '(f8.1)') xf(12)
              endif
              if (xc(13) .ne. ' ') then
                 write (yc(13), '(f7.1)') xf(13)
              endif

              write (outbuffer(34:), 160) (yc(i),i=1,13)
  160         format (a4, 5a7, a9, a6, a3, 2a5, a8, a7)

           else 
c
c             Decode all other bus types: "B ", "BE", "BS",
c             "BC", "BV", "BQ", "BG", "BO", "BT", "BX"
c
              read (chgcrd(ic), 170, err=900) (xc(i),i=1,13)
  170         format (t21,  2a5, 3a4, 3a5, 2a4, a8, a4, a3)

              read (chgcrd(ic), 180, err=900) (xf(i),i=1,10), yc(11),
     &           (xf(i),i=12,13)
  180         format (bz, t21,  2f5.0, 3f4.0, 3f5.0, 2f4.3, a8, f4.0, 
     &           f3.0)

              do i = 1, 2
                 if (xc(i) .ne. ' ') then
                    write (yc(i), '(f7.1)') xf(i)
                 endif
              enddo
              do i = 3, 5
                 if (xc(i) .ne. ' ') then
                    write (yc(i), '(f6.1)') xf(i)
                 endif
              enddo
              do i = 6, 8
                 if (xc(i) .ne. ' ') then
                    write (yc(i), '(f7.1)') xf(i)
                 endif
              enddo
              do i = 9, 10
                 if (xc(i) .ne. ' ') then
                    write (yc(i), '(f6.3)') xf(i)
                 endif
              enddo
              if (xc(12) .ne. ' ') then
                 write (yc(12), '(f7.1)') xf(12)
              endif
              if (xc(13) .ne. ' ') then
                 write (yc(13), '(f5.1)') xf(13)
              endif

              write (outbuffer(35:), 190) (yc(i),i=1,13)
  190         format (2a7, 3a6, 3a7, 2a6, 1x, a8, a7, a5)

           endif

        else if (chgcrd(ic)(1:1) .eq. 'X') then

           read (chgcrd(ic)(15:18), '(bz, f4.0)', err=900) base1
           read (chgcrd(ic)(29:32), '(bz, f4.0)', err=900) base2
           write (outbuffer, 200) nc, chgcrd(ic)(1:2), chgcrd(ic)(3:3),
     &                            chgcrd(ic)(4:6), chgcrd(ic)(7:14), 
     &                            base1, chgcrd(ic)(21:28), base2

  200      format (1x, i4, 1x, a2, 1x, a1, 1x, a3, 1x, a8, f7.1, 1x, 
     &             a8, f7.1)

           read (chgcrd(ic), 202, err=900) (xc(i),i=1,16)
  202      format (t33, 8(a1, a5))

           read (chgcrd(ic), 210, err=900) (xf(i),i=1,16)
  210      format (bz, t33, 8(f1.0, f5.0))

           do i = 1, 15, 2
              if (xc(i) .ne. ' ') then
                 write (yc(i), '(f3.0)') xf(i)
              endif
              if (xc(i+1) .ne. ' ') then
                 write (yc(i+1), '(f7.1)') xf(i+1)
              endif
           enddo

           write (outbuffer(48:), 220) (yc(i),i=1,16)
  220      format (8(a3, a7))

        else if (chgcrd(ic)(1:1) .eq. '+') then

           read (chgcrd(ic)(15:18), '(bz, f4.0)', err=900) base1
           write (outbuffer, 230) nc, chgcrd(ic)(1:2), chgcrd(ic)(3:3),
     &                            chgcrd(ic)(4:6), chgcrd(ic)(7:14), 
     &                            base1, chgcrd(ic)(19:20)

  230      format (1x, i4, 1x, a2, 1x, a1, 1x, a3, 1x, a8, f7.1, 1x, 
     &             a2)

           read (chgcrd(ic), 240, err=900) (xc(i),i=1,7)
  240      format (t21,  2a5, 2a4, 4x, 3a5)
           read (chgcrd(ic), 250, err=900) (xf(i),i=1,7)
  250      format (bz, t21,  2f5.0, 2f4.0, 4x, 3f5.0)

           do i = 1, 2
              if (xc(i) .ne. ' ') then
                 write (yc(i), '(f7.1)') xf(i)
              endif
           enddo
           do i = 3, 4
              if (xc(i) .ne. ' ') then
                 write (yc(i), '(f6.1)') xf(i)
              endif
           enddo
           do i = 5, 7
              if (xc(i) .ne. ' ') then
                 write (yc(i), '(f7.1)') xf(i)
              endif
           enddo

           write (outbuffer(35:), 260) (yc(i),i=1,7)
  260      format (2a7, 2a6, 6x, 3a7)

        else if (chgcrd(ic)(1:1) .eq. 'Q') then

           read (chgcrd(ic)(15:18), '(bz, f4.0)', err=900) base1
           write (outbuffer, 270) nc, chgcrd(ic)(1:2), chgcrd(ic)(3:3),
     &                            chgcrd(ic)(4:5), chgcrd(ic)(6:6), 
     &                            chgcrd(ic)(7:14), base1

  270      format (1x, i4, 1x, a2, 1x, a1, 1x, a2, 1x, a1, 1x, a8, 
     &             f7.1)

           read (chgcrd(ic), 280, err=900) (xc(i),i=1,18)
  280      format (t19, a2, 2a5, 15a6)
           read (chgcrd(ic), 290, err=900) xi(1), (xf(i),i=2,17)
  290      format (bz, t19, i2, 2f5.1, 15f6.2)

           if (xc(1) .ne. ' ') write (yc(1), '(i2)') xi(1)

           if (chgcrd(ic)(1:2) .eq. 'QX' .or. 
     &         chgcrd(ic)(1:2) .eq. 'QM') then
             read (chgcrd(ic)(21:30), fmt='(bz, 2f5.3)') xf(2), xf(3)
             do i = 2, 3
               if (xc(i) .ne. ' ') then
                 write (yc(i), '(f8.3)') xf(i)
               else
                 yc(i) = ' '
               endif
             enddo
           else
             do i = 2, 3
               if (xc(i) .ne. ' ') then
                 write (yc(i), '(f8.2)') xf(i)
               else
                 yc(i) = ' '
               endif
             enddo
           endif

           do i = 3, 17
              if (xc(i) .ne. ' ') then
                 write (yc(i), '(f8.2)') xf(i)
              endif
           enddo

           write (outbuffer(33:), 300) (yc(i),i=1,10)
  300      format (10(1x,a8))

        endif
        go to 940

  900   write (errbuf(1), 910) chgcrd(ic)(1:80)
  910   format (' Illegal data in field : (',a80,')')
        call prterx ('W',1)
        chgcrd(ic)(126:126) = 'E'

  940   continue

        return
        end
