C    @(#)getbrchg.f	20.3 2/13/96
        subroutine getbrchg (ic, outbuffer)
        integer ic
        character outbuffer *(*)
c
c       Parse changes in chgcrd(ic) into outbuffer
c
        include 'ipfinc/parametr.inc'

        include 'ipfinc/branch.inc'
        include 'ipfinc/changr.inc'
        include 'ipfinc/prt.inc'
c
        character xc(10)*10, yc(10)*10, linefeed*1
        real x(10)     
        integer linetype, gtbrtype

        linefeed = char(10)
        read (chgcrd(ic)(122:125), '(bz, i4)', err=900) nc

        do i = 1, 10
           yc(i) = ' '
        enddo

        linetype = gtbrtype(chgcrd(ic)(1:2))

        if (linetype .gt. 1 .and. linetype .lt. 9) then

           read (chgcrd(ic)(15:18), '(bz, f4.0)', err=900) base1
           read (chgcrd(ic)(28:31), '(bz, f4.0)', err=900) base2
           write (outbuffer,100) nc, chgcrd(ic)(1:2), chgcrd(ic)(3:3), 
     &                           chgcrd(ic)(4:6), chgcrd(ic)(7:14), 
     &                           base1, chgcrd(ic)(19:19), 
     &                           chgcrd(ic)(20:27), base2,
     &                           chgcrd(ic)(32:32), chgcrd(ic)(33:33)

  100      format (1x, i4, 1x, a2, 1x, a1, 1x, a3, 1x, a8, f7.1, 1x, 
     &             a1, 1x, a8, f7.1, 1x, a1, 1x, a1)

           if (linetype .eq. 2) then
c
c             Decode "LM" record
c
              read (chgcrd(ic), 110, err=900) (xc(i),i=1,5)
  110         format(t34, a4, 3a6, t71, a4)         
              read (chgcrd(ic), 120, err=900) (x(i),i=1,5)
  120         format(bz, t34, f4.0, 3f6.2, t71, f4.0)         
              if (xc(1) .ne. ' ') then
                 write (yc(1), '(f6.0)') x(i)
              endif
              do i = 2, 4
                 if (xc(i) .ne. ' ') then
                    write (yc(i), '(f8.2)') x(i)
                 endif
              enddo
              if (xc(5) .ne. ' ') then
                 write (yc(5), '(f6.0)') x(5)
              endif
              write (outbuffer(53:), 130) (yc(i),i=1,5), 
     &           chgcrd(ic)(75:77)
  130         format (a6, 3a8, 15x, a6, 1x, a3)

           else if (linetype .eq. 3) then
c
c             Decode "L " record
c
              read (chgcrd(ic), 140, err=900) (xc(i),i=1,7)
  140         format(t34, a4, a1, 4a6, a4)         
              read (chgcrd(ic), 150, err=900) (x(i),i=1,7)
  150         format (bz, t34, f4.0, f1.0, 4f6.5, f4.1) 

              if (xc(1) .ne. ' ') then
                 write (yc(1), '(f6.0)') x(1)
              endif
              if (xc(2) .ne. ' ') then
                 write (yc(2), '(f3.0)') x(2)
              endif
              do i = 3, 6
                 if (xc(i) .ne. ' ') then
                    write (yc(i), '(f8.5)') x(i)
                 endif
              enddo
              if (xc(7) .ne. ' ') then
                 write (yc(7), '(f6.1)') x(7)
              endif
              write (outbuffer(53:), 160) (yc(i),i=1,7), 
     &           chgcrd(ic)(75:77)
  160         format (a6, a4, 4a8, a6, 1x, a3)

           else if (linetype .eq. 4) then
c
c             Decode "R " record
c
              read (chgcrd(ic), 170, err=900) (xc(i),i=1,7)
  170         format(t34, a8, a4, 2a5, a2, 2a5)         
              read (chgcrd(ic), 180, err=900) xc(1), (x(i),i=2,7)
  180         format(bz, t34, a8, f4.0, 2f5.2, f2.0, 2f5.0)    
              if (xc(2) .ne. ' ') then
                 write (yc(2), '(f6.1)') x(2)
              endif
              do i = 3, 4
                 if (xc(i) .ne. ' ') then
                    write (yc(i), '(f7.2)') x(i)
                 endif
              enddo
              if (xc(5) .ne. ' ') then
                 write (yc(5), '(f4.0)') x(5)
              endif
              do i = 6, 7
                 if (xc(i) .ne. ' ') then
                    write (yc(i), '(f7.0)') x(i)
                 endif
              enddo
              write (outbuffer(53:), 190) (yc(i),i=1,7)
  190         format (a8, a6, 2a7, a4, 2a7)

           else if (linetype .eq. 5 .or. linetype .eq. 6) then
c
c             Decode "T " or "TP" record
c
              read (chgcrd(ic), 200, err=900) (xc(i),i=1,8)
  200         format (t34, a4, a1, 4a6, 2a5)
              read (chgcrd(ic), 210, err=900) (x(i),i=1,8)
  210         format (bz, t34, f4.0, f1.0, 4f6.5, 2f5.2)         

              if (xc(1) .ne. ' ') then
                 write (yc(1), '(f6.0)') x(1)
              endif
              if (xc(2) .ne. ' ') then
                 write (yc(2), '(f3.0)') x(2)
              endif
              do i = 3, 6
                 if (xc(i) .ne. ' ') then
                    write (yc(i), '(f8.5)') x(i)
                 endif
              enddo
              do i = 7, 8
                 if (xc(i) .ne. ' ') then
                    write (yc(i), '(f7.2)') x(i)
                 endif
              enddo

              write (outbuffer(53:), 220) (yc(i),i=1,8), 
     &           chgcrd(ic)(75:77)
  220         format (a6, a3, 4a8, 2a7, 2x, a3)

           else if (linetype .eq. 7) then
c
c             Decode "LD" record
c
              read (chgcrd(ic), 230, err=900) (xc(i),i=1,10)
  230         format (t34, a4, 3a6, a1, 2a5, 3a4)
              read (chgcrd(ic), 240, err=900) (x(i),i=1,4), yc(5), 
     &           (x(i),i=6,10)
  240         format (bz, t34, f4.0, 3f6.2, a1, 2f5.1, 2f4.1, f4.0)

              if (xc(1) .ne. ' ') then
                 write (yc(1), '(f6.0)') x(1)
              endif
              do i = 2, 4
                 if (xc(i) .ne. ' ') then
                    write (yc(i), '(f8.2)') x(i)
                 endif
              enddo
              do i = 6, 7
                 if (xc(i) .ne. ' ') then
                    write (yc(i), '(f7.1)') x(i)
                 endif
              enddo
              do i = 8, 9
                 if (xc(i) .ne. ' ') then
                    write (yc(i), '(f6.1)') x(i)
                 endif
              enddo
              if (xc(10) .ne. ' ') then
                 write (yc(10), '(f6.0)') x(10)
              endif
              write (outbuffer(53:), 250) (yc(i),i=1,10), 
     &           chgcrd(ic)(79:80)
  250         format (a6, 3a8, a3, 2a7, 3a6, 1x, a2)

           else if (linetype .eq. 8) then
c
c             Decode "E " record
c
              read (chgcrd(ic), 260, err=900) (xc(i),i=1,8)
  260         format (t34, a4, a1, 6a5)
              read (chgcrd(ic), 270, err=900) (x(i),i=1,8)
  270         format (bz, t34, f4.0, f1.0, 6f6.5) 

              if (xc(1) .ne. ' ') then
                 write (yc(1), '(f6.0)') x(1)
              endif
              if (xc(2) .ne. ' ') then
                 write (yc(2), '(f3.0)') x(2)
              endif
              do i = 3, 8
                 if (xc(i) .ne. ' ') then
                    write (yc(i), '(f8.5)') x(i)
                 endif
              enddo
              write (outbuffer(53:), 280) (yc(i),i=1,8), 
     &           chgcrd(ic)(75:77)
  280         format (a6, a3, 6a8, 1x, a3)

           else if (linetype .eq. 9) then
c
c             Decode "RZ" record
c
              read (chgcrd(ic), 290, err=900)  (xc(i),i=1,8)
  290         format (t34, a1, 2a5, a4, 4a6)
              read (chgcrd(ic), 300, err=900)  yc(1), (x(i),i=2,8)
  300         format (bz, t34, a1, 2f5.0, f4.0, 4f6.5)          

              do i = 2, 3
                 if (xc(i) .ne. ' ') then
                    write (yc(i), '(f7.0)') x(i)
                 endif
              enddo
              if (xc(4) .ne. ' ') then
                 write (yc(4), '(f6.0)') x(4)
              endif
              do i = 5, 8
                 if (xc(i) .ne. ' ') then
                    write (yc(i), '(f8.5)') x(i)
                 endif
              enddo
              write (outbuffer(53:), 310) (yc(i),i=1,8), 
     &           chgcrd(ic)(75:77)
  310         format (a3, 2a7, a6, 4a8, 1x, a4)

           endif
C 
C          Process change in extended branch ratings
C 
           last = lastch (outbuffer)
           if (chgcrd(ic)(81:92) .ne. ' ') then
              outbuffer(last+1:) = linefeed
              read (chgcrd(ic)(81:92), '(bz, 3f4.0)', err=900) 
     &           r1, r2, r3
              write (outbuffer(last+2:), '(3f6.0)', err=900)
     &           r1, r2, r3
           endif
 
        endif
        go to 940

  900   write (errbuf(1), 910) chgcrd(ic)(1:80)  
  910   format (' Illegal data in field : (',a80,')')                      
        call prterx ('W',1)                                               
        chgcrd(ic)(126:126) = 'E'

  940   continue

        return
        end

