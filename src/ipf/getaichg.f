C    @(#)getaichg.f	20.3 2/13/96
        subroutine getaichg (ic, outbuffer)
        integer ic
        character outbuffer *(*)
c
c       Parse area interchange changes in chgcrd(ic) into outbuffer
c
        include 'ipfinc/parametr.inc'

        include 'ipfinc/changr.inc'
        include 'ipfinc/prt.inc'
c
        character xc(13)*10, yc(13)*10
        real x(13)     

        read (chgcrd(ic)(122:125), '(bz, i4)', err=900) nc

        do i = 1, 13
           yc(i) = ' '
        enddo

        if (chgcrd(ic)(1:1) .eq. 'A') then

           read (chgcrd(ic)(22:25), '(bz, f4.0)', err=900) base1
           write (outbuffer,100) nc, chgcrd(ic)(1:2), chgcrd(ic)(3:3), 
     &        chgcrd(ic)(4:13), chgcrd(ic)(14:21), base1

  100      format (1x, i4, 1x, a2, 1x, a1, 1x, a10, 1x, a8, f7.1, 1x)

           read (chgcrd(ic), 110, err=900) (xc(i),i=1,13)
  110      format(t27, a8, t36, 10(a2,1x), t73, 2a4)
           read (chgcrd(ic), 120, err=900) x(1), (yc(i),i=2,11), 
     &         (x(i),i=12,13)
  120      format (bz, t27, f8.0, 1x, 10(a2,1x), t73, 2f4.3)
           if (xc(1) .ne. ' ') then
              write (yc(1), '(f10.1)') x(1)
           endif
           do i = 12, 13
              if (xc(i) .ne. ' ') then
                 write (yc(i), '(f5.3)') x(i)
              endif
           enddo
           write (outbuffer(40:), 130) (yc(i),i=1,13)
  130      format (a10, 1x, 10(a2,1x), 2x, 2a6)

        else if (chgcrd(ic)(1:1) .eq. 'I') then

           write (outbuffer, 270) nc, chgcrd(ic)(1:2), chgcrd(ic)(3:3), 
     &                            chgcrd(ic)(4:13), chgcrd(ic)(15:24)

  270      format (1x, i4, 1x, a2, 1x, a1, 1x, a10, 1x, a10, 1x)

           read (chgcrd(ic), 280, err=900) xc(1)
  280      format (t27, a8)
           read (chgcrd(ic), 290, err=900) x(1)
  290      format (bz, t27, f8.0)

           if (xc(1) .ne. ' ') then
              write (yc(1), '(f10.1)') x(1)
           endif

           write (outbuffer(35:), 300) yc(1)
  300      format (a10)

        endif
        go to 940

  900   write (errbuf(1), 910) chgcrd(ic)(1:80) 
  910   format (' Illegal data in field : (',a80,')')                      
        call prterx ('W',1)                                               
        chgcrd(ic)(126:126) = 'E'

  940   continue

        return
        end
