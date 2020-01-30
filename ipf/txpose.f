C    @(#)txpose.f	20.5 9/10/96
        character *(*) function txpose(xbuf)
C
C       This program was written by Laura Larson 3-13-81.
C       Its purpose is to transpose a branch record (ASCII).
C
        include 'ipfinc/prt.inc'
 
        common /is_batch / is_batch

        character xbuf*(*), ybuf*130, type*1,  subtyp*1,
     1            bus1*8,    base1*4,  meter*1, bus2*8,  intov*1,
     2            loc*1,     cpdc*5,   anon*4,  gmin*4,  g1*6,
     3            b1*6,      g2*6,     b2*6,    ctap1*5, ctap2*5,
     4            cpmax*5,   cpmin*5,  ctmin*5, ctmax*5, code*10,
     5            base2*4,   tempc*130
C
C       Transpose branch data
C
        ybuf = xbuf
        read (ybuf,100,err = 1100) type,subtyp,bus1,base1,
     1     meter,bus2,base2
  100   format(2a1, 4x, a8, a4, a1, a8, a4)
        if (meter .eq. '1') then
           intov = '2'
        else if (meter .eq. '2') then
           intov = '1'
        else
           intov = ' '
        endif
        write (tempc,200) type,subtyp,ybuf(3:6),bus2,
     1     base2,intov,bus1,base1,ybuf(32:)
  200   format (2a1, a4, a8, a4, a1, a8, a4, a)
C
C       Transpose "LD" branch
C
        if (type .eq. 'L' .and. subtyp .eq. 'D') then
           read (ybuf,300,err = 1100) loc,pdc,anon,gmin
  300      format (bz, 55x, a1, f5.1, 5x, 2a4)
           if (loc .eq. 'I') then
              if (abs(pdc) .lt. 1000.) then
                 pdc = -pdc
              else
                 loc = 'J'
              endif
           else if (loc .eq. 'R') then
              if (abs(pdc) .lt. 1000.) then
                 pdc = -pdc
              else
                 loc = 'S'
              endif
           else if (loc .eq. 'J') then
              loc = 'I'
           else if (loc .eq. 'S') then
              loc = 'R'
           endif
           cpdc = code(pdc,5,1)
           if (cpdc .eq. ' ' .and. pdc .eq. 0.0 .and. 
     &         ybuf(57:61) .ne. ' ') cpdc = '0.0'
           write (tempc(56:),400) loc,cpdc,ybuf(62:66),gmin,anon,
     1        ybuf(75:)
  400      format (a1, a5, a5, 2a4, a)
C
C       Transpose "E" branch
C
        else if (type .eq. 'E') then
           read (ybuf,500,err = 1100) g1,b1,g2,b2
  500      format (50x, 4a6)
           write (tempc(51:),600) g2,b2,g1,b1,ybuf(75:)
  600      format (4a6, a)
C
C       Transpose "T" branch
C
        else if (type .eq. 'T') then
           read (ybuf,700,err = 1100) tap1,tap2
  700      format (bz, 62x, 2f5.2)
           if (subtyp .eq. ' ') then
              xxx = tap1
              tap1 = tap2
              tap2 = xxx
              ctap1 = code(tap1,5,2)
              ctap2 = code(tap2,5,2)
              if (ctap1 .eq. ' ' .and. tap1 .eq. 0.0 .and.
     &            ybuf(68:72) .ne. ' ') ctap1 = '0.0'
              if (ctap2 .eq. ' ' .and. tap2 .eq. 0.0 .and.
     &            ybuf(63:67) .ne. ' ') ctap2 = '0.0'
           else
              tap1 = -tap1
              ctap1 = code(tap1,5,2)
              ctap2 = code(tap2,5,2)
              if (ctap1 .eq. ' ' .and. tap1 .eq. 0.0 .and.
     &            ybuf(63:67) .ne. ' ') ctap1 = '0.0'
              if (ctap2 .eq. ' ' .and. tap2 .eq. 0.0 .and.
     &            ybuf(68:72) .ne. ' ') ctap2 = '0.0'
           endif
           write (tempc(63:),800) ctap1, ctap2, ybuf(73:)
  800      format (2a5, a)
C
C       Transpose "R" branch
C
        else if (type .eq. 'R') then
           read (ybuf,900,err = 1100) tmax, tmin, pmax, pmin
  900      format (bz, 45x, 2f5.2, 2x, 2f5.0)
           if (subtyp  .eq. 'P' .or. subtyp .eq. 'M') then
               xxx = tmax
               tmax = -tmin
               tmin = -xxx
               ctmax = code(tmax,5,2)
               ctmin = code(tmin,5,2)
               if (ctmax .eq. ' ' .and. tmax .eq. 0.0 .and.
     &            ybuf(51:55) .ne. ' ') ctmax = '0.0'
               if (ctmin .eq. ' ' .and. tmin .eq. 0.0 .and.
     &            ybuf(46:50) .ne. ' ') ctmin = '0.0'
           else
               ctmax = code(tmax,5,2)
               ctmin = code(tmin,5,2)
               if (ctmax .eq. ' ' .and. tmax .eq. 0.0 .and.
     &            ybuf(46:50) .ne. ' ') ctmax = '0.0'
               if (ctmin .eq. ' ' .and. tmin .eq. 0.0 .and.
     &            ybuf(51:55) .ne. ' ') ctmin = '0.0'
           endif
           if (subtyp .eq. 'Q' .or. subtyp .eq. 'P') then
               pmax = -pmax
               cpmax = code(pmax,5,0)
               cpmin = code(pmin,5,0)
               if (cpmax .eq. ' ' .and. pmax .eq. 0.0 .and.
     &            ybuf(58:62) .ne. ' ') cpmax = '0.0'
               if (cpmin .eq. ' ' .and. pmin .eq. 0.0 .and.
     &            ybuf(63:67) .ne. ' ') cpmin = '0.0'
           else if (subtyp .eq. 'M' .or. subtyp .eq. 'N') then
               xxx = pmax
               pmax = -pmin
               pmin = -xxx
               cpmax = code(pmax,5,0)
               cpmin = code(pmin,5,0)
               if (cpmax .eq. ' ' .and. pmax .eq. 0.0 .and.
     &            ybuf(63:67) .ne. ' ') cpmax = '0.0'
               if (cpmin .eq. ' ' .and. pmin .eq. 0.0 .and.
     &            ybuf(58:62) .ne. ' ') cpmin = '0.0'
           else
              cpmax = ybuf(58:62)
              cpmin = ybuf(63:67)
           endif
           write (tempc(46:),1000) ctmax,ctmin,ybuf(56:57),cpmax,cpmin,
     1        ybuf(68:)
 1000      format (2a5, a2, 2a5, a)
        endif
        txpose = tempc
        return
C
C       This is an error message
C
 1100   write (errbuf(1),1200)
 1200   format(' Illegal character data in field')
        errbuf(2) = ' '
        write (errbuf(3),1300) ybuf(1:80)
 1300   format(' (', a80, ')')
        if (is_batch .eq. 0) then
           call prterx ('E',3)
        else
           call prterx ('F',3)
        endif
        return
        end
