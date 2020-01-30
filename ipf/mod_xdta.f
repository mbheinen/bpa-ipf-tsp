C    @(#)mod_xdta.f	20.4 2/13/96
        subroutine mod_xdta (ic, ix)
        integer ic, ix
c
c       Perform switched reactance modification changes from chgcrd(ic) 
c       onto xdata(*,ix).
c
        include 'ipfinc/parametr.inc'

        include 'ipfinc/alpha.inc'
c	Global variables used:
c		None
        include 'ipfinc/blank.inc'
c	Global variables used:
c		None
        include 'ipfinc/changr.inc'
c	Global variables used:
c		chgcrd
        include 'ipfinc/prt.inc'
c	Global variables used:
c		errbuf
        include 'ipfinc/xdata.inc'
c	Global variables used:
c		xdata(r*8)
  
        common /is_batch / is_batch
c
        character bus2*8, fieldfmt(16)*10
        integer find_bus, field(4,16), itemp

        data field /
     &      1, 33, 33,  7, 1, 34, 38,  8, 1, 39, 39,  9, 1, 40, 44, 10,
     &      1, 45, 45, 11, 1, 46, 50, 12, 1, 51, 51, 13, 1, 52, 56, 14,
     &      1, 57, 57, 15, 1, 58, 62, 16, 1, 63, 63, 17, 1, 64, 68, 18,
     &      1, 69, 69, 19, 1, 70, 74, 20, 1, 75, 75, 21, 1, 76, 80, 22 /

        data fieldfmt /
     &      '(bz, f1.0)', '(bz, f5.0)', '(bz, f1.0)', '(bz, f5.0)', 
     &      '(bz, f1.0)', '(bz, f5.0)', '(bz, f1.0)', '(bz, f5.0)', 
     &      '(bz, f1.0)', '(bz, f5.0)', '(bz, f1.0)', '(bz, f5.0)',
     &      '(bz, f1.0)', '(bz, f5.0)', '(bz, f1.0)', '(bz, f5.0)' /

        read (chgcrd(ic)(122:125), '(bz, i4)') nc
c
c       Perform table driven change modifications
c
        do i = 1, 12

           i1 = field(1,i)
           i2 = field(2,i)
           i3 = field(3,i)
           i4 = field(4,i)

           if (chgcrd(ic)(i2:i3) .ne. ' ') then
              if (index (fieldfmt(i), 'f') .ne. 0) then
c
c                Floating point decoding
c
                 read (chgcrd(ic)(i2:i3), fieldfmt(i), err=900)
     &              xdata(i4,ix)
              else
c
c                Integer or character decoding
c  
                 read (chgcrd(ic)(i2:i3), fieldfmt(i), err=900)
     &               itemp
                 xdata(i4,ix) = itemp
              endif
           endif
        enddo

        read (chgcrd(ic), 130, err=900) bus2, base2                
  130   format (bz, t21, a8, f4.0)                   
        if (bus2 .ne. ' ') then    
           i = find_bus (bus2, base2)
           if (i .gt. 0) then
              xdata(2,ix) = i
           else
              write (errbuf(1), 140) bus2, base2
  140         format(' Remotely controlled bus (', a8, f6.1, 
     &           ') is not in system.')
              write (errbuf(2), 150) chgcrd(ic)(1:31), nc
  150         format(' Change record (', a, ') No. ', i5)
              if (is_batch .eq. 0) then
                 call prterx ('E',2)
              else
                 call prterx ('F',2)
              endif
           endif
        else
           xdata(2,ix) = 0.0d0
        endif
        totrek = 0.0
        totcap = 0.0
        do k = 7,21,2
           xtot = xdata(k,ix) * xdata(k+1,ix)
           totrek = totrek + amin1(0.0,xtot)
           totcap = totcap + amax1(0.0,xtot)
        enddo
        xdata(3,ix) = totrek
        xdata(4,ix) = totcap
        xdata(5,ix) = totrek
        xdata(6,ix) = totcap
        go to 920

  900   write (errbuf(1), 910) chgcrd(ic)(1:80)                                  
  910   format (' Illegal data in field : (',a80,')')                      
        call prterx ('W',1)                                               
        chgcrd(ic)(126:126) = 'E'

  920   continue

        return
        end
