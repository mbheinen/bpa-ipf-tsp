C    @(#)mod_cbus.f	20.3 2/13/96
        subroutine mod_cbus (ic, icb)
        integer ic, icb
c
c       Perform continuation bus modification changes from chgcrd(ic) 
c       onto bctbl(*,icb).
c
        include 'ipfinc/parametr.inc'

        include 'ipfinc/cbus.inc'
        include 'ipfinc/changr.inc'
        include 'ipfinc/prt.inc'
c
        character fieldfmt(7)*10, month*1
        integer field(4,7)

        data field /
     &      1, 21, 25,  2, 1, 26, 30,  3, 1, 31, 34,  4, 1, 35, 38, 5,
     &      1, 43, 47,  6, 1, 48, 52, 11, 1, 53, 57, 12 /

        data fieldfmt /
     &      '(bz, f5.0)', '(bz, f5.0)', '(bz, f4.0)', '(bz, f4.0)', 
     &      '(bz, f5.0)', '(bz, f5.0)', '(bz, f5.0)' /

        read (chgcrd(ic)(122:125), '(bz, i4)') nc
c
c       Perform table driven change modifications
c
        do i = 1, 7

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
     &              bctbl(i4,icb)
              else
c
c                Integer or character decoding
c  
                 read (chgcrd(ic)(i2:i3), fieldfmt(i), err=900)
     &              kbctbl(i4,icb)
              endif
           endif
        enddo
c
c       Implement any changes in datein
c
        if (chgcrd(ic)(75:77) .ne. ' ') then
           read (chgcrd(ic)(75:77), 140, err=900) month, kdin   
  140      format (a1, i2)           
                                    
           kdin = intdte(month, kdin) 
           kbctbl(7, icb) = kdin
        endif

        go to 920

  900   write (errbuf(1), 910) chgcrd(ic)(1:80)                                  
  910   format (' Illegal data in field : (',a80,')')                      
        call prterx ('W',1)                                               
        chgcrd(ic)(126:126) = 'E'

  920   continue

        return
        end
