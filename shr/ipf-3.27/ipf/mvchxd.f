C    @(#)mvchxd.f	20.4 2/13/96
        subroutine mvchxd(nc, ix, ibus)
 
c       moves add/restore xdata from chgcrd(nc) to xdata(ix)
 
        include 'ipfinc/parametr.inc'

        include 'ipfinc/changr.inc'
c	Global variables used:
c		chgcrd
        include 'ipfinc/xdata.inc'
c	Global variables used:
c		xdata(r*8)
        include 'ipfinc/prt.inc'
c	Global variables used:
c		errbuf
 
        common /is_batch / is_batch

        character bus1*8, bus2*8
        integer find_bus

        xdt_flag = .false.   ! Reset X-data flag for interactive
C                            ! status
        
        read (chgcrd(nc), 100, err=900) bus1, base1, bus2, base2,                 
     1     (xdata(k,ix),k=7,22)                                     
  100   format(bz,6x,a8,f4.0,2x,a8,f4.0,8(f1.0,f5.0))                      
        xdata(1,ix) = ibus
        if (bus2 .eq. ' ') then                                        
           xdata(2,ix) = xdata(1,ix)
        else                                                           
           i = find_bus (bus2, base2)
           if (i .gt. 0) then
              xdata(2,ix) = i
           else
              write (errbuf(1), 110) bus2, base2
  110         format('X bus has non-existant ',
     &                  'controlled bus ', a8,f6.1,' -record ignored.') 
              write (errbuf(2), 120) chgcrd(nc)(1:80)
  120         format (11x, '(', a80, ')')
              call prterx ('W',2)             
              chgcrd(nc)(126:126) = 'E'
           endif                                                          
        endif
        totrek = 0.0                                                   
        totcap = 0.0                                                   
        do k = 7, 21, 2                                                
            xtot = xdata(k,ix) * xdata(k+1,ix)                      
            totrek = totrek + amin1 (0.0,xtot)                          
            totcap = totcap + amax1 (0.0,xtot)                             
        enddo
        total = totrek + totcap                                        
        xdata(3,ix) = totrek                                          
        xdata(4,ix) = totcap                                          
        xdata(5,ix) = totrek                                          
        xdata(6,ix) = totcap                                          
        go to 920

  900   write (errbuf(1), 910) chgcrd(nc)(1:80)                
  910   format (' illegal data in field :(',a80,')')     
        if (is_batch .eq. 0) then
           call prterx ('E',1)
        else
           call prterx ('F',1)
        endif
        chgcrd(nc)(126:126) = 'E'

  920   continue
        return
        end
