C    @(#)mvchcb.f	20.3 2/13/96
        subroutine mvchcb(nc, ic, ibus)
 
c       Moves add/restore continuation bus from chgcrd(nc) to 
c       kbctbl(*,ic) table
 
        include 'ipfinc/parametr.inc'

        include 'ipfinc/cbus.inc'
        include 'ipfinc/changr.inc'
        include 'ipfinc/prt.inc'
 
        character month*1                                        

        read (chgcrd(nc), 100, err=110) bctbl(8,ic), bctbl(10,ic),           
     &     bus2, base2, bctbl(9,ic), (bctbl(k,ic),k=2,6),
     &     bctbl(11,ic), bctbl(12,ic), month, nyear                                       
  100   format (bz,1x,a1,1x,a3,a8,f4.0,a2,2f5.0,2f4.0,4x,3f5.0,t75,
     &      a1,i2)                                                
        kbctbl(1,ic) = ibus
        kbctbl(7,ic) = intdte(month,nyear)                               
        go to 130

  110   write (errbuf(1), 120) chgcrd(nc)(1:80)                                  
  120   format (' Illegal data in field : (', a80, ')')                      
        call prterx ('W',1)                                               
        chgcrd(nc)(126:126) = 'E'

  130   continue

        return
        end
