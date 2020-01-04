C    @(#)mvchbu.f	20.4 2/13/96
        subroutine mvchbu (ic, ibus)
 
c       Moves add/restore bus from chgcrd(ic) to busdta(*,ibus) table
 
        include 'ipfinc/parametr.inc'

        include 'ipfinc/alpha.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/changr.inc'
        include 'ipfinc/prt.inc'
	 
        common /is_batch / is_batch

        character bus2*8, subtyp*1
        integer find_bus
c                                                                       
c       read and decode the bus data cards.                               
c                                                                       
        kbsdta(15,ibus) = 0
        kbsdta(16,ibus) = 0
        subtyp = chgcrd(ic)(2:2)
        call typnam (subtyp,ktype)                                         

        if (subtyp .eq. 'D') then
c                                                                       
c          Decode type "BD" bus                                              
c                                                                       
           read (chgcrd(ic), 100, err=900) owner(ibus), zone(ibus), 
     &        (busdta(k,ibus),k=3,8), bus2, base2                
  100      format (bz,t4,a3,t19,a2,3x,f2.0,5f5.1,a8,f4.0)                   
           if (bus2.eq.' ') then                                              
              kbsdta(9,ibus) = 0                                               
           else                                                              
              ix = find_bus (bus2, base2)
              if (ix .gt. 0) then
                 kbsdta(9,ibus) = ix
              else
                 read (chgcrd(ic)(122:125), '(bz, i4)') nc
                 write (errbuf(1), 110) bus2, base2
  110            format(' Commutator bus is not in system, bus ',
     &              a8, f6.1)
                 write (errbuf(2), 120) chgcrd(ic)(1:33), nc
  120            format( ' Change ', a, ' No. ',i5)
                 if (is_batch .eq. 0) then
                    call prterx ('E',2)
                 else
                    call prterx ('F',2)
                 endif
                 chgcrd(ic)(126:126) = 'E'
              endif
           endif
        else if (subtyp .eq. 'M') then
c                                                              
c          Read and decode type "BM" bus                      
c                                                              
           read (chgcrd(ic), 130, err=900) owner(ibus), zone(ibus),
     &        (busdta(k,ibus),k=3,8), bus2, base2,            
     &        busdta(12,ibus), (busdta(k,ibus),k=10,11),   
     &        (busdta(k,ibus),k=13,14)                     
  130      format(bz,t4,a3,t19,a2,3x,f2.0,5f5.1,a8,f4.0,a1,2f3.1,f6.1,
     &        f5.1)                                                          
           if (bus2.eq.' ') then                   
              kbsdta(9,ibus)=0                     
           else                                    
              ix = find_bus (bus2, base2)
              if (ix .gt. 0) then
                 kbsdta(9,ibus) = ix
              else
                 read (chgcrd(ic)(122:125), '(bz, i4)') nc
                 write (errbuf(1), 110) bus2, base2
                 write (errbuf(2), 120) chgcrd(ic)(1:33), nc
                 if (is_batch .eq. 0) then
                    call prterx ('E',2)
                 else
                    call prterx ('F',2)
                 endif
                 chgcrd(ic)(126:126) = 'E'
              endif
           endif
        else 
c                                                                       
c          Read and decode all other type buses                                     
c                                                                       
           read (chgcrd(ic), 140, err=900) owner(ibus), zone(ibus), 
     &        (busdta(k,ibus),k=3,12)                             
  140      format (bz,t4,a3,t19,a2,2f5.0,3f4.0,3f5.0,2f4.3)                 
           do i=13, 16                                                    
              busdta(i,ibus) = 0.0                                                
           enddo
           kbsdta(1,ibus) = ktype                                              
c                                                                       
c          Decode type "BG" or "BX" buses                                    
c                                                                      
           if (subtyp .eq. 'G' .or. subtyp .eq. 'X') then
              read (chgcrd(ic), 150, err=900) bus2, base2, pct     
  150         format (bz,t66,a8,f4.0,f3.0)                  
              ix = find_bus (bus2, base2)
              if (ix .gt. 0) then
                 kbsdta(13,ibus) = ix
              else if (bus2 .ne. ' ') then
                 read (chgcrd(ic)(122:125), '(bz, i4)') nc
                 write (errbuf(1), 160) bus2, base2
  160            format(' Remotely controlled bus is not in system, bus 
     &', a8, f6.1)
                 write (errbuf(2), 120) chgcrd(ic)(1:33), nc
                 if (is_batch .eq. 0) then
                    call prterx ('E',2)
                 else
                    call prterx ('F',2)
                 endif
                 chgcrd(ic)(126:126) = 'E'
              else
                 kbsdta(13,ibus) = 0
              endif
              busdta(14,ibus) = pct                        
           endif
        endif
        kbsdta(1,ibus)=ktype                                              
        call vltlim(ibus, vlimn(ibus), vlimx(ibus), vstart(ibus)) 
        e(ibus) = vstart(ibus)                                            
        f(ibus) = 0.0                                                     
        capcor(1,ibus) = 0.0                                              
        capcor(2,ibus) = -9.0e10                                          
        inp2opt(ibus) = ibus                                                
        opt2inp(ibus) = ibus
        go to 920

  900   write (errbuf(1), 910) chgcrd(ic)(1:80)                                  
  910   format (' Illegal data in field : (',a80,')')                      
        call prterx ('W',1)                                               
        chgcrd(ic)(126:126) = 'E'

  920   continue

        return
        end
