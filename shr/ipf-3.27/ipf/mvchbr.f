C    @(#)mvchbr.f	20.4 2/13/96
        subroutine mvchbr (nc, ib)
 
c       Moves add/restore branch data from chgcrd(*,nc) to brnch(*,ib)
c
        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/brtype.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/changr.inc'
        include 'ipfinc/prt.inc'
 
        common /is_batch / is_batch

      	character subtyp*1, loc*1,    id*1,     bus1*8,   
     &            bus2*8,   busc*8,   datein*3, dateot*3
        integer find_bus
 
        read (chgcrd(nc), 100, err=900) subtyp, brnch(3,ib), 
     &     bus1, base1, intovr, bus2, base2, id, ksect                        
  100   format (bz,1x,a1,1x,a3,a8,f4.0,i1,a8,f4.0,a1,i1)        
c                                                                       
c       standard data format
c
        datein = chgcrd(nc)(75:77)        
        dateot = chgcrd(nc)(78:80)        

        yearin = energd (datein)       
        if (yearin .eq. -9999.0) then                                   
           write (errbuf(1), 110) datein
  110      format ('Illegal "date-in" data (', a, 
     &             ') ignored on following record')
           write (errbuf(2), 120) chgcrd(nc)(1:80)       
  120      format (11x, '(', a80, ')')
           call prterx('W', 2)    
           yearin = 0.0
        endif                           
        yearot = denerg (dateot)    
        if (yearot .eq. -9999.0) then     
           write (errbuf(1), 130) dateot    
  130      format ('Illegal "date-out" (', a, 
     &             ') ignored on following record') 
           write (errbuf(2), 120) chgcrd(nc)(1:80)       
           call prterx('W', 2)    
           yearot = 0.0          
        endif                    
        read (datein, 140, err=900) kdin   
  140   format (bz,1x,i2)           
                                    
        kdin = intdte(datein(1:1),kdin) 

c       Decode data according to branch type

        if (chgcrd(nc)(1:2) .eq. 'L ') then
C                                                                       
C          "L" DATA         
C                           
           read (chgcrd(nc), 150, err=900) brnch(4,ib),brnch(16,ib),
     &        (brnch(k,ib),k=5,9) 
  150      format (bz,33x,f4.0,f1.0,4f6.5,f4.1) 
           if (brnch(16,ib).eq. 1.0) brnch(16,ib) = 0.0 ! num ckts
           kbrnch(11,ib)=kdin      
           mcard = BRTYP_L

        else if (chgcrd(nc)(1:1) .eq. 'T') then
C                                                                       
C          "T" or "TP" data                           
C                                                                       
           read (chgcrd(nc), 160, err=900) brnch(4,ib),brnch(16,ib),
     &        (brnch(k,ib),k=5,10)          
  160      format (bz,33x,f4.0,f1.0,4f6.5,2f5.2)         
           mcard = BRTYP_T
           if (subtyp.eq. 'P') mcard = BRTYP_TP
           if (brnch(10,ltot).eq.0.0) mcard = BRTYP_TP
           if (brnch(16,ib).eq. 1.0) brnch(16,ib) = 0.0 ! num xmfrs

        else if (chgcrd(nc)(1:2) .eq. 'LD') then
c
c            "LD" DATA
c
           read (chgcrd(nc), 170, err=900) (brnch(k,ib),k=4,7),loc,
     &          (brnch(k,ib),k=8,10),brnch(18,ib),brnch(16,ib)
  170      format (bz,33x,f4.0,3f6.2,a1,2f5.1,2f4.1,f4.0)
           if (loc .eq. 'J') then            
              loc = 'I'                      
              brnch(8,ib) = -brnch(8,ib) 
           else if (loc .eq. 'S') then       
              loc = 'R'                      
              brnch(8,ib) = -brnch(8,ib) 
           endif                             
           ksect=0                           
           if (loc.eq.'I') ksect = 2         
           if (loc.eq.'R') ksect = 1         
           id = ' '                           
           mcard = BRTYP_LD

        else if (chgcrd(nc)(1:1) .eq. 'E') then
C                                            
C          "E" data                          
C                                            
           read (chgcrd(nc), 180, err=900) brnch(4,ib),brnch(16,ib),
     &        (brnch(k,ib),k=5,10)         
  180      format (bz,33x,f4.0,f1.0,6f6.5)      
           if (brnch(16,ib).eq. 1.0) brnch(16,ib) = 0.0 ! num ckts
           mcard = BRTYP_E

        else if (chgcrd(nc)(1:2) .eq. 'RZ') then
C                                             
C          "RZ" variable series compensator Data
C                                                                       
           read (chgcrd(nc), 190, err=900)  (brnch(k,ib),k=4,10),
     &        brnch(18,ib)
  190      format (bz,33x,f1.0,2f5.0,f4.0,4f6.5)          
           call putchr(1,subtyp,kbrnch(3,ib))        
           mcard = BRTYP_RZ

        else if (chgcrd(nc)(1:1) .eq. 'R') then
C                                             
C          "R" regulator
C                                                                       
           read (chgcrd(nc), 200, err=900) busc, basec,
     &        (brnch(k,ib),k=6,10)
  200      format(bz,33x,a8,f4.0,2f5.2,f2.0,2f5.0)    
           ksect = 0                               
           id = ' '                                
           call putchr(1,subtyp,kbrnch(3,ib))    
           mcard = BRTYP_R

        else if (chgcrd(nc)(1:2) .eq. 'LM') then
C                                                  
C          "LM" data                               
C                                                  
           read (chgcrd(nc), 210, err=900) (brnch(k,ib),k=4,7),
     &        brnch(16,ib) 
  210      format(bz,33x,f4.0,3f6.2,15x,f4.0)         
           ksect=0                                 
           id= ' '                                 
           mcard = BRTYP_LM
        endif

        kbrnch(11,ib)=kdin                       
        kbrnch(17,ib)=0                          
        kbrnch(15,ib)=intovr                     
        if (ksect .gt. 0) then                     
           if (mcard .eq. BRTYP_LM .or. mcard .eq. BRTYP_R .or. 
     &         mcard .eq. BRTYP_LD) then
              msect = 0           
           else
              msect = ksect
           endif
        else
           msect = 0
        endif                                      
        kc=0                                       
        if (mcard .eq. BRTYP_R) then
           if (index (' VOPMNQ',subtyp) .gt. 0) then
              kc = find_bus (busc,basec)       
              if (kc .le. 0) then
                 write (errbuf(1), 220) busc,basec
  220            format('LTC transformer has non-existant ',
     &                  'controlled bus ', a8,f6.1,' -record ignored.') 
                 write (errbuf(2), 120) chgcrd(nc)(1:80)
                 call prterx ('W',2)             
              endif
              kbrnch(4,ib) = kc                  
           endif
        endif
C                                                  
C       Test for extended ratings.                 
C                                                  
        if (chgcrd(nc)(81:) .ne. ' ') then               
           if (mcard .eq. BRTYP_L .or. mcard .eq. BRTYP_E) then    
              read (chgcrd(nc), 230, err=900) (rateln(i,ib),i=1,2)           
  230         format (bz,80x,3f4.0)                        
              rateln(3,ib) = 0.0                    
           else if (mcard .eq. BRTYP_T .or. mcard .eq. BRTYP_TP) then    
              read (chgcrd(nc), 230) (rateln(i,ib),i=1,3)           
           else                                       
              rateln(1,ib) = 0.0                    
              rateln(2,ib) = 0.0                    
              rateln(3,ib) = 0.0                    
           endif                                      
        endif                                            

        return

  900   errbuf(1) = ' illegal data in field for change record :'
        errbuf(2) = ' ' // chgcrd(nc)
        if (is_batch .eq. 0) then
           call prterx ('E',2)
        else
           call prterx ('F',2)
        endif
        chgcrd(nc)(126:126) = 'E'

        return

        end
