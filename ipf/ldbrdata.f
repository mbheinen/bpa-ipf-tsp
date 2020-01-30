C    @(#)ldbrdata.f	20.7 2/28/00
	subroutine ldbrdata (xbuf, mbrsw, yearbr, error)
        character xbuf *(*)
C
C       Decode BCD branch data and load into the branch tables
C                                                                       
        integer  error

      	include 'ipfinc/parametr.inc'

	include 'ipfinc/blank.inc'
      	include 'ipfinc/branch.inc'
      	include 'ipfinc/bus.inc'
      	include 'ipfinc/prt.inc'

        include 'ipfinc/brtype.inc'

        common /old_branch/ old_k1, old_k2, old_sect, old_type, old_id
        integer old_k1, old_k2, old_sect, old_type, bptr1, bptr2, 
     &          msect, find_bus
        character old_id * 1

        common /is_batch / is_batch

        external find_bus, lkbrdata

      	character subtyp*1, loc*1,    id*1,     bus1*8,   
     &            bus2*8,   busc*8,   datein*3, dateot*3
     
        integer p
        logical first_br

        error = 0
        if (ltot+1 .ge. MAXBRN) then
           write (errbuf(1), 110) MAXBRN
  110      format ('More than ',i5,
     &             ' branch records. Overflow occurred at branch:') 
           write (errbuf(2), 120) xbuf(1:80)   
  120      format(11x,'(',a80,')')         
           call prterx ('W',2)                
           ltot = 1                           
        endif
        ltot = ltot + 1
        do i = 1, 18
          kbrnch(i,ltot) = 0
        enddo

c       decode the common info for all branch types...

        read (xbuf, 130, err=900) subtyp,kbrnch(3,ltot),bus1,base1,  
     1     intovr,bus2,base2,id,ksect                        
  130   format (bz,1x,a1,1x,a3,a8,f4.0,i1,a8,f4.0,a1,i1)        
C                                                                       
        if (mbrsw .ne. 1 .and. xbuf(1:2) .eq. 'LD') then        
 
C          If the "LD records originate from the branch data file
C          (MBRSW = 1) they have a standard ENERGIZATION and 
C          DEENERGIZATION DATE format. This time it is non-std.
C                                                                       
           datein = ' ' // xbuf(79:80) 
           dateot = ' '                
        else                           

c          standard data format

           datein = xbuf(75:77)        
           dateot = xbuf(78:80)        
        endif                          

        yearin = energd (datein)       
        if (yearin .eq. -9999.0) then                                   
           write (errbuf(1), 140) datein
  140      format ('Illegal "date-in" data (', a, 
     &             ') ignored on following record')
           if (is_batch .eq. 0) then
              call prterx ('E',1)
              return
           else
              call prterx ('F',1)
              call erexit (0)                                   
           endif
        endif                           

        yearot = denerg (dateot)    
        if (yearot .eq. -9999.0) then     
           write (errbuf(1), 150) dateot    
  150      format ('Illegal "date-out" (', a, 
     &             ') ignored on following record') 
           write (errbuf(2),120) xbuf(1:80)       
           call prterx('W', 2)    
           yearot = 0.0          
        endif                    

        if (mbrsw .eq. 1) then
           if (yearbr .lt. yearin .or. yearbr .ge. yearot) then
              ltot = ltot - 1
              go to 920
           endif
        endif

        read (datein, 160) kdin   
  160   format (bz,1x,i2)           
                                    
        kdin = intdte(datein(1:1),kdin) 

c       Decode the data that is dependend upon branch type..

        if (xbuf(1:2) .eq. 'L ') then
C                                                                       
C          "L" DATA         
C                           
           read (xbuf, 170, err=900) brnch(4,ltot),brnch(16,ltot),
     1        (brnch(k,ltot),k=5,9) 
  170      format (bz,33x,f4.0,f1.0,4f6.5,f4.1) 
           if (brnch(16,ltot).eq. 1.0) brnch(16,ltot) = 0.0 ! num ckts
           mcard=BRTYP_L
           kbrnch(11,ltot)=kdin      

        else if (xbuf(1:1) .eq. 'T') then
C                                                                       
C          "T" DATA                           
C                                                                       
           read (xbuf, 180, err=900) brnch(4,ltot),brnch(16,ltot),
     1        (brnch(k,ltot),k=5,10)          
  180      format (bz,33x,f4.0,f1.0,4f6.5,2f5.2)         
           mcard=BRTYP_T
           if (subtyp.eq. 'P') mcard=BRTYP_TP
           if (brnch(10,ltot).eq.0.0) mcard=BRTYP_TP
           if (brnch(16,ltot).eq. 1.0) brnch(16,ltot) = 0.0 ! num xmfrs
c
c          Store original taps in brnch(13,*) and brnch(14,*) 
c
           brnch(13,ltot) = brnch(9,ltot)
           brnch(14,ltot) = brnch(10,ltot)

        else if (xbuf(1:2) .eq. 'LD') then
c
c            "LD" DATA
c
           read (xbuf, 190, err=900) (brnch(k,ltot),k=4,7),loc,
     1          (brnch(k,ltot),k=8,10),brnch(18,ltot),brnch(16,ltot)
  190      format (bz,33x,f4.0,3f6.2,a1,2f5.1,2f4.1,f4.0)
           mcard = BRTYP_LD
           if (loc .eq. 'J') then            
              loc = 'I'                      
              brnch(8,ltot) = -brnch(8,ltot) 
           else if (loc .eq. 'S') then       
              loc = 'R'                      
              brnch(8,ltot) = -brnch(8,ltot) 
           endif                             
           ksect=0                           
           if (loc.eq.'I') ksect = 2         
           if (loc.eq.'R') ksect = 1         
           id= ' '                           

        else if (xbuf(1:1) .eq. 'E') then
C                                            
C          "E" DATA                          
C                                            
           read (xbuf, 200, err=900) brnch(4,ltot),brnch(16,ltot),
     1        (brnch(k,ltot),k=5,10)         
  200      format (bz,33x,f4.0,f1.0,6f6.5)      
           mcard=BRTYP_E
           if (brnch(16,ltot).eq. 1.0) brnch(16,ltot) = 0.0 ! num ckts

        else if (xbuf(1:2) .eq. 'RZ') then
C                                             
C          "R" regulator or "RZ" variable series compensator Data
C                                                                       
           read (xbuf, 210, err=900)  (brnch(k,ltot),k=4,10),
     1        brnch(18,ltot)
  210      format (bz,33x,f1.0,2f5.0,f4.0,4f6.5)          
           mcard = BRTYP_RZ
           call putchr(1,subtyp,kbrnch(3,ltot))        

        else if (xbuf(1:1) .eq. 'R') then
           read (xbuf, 220, err=900) busc,basec,(brnch(k,ltot),k=6,10)
  220      format(bz,33x,a8,f4.0,2f5.2,f2.0,2f5.0)    
           ksect = 0                               
           id = ' '                                
           mcard = BRTYP_R
           call putchr(1,subtyp,kbrnch(3,ltot))    

        else if (xbuf(1:2) .eq. 'LM') then
C                                                  
C          "LM" DATA                               
C                                                  
           read (xbuf, 230, err=900) (brnch(k,ltot),k=4,7),
     1        brnch(16,ltot) 
  230      format(bz,33x,f4.0,3f6.2,15x,f4.0)         
           mcard = BRTYP_LM
           ksect=0                                 
           id= ' '                                 
        endif
        kbrnch(11,ltot)=kdin                       
        kbrnch(17,ltot)=0                          
        k1 = find_bus (bus1,base1)                 
        if (k1 .le. 0) then
           write (errbuf(1), 240) bus1, base1       
  240      format(' Branch has non-existant terminal bus ', a8,f6.1)
           write (errbuf(2), 120) xbuf(1:80)        
           if (is_batch .eq. 0) then
              call prterx ('E',2)
           else
              call prterx ('F',2)
           endif
        endif
        k2 = find_bus (bus2,base2)                 
        if (k2 .le. 0) then
           write (errbuf(1), 240) bus2, base2       
           write (errbuf(2), 120) xbuf(1:80)        
           if (is_batch .eq. 0) then
              call prterx ('E',2)
           else
              call prterx ('F',2)
           endif
        endif
        if (k1 .eq. 0 .or. k2 .eq. 0) then
           ltot = ltot - 1
           error = 1
           go to 920
        endif

        kbrnch(15,ltot)=intovr                     
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
                 write (errbuf(1), 250) busc,basec
  250            format('LTC transformer has non-existant ',
     &                  'controlled bus ', a8,f6.1,' -record ignored.') 
                 write (errbuf(2),120) xbuf(1:80)
                 call prterx ('W',2)             
              endif
              kbrnch(4,ltot) = kc                  
           endif
        endif
C                                                  
C       Test for extended ratings.                 
C                                                  
        do i = 1, 3
           rateln(i,ltot) = 0.0                    
        enddo

        if (xbuf(81:) .ne. ' ') then               
           if (mbrsw .eq. 1) then                  
              if (kspare(15) .eq. 1) then          
c                                                  
c                Winter peak ratings       
c                                                                       
                 if (index ('LE', xbuf(1:1)) .ne. 0) then  
                    read (xbuf(81:), 260) (rateln(i,ltot),i=1,2)        
  260               format (bz,3f4.0)                        
                 else if (xbuf(1:1) .eq. 'T') then        
                    read (xbuf(81:), 260) (rateln(i,ltot),i=1,3)        
                 endif                                    

              else if (kspare(15) .eq. 2) then            
c                                                         
c                Extra heavy winter peak ratings, default to winter
c                                                         
                 if (index ('LE',xbuf(1:1)) .ne. 0) then  
                    if (xbuf(105:108) .eq. ' ') then
                       xbuf(105:108) = xbuf(81:84)
                    endif
                    if (xbuf(109:112) .eq. ' ') then
                       xbuf(109:112) = xbuf(85:88)
                    endif
                    read (xbuf(105:), 260) (rateln(i,ltot),i=1,2)

                 else if (xbuf(1:1) .eq. 'T') then        
                    if (xbuf(105:108) .eq. ' ') then
                       xbuf(105:108) = xbuf(81:84)
                    endif
                    if (xbuf(109:112) .eq. ' ') then
                       xbuf(109:112) = xbuf(89:92)
                    endif
                    read (xbuf(105:), 260) rateln(1,ltot)            
                    read (xbuf(85:), 260) rateln(2,ltot)
                    read (xbuf(109:), 260) rateln(3,ltot)            
                 endif                                    
              else if (kspare(15) .eq. 3) then            
c                                                                       
c                Moderate winter peak ratings, default to winter
c                                                         
                 if (index ('LE',xbuf(1:1)) .ne. 0) then  
                    if (xbuf(113:116) .eq. ' ') then
                       xbuf(113:116) = xbuf(81:84)
                    endif
                    if (xbuf(117:120) .eq. ' ') then
                       xbuf(117:120) = xbuf(85:88)
                    endif
                    read (xbuf(113:), 260) (rateln(i,ltot),i=1,2)

                 else if (xbuf(1:1) .eq. 'T') then        
                    if (xbuf(113:116) .eq. ' ') then
                       xbuf(113:116) = xbuf(81:84)
                    endif
                    if (xbuf(117:120) .eq. ' ') then
                       xbuf(117:120) = xbuf(89:92)
                    endif
                    read (xbuf(113:), 260) rateln(1,ltot)            
                    read (xbuf(85:), 260) rateln(2,ltot)
                    read (xbuf(117:), 260) rateln(3,ltot)            
                 endif                                    
              else if (kspare(15) .eq. 4) then            
c                                                         
c                Spring peak ratings, default to summer
c                                                         
                 if (index ('LE',xbuf(1:1)) .ne. 0) then 
                    if (xbuf(121:124) .eq. ' ') then
                       xbuf(121:124) = xbuf(93:96)
                    endif
                    if (xbuf(125:128) .eq. ' ') then
                       xbuf(125:128) = xbuf(97:100)
                    endif
                    read (xbuf(121:), 260) (rateln(i,ltot),i=1,2)

                 else if (xbuf(1:1) .eq. 'T') then       
                    if (xbuf(121:124) .eq. ' ') then
                       xbuf(121:124) = xbuf(93:96)
                    endif
                    if (xbuf(125:128) .eq. ' ') then
                       xbuf(125:128) = xbuf(97:100)
                    endif
                    if (xbuf(129:132) .eq. ' ') then
                       xbuf(129:132) = xbuf(101:104)
                    endif
                    read (xbuf(121:), 260) (rateln(i,ltot),i=1,3)
                 endif                                   

              else if (kspare(15) .eq. 8) then            
c                                                         
c                Summer peak ratings              
c                                                         
                 if (index ('LE',xbuf(1:1)) .ne. 0) then  
                    read (xbuf(93:), 260) (rateln(i,ltot),i=1,2)        
                 else if (xbuf(1:1) .eq. 'T') then        
                    read (xbuf(93:), 260) (rateln(i,ltot),i=1,3)        
                 endif                                    
              endif                                      
           else                                          
              if (index ('LE',xbuf(1:1)) .ne. 0) then    
                 read (xbuf(81:), 260) (rateln(i,ltot),i=1,2)           
                 rateln(3,ltot) = 0.0                    
              else if (xbuf(1:1) .eq. 'T') then          
                 read (xbuf(81:), 260) (rateln(i,ltot),i=1,3)           
              else                                       
              endif                                      
           endif                                         
        endif                                            

c       link up branch

        if (msect .ne. 0) then
c             Search the branches for this bus to see if this is the
c             first section encountered on this circuit...
           first_br = .true.
           p = kbsdta(16,k1)
           do while (p .gt. 0)
              if (ky(p).eq.k2 .and. brid(p).eq.id) first_br = .false.
              p = brnch_nxt(p)
           enddo
           if ( first_br ) then
c
c             When this is the first time this sectioned branch has been
c             encountered, Add an entry for the pi-equivalent section
C
              call lkbrdata (ltot, k1, k2, bptr1, bptr2, error)
              kx(bptr1) = k1                 
              ky(bptr1) = k2
              brid(bptr1) = id
              brsect(bptr1) = 0
              brtype(bptr1) = BRTYP_PEQ

              kx(bptr2) = k2                 
              ky(bptr2) = k1
              brid(bptr2) = id
              brsect(bptr2) = 0
              brtype(bptr2) = BRTYP_PEQ

              ltot = ltot + 1
              do i = 1, 18
                 kbrnch(i,ltot) = kbrnch(i,ltot-1)
              enddo
              do i = 1, 3
                 rateln(i,ltot) = rateln(i,ltot-1)
                 rateln(i,ltot-1) = 0.0
              enddo
           else
c             don't make anymore new pie_equivalent entries for
c             this ckt.
           endif
        endif
        call lkbrdata (ltot, k1, k2, bptr1, bptr2, error)
        kx(bptr1) = k1                                                 
        ky(bptr1) = k2
        brid(bptr1) = id
        brsect(bptr1) = ksect
        brtype(bptr1) = mcard

        kx(bptr2) = k2                                                 
        ky(bptr2) = k1
        brid(bptr2) = id
        brsect(bptr2) = ksect
        brtype(bptr2) = mcard

        old_k1 = k1
        old_k2 = k2
        old_id = id

        go to 920

  900   ltot = ltot - 1
        error = 1
        write (errbuf(1), 910) xbuf(1:80)                
  910   format (' illegal data in field :(',a80,')')     
        if (is_batch .eq. 0) then
           call prterx ('E',1)
        else
           call prterx ('F',1)
        endif

  920   return
        end
