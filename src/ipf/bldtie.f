C    @(#)bldtie.f	20.8 1/7/99
C****************************************************************
C
C   File: bldtie.f
C   Purpose: Build area intertie TIE array from scratch using sorted
c            and linked bus-branch data.
C
C   Author: IPF Staff    Date: circa 1980
C   Called by: 
C
C****************************************************************
      subroutine bldtie (zonex, numznx)                                
                                                                         
      include 'ipfinc/parametr.inc' 
c		
      include 'ipfinc/arcntl.inc' 
      include 'ipfinc/area.inc' 
      include 'ipfinc/blank.inc' 
      include 'ipfinc/branch.inc' 
      include 'ipfinc/bus.inc' 
      include 'ipfinc/cbus.inc' 
      include 'ipfinc/prt.inc' 
      include 'ipfinc/qsdup.inc' 
      include 'ipfinc/slnopt.inc' 
      include 'ipfinc/ycomp.inc' 

c     Set up branch type codes... 
      include 'ipfinc/brtype.inc' 
c	Global variables used:
c		brtyp_R, brtyp_RZ, brtyp_PEQ, brtyp_LM, brtyp_L, 
c		brtyp_T, brtyp_TP, brtyp_E
c
      common /scratch/ locsec(10), locbr(10), numac(MAXCAR),  
     &                 numdc(MAXCAR), oldnet(MAXCAR),  
     &                 nsysno(4*MAXTIE) 
 
      common /is_batch / is_batch

      character zn*2, xbuf*120, id*1, lown*4, flag*1, zonex(*)*(*) 
 
      external kmpzone, swpzon, kmparc, swparc, kmpari, 
     &         swpari, find_bus, find_zon, find_ara 
 
      integer find_bus, find_zon, find_ara, p, pnxt, q, qnxt, gtmetpnt,
     &        kerr
c
      complex * 16 y(2,2)
c
      logical xflag 
  
c     Build list of zones                                              
                                                                         
      kerr = 0                                                           
      kabort=0                                                       
      jtie = 0                                                           
      nztot = 0
C
C     Transfer and d-c zones into a-c system data.
C
      do i = 1, numznx
           do j = 1, nztot
              if (acznam(j) .eq. zonex(i)) go to 90
           enddo
           nztot = nztot + 1
           acznam(nztot) = zonex(i)
           acznum(nztot) = 0
   90      continue
      enddo
                            
c     Transfer a-c zones into system data.                     
                            
      do 140 ix = 1, ntot_alf
         i = alf2inp(ix)
         zn=zone(i)         
         do j=1,nztot   
            if (acznam(j) .eq. zn) go to 140  
         enddo
         nztot=nztot+1                        
         if (nztot .gt. MAXCZN) then 
            write (errbuf(1),120) MAXCZN      
  120       format(' More than ',i4, 
     1          ' unique zone names. Area control aborted.') 
            call prterx ('W',1)               
            jtie=0                            
            kabort = 1                        
            kerr=1                            
            go to 900                         
         endif 
         acznam(nztot)=zn                     
         acznum(nztot)=0                      
  140 continue                                
                                              
      if (nztot .gt. 1) then 
         call qiksrt(1, nztot, kmpzone, swpzon) 
      endif 
       
c     Search for duplicate areas                                       
                                                                         
      if (iopton(17) .eq. 3) then                                      
         if (ntotc.gt.0) then                                          
            iopton(17)=1                                               
         else                                                          
            iopton(17)=0                                               
         endif                                                         
      endif                                                            
                                                                         
      if (ntotc .eq. 0) then 
         if (iopton(17) .ne. 0) then               
            write (errbuf(1),150)                  
  150       format('0 No Area Interchange data encountered. Area ', 
     1             'Interchange Control aborted.')  
            call prterx ('W',1)               
            kerr=1                            
            kabort=1                          
            iopton(17) = 0                    
         endif                                
         go to 900                            
      endif 
                                                                         
c     Sort area interchange "AC" records                               
                                                                         
      dupsw = .false.    
      call qiksrt (1, ntotc, kmparc, swparc) 
      if (dupsw) then                            
                                                 
c         Flag and remove duplicate records       
                                                 
         j = 1                                   
         k = 2                                   
  152    if (k .le. ntotc) then                  
            if ( kmparc(j,k) .lt. 0 ) then       
               j = j + 1                                            
               if (j .lt. k) call swparc (j,k)                       
               k = k + 1                                            
            else                                                    
               write (errbuf(1),154)                                
  154          format('0 Duplicate area interchange records..', 
     &                ' Case Aborted!')         
               call bcdarc (j,xbuf)                               
               write (errbuf(2), 156) xbuf(1:80) 
  156          format(1x,'(',a,')')                                 
               call bcdarc (k,xbuf)                               
               write (errbuf(3), 156) xbuf(1:80) 
               if (is_batch .eq. 0) then
                 call prterx ('E',3)
               else
                 call prterx ('F',3)
               endif
               k = k + 1 
               kabort=1 
               kerr=1 
            endif  
            goto 152                                                
         else                                                       
            ntotc = j                                               
         endif                                                      
      endif 
                                                                         
c     Sort area intertie "I" records                                   
                                                                         
      if (ntotic.gt.0) then                                            
         dupsw = .false.                      
         call qiksrt (1, ntotic, kmpari, swpari)    
         if (dupsw) then                   
                                           
c           Remove duplicate records       
                                           
            ntotxx = ntotic                
            do 210 i=2,ntotxx              
               if (i .gt. ntotic) go to 212   
               if (arcint(1,i) .eq. arcint(1,i-1) .and.    
     1             arcint(2,i) .eq. arcint(2,i-1)) then    
                                                           
c                  Duplicate found. If low-high alpha, delete second  
c                  entity. If high-low alpha, delete corresponding  
c                  transposed duplicate intertie.           
                                                                         
                  if (kompr(arcint(1,i),arcint(2,i),junk).lt.0) then 
                     ikeep = i                                 
                     idelet = i-1                              
                  else                                         
                     do 160 j=1,i-1                            
                       if (arcint(1,j) .eq. arcint(2,i) .and.  
     1                    arcint(2,j) .eq. arcint(1,i)) then   
                          if (arcinp(j).eq.-arcinp(i)) then    
                             ikeep = i                         
                             idelet = i-1                      
                          else                                 
                             ikeep = i-1                       
                             idelet = i                        
                          endif                                
                       go to 180                               
                       endif                                   
  160                continue                                  
                     write (errbuf(1),170)                     
  170                format (' Transpose of area intertie "I" ', 
     &                       'record is not in system.')  
                     call bcdari (i,xbuf)               
                     write (errbuf(2),122) xbuf(1:80)     
  122                format(2x,'(', a, ')') 
                     if (is_batch .eq. 0) then
                        call prterx ('E',2)
                     else
                        call prterx ('F',2)
                     endif
                     ikeep = i-1            
                     idelet = i             
                  endif                     
  180             write (errbuf(1),190)     
  190             format ('0 Duplicate area intertie "I" records. ', 
     &                    ' Case Aborted!')         
                  call bcdari (ikeep,xbuf)      
                  write(errbuf(2),156) xbuf(1:80) 
                  call bcdari (idelet,xbuf)     
                  write (errbuf(3),156) xbuf(1:80) 
                  if (is_batch .eq. 0) then
                     call prterx ('E',3)
                  else
                     call prterx ('F',3)
                  endif
                  if (ikeep.lt.idelet) then       
                     istart = ikeep               
                  else                            
                     istart = ikeep + 2           
                  endif                           
                  do 200 j=istart,ntotic          
                     arcint(1,j-1) = arcint(1,j)  
                     arcint(2,j-1) = arcint(2,j)  
                     arcinp(j-1) = arcinp(j)      
  200             continue                        
                  ntotic = ntotic - 1             
                  kabort=1 
                  kerr=1 
               endif                              
  210       continue                             
  212       continue 
         endif                                   
      endif 
                                                                         
c     Complete "area" array                                             
                                                                         
c     If area intertie records are present, use them to define         
c     net area export                                                  
                                                                         
      if (ntotic.gt.0) then                                            
                                                                         
         do i=1,ntotc                      
            oldnet(i) = arcnet(i)                
            arcnet(i) = -1.0e-10                 
         enddo 
         do i=1,ntotic                                           
            ka1 = find_ara(arcint(1,i))  
            if (ka1 .le. 0) then 
               write (errbuf(1),1220)  
 1220          format(' Area 1 of "I" record is not in system. Area',  
     &                ' interchange aborted.')     
               errbuf(2) = ' '    
               call bcdari (i,xbuf)  
               write (errbuf(3),122) xbuf(1:80)  
               if (is_batch .eq. 0) then
                  call prterx ('E',3)
               else
                  call prterx ('F',3)
               endif
               kerr = 1                          
               kabort = 1                        
            else 
               arcnet(ka1) = arcnet(ka1) + arcinp(i)/bmva         
            endif                                                 
         enddo 
         do i = 1,ntotc                                          
            if (arcnet(i).eq.1.0e-10) then                        
               write(errbuf(1),1250) arcnam(i)                           
 1250          format(' AC record ',a, 
     &                ' has missing intertie "I" records.')  
               if (is_batch .eq. 0) then
                  call prterx ('E',1)
               else
                  call prterx ('F',1)
               endif
               kerr = 1                                      
               kabort = 1                                    
            else if (abs (arcnet(i)-oldnet(i)) .gt. 1.0e-3) then  
               write(outbuf,1252) arcnam(i),oldnet(i)*bmva, 
     &            arcnet(i)*bmva                                       
 1252          format (' Ac record ',a, 
     &                 ' has net modified by "I" records. ', 
     &                 'Original net:',f10.1, 
     &                 ' mw; new net :',f10.1,' mw.')       
               call prtout (1)                              
            endif                                           
         enddo 
      endif                                                            
                                                                         
      xtot=0.0                                                         
      do 282 i=1,ntotc                                                 
                                                                         
c        Process area slack bus       
                                                                         
         k1 = find_bus (arcbus(i),arcbas(i))  
         if (k1 .le. 0) then 
            write (errbuf(1),220) arcbus(i),arcbas(i),arcnam(i) 
  220       format(' Slack bus ',a8,f6.1,' for area ',a10, 
     &             ' is not in system. Area control aborted.')  
            if (is_batch .eq. 0) then
              call prterx ('E',1)
            else
              call prterx ('F',1)
            endif
            kabort=1                                            
            kerr=1                                              
            k1=1                                                
         endif 
                                                                
         karea(1,i)=k1                                          
         area(2,i)=dble(arcnet(i))
         xtot=xtot+arcnet(i)                                    
                                                                
         do 231 j=3,8                                           
  231    karea (j,i)=0                                          
         numac(i) = 0                                           
         numdc(i) = 0                                           
                                                                         
c        Obtain count NUMXZN of zones for this area.            
                                                                
         do 232 j = 1, MAXCAZ                                   
            zn = arczns(j,i)                                    
            if (zn .eq. '  ' .and. j .gt. 1) then               
               numxzn = j - 1                                   
               go to 234                                        
            endif                                               
  232    continue                                               
         numxzn = MAXCAZ                                        
  234    continue                                               
                                                                         
c        Check for duplicate or non-existant zones within each area. 
                                                                         
         j = 1
         do while (j .le. numxzn)
                                                          
c           Check for zone in system.                     
                                                          
  236       do k = 1, nztot                           
               if (acznam(k) .eq. arczns(j,i)) go to 243  
            enddo
            write (errbuf(1), 238) arczns(j,i), arcnam(i) 
  238       format(' Zone ',a2,' in area ',a10, 
     &             ' is not in system. Zone ignored.') 
            call bcdarc (i,xbuf)                 
            errbuf(2) = ' '                        
            write (errbuf(3),122) xbuf(1:80)       
            call prterx ('I',3)                    
            do l = j, numxzn-1                 
               arczns(l,i) = arczns(l+1,i)         
            enddo
            arczns(numxzn,i) = ' '                 
            numxzn = numxzn - 1                    
            if (j .le. numxzn) then                
               go to 236                           
            else                                   
               go to 248                           
            endif                                  
                                                   
c           Zone is in system.  Check for duplicates.  
                                                       
  243       do k = j+1, numxzn                     
               if (arczns(k,i) .eq. arczns(j,i) .and.  
     1             arczns(k,i) .ne. ' ') then          
                  write (errbuf(1),244) arczns(j,i), arcnam(i) 
  244             format('Duplicate zone ',a2,' in area ',a10, 
     1             ' removed from record.')                              
                  call bcdarc (i, xbuf)                      
                  errbuf(2) = ' '                              
                  write (errbuf(3),122) xbuf(1:80)             
                  call prterx ('W',3)                          
                  do l = k, numxzn-1                       
                     arczns(l,i) = arczns(l+1,i)               
                  enddo
                  arczns(numxzn,i) = ' '                       
                  numxzn = numxzn - 1                          
                  if (j .le. numxzn) then                      
                     go to 236                                 
                  else                                         
                     go to 248                                 
                  endif                                        
               endif                                           
            enddo
            j = j + 1
         enddo
  248    continue                                              
                                                                         
c        Find area number of each zone.                        
                                                               
         do 280 j = 1, numxzn                                  
            zn = arczns(j,i)                                   
            k = find_zon(zn) 
            if (k .le. 0) then                                 
               write (errbuf(1),260) zn,arcnam(i)              
  260          format(' Zone "',a2,'" in controlled area ',a10, 
     &                ' is not in system. Zone ignored.')  
               call bcdarc (i,xbuf)                      
               errbuf(2) = ' '                             
               write (errbuf(3),122) xbuf(1:80)            
               call prterx ('I',3)                         
               kerr = 1                                    
               go to 280                                   
            endif 
                                                           
            k1 = acznum(k)                                 
            if (k1 .eq. 0) go to 276                       
            if (k1 .eq. i) go to 280                       
                                                                         
            write (errbuf(1),272) arczns(j,i)              
  272       format(' Zone "',a2, 
     &             '" is duplicated is the following areas:')  
             
            call bcdarc (k1,xbuf)                        
            errbuf(2) = ' '                                
            write (errbuf(3),122) xbuf(1:80)               
            call bcdarc (i,xbuf)                         
            write (errbuf(4),122) xbuf(1:80)               
            write (errbuf(5),274)                          
  274       format(' Area interchange control aborted.')   
            if (is_batch .eq. 0) then
               call prterx ('E',5)
            else
               call prterx ('F',5)
            endif
            kerr=1                                         
            kabort = 1                                     
            go to 280                                      
                                                           
  276       acznum(k)=i                                    
  280    continue                                          
 
  282 continue                                             
 
      xtot=xtot*bmva                                       
      if (abs (xtot) .gt. 0.1) then 
         write (errbuf(1),290) xtot                                
  290    format(' Net area export does not balance : error ', 
     &          '(mismatch) = ',f8.1,' mw. Area control aborted.') 
         if (is_batch .eq. 0) then
            call prterx ('E',1)
         else
            call prterx ('F',1)
         endif
         kerr=1                                                    
         kabort=1                                                  
      endif 
                                                                         
c     Check for missing zones                                          
                                                                         
      do 320 i=1,nztot                                                 
         if (acznum(i) .eq. 0) then 
             write (errbuf(1),310) acznam(i)          
  310        format(' Zone "',a2,'" in system is not specified in ', 
     &              'any controlled area. ', 
     &              'Interchange control aborted.')                 
             if (is_batch .eq. 0) then
                call prterx ('E',1)
             else
                call prterx ('F',1)
             endif
             kerr=1                                                 
             kabort=1                                               
         endif 
  320 continue                                                         
                                                                         
c     Find area number of each bus                                     
                                                                         
      do 370 i=1,ntot                                                  
         if (inp2alf(i) .le. ntot_alf) then
            zn=zone(i)                                        
            jarzn(i)=0                                       
            do j=1,nztot                                 
               if (zn .eq. acznam(j)) go to 340              
            enddo
            call erexit (1)
  340       k = acznum(j)                                    
            if (k .eq. 0) then 
                                                          
               write (errbuf(1),350) zn,bus(i),base(i)       
  350          format(' Zone "',a2,'", bus ',a8,f6.1, 
     &             ' is not specified in any controlled area. ', 
     &             'Interchange control aborted.') 
               if (is_batch .eq. 0) then
                  call prterx ('E',1)
               else
                  call prterx ('F',1)
               endif
               kerr=1                                        
               kabort=1                                      
               go to 370                                     
            else 
               jarzn(i) = k                                  
            endif 
         endif
  370 continue                                                         
                                                                         
c     Check area interchange for inclusion of system slack buses.      
                                                                         
      do 380 i = 1, nbslck                         
         k1=nslkxx(1,i)                            
         n=jarzn(k1)                               
         if (n.gt.0) karea(8,n) = karea(8,n) + 1   
  380 continue                                     
                                                   
      do 430 i=1,ntotc                             
         n=karea(8,i)                              
         if (n .gt. 0) then 
            do 390 j=1 ,nbslck                     
               if (karea(1,i).eq.nslkxx(1,j)) go to 430  
  390       continue                                     
            k1 = karea(1,i)                              
                                                                         
            write (errbuf(1),400) arcnam(i),bus(k1),base(k1)  
  400       format(' Area ',a10,' slack bus ',a8,f6.1, 
     &             ' is not one of the following system slack buses:') 
            do 420 j=1,nbslck                                      
               k1=nslkxx(1,j)                                      
               if (jarzn(k1) .eq. i) then 
                  write (errbuf(n+1),410) bus(k1),base(k1),zone(k1) 
  410             format(23x,'bus ',a8,f6.1,' zone "',a2,'"')       
               endif 
  420       continue                                                
            call prterx ('W',n+1)                                   
         endif 
  430 continue                                                         
  440 continue                                                         
                                                                         
c     check validity of area slack bus                                 
                                                                         
      do 460 i=1,ntotc                                                 
         k1=karea(1,i)                                              
         if (jarzn(k1) .ne. i) then 
            write (errbuf(1),450) arcnam(i),bus(k1),base(k1),zone(k1) 
  450       format(' Area ',a10,' slack bus ',a8,f6.1,' in zone "',a2, 
     &             '" is external to the area.')                    
            if (is_batch .eq. 0) then
               call prterx ('E',1)
            else
               call prterx ('F',1)
            endif
            kerr=1                                              
         endif 
                                                                         
c        Determine initial and final slack bus quantities    
                                                             
  452    pgen = busdta(8,k1)                                 
         pmax = busdta(7,k1)                                 
         ncb = kbsdta(15,k1)                                 
         do while (ncb .gt. 0) 
            pgen = pgen + bctbl(6,ncb)       
            ncb = bctbl_nxt(ncb) 
         enddo 
                                             
  456    area(6,i)=dble(pmax)                      
         area(7,i)=dble(pgen)                
         area(8,i)=dble(pgen)                 
  460 continue                                                         
 
c     Build tie array                                                  
                                                                         
      jtie=0                                                           
      if (kabort .ne. 0) then                                            
         iopton(17) = 0                                                
         go to 900                                                     
      endif                                                            
                                                                         
      do 750 k1 = 1, ntot 
         p = kbsdta(16,k1) 
         do while (p .gt. 0) 
            k2 = ky(p) 
            if (inp2alf(k1) .lt. inp2alf(k2)) then 

c              Process branches low-to-high only. 

               if (jarzn(k1) .ne. jarzn(k2)) then 
                  ltype = brtype(p) 
                  id = brid(p) 
                  if (ltype .eq. BRTYP_R .or. ltype .eq. BRTYP_RZ) then 
c
c                    Set id to NULL to force next-parallel logic into 
c                    first transformer.
c
                     id = char(0)
                  else
                     q = brnch_ptr(p) 
                     nbr = iabs(q) 
                     jtie=jtie+1             
                     if (jtie .gt. MAXTIE) then 
                        write (errbuf(1),530) MAXTIE   
  530                   format(' More than ',i4, 
     &                         ' interchange tie lines.', 
     &                         'Interchange control aborted.')     
                        if (is_batch .eq. 0) then
                           call prterx ('E',1)
                        else
                           call prterx ('F',1)
                        endif
                        jtie=1               
                        kabort=1             
                     endif 
                                                                         
c                    Determine metering point 
                                                                         
                     intovr = kbrnch(15,nbr) 
                     if (intovr .gt. 0 .and. q .lt. 0)  
     &                  intovr = 3 - intovr 
                     if (ltype .eq. BRTYP_LD) then   
  
c                       If DC line, set metering point at receiving end 
 
                        if (intovr .eq. 0) then  
                           pdc = brnch(8,nbr)     
                           if (q .lt. 0) pdc = -pdc 
                           if (pdc .lt. 0.0) then  
                              intovr = 1 
                           else                                
                              intovr = 2 
                           endif                               
                        endif                                  
                        if (q .lt. 0) then 
                           kbrnch(15,nbr) = 3 - intovr 
                        else
                           kbrnch(15,nbr) = intovr 
                        endif
                     else if (ltype .eq. BRTYP_PEQ) then 
  
c                       check for consistency in sections. 
  
                        jt = 1 
                        locsec(jt) = 0
                        locbr(jt) = p

                        pnxt = brnch_nxt(p) 
                        do while (pnxt .gt. 0) 
                           if (ky(pnxt) .eq. k2) then 
                              if (brid(pnxt) .eq. id   .and. 
     &                            brsect(pnxt) .gt. 0  .and. 
     &                            brtype(pnxt) .ne. BRTYP_RZ) then      
                                 jt = jt + 1 
                                 locbr(jt) = pnxt 
                                 qnxt = brnch_ptr(pnxt) 
                                 nbrnxt = iabs(qnxt) 
                                 intovr2 = kbrnch(15,nbrnxt) 
                                 if (intovr2 .gt. 0 .and. qnxt .lt. 0)  
     &                              intovr2 = 3 - intovr2 
                                 if (intovr2  .ne. 0) then       
                                    locsec(jt) = intovr2 + 100   
                                 else                            
                                    call getchr (3, lown,
     &                                           kbrnch(3,nbrnxt)) 
                                    if (owner(k1) .eq. owner(k2)) then 
                                       locsec(jt) = 1                  
                                       if (inp2alf(k1) .gt. inp2alf(k2))
     &                                    locsec(jt) = 2  
                                    else if (lown .eq. owner(k1) .and.
     &                                       lown .ne. owner(k2)) then 
                                       locsec(jt) = 2 + 10             
                                    else if (lown .eq. owner(k2) .and.
     &                                       lown .ne. owner(k1)) then 
                                       locsec(jt) = 1 + 10             
                                    else                               
                                       locsec(jt) = 1                  
                                       if (inp2alf(k1) .gt. inp2alf(k2))
     &                                     locsec(jt) = 2 
                                    endif                       
                                 endif                          
                              endif                             
                           endif 
                           pnxt = brnch_nxt(pnxt) 
                        enddo 
                                                                         
c                       examine consistency of metering point   
                                                                         
                        loc1=0                                  
                        loc2=0                                  
                                                                         
                        do 580 j=1,jt                           
                          if (mod(locsec(j),10).eq.1)  
     &                       loc1=loc1+locsec(j)                  
                          if (mod(locsec(j),10).eq.2)  
     &                       loc2=loc2+locsec(j) - 1              
  580                   continue                                
                                                                         
                        ksw = 2                                 
                        if (loc1.gt.loc2) then                  
                           loc = 1                              
                        else if (loc2.gt.loc1) then             
                           loc = 2                              
                        else if (inp2alf(k1).lt.inp2alf(k2)) then 
                           loc = 1                              
                        else                                    
                           loc = 2                              
                        endif                                   

                        intovr = loc   ! Metering point established !

                        if (loc1 .ne. 0 .and. loc2 .ne.0) ksw = 1 
                        if (jt .gt. 1) then 
                           if (ksw .eq. 1) then                 
                              write (errbuf(1),590)             
  590                         format(' Inconsistent metering point ', 
     &                               'on Area Interchange tie line. ', 
     &                               '("*" indicates selected ', 
     &                               'metering point.)' )         
                              write (errbuf(2),600)             
  600                         format(2x,'Metering point     ', 
     &                               '-------- Line -----------')     
                           endif                                
                                                                         
                           do j=1,jt                            
                              pnxt = locbr(j) 
                              errbuf(j+2) = ' '                 
                              qnxt = brnch_ptr(pnxt) 
                              nbrnxt = iabs(qnxt) 
                              if (brtype(pnxt) .ne. BRTYP_PEQ) then 
                                 call bcdbrn (pnxt,xbuf)  
                                 loc1 = mod (locsec(j),10)  
                                 flag = ' '                 
                                 if (loc .eq. loc1) flag = '*'  
                                 if (ksw .eq. 1) then 
                                    write (errbuf(j+2),610) loc1, flag,
     &                                 xbuf(1:80)   
  610                               format(10x,i1,a1,9x,'(',a80,')')  
                                 endif 
                              endif 
                              if (brnch_ptr(pnxt) .gt. 0) then 
                                 kbrnch(15,nbrnxt)=loc          
                              else 
                                 kbrnch(15,nbrnxt)=3-loc        
                              endif 
                           enddo 
                           if (ksw .eq. 1) call prterx ('W',jt+2)  
                        endif 
                     else 
  
c                       Process other branches (no sections or  
c                       d-c lines) 
  
                        if (intovr  .ne. 0) then               
                        else                                   
                           call getchr (3,lown,kbrnch(3,nbr))   
                           if (owner(k1) .eq. owner(k2)) then  
                              intovr = 1                   
                              if (inp2alf(k1) .gt. inp2alf(k2)) 
     &                           intovr = 2   
                           else                                
                              if (lown .eq. owner(k1) .and.
     &                            lown .ne. owner(k2)) then    
                                 intovr = 2 
                              else if (lown .eq. owner(k2) .and.
     &                                 lown .ne. owner(k1)) then 
                                 intovr = 1 
                              else                               
                                 intovr = 1                  
                                 if (inp2alf(k1) .gt. inp2alf(k2))  
     &                               intovr = 2  
                              endif                              
                           endif                                 
                        endif                                    
                     endif 
c
c                    Check derived metering point for consistency
c                    with function gtmetpnt()
c
                     loc = gtmetpnt(p)
                     if (intovr .ne. loc) then
                        write (errbuf(1), 620) bus(k1), base(k1),
     &                     bus(k2), brid(p), intovr, loc
  620                   format(' Inconsistently derived metering point o
     &n tie line ', a8, f7.1, 1x, a8, f7.1, 1x, a1, 2i2)
                        call prterx ('W',1)                                    
                     endif
c
c                    build tie entity according to metering point  
                                                                         
                     if (intovr .eq.1) then                           
                        tie(1,jtie)=k1                            
                        tie(7,jtie)=k2                            
                        tie(2,jtie)=jarzn(k1)                     
                        tie(8,jtie)=jarzn(k2)                     
                     else                                          
                        tie(1,jtie)=k2                            
                        tie(7,jtie)=k1                            
                        tie(2,jtie)=jarzn(k2)                     
                        tie(8,jtie)=jarzn(k1)                     
                     endif                                         
 
                     tie(10,jtie)=p                               
                     if (ltype .eq. BRTYP_LM) then 
                        tie(9,jtie) = 1                             
                     else if (ltype .eq. BRTYP_LD) then 
                        tie(9,jtie) = 2                              
                     else 
                        tie(9,jtie) = 0 
                     endif 
 
                     if (ltype.eq.BRTYP_PEQ) then  
                        if ((intovr .eq. 1 .and. q .gt. 0) .or. 
     &                      (intovr .eq. 2 .and. q .lt. 0)) then 
                            tie (3,jtie)=dble(brnch(4,nbr))
                            tie (4,jtie)=dble(brnch(5,nbr))
                            tie (5,jtie)=dble(brnch(6,nbr))
                            tie (6,jtie)=dble(brnch(7,nbr))
                        else                                       
                            tie(3,jtie)=brnch(10,nbr)               
                            tie(4,jtie)=brnch(11,nbr)               
                            tie(5,jtie)=brnch(8,nbr)                
                            tie(6,jtie)=brnch(9,nbr)                
                        endif                                      
                     else if (ltype .eq. BRTYP_L .or.  
     1                        ltype .eq. BRTYP_T .or. 
     2                        ltype .eq. BRTYP_TP .or.  
     3                        ltype .eq. BRTYP_E) then 
                        call pieqiv (p,y,kerr)                   
                        if (intovr .eq. 1) then                       
                           tie(3,jtie)=dreal (y(1,1))               
                           tie(4,jtie)=dimag(y(1,1))               
                           tie(5,jtie)=dreal(y(1,2))                
                           tie(6,jtie)=dimag(y(1,2))               
                        else                                       
                           tie (3,jtie)=dreal(y(2,2))               
                           tie (4,jtie)=dimag(y(2,2))              
                           tie (5,jtie)=dreal(y(2,1))               
                           tie (6,jtie)=dimag(y(2,1))              
                        endif                                      
                     endif                                         
                                                                   
                     ka1=jarzn(k1)                                 
                     ka2=jarzn(k2)                                 
                     if (ltype.eq.BRTYP_LM .or. ltype.eq.BRTYP_LD) then 
                        numdc(ka1)=numdc(ka1)+1                    
                        numdc(ka2)=numdc(ka2)+1                    
                     else                                          
                        numac(ka1)=numac(ka1)+1                    
                        numac(ka2)=numac(ka2)+1                    
                     endif                                         
                  endif 
               endif 
            endif 
  
c           Advance to next parallel branch 
  
            p = brnch_nxt(p) 
            xflag = .false. 
            do while (p .gt. 0 .and. .not.xflag)                   
               if (ky(p) .ne. k2) then 
                  xflag = .true. 
               else if (brid(p) .ne. id) then 
                  xflag = .true. 
               else 
                  p = brnch_nxt(p) 
               endif 
            enddo 
         enddo 
  750 continue                                                         
                                                                         
c     check enclosure                                                  
                                                                         
      do 770 i=1,ntotc                                                 
         if (numac(i) .eq. 0) then                                 
            write (errbuf(1),760) arcnam(i),numac(i),numdc(i)      
  760       format(' Controlled interchange area ',a10,                  
     1         ' has ',i4,' a-c tie lines and ',i4,                
     2         ' d-c tie lines. Interchange control aborted.')     
            if (is_batch .eq. 0) then
               call prterx ('E',1)
            else
               call prterx ('F',1)
            endif
            kerr=1                                                 
            kabort=1                                               
         endif                                                     
  770 continue                                                         
                                                                         
c     Check "RZ" records for tie lines                                 
                                                                         
      do 800 jt = 1,nycomp                                             
         k1 = kycomp(1,jt)                                         
         k2 = kycomp(2,jt)                                         
         if (jarzn(k1) .eq. jarzn(k2)) then                        
            kycomp(7,jt) = 0                                       
         else                                                      
            id = char (kycomp(3,jt))                               
            ksect = kycomp(4,jt)                                   
            nbr = numbrn (k1, k2, id, 0)
            do 780 i = 1,jtie                                      
               if (tie(10,i) .eq. nbr) then                       
                  kycomp(7,jt) = i                                 
                  go to 800                                        
               endif                                               
  780       continue                                               
            nbr = numbrn (k2, k1, id, 0)
            do 790 i = 1,jtie                                      
               if (tie(10,i) .eq. nbr) then                       
                  kycomp(7,jt) = i                                 
                  go to 800                                        
               endif                                               
  790       continue                                               
            kycomp(7,jt) = 0                                       
            call erexit (1)                                        
         endif                                                
  800 continue                                                         
                                                                         
c     Check Intertie records for actual tie lines.                     
                                                                         
      do 850 i=1,ntotic                                             
         if (arcinp(i) .ne. 0.0) then                               
            ka1 = find_ara(arcint(1,i)) 
            ka2 = find_ara(arcint(2,i)) 
            if (ka1 .gt. 0 .and. ka2 .gt. 0) then           
               do 830 j = 1, jtie                                      
                  if (tie(2,j) .eq. ka1 .and. tie(8,j) .eq. ka2)     
     &               go to 850                                         
                  if (tie(8,j) .eq. ka1 .and. tie(2,j) .eq. ka2)     
     &               go to 850                                         
  830          continue                                                
               write (errbuf(1),840) arcint(1,i), arcint(2,i),         
     &            arcinp(i)                                            
  840          format(' Intertie ',a10,' to ',a10, 
     &                ' scheduled interchange ',f8.1, 
     &                ' mw has no actual intertie lines') 
              call prterx ('W',1)                                     
            endif 
         endif                                                      
  850 continue                                                      
 
  900 continue                                                         
 
      return                                                           
      end                                                              
