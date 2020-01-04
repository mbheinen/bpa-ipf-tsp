C    %W% %G%
      subroutine lldrop                                                 
C                                                                       
C     THIS SUBROUTINE CONTAINS LOGIC TO REMOVE LINES FROM THE ADMITTAN
C     MATRIX.  IT IS CALLED BY RELAY.                                 
C                                                                       
      include 'tspinc/params.inc' 
      include 'tspinc/blkcom1.inc' 
      include 'tspinc/lnk12.inc' 
      include 'tspinc/matrow.inc' 
      include 'tspinc/newton.inc' 
      include 'tspinc/fltopt.inc' 
      include 'tspinc/param.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/ldropn.inc' 

      common /relay3/ ldbus, pshed1, qshed1, pshed2, qshed2, pshed3,
     &                qshed3, pshed4, qshed4, ldmde                                              
      equivalence (pshed1,plz), (qshed1,qlz), (pshed2,plcp), 
     &            (qshed2,qlcq), (pshed3,plpp), (qshed3,qlpq),
     &            (pshed4,pldf), (qshed4,qldf)          

      iflip=1                                                           

C                                                                       
C     IRCALL .GT. 6 MEANS THIS IS A DEFAULT DISTANCE RELAY ACTION     
C                                                                       
      if(ircall.gt.6) go to 219                                         
      idum = 0                                                          
      gij = tempn(1, 9)                                                 
      bij = tempn(2, 9)                                                 
      gio = tempn(1, 10)                                                
      bio = tempn(2, 10)                                                
      gjo = tempn(1, 11)                                                
      bjo = tempn(2, 11)                                                
  100 call getmat(ibus, ii)                                                 
      iii = ii                                                          
      do 120 i = 4, iii, 3                                              
      if(jbus.eq.matrow(i)) go to 160                                   
  120 continue                                                          
      write (errbuf(1),140) jbus                                        
      call prterr ('E',1)                                               
  140 format('0', 5x, 'ERROR IN LINE DROP LOGIC -- UNABLE TO FIND BUS NO
     &. 1', i5, '  Program stopped')                                         
      call erexit                                                       
C                                                                       
C     CHECK IF THE BRANCH TO BE OPENED IS STILL CONNECTED            
C                                                                       
  160 if(abs(atrow(i+1)).ge.0.0001) go to 170                           
      if(abs(atrow(i+2)).ge.0.0001) go to 170                           
      write (errbuf(1),165) name1,bkv1,name2,bkv2                       
      call prterr ('E',1)                                               
  165 format('0', 5x,' No branch exists between ', a8, 1x, f5.1, ' and ',
     &  ,a8, 1x, f5.1, ' -- hence, no tripping.')                                
      return                                                            
  170 atrow(ii-1) = atrow(ii-1) -gij - gio                              
      atrow(ii) = atrow(ii) - bij - bio                                 
      atrow(i+1) = atrow(i+1) + gij                                     
      atrow(i+2) = atrow(i+2) + bij                                     
      call putmat(ibus, ii)                                                 
      if(keybrd(30) .ne. 0) then                                        
         write (outbuf, 172)                                            
  172    format('0 LLDROP, S170+6, AFT ')                               
         call prtout (1)                                                
         write(outbuf,174)jbus,atrow(i+1),atrow(i+2),ibus,atrow(ii-1),  
     1                   atrow(ii)                                      
  174    format(2x,'JBUS,GIJ,BIJ,IBUS,GII,BII ',2(i5,1x,e16.6,1x,e16.6))
         call prtout(1)                                                 
      endif                                                             
      if(ibus.lt.lfrst) lfrst = ibus                                    
      if(idum.eq.jbus) go to 200                                        
      write (outbuf,180) name1,bkv1,name2,bkv2,to                       
      call prtout (1)                                                   
  180 format('0', 5x, 'Line between ', a8, 1x, f5.1, ' and ', a8, 1x,
     &   f5.1, ' opened due to relay action at ', f7.2, ' cycles.')
      iflip=2                                                           
      idum = ibus                                                       
      ibus = jbus                                                       
      jbus = idum                                                       
      gio = gjo                                                         
      bio = bjo                                                         
      go to 100                                                         
  200 ivpc = 2                                                          
      if(iflip.eq.1)go to 202                                           
      idum=ibus                                                         
      ibus=jbus                                                         
      jbus=idum                                                         
  202 return                                                            
  219 idum = 0                                                          
  220 call getmat(ibus, ii)                                                 
      do 240 i=4,ii,3                                                   
      if(jbus.eq.matrow(i)) go to 260                                   
  240 continue                                                          
      write (errbuf(1),140) jbus                                        
      call prterr ('E',1)                                               
      call erexit                                                       
  260 gij = -atrow(i+1)                                                 
      bij = -atrow(i+2)                                                 
C                                                                       
C     IRCALL = 7  OPEN LINE DUE TO DEFAULT DISTANCE RELAY ACTION      
C                                                                       
      atrow(ii-1) = atrow(ii-1) - gij                                   
      atrow(ii  ) = atrow(ii  ) - bij                                   
      atrow(i+1) = 0.0                                                  
      atrow(i+2) = 0.0                                                  
      if(ircall.eq.7) go to 280                                         
C                                                                       
C     RECLOSING OF LINE DUE TO DEF. DIST. RELAY ACTION               
C                                                                       
      atrow(ii-1) = atrow(ii-1) + gije                                  
      atrow(ii  ) = atrow(ii  ) + bije                                  
      atrow(i+1) = - gije                                               
      atrow(i+2) = - bije                                               
  280 call putmat(ibus, ii)                                                 
      if(keybrd(30) .ne. 0) then                                        
         write (outbuf, 282)                                            
  282    format('0 LLDROP, S280+2, AFT ')                               
         call prtout (1)                                                
         write(outbuf,174)jbus,atrow(i+1),atrow(i+2),ibus,atrow(ii-1),  
     1                   atrow(ii)                                      
         call prtout(1)                                                 
      endif                                                             
      if(ibus.lt.lfrst) lfrst = ibus                                    
      if(jbus.eq.idum)go to 200                                         
      if (ircall.eq.8) then                                             
         write (outbuf,320) name1,bkv1,name2,bkv2,to                    
  320    format('0', 5x, 'Line between ', a8, 1x, f5.1, ' and ', a8, 
     &     1x, f5.1, 
     &     ' reclosed due to default distance relay action at ', f7.2,      
     &     ' cycles.')                                                     
         call prtout (1)                                                
         write (outbuf,340) gije,bije,atrow(ii-1),atrow(ii)             
 340     format('0', 5x, ' GIJE BIJE ATROW(II-1),ATROW(II)',              
     1         4(1x,e16.6))                                             
         call prtout(1)                                                 
      endif                                                             
      if (ircall.eq.7) then                                             
         write (outbuf,300) name1,bkv1,name2,bkv2,to                    
  300    format('0', 5x, 'Line between ', a8, 1x, f5.1, ' and ', a8,
     &     1x, f5.1, ' opened due to default distance relay action at ',
     &     f7.2, ' cycles.') 
         call prtout (1)                                                
         write (outbuf,360) gij,bij,atrow(ii-1),atrow(ii)               
  360    format('0', 5x, ' GIJ BIJ ATROW(II-1),ATROW(II)',                
     1         4(1x,e16.6))                                             
         call prtout(1)                                                 
      endif                                                             
      iflip=2                                                           
      idum = ibus                                                       
      ibus = jbus                                                       
      jbus = idum                                                       
      go to 220                                                         
      end                                                               
