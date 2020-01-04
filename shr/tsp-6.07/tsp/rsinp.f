C    %W% %G%
      subroutine rsinp                                                  
C * * *                                                                 
C * * * THIS SUBROUTINE DECODES THE RS CARDS AND BUILDS DATA TABLES.    
C * * * IT IS CALLED BY INPUT2.                                         
C * * *                                                                 
      include 'tspinc/params.inc' 
      include 'tspinc/rscom.inc' 
      include 'tspinc/pointr.inc' 
      include 'tspinc/cf1.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/param.inc' 
      include 'tspinc/relays.inc' 
      character*8 namec                                                 
      data pi /3.14159265/                                              
C     -     begin     begin     begin     begin     begin     begin 
      do 1500 jnt = 1,nufreq                                            
      if (jnt .gt. MAXRS)then                                            
         write(errbuf(1),200)MAXRS                                      
 200     format(5x,' NUMBER OF RS CARDS EXCEEDS ',i4,' JOB WILL ABORT.')
         call prterr('E',1)                                             
         iabort = 1                                                     
         return                                                         
      endif                                                             
      read (cf1t80(jnt),1000) namec,base,frelay,rstlev(jnt),             
     1                       rsbdly(jnt),(rsldrp(k,jnt),k = 1,8)        
 1000   format (bz,3x,a8,f4.0,5x,3f4.2,8f6.0)                                 
      kb=nambas(base)                                                   
      nbus=inam(namec,kb)                                               
      if (nbus.eq.0) then                                               
         write (outbuf,1200) (cf1tab(ir,jnt),ir=1,8)                    
         call prtout (1)                                                
 1200    format(1x,8a10)                                                
         write(errbuf(1),1300)                                          
 1300    format(1x,' THE BUS NAME ON THE ABOVE CARD IS INCORRECT.',     
     1         ' THIS RS CARD WILL BE IGNORED.')                        
         call prterr('E',1)                                             
         iabort = 1                                                     
         go to 1500                                                     
      endif                                                             
      irstcd(jnt) = 1                                                   
      irsbno(jnt) = nbus                                                
      rsfeq1(jnt) = (frelay - frqbse)*2.*pi/frqbse                      
      rsckr(jnt)=frqbse/(2.0*pi*(2.3+0.125*(frelay-(frqbse -1.))))      
      rstlev(jnt) = rstlev(jnt)*frqbse                                  
      do 1400 i = 1,8                                                   
 1400 rsldrp(i,jnt) = rsldrp(i,jnt) / bmva                              
 1500 continue                                                          
      return                                                            
      end                                                               
