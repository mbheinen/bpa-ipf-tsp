C    %W% %G%
       subroutine modinp(imodkt,jnt,itrr)                               
C * * *                                                                 
C * * * THIS SUBROUTINE DECODES THE DC MODULATION CARD AND              
C * * * FORMS INITIAL DATA TABLES                                       
C * * *                                                                 
      include 'tspinc/params.inc' 
      include 'tspinc/ectba.inc' 
      include 'tspinc/dcard.inc' 
      include 'tspinc/dcmodd.inc' 
      include 'tspinc/pointr.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/dcinfo.inc' 
      if(itrr .eq. 2) go to 300                                         
C * * *                                                                 
C * * * DECODE THE FIRST DC MODULATION CARD                             
C * * *                                                                 
       read (dccd80(jnt),50) tdmod1(imodkt),tfmod1(imodkt),
     1   emod1(imodkt),camod1(imodkt),cbmod1(imodkt),ccmod1(imodkt),     
     2   cdmod1(imodkt),ckmod1(imodkt),pnmod1(imodkt),pxmod1(imodkt),
     3   lohi   
 50      format (bz,15x,f3.3,f3.3,3x,f3.3,4f3.1,f4.3,f5.4,f5.5,i1)         
C * * *                                                                 
C * * * TDMOD1 IS TIME CONSTANT TD,TFMOD1 IS TIME CONSTANT TF,          
C * * *  EMOD1 IS TIME CONSTANT E, CAMOD1 IS CONSTANT A                 
C * * * CBMOD1 IS CONSTANT B, CCMOD1 IS CONSTANT C                      
C * * * CDMOD1 IS CONSTANT C, CKMOD1 IS CONSTANT K                      
C * * * PNMOD1 IS PMIN, PXMOD1 IS PMAX                                  
C * * *                                                                 
      if(moderi .ne. 1)ckmod1(imodkt) = -ckmod1(imodkt)                 
      if(pnmod1(imodkt) .gt. pxmod1(imodkt))then                        
         write(errbuf(1),100) (dccard(ix,jnt),ix = 1,8)                 
         write(errbuf(2),60)                                            
  60     format(1h0,5x,'PMIN IS GREATER THAN PMAX ON THE ABOVE CARD')   
         call prterr('E',2)                                             
         iabort = 1                                                     
         imodkt = imodkt -  1                                           
      modcod = 0                                                        
         return                                                         
      endif                                                             
      if(pxmod1(imodkt) .lt. 0.0)then                                   
         write(errbuf(1),100) (dccard(ix,jnt),ix = 1,8)                 
         write(errbuf(2),75)                                            
  75     format(1h0,5x,'PMAX MUST BE GREATER THAN 0.0 ON THE',          
     1   ' ABOVE CARD')                                                 
         call prterr('E',2)                                             
         iabort = 1                                                     
         imodkt = imodkt -  1                                           
      modcod = 0                                                        
         return                                                         
      endif                                                             
C * * *                                                                 
C * * *LOHI = 0 DUAL FREQUENCY MODULATION                               
C * * *LOHI = 1 LOW LEVEL MODULATION WITH POWER INPUT  INSERTED         
C * * * LOHI = 2 LOW LEVEL MODULATION WITH CURRENT INPUT                
C * * * LOHI = 3 HIGH LEVEL MODULATION WITH POWER INPUT                 
C * * * LOHI = 4 HIGH LEVEL MODULATION WITH CURRENT INPUT               
C * * * LOHI = 5 GAMMA MODULATION                                       
      modcod = 0                                                        
C * * *                                                                 
      if(lohi .eq. 5)then                                               
        write(errbuf(1),100)  (dccard(ix,jnt),ix =1,8)                  
        write(errbuf(2),200)                                            
 100    format(1x,8a10)                                                 
 200    format(1h0,' GAMMA MODULATION IS NOT ALLOWED ON A RECTIFIER ')  
        call prterr('E',2)                                              
        iabort = 1                                                      
        imodkt = imodkt-1                                               
        return                                                          
      endif                                                             
      modcod = lohi                                                     
      if(lohi .eq. 0)modcod = 6                                         
C * * *                                                                 
C * * * IDCBS1 AND JDCBS1 ARE THE BUS NUMBERS OF THE MONITORED LINE     
C * * *                                                                 
      idcbs1(imodkt) = ii2                                              
      jdcbs1(imodkt) = ii3                                              
      return                                                            
C * * *                                                                 
C * * * DECODE THE SECOND DC MODULATION CARD FOR DUAL FREQUENCY MODULATI
C * * *                                                                 
 300  read (dccd80(jnt),50) tdmod2(imodkt),tfmod2(imodkt),emod2(imodkt), 
     1 camod2(imodkt),cbmod2(imodkt),ccmod2(imodkt),cdmod2(imodkt),     
     2 ckmod2(imodkt),pmin,pmax                                         
C * * *                                                                 
C * * * TDMOD2 IS TIME CONSTANT TD,TFMOD2 IS TIME CONSTANT TF,          
C * * *  EMOD2 IS TIME CONSTANT E, CAMOD2 IS CONSTANT A                 
C * * * CBMOD2 IS CONSTANT B, CCMOD2 IS CONSTANT C                      
C * * * CDMOD2 IS CONSTANT C, CKMOD2 IS CONSTANT K                      
C * * *                                                                 
      if(moderi .ne. 1)ckmod2(imodkt) = -ckmod2(imodkt)                 
      jdcbs1(imodkt) = ii3                                              
      pxmod1(imodkt) = amin1(pxmod1(imodkt),pmax)                       
      pnmod1(imodkt) = amax1(pnmod1(imodkt),pmin)                       
      if(ii2 .eq. ii3) then                                             
         return                                                         
      endif                                                             
      write (outbuf,100) (dccard(kzz,kntsav),kzz=1,8)                   
      call prtout (1)                                                   
      write (outbuf,100) (dccard(kzz,jntsav),kzz=1,8)                   
      call prtout (1)                                                   
      write (errbuf(1),400)                                             
      call prterr ('E',1)                                               
 400  format(1x,'COLUMNS 56-67 AND 69-80 MUST BE THE SAME FOR EACH DS CA
     1RD AND TWO OF THEM MUST BE PROVIDED.')                            
      iabort=1                                                          
      imodkt = imodkt - 1                                               
      modcod = 0                                                        
      return                                                            
      end                                                               
