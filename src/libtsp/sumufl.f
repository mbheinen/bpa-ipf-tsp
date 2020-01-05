C    %W% %G%
      subroutine sumufl(ibno,name,bkv,pdrp,freq)                        
C * * *                                                                 
C * * * THIS SUBROUTINE KEEPS TRACK OF HOW MUCH UNDERFREQUENCY LOAD     
C * * * HAS BEEN SHED, AT WHAT FREQUENCY IT WAS SHED, AND AT WHAT       
C * * * BUS IT WAS SHED.  NAME IS THE BUS NAME, BKV IS THE BASE KV      
C * * * WHERE THE LOAD WAS SHED.  PDRP IS THE AMOUNT OF LOAD SHED       
C * * * IN PER UNIT, AND FREQ IS THE SHEDDING FREQUENCY DEVIATION       
C * * * IN RADIANS/CYCLE.  IBNO IS THE BUS NUMBER FOR BUS NAME.         
C * * * SUMUFL IS CALLED BY SHDSOL.                                     
C * * *                                                                 
      include 'tspinc/params.inc' 
      include 'tspinc/param.inc' 
      include 'tspinc/sumuf.inc' 
      include 'tspinc/namec.inc' 
      character*8 name                                                  
      character*2 izone                                                 
      data fconv /9.549297/                                             
      if(pdrp .eq. 0.0)return                                           
C * * *                                                                 
C * * * CONVERT LOAD DROPPED TO MEGAWATTS                               
C * * *                                                                 
      pdrop = pdrp*bmva                                                 
C * * *                                                                 
C * * * TUFLOD IS TOTAL UNDERFREQUENCY LOAD SHED                        
C * * *                                                                 
      tuflod = tuflod + pdrop                                           
C * * *                                                                 
C * * * FIND THE ZONE THIS BUS IS IN                                    
C * * *                                                                 
      izone = exzonc(ibno)                                              
      if(iznuf .eq. 0)go to 60                                          
      do 50 itrr = 1, iznuf                                             
      if(izone .eq. ufzone(itrr))then                                   
         ufznto(itrr) = ufznto(itrr) + pdrop                            
         iznid = itrr                                                   
         go to 70                                                       
      endif                                                             
  50  continue                                                          
  60  iznuf = iznuf + 1                                                 
      ufzone(iznuf) = izone                                             
      ufznto(iznuf) = ufznto(iznuf) + pdrop                             
      iznid = iznuf                                                     
  70  if(indfeq .eq. 0)go to 200                                        
      do 100 itrr = 1, indfeq                                           
      if(uffreq(itrr) .eq. freq)then                                    
         uflod(itrr) = uflod(itrr)+pdrop                                
         jtrr = ibusfq(itrr) + 1                                        
         ibusfq(itrr) = jtrr                                            
         ufbusn(itrr,jtrr) = name                                       
         ufbkv(itrr,jtrr) = bkv                                         
         ufcyc(itrr,jtrr) = to                                          
         ufblod(itrr,jtrr) = pdrop                                      
         iufzon(itrr,jtrr) = exzonc(ibno)                               
         ufznld(itrr,iznid) = ufznld(itrr,iznid)+pdrop                  
         go to 300                                                      
      endif                                                             
      if(freq .gt. uffreq(itrr))then                                    
         indfeq = indfeq + 1                                            
         do 90 ktrr = indfeq,itrr+1,-1                                  
         ktrrm = ktrr -1                                                
         uflod(ktrr) = uflod(ktrrm)                                     
         ibusfq(ktrr) = ibusfq(ktrrm)                                   
         uffreq(ktrr) = uffreq(ktrrm)                                   
         do 80 jtrr = 1,ibusfq(ktrr)                                    
         ufbusn(ktrr,jtrr) = ufbusn(ktrrm,jtrr)                         
         ufbkv(ktrr,jtrr) =  ufbkv(ktrrm,jtrr)                          
         ufcyc(ktrr,jtrr) = ufcyc(ktrrm,jtrr)                           
         ufblod(ktrr,jtrr) = ufblod(ktrrm,jtrr)                         
         iufzon(ktrr,jtrr) = iufzon(ktrrm,jtrr)                         
   80    continue                                                       
         do 85 jtrr = 1, iznuf                                          
         ufznld(ktrr,jtrr) = ufznld(ktrrm,jtrr)                         
   85    continue                                                       
   90    continue                                                       
         do 95 jtrr = 1,iznuf                                           
         ufznld(itrr,jtrr) = 0.0                                        
   95    continue                                                       
         uflod(itrr) = pdrop                                            
         jtrr = 1                                                       
         uffreq(itrr) = freq                                            
         ibusfq(itrr) = jtrr                                            
         ufbusn(itrr,jtrr) = name                                       
         ufbkv(itrr,jtrr) = bkv                                         
         ufcyc(itrr,jtrr) = to                                          
         ufblod(itrr,jtrr) = pdrop                                      
         iufzon(itrr,jtrr) = exzonc(ibno)                               
         ufznld(itrr,iznid) = pdrop                                     
         go to 300                                                      
      endif                                                             
 100  continue                                                          
 200  indfeq = indfeq + 1                                               
      uffreq(indfeq) = freq                                             
      uflod(indfeq) = pdrop                                             
      ibusfq(indfeq) = 1                                                
      jtrr = ibusfq(indfeq)                                             
      ufbusn(indfeq,jtrr) = name                                        
      ufbkv(indfeq,jtrr) = bkv                                          
      ufcyc(indfeq,jtrr) = to                                           
      ufblod(indfeq,jtrr) = pdrop                                       
      iufzon(indfeq,jtrr) = exzonc(ibno)                                
      ufznld(indfeq,iznid) = pdrop                                      
 300  return                                                            
      end                                                               
