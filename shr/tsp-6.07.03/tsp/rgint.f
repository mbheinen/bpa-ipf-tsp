C    %W% %G%
      subroutine rgint                                                  
C * * *                                                                 
C * * * THIS SUBROUTINE CONVERTS THE BUS NUMBERS TO THE SWING           
C * * * INTERNAL ORDER FOR THE LINE CONTAINING THE SERIES CAPACITOR     
C * * * GAP.                                                            
C * * * IT ALSO SEARCHES THE REMOTE RELAY TABLES TO FIND THE INDICES    
C * * * OF THE REMOTE RELAYS FOR THIS RELAY. IT IS CALLED BY INITL2.    
C * * *                                                                 
      include 'tspinc/params.inc' 
      include 'tspinc/busnum.inc' 
      include 'tspinc/rrcom.inc' 
      include 'tspinc/rgcom.inc' 
      include 'tspinc/namec.inc' 
      include 'tspinc/pointr.inc' 
      include 'tspinc/kntrly.inc' 
      include 'tspinc/prt.inc' 
      character*8 name1c,name2c                                         
      character*1 id                                                    
C * * *                                                                 
C * * * REORDER BUS NUMBERS IN SERIES CAPACITOR TABLE                   
C * * *                                                                 
      do 1000 itrr = 1,kntrg                                            
      ibusrg(itrr) = indx2n(ibusrg(itrr))                               
      jbusrg(itrr) = indx2n(jbusrg(itrr))                               
      ibusn = ibusrg(itrr)                                              
      jbusn = jbusrg(itrr)                                              
C * * *                                                                 
C * * * SEARCH REMOTE RELAY TABLES TO FIND REMOTE RELAYS                
C * * * ATTACHED TO THIS RELAY                                          
C * * *                                                                 
      id = iparrg(itrr)                                                 
      if(kntrr .ne. 0)then                                              
         knt = 0                                                        
         do 900 ktrr = 1,kntrr                                          
         if(ibusn .ne. ibs1rr(ktrr))go to 900                           
         if(jbusn .ne. jbs1rr(ktrr))go to 900                           
         if(kompr(id,ipr1rr(ktrr),kdum))900,800,900                     
  800    if(rltprr(ktrr) .ne. 4) go to 900                              
         knt = knt + 1                                                  
         if(knt .gt. 5)then                                             
            name1c = exnamc(ibusn)                                      
            kv1 = ixnamn(ibusn)                                         
            bkv1 = basekv(kv1)                                          
            name2c = exnamc(jbusn)                                      
            kv2 = ixnamn(jbusn)                                         
            bkv2 = basekv(kv2)                                          
            write(errbuf(1), 850)name1c,bkv1,name2c,bkv2,id             
  850       format(5x,' SERIES CAPACITOR GAP ',2(1x,a8,1x,f5.1),1x,      
     1             a1,' HAS MORE THAN 5 REMOTE RELAYS.')                
            iabort = 1                                                  
            go to 1000                                                  
         endif                                                          
         idrrrg(knt,itrr) = ktrr                                        
         rltprr(ktrr) = -1.0                                            
  900    continue                                                       
         icd4rg(itrr) = knt                                             
      endif                                                             
 1000 continue                                                          
      return                                                            
      end                                                               
