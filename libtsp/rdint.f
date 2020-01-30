C    %W% %G%
      subroutine rdint                                                  
C * * *                                                                 
C * * * THIS SUBROUTINE CONVERTS THE BUS NUMBERS TO THE SWING           
C * * * INTERNAL ORDER FOR THE LINE CONTAINING THE DISTANCE RELAY.      
C * * * IT ALSO SEARCHES THE REMOTE RELAY TABLES TO FIND THE INDICES    
C * * * OF THE REMOTE RELAYS FOR THIS RELAY. IT IS CALLED BY INITL2.    
C * * *                                                                 
      include 'tspinc/params.inc' 
      include 'tspinc/busnum.inc' 
      include 'tspinc/rrcom.inc' 
      include 'tspinc/rdcom.inc' 
      include 'tspinc/namec.inc' 
      include 'tspinc/pointr.inc' 
      include 'tspinc/kntrly.inc' 
      include 'tspinc/prt.inc' 
      character*8 name1c,name2c                                         
      character*1 id                                                    
C * * *                                                                 
C * * * REORDER BUS NUMBERS IN POWER RATE RELAY TABLE                   
C * * *                                                                 
      do 1000 itrr = 1,kntrd                                            
      ibusrd(itrr) = indx2n(ibusrd(itrr))                               
      jbusrd(itrr) = indx2n(jbusrd(itrr))                               
      ibusn = ibusrd(itrr)                                              
      jbusn = jbusrd(itrr)                                              
C * * *                                                                 
C * * * SEARCH REMOTE RELAY TABLES TO FIND REMOTE RELAYS                
C * * * ATTACHED TO THIS RELAY                                          
C * * *                                                                 
      id = iparrd(itrr)                                                 
      if(kntrr .ne. 0)then                                              
         knt = 0                                                        
         do 900 ktrr = 1,kntrr                                          
         if(ibusn .ne. ibs1rr(ktrr))go to 900                           
         if(jbusn .ne. jbs1rr(ktrr))go to 900                           
         if(kompr(id,ipr1rr(ktrr),kdum))900,800,900                     
  800    if(rltprr(ktrr) .ne. 1 .and. rltprr(ktrr) .ne. 2) go to 900    
         knt = knt + 1                                                  
         if(knt .gt. 5)then                                             
            name1c = exnamc(ibusn)                                      
            kv1 = ixnamn(ibusn)                                         
            bkv1 = basekv(kv1)                                          
            name2c = exnamc(jbusn)                                      
            kv2 = ixnamn(jbusn)                                         
            bkv2 = basekv(kv2)                                          
            write(errbuf(1), 850)name1c,bkv1,name2c,bkv2,id             
  850       format(5x,' DISTANCE RELAY ',2(1x,a8,1x,f5.1),1x,a1,         
     1             ' HAS MORE THAN 5 REMOTE RELAYS.')                   
            iabort = 1                                                  
            go to 1000                                                  
         endif                                                          
         rltprr(ktrr) = -1.0                                            
         idrrrd(knt,itrr) = ktrr                                        
  900    continue                                                       
         icd4rd(itrr) = knt                                             
      endif                                                             
 1000 continue                                                          
      return                                                            
      end                                                               
