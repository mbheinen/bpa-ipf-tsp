C    %W% %G%
      subroutine r1int                                                  
C * * *                                                                 
C * * * THIS SUBROUTINE CONVERTS THE BUS NUMBERS TO THE SWING           
C * * * INTERNAL ORDER AND COMPUTES THE INITIAL LINE FLOW               
C * * * FOR THE LINE CONTAINING THE POWER RATE RELAY.                   
C * * * IT ALSO SEARCHES THE REMOTE RELAY TABLES TO FIND THE INDICES    
C * * * OF THE REMOTE RELAYS FOR THIS RELAY. IT IS CALLED BY INITL2.    
C * * *                                                                 
      include 'tspinc/params.inc' 
      include 'tspinc/cntrl2.inc' 
      include 'tspinc/busnum.inc' 
      include 'tspinc/rrcom.inc' 
      include 'tspinc/r1com.inc' 
      include 'tspinc/namec.inc' 
      include 'tspinc/pointr.inc' 
      include 'tspinc/kntrly.inc' 
      include 'tspinc/busvolt.inc' 
      include 'tspinc/prt.inc' 
      character*8 name1c,name2c                                         
      character*1 id                                                    
C * * *                                                                 
C * * * REORDER BUS NUMBERS IN POWER RATE RELAY TABLE                   
C * * *                                                                 
      do 1000 itrr = 1,kntr1                                            
      ibusr1(itrr) = indx2n(ibusr1(itrr))                               
      jbusr1(itrr) = indx2n(jbusr1(itrr))                               
C * * *                                                                 
C * * * CALCULATE INITIAL POWER FLOW ON RELAYED LINE                    
C * * *                                                                 
      ibusn = ibusr1(itrr)                                              
      jbusn = jbusr1(itrr)                                              
      gij = gijr1(itrr)                                                 
      bij = bijr1(itrr)                                                 
      gio = gior1(itrr)                                                 
      bio = bior1(itrr)                                                 
      gt = gio + gij                                                    
      bt = bio + bij                                                    
      ei = eyr(ibusn)                                                   
      fi = eyi(ibusn)                                                   
      ej = eyr(jbusn)                                                   
      fj = eyi(jbusn)                                                   
      c1r = ei*gt - fi*bt                                               
      c1i = ei*bt + fi*gt                                               
      c2r = ej*gij - fj*bij                                             
      c2i = ej*bij + fj*gij                                             
      ctr = c1r - c2r                                                   
      cti = c1i -c2i                                                    
      p3iold = ei*ctr + fi*cti                                          
      piiold = p3iold                                                   
      piold = p3iold                                                    
      piir1(itrr) = piiold                                              
      piiir1(itrr) = p3iold                                             
      piodr1(itrr) = piold                                              
      riir1(itrr) = 0.0                                                 
      rir1(itrr) = 0.0                                                  
C * * *                                                                 
C * * * SEARCH REMOTE RELAY TABLES TO FIND REMOTE RELAYS                
C * * * ATTACHED TO THIS RELAY                                          
C * * *                                                                 
      id = iparr1(itrr)                                                 
      if(kntrr .ne. 0)then                                              
         knt = 0                                                        
         do 900 ktrr = 1,kntrr                                          
         if(ibusn .ne. ibs1rr(ktrr))go to 900                           
         if(jbusn .ne. jbs1rr(ktrr))go to 900                           
         if(kompr(id,ipr1rr(ktrr),kdum))900,800,900                     
  800    if(rltprr(ktrr) .gt. 7 .or. rltprr(ktrr) .lt. 5) go to 900     
         knt = knt + 1                                                  
         if(knt .gt. 5)then                                             
            name1c = exnamc(ibusn)                                      
            kv1 = ixnamn(ibusn)                                         
            bkv1 = basekv(kv1)                                          
            name2c = exnamc(jbusn)                                      
            kv2 = ixnamn(jbusn)                                         
            bkv2 = basekv(kv2)                                          
            write(errbuf(1), 850)name1c,bkv1,name2c,bkv2,id             
  850       format(5x,' POWER RATE RELAY ',2(1x,a8,1x,f5.1),1x,a1,       
     1             ' HAS MORE THAN 5 REMOTE RELAYS.')                   
            iabort = 1                                                  
            go to 1000                                                  
         endif                                                          
         rltprr(ktrr) = -1.0                                            
         idrrr1(knt,itrr) = ktrr                                        
  900    continue                                                       
         icd4r1(itrr) = knt                                             
      endif                                                             
 1000 continue                                                          
      return                                                            
      end                                                               
