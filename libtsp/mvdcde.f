C    %W% %G%
      subroutine mvdcde                                                 
C * * *                                                                 
C * * * THIS SUBROUTINE DECODES THE TYPE MV OUTPUT DATA CARDS AND       
C * * * FORMS INITIAL TABLES FOR THE TWO VARIABLE OUTPUT LOGIC.  IT     
C * * * IS CALLED BY NOUT1.                                             
C * * *                                                                 
      include 'tspinc/params.inc' 
      include 'tspinc/indx2n.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/mvn.inc' 
      include 'tspinc/reread.inc' 
      character*8 name1,name2                                           
      character*1 cde1,cde2,id1,id2                                     
      mvkt = mvkt + 1                                                   
      if (mvkt .gt. 10) then                                             
         write (outbuf,50)                                               
  50     format('0 MORE THAN 10 MV CARDS HAVE BEEN ENTERED.  ONLY THE ',
     1          'FIRST TEN WILL BE USED.')                              
         call prtout(1)                                                 
         return                                                         
       endif                                                            
       isw = 0                                                          
C * * *                                                                 
C * * * READ Y AXIS OPTION AND CODE                                     
C * * *                                                                 
      read (buffer,100) cde1,iopt1                                        
 100  format (bz,54x,a1,i2)                                                 
C * * *                                                                 
C * * * THE Y AXIS IS A GENERATOR VARIABLE                              
C * * *                                                                 
      if(cde1 .eq. 'G')then                                             
         isw = 1                                                        
         read (buffer,200) name1,bkv1,id1                                 
 200     format (bz,3x,a8,f4.0,1x,a1)                                       
         kb1 = nambse(bkv1)                                             
         k1 = igensr(name1,kb1,id1)                                     
         if (k1 .eq. 0)then                                              
            write(outbuf,300)name1,bkv1,id1                             
            call prtout(1)                                              
 300        format('0 GENERATOR ',a8,1x,f5.1,1x,a1,' ON THE MV CARD',   
     1             ' CANNOT BE LOCATED.')                               
            mvkt = mvkt -1                                              
            return                                                      
         endif                                                          
         mvgkt = mvgkt+ 1                                               
         n = 2*mvkt -1                                                  
         mvnm11(n) = name1                                              
         pkv11(n) = bkv1                                                
         mvid(n) = id1                                                  
         mvcde(n) = cde1                                                
         mvopt(n) = iopt1                                               
         mvgen(n) = k1                                                  
         go to 1000                                                     
      endif                                                             
C * * *                                                                 
C * * * THE Y AXIS IS A GENERATOR DIFFERENCE VARIABLE                   
C * * *                                                                 
      if(cde1 .eq. 'Y')then                                             
         isw = 1                                                        
         read (buffer,320) name1,bkv1,id1                                 
 320     format (bz,3x,a8,f4.0,1x,a1)                                       
         kb1 = nambse(bkv1)                                             
         k1 = igensr(name1,kb1,id1)                                     
         if(k1 .eq. 0)then                                              
            write(outbuf,340)name1,bkv1,id1                             
            call prtout(1)                                              
 340        format('0 GENERATOR ',a8,1x,f5.1,1x,a1,' ON THE MV CARD',   
     1             ' CANNOT BE LOCATED.')                               
            mvkt = mvkt -1                                              
            return                                                      
         endif                                                          
         mvgkt = mvgkt+ 1                                               
         n = 2*mvkt -1                                                  
         mvnm11(n) = name1                                              
         pkv11(n) = bkv1                                                
         mvid(n) = id1                                                  
         mvcde(n) = cde1                                                
         mvopt(n) = iopt1                                               
         mvgen(n) = k1                                                  
         go to 1000                                                     
      endif                                                             
C * * *                                                                 
C * * * THE Y AXIS IS A BUS VARIABLE                                    
C * * *                                                                 
      if(cde1 .eq. 'B')then                                             
         isw = 1                                                        
         read (buffer,400) name1,bkv1                                     
 400     format (bz,3x,a8,f4.0)                                             
         kb1 = nambse(bkv1)                                             
         k1 = innam5(name1,kb1)                                         
         if(k1 .eq. 0)then                                              
            mvkt = mvkt-1                                               
            return                                                      
         endif                                                          
         k1 = indx2n(k1)                                                
         mvbkt = mvbkt+ 1                                               
         n = 2*mvkt -1                                                  
         mvnm11(n) = name1                                              
         pkv11(n) = bkv1                                                
         mvcde(n) = cde1                                                
         mvopt(n) = iopt1                                               
         mvgen(n) = k1                                                  
         go to 1000                                                     
      endif                                                             
C * * *                                                                 
C * * * THE Y AXIS IS A LINE VARIABLE                                   
C * * *                                                                 
      if(cde1 .eq. 'L')then                                             
         isw = 1                                                        
         read (buffer,600) name1,bkv1,name2,bkv2,id1                      
 600     format (bz,3x,a8,f4.0,a8,f4.0,a1)                                  
         kb1 = nambse(bkv1)                                             
         k1 = innam5(name1,kb1)                                         
         if(k1 .eq. 0)then                                              
            mvkt = mvkt-1                                               
            return                                                      
         endif                                                          
         kb2 = nambse(bkv2)                                             
         k2 = innam5(name2,kb2)                                         
         if(k2 .eq. 0)then                                              
            mvkt = mvkt-1                                               
            return                                                      
         endif                                                          
         mvlkt = mvlkt+ 1                                               
         n = 2*mvkt -1                                                  
         mvgen(n) = k1                                                  
         mvgen2(n) = k2                                                 
         mvnm11(n) = name1                                              
         mvnm12(n) = name2                                              
         pkv11(n) = bkv1                                                
         pkv12(n) = bkv2                                                
         mvid(n) = id1                                                  
         mvcde(n) = cde1                                                
         mvopt(n) = iopt1                                               
         go to 1000                                                     
      endif                                                             
C * * *                                                                 
C * * * THE Y AXIS IS A DC VARIABLE                                     
C * * *                                                                 
      if(cde1 .eq. 'D')then                                             
         isw = 1                                                        
         read (buffer,700) name1,bkv1                                     
 700     format (bz,3x,a8,f4.0)                                             
         kb1 = nambse(bkv1)                                             
         k1 = innam5(name1,kb1)                                         
         if(k1 .eq. 0)then                                              
            mvkt = mvkt-1                                               
            return                                                      
         endif                                                          
         mvdkt = mvdkt+ 1                                               
         n = 2*mvkt -1                                                  
         mvnm11(n) = name1                                              
         pkv11(n) = bkv1                                                
         mvcde(n) = cde1                                                
         mvopt(n) = iopt1                                               
         mvgen(n) = k1                                                  
         go to 1000                                                     
      endif                                                             
C * * *                                                                 
C * * * READ THE X AXIS OPTION AND CODE                                 
C * * *                                                                 
1000  if(isw .eq. 0)then                                                
         write(outbuf,1020)buffer                                       
1020     format(1h0,5x,a80)                                             
         call prtout(1)                                                 
         write(outbuf,1030)                                             
1030     format(1h0,5x,'THE CODE IN COLUMN 55 OF THE ABOVE CARD',       
     1          ' IS INCORRECT.')                                       
         call prtout(1)                                                 
         return                                                         
      endif                                                             
      read (buffer,1040) cde2,iopt2                                       
1040  format (bz,58x,a1,i2)                                                 
C * * *                                                                 
C * * * THE X AXIS IS TIME                                              
C * * *                                                                 
      if (cde2 .eq. 'T')then                                             
         n = n+1                                                        
         mvcde(n) = cde2                                                
         return                                                         
      endif                                                             
C * * *                                                                 
C * * * THE X AXIS IS A GENERATOR VARIABLE                              
C * * *                                                                 
      if(cde2 .eq. 'G')then                                             
         read (buffer,1100) name2,bkv2,id2                                
1100     format (bz,28x,a8,f4.0,1x,a1)                                      
         kv2 = nambse(bkv2)                                             
         k2 = igensr(name2,kv2,id2)                                     
         if(k2 .eq. 0)then                                              
            write(outbuf,1200)name2,bkv2,id2                            
            call prtout(1)                                              
1200        format('0 GENERATOR ',a8,1x,f5.1,1x,a1,' ON THE MV CARD',   
     1             ' CANNOT BE LOCATED.')                               
            mvkt = mvkt -1                                              
            return                                                      
         endif                                                          
         n = n + 1                                                      
         mvgkt = mvgkt+ 1                                               
         mvnm11(n) = name2                                              
         pkv11(n) = bkv2                                                
         mvid(n) = id2                                                  
         mvcde(n) = cde2                                                
         mvopt(n) = iopt2                                               
         mvgen(n) = k2                                                  
         return                                                         
      endif                                                             
C * * *                                                                 
C * * * THE X AXIS IS A GENERATOR DIFFERENCE VARIABLE                   
C * * *                                                                 
      if(cde2 .eq. 'Y')then                                             
         read (buffer,1220) name2,bkv2,id2                                
1220     format (bz,28x,a8,f4.0,1x,a1)                                      
         kv2 = nambse(bkv2)                                             
         k2 = igensr(name2,kv2,id2)                                     
         if(k2 .eq. 0)then                                              
            write(outbuf,1240)name2,bkv2,id2                            
            call prtout(1)                                              
1240        format('0 GENERATOR ',a8,1x,f5.1,1x,a1,' ON THE MV CARD',   
     1             ' CANNOT BE LOCATED.')                               
            mvkt = mvkt -1                                              
            return                                                      
         endif                                                          
         n = n + 1                                                      
         mvgkt = mvgkt+ 1                                               
         mvnm11(n) = name2                                              
         pkv11(n) = bkv2                                                
         mvid(n) = id2                                                  
         mvcde(n) = cde2                                                
         mvopt(n) = iopt2                                               
         mvgen(n) = k2                                                  
         return                                                         
      endif                                                             
C * * *                                                                 
C * * * THE X AXIS IS A BUS VARIABLE                                    
C * * *                                                                 
      if(cde2 .eq. 'B')then                                             
         read (buffer,1300) name2,bkv2                                    
1300     format (bz,28x,a8,f4.0)                                            
         kb2 = nambse(bkv2)                                             
         k2 = innam5(name2,kb2)                                         
         if(k2 .eq. 0)then                                              
            mvkt = mvkt-1                                               
            return                                                      
         endif                                                          
         k2 = indx2n(k2)                                                
         mvbkt = mvbkt+ 1                                               
         n = n + 1                                                      
         mvnm11(n) = name2                                              
         pkv11(n) = bkv2                                                
         mvcde(n) = cde2                                                
         mvopt(n) = iopt2                                               
         mvgen(n) = k2                                                  
         return                                                         
      endif                                                             
C * * *                                                                 
C * * * THE X AXIS IS A LINE VARIABLE                                   
C * * *                                                                 
      if(cde2 .eq. 'L')then                                             
         read (buffer,1500) name1,bkv1,name2,bkv2,id2                     
1500     format (bz,28x,a8,f4.0,a8,f4.0,a1)                                 
         kb1 = nambse(bkv1)                                             
         k1 = innam5(name1,kb1)                                         
         if(k1 .eq. 0)then                                              
            mvkt = mvkt-1                                               
            return                                                      
         endif                                                          
         kb2 = nambse(bkv2)                                             
         k2 = innam5(name2,kb2)                                         
         if(k2 .eq. 0)then                                              
            mvkt = mvkt-1                                               
            return                                                      
         endif                                                          
         mvlkt = mvlkt+ 1                                               
         n = n + 1                                                      
         mvnm11(n) = name1                                              
         pkv11(n) = bkv1                                                
         mvnm12(n) = name2                                              
         pkv12(n) = bkv2                                                
         mvcde(n) = cde2                                                
         mvopt(n) = iopt2                                               
         mvid(n) = id2                                                  
         mvgen(n) = k1                                                  
         mvgen2(n) = k2                                                 
         return                                                         
      endif                                                             
C * * *                                                                 
C * * * THE X AXIS IS A DC VARIABLE                                     
C * * *                                                                 
      if(cde2 .eq. 'D')then                                             
         read (buffer,1600) name2,bkv2                                    
1600     format (bz,28x,a8,f4.0)                                            
         kb2 = nambse(bkv2)                                             
         k2 = innam5(name2,kb2)                                         
         if(k2 .eq. 0)then                                              
            mvkt = mvkt-1                                               
            return                                                      
         endif                                                          
         mvdkt = mvdkt+ 1                                               
         n = n + 1                                                      
         mvnm11(n) = name2                                              
         pkv11(n) = bkv2                                                
         mvcde(n) = cde2                                                
         mvopt(n) = iopt2                                               
         mvgen(n) = k2                                                  
         return                                                         
      endif                                                             
      write(outbuf,1020)buffer                                          
      call prtout(1)                                                    
      write(outbuf,1650)                                                
1650  format(1h0,5x,'THE CODE IN COLUMN 59 OF THE ABOVE CARD',          
     1          ' IS INCORRECT.')                                       
      call prtout(1)                                                    
      return                                                            
      end                                                               
