C    %W% %G%
      subroutine wgvcal(to,vfdint)                                      
C * * *                                                                 
C * * * THIS SUBROUTINE CALCULATES GENERATOR FIELD VOLTAGE DEVIATION    
C * * * AT EACH TIME STEP AND SAVES THE LOWEST 20 DEVIATIONS,           
C * * * BUS NAMES WHERE THEY OCCUR, AND THE TIME WHEN THEY              
C * * * OCCURRED.  IT IS CALLED BY NOUT2.                               
C * * *                                                                 
      include 'tspinc/params.inc' 
      include 'tspinc/comn56.inc' 
      include 'tspinc/link56.inc' 
      include 'tspinc/room.inc' 
      include 'tspinc/wstequ.inc' 
      include 'tspinc/wgv.inc' 
      include 'tspinc/nwgntn.inc' 
      dimension vfdint(MAXGEN)                                          
      do 500 i = 1,isg                                                  
      if(vfldt(1,i) .eq. 0.0)go to 500                                  
      vltdev = abs(vfldt(1,i) - vfdint(i))                              
      ibkv = nwgntn(i)                                                  
      do 200 j = 1,20                                                   
      if(nwgntc(1,i).eq.wgvnam(j).and.basekv(ibkv) .eq. wgvkv(j)        
     1  .and. nwgntc(2,i) .eq. wgvid(j))then                            
          if(vltdev .gt. wgvval(j))then                                 
            if(j .eq. 20)go to 250                                      
            do 175 m = j,19                                             
            wgvval(m) = wgvval(m+1)                                     
            wgvnam(m) = wgvnam(m+1)                                     
            wgvid(m) = wgvid(m+1)                                       
            wgvkv(m) = wgvkv(m+1)                                       
 175        wgvtim(m) = wgvtim(m+1)                                     
            go to 250                                                   
         endif                                                          
         go to 500                                                      
      endif                                                             
 200  continue                                                          
 250  do 400 j= 1,20                                                    
      if(vltdev .gt. wgvval(j))then                                     
         l = 21                                                         
         do 300 k = j+1,20                                              
         l = l - 1                                                      
         wgvval(l) = wgvval(l-1)                                        
         wgvtim(l) = wgvtim(l-1)                                        
         wgvnam(l) = wgvnam(l-1)                                        
         wgvid(l) = wgvid(l-1)                                          
 300     wgvkv(l) = wgvkv(l-1)                                          
         wgvval(j) = vltdev                                             
         wgvtim(j) = to                                                 
         wgvnam(j) = nwgntc(1,i)                                        
         wgvid(j) = nwgntc(2,i)                                         
         wgvkv(j) = basekv(ibkv)                                        
         go to 500                                                      
      endif                                                             
 400  continue                                                          
 500  continue                                                          
      return                                                            
      end                                                               
