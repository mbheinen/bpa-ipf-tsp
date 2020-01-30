C    %W% %G%
      subroutine wgfcal(to)                                             
C * * *                                                                 
C * * * THIS SUBROUTINE CALCULATES GENERATOR FREQUENCY DEVIATION        
C * * * AT EACH TIME STEP AND SAVES THE LOWEST 20 DEVIATIONS,           
C * * * BUS NAMES WHERE THEY OCCUR, AND THE TIME WHEN THEY              
C * * * OCCURRED.  IT IS CALLED BY NOUT2.                               
C * * *                                                                 
      include 'tspinc/params.inc' 
      include 'tspinc/comn56.inc' 
      include 'tspinc/link56.inc' 
      include 'tspinc/room.inc' 
      include 'tspinc/wstequ.inc' 
      include 'tspinc/wgf.inc' 
      include 'tspinc/nwgntn.inc' 
      data convc / 9.54923 /                                            
      do 500 i = 1,isg                                                  
      feqdev = abs(angp2(i)*convc)                                      
      ibkv = nwgntn(i)                                                  
      do 200 j = 1,20                                                   
      if(nwgntc(1,i).eq.wgfnam(j).and.basekv(ibkv) .eq. wgfkv(j)        
     1  .and. nwgntc(2,i) .eq. wgfid(j))then                            
          if(feqdev .gt. wgfval(j))then                                 
            if(j .eq. 20)go to 250                                      
            do 175 m = j,19                                             
            wgfval(m) = wgfval(m+1)                                     
            wgfnam(m) = wgfnam(m+1)                                     
            wgfid(m) = wgfid(m+1)                                       
            wgfkv(m) = wgfkv(m+1)                                       
 175        wgftim(m) = wgftim(m+1)                                     
            go to 250                                                   
         endif                                                          
         go to 500                                                      
      endif                                                             
 200  continue                                                          
 250  do 400 j= 1,20                                                    
      if(feqdev .gt. wgfval(j))then                                     
         l = 21                                                         
         do 300 k = j+1,20                                              
         l = l - 1                                                      
         wgfval(l) = wgfval(l-1)                                        
         wgftim(l) = wgftim(l-1)                                        
         wgfnam(l) = wgfnam(l-1)                                        
         wgfid(l) = wgfid(l-1)                                          
 300     wgfkv(l) = wgfkv(l-1)                                          
         wgfval(j) = feqdev                                             
         wgftim(j) = to                                                 
         wgfnam(j) = nwgntc(1,i)                                        
         wgfid(j) = nwgntc(2,i)                                         
         wgfkv(j) = basekv(ibkv)                                        
         go to 500                                                      
      endif                                                             
 400  continue                                                          
 500  continue                                                          
      return                                                            
      end                                                               
