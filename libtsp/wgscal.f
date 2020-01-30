C    %W% %G%
      subroutine wgscal(to)                                             
C * * *                                                                 
C * * * THIS SUBROUTINE CALCULATES GENERATOR PSS DEVIATION              
C * * * AT EACH TIME STEP AND SAVES THE LOWEST 20 DEVIATIONS,           
C * * * BUS NAMES WHERE THEY OCCUR, AND THE TIME WHEN THEY              
C * * * OCCURRED.  IT IS CALLED BY NOUT2.                               
C * * *                                                                 
      include 'tspinc/params.inc' 
      include 'tspinc/comn56.inc' 
      include 'tspinc/link56.inc' 
      include 'tspinc/room.inc' 
      include 'tspinc/wstequ.inc' 
      include 'tspinc/wgs.inc' 
      include 'tspinc/nwgntn.inc' 
      do 500 i = 1,isg                                                  
      ibkv = nwgntn(i)                                                  
      pssdev = sup(2,i)                                                 
      do 200 j = 1,20                                                   
      if(nwgntc(1,i).eq.wgsnam(j).and.basekv(ibkv) .eq. wgskv(j)        
     1  .and. nwgntc(2,i) .eq. wgsid(j))then                            
          if(pssdev .gt. wgsval(j))then                                 
            if(j .eq. 20)go to 250                                      
            do 175 m = j,19                                             
            wgsval(m) = wgsval(m+1)                                     
            wgsnam(m) = wgsnam(m+1)                                     
            wgsid(m) = wgsid(m+1)                                       
            wgskv(m) = wgskv(m+1)                                       
 175        wgstim(m) = wgstim(m+1)                                     
            go to 250                                                   
         endif                                                          
         go to 500                                                      
      endif                                                             
 200  continue                                                          
 250  do 400 j= 1,20                                                    
      if(pssdev .gt. wgsval(j))then                                     
         l = 21                                                         
         do 300 k = j+1,20                                              
         l = l - 1                                                      
         wgsval(l) = wgsval(l-1)                                        
         wgstim(l) = wgstim(l-1)                                        
         wgsnam(l) = wgsnam(l-1)                                        
         wgsid(l) = wgsid(l-1)                                          
 300     wgskv(l) = wgskv(l-1)                                          
         wgsval(j) = pssdev                                             
         wgstim(j) = to                                                 
         wgsnam(j) = nwgntc(1,i)                                        
         wgsid(j) = nwgntc(2,i)                                         
         wgskv(j) = basekv(ibkv)                                        
         go to 500                                                      
      endif                                                             
 400  continue                                                          
 500  continue                                                          
      return                                                            
      end                                                               
