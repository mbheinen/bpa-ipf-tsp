C    %W% %G%
      subroutine wgacal(to,angint)                                      
C * * *                                                                 
C * * * THIS SUBROUTINE CALCULATES GENERATOR ANGLE DEVIATION            
C * * * AT EACH TIME STEP AND SAVES THE HIGHEST 20 DEVIATIONS,          
C * * * BUS NAMES WHERE THEY OCCUR, AND THE TIME WHEN THEY              
C * * * OCCURRED.  IT IS CALLED BY NOUT2.                               
C * * *                                                                 
      include 'tspinc/params.inc' 
      include 'tspinc/comn56.inc' 
      include 'tspinc/link56.inc' 
      include 'tspinc/room.inc' 
      include 'tspinc/wstequ.inc' 
      include 'tspinc/wga.inc' 
      include 'tspinc/nwgntn.inc' 
      include 'tspinc/out512.inc' 
      dimension angint(MAXGEN)                                          
      do 500 i = 1,isg                                                  
      bangl = angl(i)                                                   
      if(iref .ne. 0)bangl = bangl - angl(iref)                         
      angdev = abs(bangl - angint(i))                                   
      ibkv = nwgntn(i)                                                  
      do 200 j = 1,20                                                   
      if(nwgntc(1,i).eq.wganam(j).and.basekv(ibkv) .eq. wgakv(j)        
     1  .and. nwgntc(2,i) .eq. wgaid(j))then                            
          if(angdev .gt. wgaval(j))then                                 
            if(j .eq. 20)go to 250                                      
            do 175 m = j,19                                             
            wgaval(m) = wgaval(m+1)                                     
            wganam(m) = wganam(m+1)                                     
            wgaid(m) = wgaid(m+1)                                       
            wgakv(m) = wgakv(m+1)                                       
 175        wgatim(m) = wgatim(m+1)                                     
            go to 250                                                   
         endif                                                          
         go to 500                                                      
      endif                                                             
 200  continue                                                          
 250  do 400 j= 1,20                                                    
      if(angdev .gt. wgaval(j))then                                     
         l = 21                                                         
         do 300 k = j+1,20                                              
         l = l - 1                                                      
         wgaval(l) = wgaval(l-1)                                        
         wgatim(l) = wgatim(l-1)                                        
         wganam(l) = wganam(l-1)                                        
         wgaid(l) = wgaid(l-1)                                          
 300     wgakv(l) = wgakv(l-1)                                          
         wgaval(j) = angdev                                             
         wgatim(j) = to                                                 
         wganam(j) = nwgntc(1,i)                                        
         wgaid(j) = nwgntc(2,i)                                         
         wgakv(j) = basekv(ibkv)                                        
         go to 500                                                      
      endif                                                             
 400  continue                                                          
 500  continue                                                          
      return                                                            
      end                                                               
