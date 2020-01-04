C    %W% %G%
      subroutine wstvlt(to)                                             
C * * *                                                                 
C * * * THIS SUBROUTINE CALCULATES THE BUS VOLTAGE MAGNITUDE            
C * * * AT EACH TIME STEP AND SAVES THE LOWEST 20 BUS VOLTAGES          
C * * * BUS NAMES WHERE THEY OCCUR AND THE TIME WHEN THEY               
C * * * OCCURRED.  IT IS CALLED BY NOUT2.                               
C * * *                                                                 
      include 'tspinc/params.inc' 
      include 'tspinc/comn56.inc' 
      include 'tspinc/link56.inc' 
      include 'tspinc/newtab.inc' 
      include 'tspinc/worst.inc' 
      include 'tspinc/fltim.inc' 
      include 'tspinc/room.inc' 
      include 'tspinc/wstequ.inc' 
C * * *                                                                 
C * * * DO NOT CALCULATE VOLTAGES IF A FAULT IS APPLIED AT THIS TIME    
C * * *                                                                 
      if(ifltkt .ne. 0)then                                             
         do 100 l = 1,ifltkt                                            
         if(to .eq. fstrt(l) .and. to .eq. tlast)return                 
         if(to .eq.  fstop(l) .and. to .ne. tlast)return                
         if((to .gt. fstrt(l)) .and. (to .lt. fstop(l)))return          
 100     continue                                                       
      endif                                                             
      do 500 i = 1,nmx                                                  
      vlt = sqrt(eyr(i)*eyr(i) + eyi(i)*eyi(i))                         
      if(vlt .gt. 1.0)go to 500                                         
      if(vlt .le. 0.0)go to 500                                         
      ibkv = inwtb(i)                                                   
      do 200 j = 1,20                                                   
      if(newtbc(i) .eq. wrstnm(j).and.basekv(ibkv) .eq. wrstkv(j))then  
         if(vlt .lt. wrstvt(j))then                                     
            if(j .eq. 20)go to 250                                      
            do 175 m = j,19                                             
            wrstvt(m) = wrstvt(m+1)                                     
            wrstnm(m) = wrstnm(m+1)                                     
            wrstkv(m) = wrstkv(m+1)                                     
            iwrsno(m) = iwrsno(m+1)                                     
 175        wrsttm(m) = wrsttm(m+1)                                     
            go to 250                                                   
         endif                                                          
         go to 500                                                      
      endif                                                             
 200  continue                                                          
 250  do 400 j= 1,20                                                    
      if(vlt .lt. wrstvt(j))then                                        
         l = 21                                                         
         do 300 k = j+1,20                                              
         l = l - 1                                                      
         wrstvt(l) = wrstvt(l-1)                                        
         wrsttm(l) = wrsttm(l-1)                                        
         wrstnm(l) = wrstnm(l-1)                                        
         iwrsno(l) = iwrsno(l-1)                                        
 300     wrstkv(l) = wrstkv(l-1)                                        
         wrstvt(j) = vlt                                                
         wrsttm(j) = to                                                 
         wrstnm(j) = newtbc(i)                                          
         wrstkv(j) = basekv(ibkv)                                       
         iwrsno(j) = i                                                  
         go to 500                                                      
      endif                                                             
 400  continue                                                          
 500  continue                                                          
      return                                                            
      end                                                               
