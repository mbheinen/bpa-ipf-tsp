C    %W% %G%
      subroutine hivolt(to)                                             
C * * *                                                                 
C * * * THIS SUBROUTINE CALCULATES THE BUS VOLTAGE MAGNITUDE            
C * * * AT EACH TIME STEP AND SAVES THE HIGHEST 20 BUS VOLTAGES         
C * * * BUS NAMES WHERE THEY OCCUR AND THE TIME WHEN THEY               
C * * * OCCURRED.  IT IS CALLED BY NOUT2.                               
C * * *                                                                 
      include 'tspinc/params.inc' 
      include 'tspinc/comn56.inc' 
      include 'tspinc/link56.inc' 
      include 'tspinc/newtab.inc' 
      include 'tspinc/hivlt.inc' 
      include 'tspinc/worst.inc' 
      include 'tspinc/fltim.inc' 
      include 'tspinc/room.inc' 
      include 'tspinc/wstequ.inc' 
C * * *                                                                 
C * * * IF A TIME WINDOW HAS BEEN ENTERED, CALCULATE BUS VOLTAGES       
C * * * DURING THE WINDOW                                               
C * * *                                                                 
      if(to .lt. wtim1 .or. to .gt. wtim2 .and. wtim2 .ne. -1)return    
      do 500 i = 1,nmx                                                  
      ibkv = inwtb(i)                                                   
      bkv = basekv(ibkv)                                                
      vlt = sqrt(eyr(i)*eyr(i) + eyi(i)*eyi(i))                         
C * * *                                                                 
C * * * IF THE BASE KV IS 200. CONVERT PU VOLTAGE TO 230KV BASE         
C * * *                                                                 
      if(bkv .eq. 200.)then                                             
         vlt = vlt*0.86975                                              
         bkv = 230.                                                     
      endif                                                             
C * * *                                                                 
C * * * IF THE BASE KV IS 100. CONVERT PU VOLTAGE TO 115KV BASE         
C * * *                                                                 
      if(bkv .eq. 100.)then                                             
         vlt = vlt*0.86975                                              
         bkv = 115.                                                     
      endif                                                             
      if(vlt .lt. .95)go to 500                                         
      do 200 j = 1,20                                                   
      if(newtbc(i) .eq. hivnm(j).and. bkv .eq. hivkv(j))then            
         if(vlt .gt. hivvt(j))then                                      
            if(j .eq. 20)go to 250                                      
            do 175 m = j,19                                             
            hivvt(m) = hivvt(m+1)                                       
            hivnm(m) = hivnm(m+1)                                       
            hivkv(m) = hivkv(m+1)                                       
            ihivno(m) = ihivno(m+1)                                     
 175        hivtim(m) = hivtim(m+1)                                     
            go to 250                                                   
         endif                                                          
         go to 500                                                      
      endif                                                             
 200  continue                                                          
 250  do 400 j= 1,20                                                    
      if(vlt .gt. hivvt(j))then                                         
         l = 21                                                         
         do 300 k = j+1,20                                              
         l = l - 1                                                      
         hivvt(l) = hivvt(l-1)                                          
         hivtim(l) = hivtim(l-1)                                        
         hivnm(l) = hivnm(l-1)                                          
         ihivno(l) = ihivno(l-1)                                        
 300     hivkv(l) = hivkv(l-1)                                          
         hivvt(j) = vlt                                                 
         hivtim(j) = to                                                 
         hivnm(j) = newtbc(i)                                           
         hivkv(j) = bkv                                                 
         ihivno(j) = i                                                  
         go to 500                                                      
      endif                                                             
 400  continue                                                          
 500  continue                                                          
      return                                                            
      end                                                               
