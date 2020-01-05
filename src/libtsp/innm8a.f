C    %W% %G%
        function innm8a(busnam, kvcode)                                 
C       <> THIS FUNCTION MATCHES EACH EXTERNAL BUS NAME AND <>          
C       <> KV CODE IN EXNAMEC AND IXBASE WITH AN INTERNAL   <>          
C       <> BUS NAME AND KV CODE IN NEWTBC AND INWTB. THE  <>            
C       <> LOCATION IN NEWTABC WHERE THE MATCH WAS MADE IS  <>          
C       <> STORED INTO IXBASE.                              <>          
C                                                                       
      include 'tspinc/params.inc' 
        character * 8 busnam                                            
      include 'tspinc/newtab.inc' 
      include 'tspinc/link56.inc' 
        do 10 i=1,nmx                                                   
            if(newtbc(i) .eq. busnam .and. inwtb(i) .eq. kvcode) then   
                innm8a = i                                              
                                                                        
                return                                                  
                                                                        
           endif                                                        
10      continue                                                        
        innm8a = 0                                                      
        return                                                          
        end                                                             
