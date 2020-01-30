C    %W% %G%
        function igensr(busnam,kvcode,idch)                             
C       <> THIS FUNCTION IS USED TO LOCATE THE POSITION  <>             
C       <> WITHIN TABLE NWGNTC WHERE THE IDENTIFICATION   <>            
C       <> OF ANY GENERATOR IS MATCHED WITH THE SAME     <>             
C       <> GENERATOR IDENTIFICATION IN IGENT.            <>             
      include 'tspinc/params.inc' 
        character * 8 busnam                                            
           character *(*) idch                                          
      include 'tspinc/blkcom1.inc' 
      include 'tspinc/link56.inc' 
      include 'tspinc/nwgntn.inc' 
      include 'tspinc/prt.inc' 
        do 20 i = 1,isg                                                 
                if (nwgntc(1,i) .eq. busnam .and. nwgntn(i) .eq.        
     1                 kvcode .and. nwgntc(2, i) .eq. idch(1:1)) then   
                        igensr = i                                      
                        return                                          
                endif                                                   
 20     continue                                                        
        igensr = 0                                                      
        if (kvcode .gt. 0) then
           basex = basekv(kvcode)
        else
           basex = 0.0
        endif
        write (errbuf,40) busnam,basex,idch
        call prterr ('E',1)                                             
   40   format (1h0, 'THE GENERATOR WITH IDENTIFICATION ',              
     1    a8,1x,f5.1,1x,a1,1x,                                          
     2    'COULD NOT BE LOCATED IN THE IGENT TABLE. ')                  
        return                                                          
        end                                                             
