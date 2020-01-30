C    %W% %G%
        subroutine stime(a)                                             
        character*(*) a                                                 
c                                                                       
c       Routine to time swing functions                         
c                                                                       
        include 'tspinc/prt.inc' 

        character*8 timed                                          

        save

        data tt, ts / 0.0, 0.0 /
C                                                                       
        if (a.ne.'START') then                                           
          te = cpu_secs (ts)

c         ta = Elapsed cpu time

          ta=te-ts                                                

c         Accum CPU time

          tt=tt+ta                                                
          ts=te                                                   

c         Time of day

          call time(timed)                                        
          itemp = crtsw                                           
          crtsw = 1                                               
          write (outbuf, 100)   a,ta                                      
  100     format('0 MODULE "',a16,'" PROCESSED:  ELAPSED CP TIME =',
     1       f8.2 )                                                  
          call prtout (1)                                         
          write (outbuf, 110)   tt, timed                                  
  110     format('  PROCESS TIME =', f8.2, ' wall clock ', a)          
          call prtout (1)                                         
          crtsw = itemp                                           
                                                                       
        else                                                            
                                                                       
          tt=0                                                    
          nof=0                                                   
          ts = cpu_secs (0.0)

        endif                                                           
                                                                       
        return                                                          
        end                                                             
