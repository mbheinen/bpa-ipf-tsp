C    %W% %G%
        subroutine erexit                                               
c                                                                       
c       This subroutine is called whenever the program encounters       
C       a fatal error.  It prints a fatal error message, closes         
C       all files, and stops the program.                               
c                                                                       
        include 'tspinc/blkcom1.inc' 
        include 'tspinc/prt.inc' 
        include 'tspinc/errmsg.inc' 

        common /error_code/ error_code
        integer error_code

        character job*30                                                
        data job /'                              '/                     
c                                                                       
c       WRITE ERROR MESSAGE                                             
c                                                                       
        write (errbuf(1), 90) error_code
   90   format (' Program aborted for reasons given (', i2, ')')
        call prterr ('E',1)

        write(errbuf(1),100)                                            
  100   format ('0 Program terminated by error conditions. If "F" errors
     1 have been encountered, correct and rerun.')                      
        write (errbuf(2),110)                                           
  110   format ('  Otherwise report to programming staff.')             
        call prterr('E',2)                                              
        call proerr                                                     
        job(1:19) = 'ABORT SWING '//ver                                 
        job(21:30) = scase                                              
        call prgmon(job,0)                                              
        call stime('ABORTED RUN')                                       

        call plot(0.0, 0.0, 999)                                            
        call closts(1,1)                                                
        call closts(2,1)                                                
        call closts(8,0)                                                
        call closts(9,0)                                                
        call closts(mfich,0)                                            
        stop                                                            
C                                                                       
C                                                                       
C       This entry is similar to EREXIT except it is called from        
C       PRTERR and cannot recursively recall PRTERR as is done in       
C       EREXIT                                                          
C                                                                       
        entry erquit                                                    
        call proerr                                                     
        job(1:19) = 'ABORT SWING '//ver                                 
        job(21:30) = scase                                              
        call prgmon(job,0)                                              
        call stime('ABORTED RUN')                                       
        call plot(0.0, 0.0, 999)                                            
        call closts(1,1)                                                
        call closts(2,1)                                                
        call closts(8,0)                                                
        call closts(9,0)                                                
        call closts(mfich,0)                                            
C                                                                       
        stop                                                            
C                                                                       
        end                                                             
