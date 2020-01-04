C    %W% %G%
        subroutine varfmt (number,field,len)                            
                                                                        
        real number                                                     
      include 'tspinc/prt.inc' 
        character*(*) field                                             
        integer len                                                     
        data ex1 /.0001/                                                
        data ex2 /-.0001/                                               
        if (number .eq. 0.0) then                                       
          field(1:len)='              '                                 
          field(len-2:len) = '0.0'                                      
          return                                                        
        endif                                                           
                                                                        
        if (number .gt. 0.0) then                                       
          if (len .lt. 7) then                                          
            if (number .ge. 10.**(len-2)) then                          
C       WRITE ERROR MSG -- NUMBER TOO BIG AND NO ROOM FOR EXPONENT      
                write (errbuf, 10)                                      
                call prterr ('E',1)                                     
10              format( ' VARFMT -- FIELD OVERFLOW ')                   
                field(1:len) = '******'                                 
            else                                                        
C * * *                                                                 
C * * * FIELD WONT FIT HERE                                             
C * * *                                                                 
             call fxdec (number,field,len)                            
            endif                                                       
          else                                                          
            if (number .lt. ex1) then                                   
C * * *                                                                 
C * * * OPTIMUM CUTOFF SIGNIFICANTCE                                    
C * * *                                                                 
          call expnt (number,field,len)                                 
            else                                                        
              if (number .ge. 10.**(len-2)) then                        
C * * *                                                                 
C * * * PREVENT FIELD OVERFLOW                                          
C * * *                                                                 
                call expnt (number,field,len)                           
              else                                                      
C * * *                                                                 
C * * * ALL ELSE FLOATS                                                 
C * * *                                                                 
                call fxdec (number,field,len)                           
              endif                                                     
                                                                        
            endif                                                       
          endif                                                         
          return                                                        
        endif                                                           
                                                                        
        if (number .lt. 0.0) then                                       
          if (len .lt. 8) then                                          
            if (number .le. -10.**(len-3)) then                         
                write (errbuf, 10)                                      
                call prterr ('E',1)                                     
                field(1:len) = '******'                                 
            else                                                        
                call fxdec (number,field,len)                           
            endif                                                       
          else                                                          
            if (number .gt. ex2) then                                   
                call expnt (number,field,len)                           
            else                                                        
              if (number .le. -10.**(len-3)) then                       
                call expnt (number,field,len)                           
              else                                                      
                call fxdec (number,field,len)                           
              endif                                                     
            endif                                                       
          endif                                                         
          return                                                        
        endif                                                           
        end                                                             
