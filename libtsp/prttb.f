C    %W% %G%
        subroutine prttb(lp,pagenm,line,mxline)                         
                                                                        
C               Print the TOP or BOTTOM heading lines                   
                                                                        
C               End current page and start the next with same titles    
                                                                        
      include 'tspinc/prt.inc' 
                                                                        
        integer pagenm                                                  
                                                                        
        entry prtbtm(lp,pagenm,line,mxline)                             
                                                                        
C                BLANKS LINES TO BOTTOM OF PAGE AND PRINTS FOOTER       
                                                                        
C          DETERMINE WHEATHER WE ARE ALREADY AT THE BOTTOM OF A PAGE    
C          AND ADVANCE TO THE BOTTOM IF WE ARE NOT.                     
                                                                        
           j = mxline-1                                                 
                                                                        
           if(line.le.j) then                                           
                do 10 i = line, j                                       
                   write(lp, 100)                                       
 100               format(1x)                                           
  10            continue                                                
                                                                        
           endif                                                        
                                                                        
C                WRITE THE FOOTER                                       
                                                                        
           write(lp, 101) header, repnam, pagenm, rdate                 
 101       format(1x, a, a, 3x,'PAGE', i4,2x, a)                        
C                                                                       
        return                                                          
        entry prttop(lp,pagenm,line,mxline)                             
                                                                        
C               STARTS A NEW PAGE WITH HEADER, COMENTS, & SUBHEADER     
                                                                        
C                ADVANCE TO NEXT PAGE AND WRITE HEADER                  
                                                                        
           pagenm = pagenm + 1                                          
C                                                                       
           write(lp, 102) header, repnam, pagenm, rdate                 
 102       format(1h1, a, a, 3x,'PAGE', i4, 2x,a)                       
                                                                        
C                WRITE THE TWO LINES OF COMMENT                         
                                                                        
           line=1                                                       
           if (coment(1) .ne. ' ') then                                 
              write(lp,100)                                             
              line=line+1                                               
           endif                                                        
           do 20 i=1,2                                                  
                                                                        
              if(coment(i).ne.' ') then                                 
                 write(lp,101)coment(i)(:132)                           
                 line=line+1                                            
                                                                        
              endif                                                     
                                                                        
  20       continue                                                     
                                                                        
C                SPACE BEFORE WRITING SUBHEADER                         
                                                                        
           write(lp, 100)                                               
           line=line+1                                                  
                                                                        
C               SET LINE COUNTER FOR CURRENT POSITION ON NEW PAGE       
C                   WRITE THE SUBHEADER                                 
                                                                        
           do 30 i = 1, 5                                               
              if(subhed(i).ne.' ') then                                 
                 write(lp, 103) subhed(i)                               
 103             format(a)                                              
                 line = line + 1                                        
                 if( subhed(i)(1:1).eq.'0') line = line + 1             
              endif                                                     
                                                                        
  30       continue                                                     
                                                                        
                                                                        
        return                                                          
                                                                        
        end                                                             
