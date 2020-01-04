C    @(#)section.f	20.3 2/13/96
        subroutine section (y)                                           

        complex y(2,2), yeq(3,3), z
C                                                                       
C       This subroutine consolidates sections. the first section is     
C       processed with                                                  
C                                                                       
C               CALL FIRST_SECT (Y)                                         
C                                                                       
C       and subsequent sections with                                    
C                                                                       
C               CALL NEXT_SECT (Y)                                         
C                                                                       
C       "Y" is the complex 2-port admittance matrix of the              
C       section being processed. It is not altered by either of         
C       the aforementioned calls.                                      
C                                                                       
C       The pi equivalent of cumulative sections can be retrieved with  
C                                                                       
C               CALL FINAL_SECT (Y)                                         
C                                                                       
C       where Y contains the complex 2-port admittance matrix.  
C                                                                       
C       Process first section                                           
C       
        save yeq
c                                                                
        entry first_sect (y)                                                
        do i = 1, 2                                                    
           do j = 1, 2                                                    
              yeq(j,i) = y(j,i)                                               
           enddo
        enddo
        do i = 1, 3                                                    
           yeq(i,3) = (0.0,0.0)                                            
           yeq(3,i) = (0.0,0.0)                                            
        enddo
        return                                                          
C                                                                       
C       Process subsequent sections                                     
C                                                                       
        entry next_sect (y)                                                
        do i = 2, 3                                                    
           do j = 2, 3                                                    
              yeq(j,i) = yeq(j,i) + y(j-1,i-1)                                
           enddo
        enddo
C                                                                       
C       Eliminate "interior" node                                       
C                                                                       
        z=yeq(2,2)                                                      
        if (z .eq. 0.0) z = 1.0e-6                                      
        do j = 1, 3, 2                                                  
           yeq(2,j) = yeq(2,j) / z                                             
        enddo
        do i = 1, 3, 2                                                  
           z = yeq(i,2)                                                      
           do j = 1, 3, 2                                                  
              yeq(i,j) = yeq(i,j) - z * yeq(2,j)                                    
           enddo
        enddo
C                                                                       
C       Reorder equivalent matrix                                       
C                                                                       
        yeq(1,2)=yeq(1,3)                                               
        yeq(2,2)=yeq(3,3)                                               
        yeq(2,1)=yeq(3,1)                                               
        do i = 1, 3                                                    
           yeq(3,i) = (0.0,0.0)                                              
           yeq(i,3) = (0.0,0.0)                                              
        enddo
        return                                                          
C                                                                       
C       Retrieve pi equivalent                                          
C                                                                       
        entry final_sect (y)                                                
        do i = 1, 2                                                    
           do j = 1, 2                                                    
              y(j,i) = yeq(j,i)                                                 
           enddo
        enddo
        return                                                          
        end                                                             
