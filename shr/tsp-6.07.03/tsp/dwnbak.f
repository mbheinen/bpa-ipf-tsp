C    %W% %G%
        subroutine dwnbak                                               
c                                                                       
c       THIS SUBROUTINE PERFORMS THE DOWNWARD AND BACK SUBSTITUTION     
c       ON THE CURRENT VECTOR TO FORM THE VOLTAGE VECTOR.  IT IS        
c       CALLED BY DERIV.                                                
c                                                                       
        include 'tspinc/params.inc' 
        include 'tspinc/busvolt.inc' 
        include 'tspinc/yfactr.inc' 
        include 'tspinc/cntrl2.inc' 
        include 'tspinc/param.inc' 
        include 'tspinc/contrl.inc' 
        include 'tspinc/search.inc' 
        include 'tspinc/lnk12.inc' 
        include 'tspinc/link2.inc' 
        include 'tspinc/lnk1a.inc' 
        include 'tspinc/newton.inc' 
        include 'tspinc/fltopt.inc' 
        include 'tspinc/vrgov.inc' 
        include 'tspinc/equiv.inc' 
        include 'tspinc/bcur.inc' 

        double precision vyr(MAXBUS), vyi(MAXBUS), slnr, slni, eyrt,      
     1                   eyit                                                          
        logical found
C                                                                       
C       CALCULATE NEW VOLTS                                             
C                                                                       
        do i = 1, nmx                                               
           vyr(i) = bcurr(i)                                         
           vyi(i) = bcuri(i)                                         
        enddo

        irow = 1                                                        
        do while (irow .le. nmx-1) 
           if (vyr(irow) .ne. 0.0 .or. vyi(irow) .ne. 0.0) then
              istart = ilocf(irow)                                              
              iend = ilocf(irow+1)-1                                          
              slnr = vyr(irow)                                                
              slni = vyi(irow)                                                
              ipvot = irow                                                    
              do ii = istart, iend                                            
                 i1 = ii + 1                                              
                 kk = indxz(ii)                                             
C                                                                       
C                CHECK FOR SKEWSYMMETRIC TERM                                    
C                                                                       
                 if (kk .ne. janti) then                                 
                    vyr(kk) = vyr(kk) - slnr*gtri(i1) + slni*btri(i1)             
                    vyi(kk) = vyi(kk) - slni*gtri(i1) - slnr*btri(i1)       
                 else                                                    
C                                                                       
C                SKEWSYMMETRIC TERM IS NEGATIVE TO LEFT OF DIAGONAL              
C                                                                       
                    kk = indxz(ii+1)                                           
                    vyr(kk) = vyr(kk) + slnr*gtri(i1) - slni*btri(i1)       
                    vyi(kk) = vyi(kk) + slni*gtri(i1) + slnr*btri(i1)       
                 endif                                                   
              enddo
           endif
           irow = irow + 1                                                   
        enddo
C                                                                       
C       GET PROD. OF SOLN COL AND DIAG                                  
C                                                                       
        do i = 1, nmx                                                  
           eyrt = rdiar(i)*vyr(i) - rdiax(i)*vyi(i)                      
           eyit = rdiax(i)*vyr(i) + rdiar(i)*vyi(i)                      
           vyr(i) = eyrt                                                 
           vyi(i) = eyit                                                 
        enddo
C                                                                       
C       BACK SUBSTITUTION                                               
C                                                                       
        if (iesc .ne. 3) then
C ***                                                                   
C ***      Faulted reorder option
C ***
        else if (ifltsw .eq. 1 .or. ifltb .eq. nmx) then
           vyr(nmx) = 0.0                                              
           vyi(nmx) = 0.0                                              
        endif

        irow = nmx - 1                                                       
        do while (irow .gt. 0)
           iend = ilocf(irow+1) - 1                                             
           istart = ilocf(irow)                                                 
           ipvot = irow                                                       
C  ***                                                                   
C  ***     Faulted bus reorder option
C  ***                                                                   
           found = .false.
           if (iesc .eq. 3) then
              if (irow .eq. ifltb) then
                 if (ifltsw .eq. 2) then                                        
                    vyr(irow) = 0.0                                                    
                    vyi(irow) = 0.0                                                    
                    found = .true.
                 endif                                                           
              endif
           endif
           if (.not. found) then
              do ii = istart, iend                                           
                 i1 = ii + 1                                             
                 kk = indxz(ii)                                              
C                                                                       
C                CHECK FOR SKEWSYMMETRIC TERM                                    
C                                                                       
                 if (kk .eq. janti) kk = indxz(ii+1)                     
                 vyr(irow) = vyr(irow) - vyr(kk)*gtri(i1) 
     &                                 + vyi(kk)*btri(i1)         
                 vyi(irow) = vyi(irow) - vyi(kk)*gtri(i1) 
     &                                 - vyr(kk)*btri(i1)       
              enddo
           endif
           irow = irow - 1                                                         
        enddo

        do i = 1,nmx                                                
           eyr(i) = vyr(i)                                                 
           eyi(i) = vyi(i)                                                 
        enddo
        return                                                          
        end                                                             

