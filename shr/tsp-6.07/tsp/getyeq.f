C    %W% %G%
      subroutine getyeq (k1,k2,id,ksect,yeq,y1,yxy,y2,y3,ierr)          
C * * *                                                                 
C * * *  This subroutine compute following 2-port Y-matrices:           
C * * *                                                                 
C * * *  YEQ - Equivalent parallel 2-port                               
C * * *  Y1  - 2-port left of section KSECT                             
C * * *  YXY - 2-port for section KSECT                                 
C * * *  Y2  - 2-port right of section KSECT                            
C * * *  Y3  - Equivalent parallel 2-port with section short-circuited. 
C * * *                                                                 
C * * *  IERR  ERROR CODE IERR = 0 LINE HAS BEEN FOUND                  
C * * *                   IERR = -1 LINE CANNOT BE FOUND                
C * * *                   IERR = -3 LINE IS EITHER A DC LINE OR         
C * * *                           A REGULATING TRANSFORMER              
C * * *                                                                 
      include 'tspinc/params.inc' 
      include 'tspinc/brnch.inc' 
      include 'tspinc/comn34.inc' 
      character id * 1, nxid * 1                                        
      integer count1, count2, first1, first2, first                     
      complex y(2,2), yeq(2,2), y1(2,2), y2(2,2), yxy(2,2), y3(2,2)     
      complex * 16 yscr(3,3)                                            
      logical found                                                     
C                                                                       
C     FIND BRANCH STARTING INDEX "NBR" BY BINARY SEARCH.                
C                                                                       
      ierr = 0                                                          
      i1 = 1                                                            
      i2 = ltot                                                         
  100 nbr = (i1 + i2) / 2                                               
      if (jbrnch(2,nbr) .lt. k1) then                                   
         i1 = nbr + 1                                                   
      else if (jbrnch(2,nbr) .gt. k1) then                              
         i2 = nbr - 1                                                   
      else                                                              
         if (jbrnch(12,nbr) .lt. k2) then                               
            i1 = nbr + 1                                                
         else if (jbrnch(12,nbr) .gt. k2) then                          
            i2 = nbr - 1                                                
         else                                                           
            go to 120                                                   
         endif                                                          
      endif                                                             
      if (i1 .le. i2) go to 100                                         
      go to 900                                                         
 120  call target                                                       !dem
C     -  Must backtrack to first branch with this bus pair              !dem
      la = nbr                                                          !dem
      ka = 0                                                            !dem
      do while (la .gt. ka)                                             !dem
        la = la - 1                                                     !dem  
        if (jbrnch(2,la) .ne. k1) ka = la                               !dem
        if (jbrnch(12,la) .ne. k2) ka = la                              !dem
      enddo                                                             !dem
      nbr = ka + 1                                                      !dem
C     -
      count1 = 0                                                        !dem
c 120 COUNT1 = 0                                                        
      count2 = 0                                                        
      found = .false.                                                   
      first1 = 0                                                        
      last1 = 0                                                         
      last2 = 0                                                         
      first2 = 0                                                        
      match = 0                                                         
      last = min0 (nbr + 100, ltot)                                     
      do 260 i = nbr, last                                              
         if (jbrnch(2,i) .ne. k1) then                                  
            go to 270                                                   
         else if (jbrnch(12,i) .eq. k2) then                            
C * * *                                                                 
C * * * IF THIS LINE IS A DC LINE OR A REGULATING TRANSFORMER,          
C * * * SET ERROR FLAG AND RETURN                                       
C * * *                                                                 
            if (jbrnch(1,i) .eq. 4 .or. jbrnch(1,i) .eq. 2 .or.         
     1          jbrnch(1,i) .eq. 7) then                                
               ierr = -3                                                
               return                                                   
            endif                                                       
            nxid = char(jbrnch(13,i))                                   
            if (id .eq. nxid) then                                      
               if (jbrnch(14,i) .eq. 0) then                            
                  if (ksect .eq. jbrnch(14,i)) then                     
                     call pieqix (jbrnch(1,i), gk1, bk1, gk2, bk2,      
     1                  gkm, bkm, gmk, bmk)                             
                     yxy(1,1) = cmplx (gk1+gkm, bk1+bkm)                
                     yxy(1,2) = cmplx (-gkm, -bkm)                      
                     yxy(2,1) = cmplx (-gmk, -bmk)                      
                     yxy(2,2) = cmplx (gk2+gmk, bk2+bmk)                
                  endif                                                 
                  if (jbrnch(1,i) .eq. 1) go to 260                     
               endif                                                    
               if (ksect .ne. jbrnch(14,i)) then                        
                  if (.not.found) then                                  
                     count1 = count1 + 1                                
                     if (first1 .eq. 0) first1 = i                      
                     last1 = i                                          
                  else                                                  
                     count2 = count2 + 1                                
                     if (first2 .eq. 0) first2 = i                      
                     last2 = i                                          
                  endif                                                 
               else                                                     
                  match = i                                             
                  call pieqix (jbrnch(1,i), gk1, bk1, gk2, bk2,         
     1               gkm, bkm, gmk, bmk)                                
                  yxy(1,1) = cmplx (gk1+gkm, bk1+bkm)                   
                  yxy(1,2) = cmplx (-gkm, -bkm)                         
                  yxy(2,1) = cmplx (-gmk, -bmk)                         
                  yxy(2,2) = cmplx (gk2+gmk, bk2+bmk)                   
                  if (.not.found) then                                  
                     found = .true.                                     
                  endif                                                 
               endif                                                    
            endif                                                       
         else if (jbrnch(12,i) .gt. k2) then                            
            go to 270                                                   
         endif                                                          
  260 continue                                                          
  270 continue                                                          
C * * *                                                                 
C * * *   The equivalent pi admittance for a branch with sections is not
C * * *   available; also, it cannot be computed using conventional call
C * * *   to FIRSEC, NEXSEC, and FINSEC since that would jeopardize the 
C * * *   data being stored for the same branch!                        
C * * *                                                                 
C * * *   EQVFIR, EQVNEX, and EQVFIN are entry points in EQVSEC.  It    
C * * *   is a sharable image.  All data including the equivalent Y-matr
C * * *   is stored in the calling program.                             
C * * *                                                                 
C * * *   IF SECTION KSECT HAS NOT BEEN FOUND, SET ERROR MESSAGE AND RET
C * * *                                                                 
      if(.not. found)then                                               
         ierr = -1                                                      
         return                                                         
      endif                                                             
      nsect = 0                                                         
      first = first1                                                    
      if (first .eq. 0) first = match                                   
      last = last2                                                      
      if (last .eq. 0) last = match                                     
      if (last .eq. 0) last = last1                                     
      do 280 i = first, last                                            
         if (jbrnch(1,i) .eq. 9) go to 280                              
         call pieqix (jbrnch(1,i), gk1, bk1, gk2, bk2,                  
     1      gkm, bkm, gmk, bmk)                                         
         y(1,1) = cmplx (gk1+gkm, bk1+bkm)                              
         y(1,2) = cmplx (-gkm, -bkm)                                    
         y(2,1) = cmplx (-gmk, -bmk)                                    
         y(2,2) = cmplx (gk2+gmk, bk2+bmk)                              
         nsect = nsect + 1                                              
         if (nsect .eq. 1) then                                         
            call eqvfir (y,yscr)                                        
         else                                                           
            call eqvnex (y,yscr)                                        
         endif                                                          
  280 continue                                                          
      call eqvfin (yeq,yscr)                                            
C * * *                                                                 
C * * *  Step 1. YEQ is now completed. Get sections to left of KSECT.   
C * * *                                                                 
      nsect = 0                                                         
      if (count1 .gt. 0) then                                           
         do 300 i = first1, last1                                       
            if (jbrnch(1,i) .eq. 9) go to 300                           
            call pieqix (jbrnch(1,i), gk1, bk1, gk2, bk2,               
     1         gkm, bkm, gmk, bmk)                                      
            y(1,1) = cmplx (gk1+gkm, bk1+bkm)                           
            y(1,2) = cmplx (-gkm, -bkm)                                 
            y(2,1) = cmplx (-gmk, -bmk)                                 
            y(2,2) = cmplx (gk2+gmk, bk2+bmk)                           
            nsect = nsect + 1                                           
            if (nsect .eq. 1) then                                      
               call eqvfir (y,yscr)                                     
            else                                                        
               call eqvnex (y,yscr)                                     
            endif                                                       
  300    continue                                                       
         call eqvfin (y1,yscr)                                          
      else                                                              
         do 310 k = 1, 2                                                
            do 310 l = 1, 2                                             
               y1(k,l) = cmplx (0.0,0.0)                                
  310    continue                                                       
      endif                                                             
C * * *                                                                 
C * * *   Step 2. Y1 is now completed. Get sections to right of KSECT   
C * * *                                                                 
      nsect = 0                                                         
      if (count2 .gt. 0) then                                           
         do 330 i = first2, last2                                       
            if (jbrnch(1,i) .eq. 9) go to 330                           
            call pieqix (jbrnch(1,i), gk1, bk1, gk2, bk2,               
     1         gkm, bkm, gmk, bmk)                                      
            y(1,1) = cmplx (gk1+gkm, bk1+bkm)                           
            y(1,2) = cmplx (-gkm, -bkm)                                 
            y(2,1) = cmplx (-gmk, -bmk)                                 
            y(2,2) = cmplx (gk2+gmk, bk2+bmk)                           
            nsect = nsect + 1                                           
            if (nsect .eq. 1) then                                      
               call eqvfir (y,yscr)                                     
            else                                                        
               call eqvnex (y,yscr)                                     
            endif                                                       
  330    continue                                                       
         call eqvfin (y2,yscr)                                          
      else                                                              
         do 340 k = 1, 2                                                
            do 340 l = 1, 2                                             
               y2(k,l) = cmplx (0.0,0.0)                                
  340    continue                                                       
      endif                                                             
C * * *                                                                 
C * * *    Y2 is now completed.  Compute Y3.                            
C * * *                                                                 
      if (count1 .eq. 0 .and. count2 .eq. 0) then                       
         do 350 k = 1, 2                                                
            do 350 l = 1, 2                                             
               y3(k,l) = cmplx (0.0, 0.0)                               
  350    continue                                                       
      else if (count1 .eq. 0 .and. count2 .gt. 0) then                  
         do 360 k = 1, 2                                                
            do 360 l = 1, 2                                             
               y3(k,l) = y2(k,l)                                        
  360    continue                                                       
      else if (count1 .gt. 0 .and. count2 .eq. 0) then                  
         do 370 k = 1, 2                                                
            do 370 l = 1, 2                                             
               y3(k,l) = y1(k,l)                                        
  370    continue                                                       
      else                                                              
         call eqvfir (y1,yscr)                                          
         call eqvnex (y2,yscr)                                          
         call eqvfin (y3,yscr)                                          
      endif                                                             
      return                                                            
C * * *                                                                 
C * * * ERROR EXIT - BRANCH NOT FOUND                                   
C * * *                                                                 
  900 ierr = -1                                                         
      return                                                            
      end                                                               
