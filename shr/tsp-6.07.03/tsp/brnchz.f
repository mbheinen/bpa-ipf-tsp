C    %W% %G%
      subroutine brnchz (k1, k2, id, ierr, gkm, bkm, gk1, bk1, gk2, bk2)
C                                                                       
C     THIS SUBROUTINE COMPUTES THE PI-EQUIVALENT FOR BRANCH K1-K2-ID.   
C     IF ID = '*', ALL PARALLELS ARE COMBINED. GKM AND BKM ARE THE      
C     THRU ADMITTANCE OF THE LINE.  GK1 AND BK1 ARE THE SHUNT ADMITTANCE
C     AT BUS K1.  GK2 AND BK2 ARE THE SHUNT ADMITTANCE AT BUS K2.       
C     "IERR" FLAGS RETURN STATUS: 0 = NORMAL                            
C                                -1 = ERROR (BRANCH NOT FOUND)          
C                                -3 = ERROR (DC LINE)                   
C     BRNCHZ IS CALLED BY NOUT2.                                        
C                                                                       
      include 'tspinc/params.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/out512.inc' 
      include 'tspinc/brnch.inc' 
      include 'tspinc/namec.inc' 
      include 'tspinc/link56.inc' 
      character*1 id,idx                                                
C                                                                       
C     INITIALIZE VARIABLES                                              
C                                                                       
      ierr = -1                                                         
      gkm = 0.0                                                         
      bkm = 0.0                                                         
      gk1 = 0.0                                                         
      bk1 = 0.0                                                         
      gk2 = 0.0                                                         
      bk2 = 0.0                                                         
C                                                                       
C     FIND BRANCH STARTING INDEX "NBR" BY BINARY SEARCH.                
C                                                                       
      i1 = 1                                                            
      i2 = ltoto                                                        
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
C                                                                       
C     ALIGN "NBR" TO FIRST OCCURRENCE OF K1-K2-ID.                      
C                                                                       
  120 do while (nbr .gt. 1 .and. 
     &         (jbrnch(2,nbr-1) .eq. k1 .and. jbrnch(12,nbr-1) .eq. k2))
         nbr = nbr - 1
      enddo
                                                                        
      do 160 i = nbr, ltoto                                             
      if (jbrnch(2,i) .ne. k1 .or. jbrnch(12,i) .ne. k2) go to 900      
C                                                                       
C     SKIP "R" AND "RX" RECORDS                                         
C                                                                       
      if( jbrnch(1,i) .eq. 4 .or. jbrnch(1,i) .eq. 9)then               
C     IF WILDCARD ("*") PARALLEL, USE FIRST PARALLEL.                   
C                                                                       
      else if (id .eq. '*') then                                        
         go to 170                                                      
      else                                                              
         idx = char(jbrnch(13,i))                                       
  150    format (a1)                                                    
         if (idx .eq. id) go to 170                                     
      endif                                                             
  160 continue                                                          
      go to 900                                                         
C                                                                       
C     PARALLELS MATCHED. COMPUTE PI-EQUIVALENT.                         
C                                                                       
  170 do 180 i = nbr, ltoto                                             
      if (jbrnch(2,i) .ne. k1 .or. jbrnch(12,i) .ne. k2) go to 190      
      if (jbrnch(1,i) .eq. 4 .or. jbrnch(1,i) .eq. 9 ) go to 180        
      if (jbrnch(1,i) .eq. 2 .or. jbrnch(1,i) .eq. 7) go to 800         
      if (id .eq. '*') then                                             
         if (jbrnch(14,i) .eq. 0) then                                  
            ierr = 0                                                    
            call pieqiv (jbrnch(1,i),g1,b1,g2,b2,g12,b12,b21,b21)       
            gkm = gkm + g12                                             
            bkm = bkm + b12                                             
            gk1 = gk1 + g1                                              
            bk1 = bk1 + b1                                              
            gk2 = gk2 + g2                                              
            bk2 = bk2 + b2                                              
         endif                                                          
      else                                                              
         idx = char(jbrnch(13,i))                                       
         if (idx .eq. id .and. jbrnch(14,i) .eq. 0) then                
            ierr = 0                                                    
            call pieqiv (jbrnch(1,i),gk1,bk1,gk2,bk2,gkm,bkm,gmk,bmk)   
            go to 190                                                   
         endif                                                          
      endif                                                             
  180 continue                                                          
  190 return                                                            
C                                                                       
C     ERROR EXIT - INELIGIBLE BRANCH                                    
C                                                                       
  800 write (errbuf(1),810) exnamc(k1),basekv(ixnamn(k1)),              
     1   exnamc(k2),basekv(ixnamn(k2)),id, jbrnch(1,i)                  
  810 format (' BRANCH ',a8,f6.1,1x,a8,f6.1,2x,a1,' TYPE ',i1,          
     1  ' IS INELIGIBLE')                                               
      call prterr ('W',1)                                               
      if(jbrnch(1,i) .eq. 7 .or. jbrnch(1,i) .eq. 2)then                
         ierr = -3                                                      
         write(errbuf(1),830)                                           
 830     format(5x,' BRANCH IS INELIGIBLE BECAUSE IT IS A DC LINE')     
         call prterr ('W',1)                                            
         return                                                         
      endif                                                             
C                                                                       
C     ERROR EXIT - BRANCH NOT FOUND                                     
C                                                                       
  900 write (errbuf(1),910) exnamc(k1),basekv(ixnamn(k1)),              
     1   exnamc(k2),basekv(ixnamn(k2)),id                               
  910 format(' BRANCH ',a8,f6.1,1x,a8,f6.1,2x,a1,' COULD NOT BE FOUND') 
      call prterr ('W',1)                                               
      ierr = -1                                                         
      return                                                            
      end                                                               
