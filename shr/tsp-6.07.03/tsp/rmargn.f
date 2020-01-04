C    %W% %G%
      subroutine rmargn(bus1,base1,bus2,base2,ipar,lin6)                
C * * *                                                                 
C * * * THIS SUBROUTINE CALCULATES THE DISTANCE BETWEEN THE             
C * * * TRIPPING LINE AND THE R AND RDOT OF A LINE.  THIS               
C * * * DISTANCE IS KNOWN AS THE MARGIN.  BUS1,BASE1,BUS2,              
C * * * BASE2,IPAR IDENTIFY THE LINE BEING PROCESSED.  LIN6             
C * * * IS THE STARTING ADDRESS FOR R AND RDOT IN THE TABLE             
C * * * WORKSP.  RMARGN IS CALLED BY LINOUT.                            
C * * *                                                                 
      include 'tspinc/params.inc' 
      include 'tspinc/room.inc' 
      include 'tspinc/comn56.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/rddat.inc' 
      include 'tspinc/worst.inc' 
      dimension rmar(2000)                                              
      character*8 bus1,bus2                                             
      character*1 ipar                                                  
      if(irdknt .eq. 0)return                                           
      do 200 ltrr = 1,irdknt                                            
      if(rname1(ltrr) .ne. bus1 .or. rname2(ltrr) .ne. bus2) go to 200  
      if(rbkv1(ltrr) .ne. base1 .or. rbkv2(ltrr) .ne. base2) go to 200  
      if(rpar(ltrr) .eq. ipar) go to 300                                
 200  continue                                                          
      return                                                            
C * * *                                                                 
C * * * CALCULATE THE INTERCEPT FOR THE TRIP LINE--B1                   
C * * * THE TRIP LINE EQUATION IS Y = SLOPE1*X + B1                     
C * * *                                                                 
 300  slope1 = rslope(ltrr)                                             
      x0 = rzero(ltrr)                                                  
      b1 = -slope1*x0                                                   
C * * *                                                                 
C * * * SLOPE2 IS THE SLOPE OF A LINE PERPENDICULAR TO                  
C * * * THE TRIP LINE                                                   
C * * *                                                                 
      slope2 = -1.0/slope1                                              
      rrdmax(ltrr) = -10000.                                            
      rrdmin(ltrr) = 10000.                                             
      ktrr = 0                                                          
      rtmax(ltrr) = 0.0                                                 
      rtmin(ltrr) = 0.0                                                 
      icnt2 = 2*icount                                                  
      do 400 jtrr = 1,icnt2,2                                           
      ktrr = ktrr+1                                                     
      r = worksp(lin6+jtrr)                                             
      rdot = worksp(lin6 +jtrr+1)                                       
C * * *                                                                 
C * * * IF RDOT IS POSITIVE, SET MARGIN TO A LARGE NUMBER AND           
C * * * GO TO THE NEXT POINT.  TRIP LINE IS DEFINED ONLY FOR            
C * * * NEGATIVE VALUES OF RDOT                                         
C * * *                                                                 
      if(rdot .gt. 0.0)then                                             
        rmar(ktrr) = 1000.                                              
        go to 400                                                       
      endif                                                             
C * * *                                                                 
C * * * THE EQUATION OF A LINE PERPENDICULAR TO THE TRIP                
C * * * LINE WHICH PASSES THROUGH THE POINT (R,RDOT) IS                 
C * * * RDOT = SLOPE2*R + B2.  THIS IS THE SHORTEST                     
C * * * DISTANCE BETWEEN THE TRIP LINE AND THE POINT (R,RDOT)           
C * * *                                                                 
      b2 = rdot - slope2*r                                              
C * * *                                                                 
C * * * FIND THE POINT (R1,RDOT1) WHICH IS THE INTERSECTION             
C * * * OF THE TRIP LINE AND THE SHORTEST DISTANCE LINE.                
C * * * IT SATISFIES BOTH RDOT1 = SLOPE1*R1 + B1 AND                    
C * * * RDOT1 = SLOPE2*R1 + B2. SO SLOPE1*R1 + B1 = SLOPE2*R1 + B2      
C * * *                                                                 
      r1 = (b2-b1)/(slope1-slope2)                                      
C * * *                                                                 
C * * * THE MARGIN IS THE DIFFERENCE BETWEEN R1 AND R                   
C * * *                                                                 
      rmar(ktrr) = r - r1                                               
      if(wtim2 .ne. -1)then                                             
         indx = (jtrr+1)/2                                              
         tnow = t(indx)                                                 
         if(tnow .gt. wtim1 .and. tnow .lt. wtim2)then                  
            if(rmar(ktrr) .gt. rrdmax(ltrr))then                        
               rrdmax(ltrr) = rmar(ktrr)                                
               rtmax(ltrr) = t(ktrr)                                    
            endif                                                       
            if(rmar(ktrr) .lt. rrdmin(ltrr))then                        
               rrdmin(ltrr) = rmar(ktrr)                                
               rtmin(ltrr) = t(ktrr)                                    
            endif                                                       
         endif                                                          
      else                                                              
         if(rmar(ktrr) .gt. rrdmax(ltrr))then                           
            rrdmax(ltrr) = rmar(ktrr)                                   
            rtmax(ltrr) = t(ktrr)                                       
         endif                                                          
         if(rmar(ktrr) .lt. rrdmin(ltrr))then                           
            rrdmin(ltrr) = rmar(ktrr)                                   
            rtmin(ltrr) = t(ktrr)                                       
         endif                                                          
      endif                                                             
 400  continue                                                          
      write(outbuf,500)                                                 
 500  format('0',5x,' TRIPPING MARGIN IN OHMS ')                        
      call prtout(1)                                                    
      if(wtim2 .ne. -1)then                                             
         write(outbuf,550)wtim1,wtim2                                   
  550    format(5x,'DURING TIME WINDOW ',f8.2,' CYCLES TO ',f8.2,       
     1          ' CYCLES.')                                             
         call prtout(1)                                                 
      endif                                                             
      write(outbuf,600)rrdmax(ltrr),rtmax(ltrr),rrdmin(ltrr),rtmin(ltrr)
 600  format('0',5x,' MAXIMUM MARGIN =',f8.2,' AT ',f8.2,' CYCLES. ',   
     1       ' MINIMUM MARGIN = ',f8.2,' AT ',f8.2,' CYCLES. ')         
      call prtout(1)                                                    
      do 800 jjj=1,icount,5                                             
      kkk = min0(jjj+4,icount)                                          
      write(outbuf,700) (t(jj),rmar(jj),jj=jjj,kkk)                     
 700  format(5(1x,f8.2,'CYCLES',f9.2))                                  
      call prtout(1)                                                    
 800  continue                                                          
      return                                                            
      end                                                               
