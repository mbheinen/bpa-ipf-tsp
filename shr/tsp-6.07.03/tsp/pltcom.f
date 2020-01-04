C    %W% %G%
      subroutine pltcom                                                 
C * * *                                                                 
C * * * THIS SUBROUTINE PRINTS THE STUDY COMMENTS CARDS, LS CARDS,      
C * * * AND THE 20 LOWEST BUS VOLTAGES AND FREQUENCIES ON THE PLOT FILE.
C * * * IT IS CALLED BY CALPLT.                                         
C * * *                                                                 
      include 'tspinc/params.inc' 
      include 'tspinc/wcom.inc' 
      include 'tspinc/comn56.inc' 
      dimension xclf(15)                                                
      character*80 xclf                                                 
      equivalence (clf,xclf)                                            
      character*8 blnk8                                                 
      logical bias, normal                                              
      data blnk8/'        '/                                            
      call plots( 1, 0, 0 )                                          
      normal = .false. ! PORTRAIT ORIENTATION FOR COMMENTS              
      call pltori( normal, bias )                                       
C * * *                                                                 
C * * * PRINT COMMENT CARDS AT TOP OF PLOT                              
C * * *                                                                 
      xcom=1.0                                                          
      ycom=9.85                                                         
      space=0.15                                                        
                                                                        
      if (ktcom .ne. 0) then                                            
                                                                        
         do 100 k=1,ktcom                                               
            k1 = ( k-1 )*8 + 1                                          
            lineno = lineno + 1                                         
            call symbol( xcom, ycom, .0857, xclf(k), 0.0, 80 )  
            ycom = ycom - space                                         
  100    continue                                                       
                                                                        
      end if                                                            
                                                                        
      call symbol( xcom, ycom, .0857, blnk8, 0.0, 8 )           
C * * *                                                                 
C * * * PRINT SUMMARY OF LOWEST 20 VOLTAGES AND FREQUENCIES             
C * * *                                                                 
      do 200 l = 1, iwckt                                               
      ycom = ycom - space                                               
      call symbol( xcom, ycom, .0857, wrscom(l),0.0, 80)        
      lineno = lineno + 1                                               
C * * *                                                                 
C * * * IF MORE THAN 60 LINES HAVE BEEN PLOTTED, SKIP TO NEW PLOT PAGE  
C * * *                                                                 
      if(lineno .eq. 60)then                                            
         call plot( 0.0, 0.0, -999 )                                        
         call plots( 1, 0, 0 )                                       
         normal = .false. ! PORTRAIT ORIENTATION FOR COMMENTS           
         call pltori( normal, bias )                                    
         ycom = 9.85                                                    
         lineno = 0                                                     
      endif                                                             
                                                                        
  200 continue                                                          
      call plot( 0.0, 0.0, -999 )                                           
      return                                                            
      end                                                               
