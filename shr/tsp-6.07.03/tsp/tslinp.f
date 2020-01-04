C    %W% %G%
      subroutine tslinp(j)                                              
                                                                        
C * * *                                                                 
C * * * THIS SUBROUTINE READS THE CARDS FOR THE 'ST' CARD DATA          
C * * * FOR TRANSIENT STABILIZERS AND FORMS DATA TABLES                 
C * * *                                                                 
      include 'tspinc/params.inc' 
      include 'tspinc/inp3.inc' 
      include 'tspinc/blkcom1.inc' 
      include 'tspinc/tsldat.inc' 
      include 'tspinc/param.inc' 
      include 'tspinc/prt.inc' 
C     -     begin     begin     begin     begin     begin     begin 
      if (msupp .eq. 0 .or. msupp .eq. 3)then                            
         imchn = 4                                                      
         iabort = 1                                                     
         write(errbuf(1),50)nbname,bkv,nid                              
 50      format(1h0,5x,a8,1x,f5.1,1x,a1,1x,' TRANSIENT STABILIZER CAN', 
     1   ' ONLY BE USED WITH SS AND SP TYPE PSS CARDS')                 
         call prterr('E',1)                                             
      endif                                                             
      if (msupp .eq. 1)msupp = 4                                         
      if (msupp .eq. 2)msupp = 5                                         
      itsl = j                                                          
       read (work80(icard),100) tslt1(itsl),tslt2(itsl),tsldt3(itsl),    
     1   tslk(itsl),dwt1(itsl),dwt2(itsl),dwpt,tdlay(itsl),tsldt5(itsl)   
 100     format (bz,16x,3f5.4,f6.3,3f5.4,f5.1,f5.4)                           
       if (dwt1(itsl) .gt. 0.0)dwt1(itsl) = -dwt1(itsl)                  
       if (dwt2(itsl) .gt. 0.0)dwt2(itsl) = -dwt2(itsl)                  
       if (dwpt .gt. 0.0)dwpt = -dwpt                                    
       if (dwt1(itsl) .lt. dwt2(itsl))then                               
         write (errbuf(1),150) nbname,bkv,nid                             
150      format(1h0,5x,a8,1x,f5.1,1x,a1,1x,' TRANSIENT STABILIZER DWT2',
     1   ' MUST BE LESS THAN DWT1 ')                                    
         call prterr('E',1)                                             
         imchn = 4                                                      
         iabort = 1                                                     
      endif                                                             
      if (tslt2(itsl) .le. 0.0) then                                      
         write (errbuf(1),170) nbname,bkv,nid                             
170      format(1h0,5x,a8,1x,f5.1,1x,a1,1x,' TRANSIENT STABILIZER T2',  
     1   ' MUST BE GREATER THAN 0.0 ')                                  
         call prterr('E',1)                                             
         imchn = 4                                                      
         iabort = 1                                                     
      endif                                                             
C * * *                                                                 
C * * * CONVERT TIME CONSTANTS FROM SECONDS TO CYCLES                   
C * * *                                                                 
      tslt1(itsl) = tslt1(itsl)*frqbse                                  
      tslt2(itsl) = tslt2(itsl)*frqbse                                  
      tsldt3(itsl) = tsldt3(itsl)*frqbse                                
      tsldt5(itsl) = tsldt5(itsl)*frqbse                                
      tcon = 2./dt                                                      
      tsldt3(itsl) = tsldt3(itsl)*tcon                                  
      tsldt4(itsl) = tsldt3(itsl) + 1.0                                 
      tsldt5(itsl) = tsldt5(itsl)*tcon +1.                              
C * * *                                                                 
C * * *CONVERT DELTA FREQUENCY FROM HERTZ TO RADIANS PER CYCLE          
C * * *                                                                 
      const = 6.2831853/frqbse                                          
      dwt1(itsl) = dwt1(itsl)*const                                     
      dwt2(itsl) = dwt2(itsl)*const                                     
C * * *                                                                 
C * * * CONVERT HERTZ/SEC TO RADIANS/CYCLE SQUARED                      
C * * *                                                                 
      dwpt = dwpt*const/frqbse                                          
C * * *                                                                 
C * * * CALCULATE KT CONSTANT                                           
C * * *                                                                 
      tslk(itsl) = tslk(itsl)*6.2831853                                 
      tslkt(itsl) = dwpt/(dwt1(itsl)-dwt2(itsl))                        
      tslhb3(itsl) = 0.0                                                
      tslhb4(itsl) = 0.0                                                
      tslhb5(itsl) = 0.0                                                
      tbase(itsl) = 0.0                                                 
      itrig(itsl) = 0.0                                                 
      efun(itsl) = 0.0                                                  
      if (imchn .eq. 4) itsl = itsl-1                                     
      return                                                            
      end                                                               
