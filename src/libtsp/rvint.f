C    %W% %G%
      subroutine rvint                                                  
C * * *                                                                 
C * * * THIS SUBROUTINE INITIALIZES THE DATA TABLES FOR THE             
C * * * RV VOLTAGE DIFFERENCE LOAD DROPPING RELAY. IT IS CALLED         
C * * * BY INITL2.                                                      
C * * *                                                                 
      include 'tspinc/params.inc' 
      include 'tspinc/busvolt.inc' 
      include 'tspinc/busnum.inc' 
      include 'tspinc/rvcom.inc' 
      include 'tspinc/relays.inc' 
      include 'tspinc/param.inc' 
      do 1000 i = 1,kntrv                                               
      ibusno = irvbno(i)                                                
      irvbno(i) = indx2n(ibusno)                                        
      ibusno = irvbno(i)                                                
      vmag = sqrt(eyr(ibusno)*eyr(ibusno) + eyi(ibusno)*eyi(ibusno))    
      rvstan(i) = vmag                                                  
      rvpdp1(i) = rvpdp1(i)/bmva                                        
      rvqdp1(i) = rvqdp1(i)/bmva                                        
 1000 continue                                                          
      return                                                            
      end                                                               
