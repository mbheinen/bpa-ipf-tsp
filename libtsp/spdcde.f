C    %W% %G%
      subroutine spdcde                                                 
C * * *                                                                 
C * * * THIS SUBROUTINE READS THE SP CARD FOR SPARE POINTS OUTPUT.      
C * * * IT IS CALLED BY NOUT1.                                          
C * * *                                                                 
      include 'tspinc/params.inc' 
      include 'tspinc/spare1.inc' 
      include 'tspinc/spare2.inc' 
      include 'tspinc/reread.inc' 
      character*10 axis1,axis2                                          
C     -     begin     begin     begin     begin     begin     begin 
      ispknt = ispknt + 1                                               
      read (buffer,100) icde,axis1,axis2,ymax,ymin                        
 100  format (bz,3x,i2,1x,2a10,1x,f5.2,1x,f5.2)                             
      spaxis(1,ispknt) = axis1                                          
      spaxis(2,ispknt) = axis2                                          
      ispcde(ispknt) = icde                                             
      spmax(ispknt) = ymax                                              
      spmin(ispknt) = ymin                                              
      return                                                            
      end                                                               
