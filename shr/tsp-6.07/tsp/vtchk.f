C    %W% %G%
      subroutine vtchk                                                  
C                                                                       
C     THIS SUBROUTINE IS CALLED ONLY AFTER THE STUDY HAS SOLVED THE   
C     FIRST 30 CYCLES.  IT CHECKS THE VOLTAGE MAGNITUDE AT EACH BUS   
C     IN THE NETWORK AND IF ANY MAGNITUDE IS LESS THAN 0.4 PU, IT     
C     SETS A FLAG (LOVLT = 1).  THE PROGRAM WILL THEN STOP AFTER AN   
C     ADDITIONAL 30 CYCLES HAS BEEN SOLVED.                           
C                                                                       
      include 'tspinc/params.inc' 
      include 'tspinc/vtchkc.inc' 
      include 'tspinc/busvolt.inc' 
      include 'tspinc/param.inc' 
      include 'tspinc/pointr.inc' 
      include 'tspinc/bname.inc' 
      include 'tspinc/buskv.inc' 
      include 'tspinc/prt.inc' 
      character*8 name                                                  

      if(lovlt .eq. 1) return                                           
C                                                                       
C     CALCULATE VOLTAGE MAGNITUDE AT EACH BUS                         
C                                                                       
      do 1000 ivt = 1,nmx                                               
      esq = sqrt(eyr(ivt)*eyr(ivt) + eyi(ivt)*eyi(ivt))                 
      name = bname(ivt)                                                 
C                                                                       
C     MAKE SURE THIS BUS IS NOT THE INTERMEDIATE FAULTED BUS          
C                                                                       
      if (name .eq. 'INTRMDTE') go to 1000                                
C                                                                       
C     IF VOLTAGE IS LOW SET FLAG AND CHANGE END TIME                  
C                                                                       
      if (esq .lt. 0.4 .and. esq .gt. .0001) then                         
        lovlt = 1                                                       
        endt = to + 30.                                                 
        bkv  = buskv(ivt)                                               
        write(outbuf,500) name,bkv,to                                   
 500    format('0', 5x, 'LOW VOLTAGE CONDITION AT ', a8, 2x, f5.1,           
     1  '  AT  ', f7.2, '  CYCLES.  ')                                   
        call prtout(1)                                                  
      endif                                                             
 1000 continue                                                          
      return                                                            
      end                                                               
