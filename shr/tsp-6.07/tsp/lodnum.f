C    %W% %G%
      subroutine lodnum                                                 
C * * * THIS SUBROUTINE CONVERTS THE DATA TABLES USED FOR               
C * * * LOAD REPRESENTATION TO THE INTERNAL ORDER.  IT IS               
C * * * CALLED BY INITL2                                                
      include 'tspinc/params.inc' 
      include 'tspinc/pointr.inc' 
      include 'tspinc/ldidxn.inc' 
      include 'tspinc/indn2x.inc' 
      include 'tspinc/ldndxp.inc' 
      include 'tspinc/param.inc' 
      include 'tspinc/busnum.inc' 
      include 'tspinc/comn34.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/brnch.inc' 
      integer ldidx
      dimension ldidx(6,MAXBUS)                                         
c dlc      EQUIVALENCE (LDIDX(1,1), BRNCH(1,1))                              
C * * *                                                                 
C * * * CONVERT LDIDXF TABLE TO NEW ORDER                               
C * * *                                                                 
      do 1815 itrr = 1,nmx                                              
      do 1810 itrr1 = 1,6                                               
 1810 ldidx(itrr1,itrr) = 0                                             
 1815 continue                                                          
C * * *                                                                 
C * * * LOAD LDIDXN TABLE INTO TEMPORARY LDIDX TABLE                    
C * * *                                                                 
      do 1817 itrr = 1,ntotd                                            
      do 1816 itrr1 = 1,6                                               
 1816 ldidx(itrr1,itrr) = ldidxn(itrr1,itrr)                            
 1817 continue                                                          
      do 1820 indn = 1,nmx                                              
      indo = indn2x(indn)                                               
      do 1820 i=1,6                                                     
 1820 ldidxn(i,indn)=ldidx(i,indo)                                      
      if (keybrd(7) .ne. 0) then                                        
         call forbtm                                                    
         call fortop                                                    
         write (outbuf, 1840)                                           
         call prtout (1)                                                
         do 1842 jjj = 1,nmx,10                                         
         kkk = min0 (jjj+9,nmx)                                         
         write (outbuf, 1841) (indx2n(i), i=jjj,kkk)                    
         call prtout (1)                                                
 1842    continue                                                       
 1840    format ('0INDX2N')                                             
 1841    format (10i10)                                                 
      endif                                                             
C * * *                                                                 
C * * *  CONVERT LOAD REPRESENTATION LDNDXP                             
C * * *  SUCH THAT LOAD REPRESENTATION BUSES ARE ENCOUNTERED            
C * * *  IN THE INTERNAL SWING ORDER                                    
C * * *                                                                 
      lrepp = lrep                                                      
      k = 0                                                             
      do 4000 i=1,lrep                                                  
      ii=ldndxp(4,i)                                                    
      ii=indx2n(ii)                                                     
C * * *                                                                 
C * * * HERE WE HAVE TO CHECK FOR PASSIVE DC BUSSES AND BYPASS          
C * * * RENUMBERING PROCEDURE FOR THOSE.  IF II=0, IT IS A PASSIVE      
C * * * DC BUS                                                          
C * * *                                                                 
      k = k + 1                                                         
      if(ii.ne.0) go to 3950                                            
      lrepp = lrepp - 1                                                 
      k = k - 1                                                         
      go to 4000                                                        
C * * *                                                                 
C * * * SORT THE LDNDXP TABLE SO THE BUS NUMBERS, LDNDXP(4,I) ARE       
C * * * IN LOW TO HIGH ORDER                                            
C * * *                                                                 
3950  ldndxp(4, k) = ii                                                 
      ldi1=ldndxp(1,k)                                                  
      ldi2=ldndxp(2,k)                                                  
      ldi3=ldndxp(3,k)                                                  
      ldi4=ldndxp(4,k)                                                  
      if(k.eq.1) go to 4000                                             
      do 3960 j = 2, k                                                  
      j1 = k - j + 1                                                    
      ibno=ldndxp(4,j1)                                                 
      if (ibno.le.ii) go to 3980                                        
      ldndxp(1,j1+1)=ldndxp(1,j1)                                       
      ldndxp(2,j1+1)=ldndxp(2,j1)                                       
      ldndxp(3,j1+1)=ldndxp(3,j1)                                       
      ldndxp(4,j1+1)=ldndxp(4,j1)                                       
3960  continue                                                          
      j1=j1-1                                                           
3980  ldndxp(1,j1+1)=ldi1                                               
      ldndxp(2,j1+1)=ldi2                                               
      ldndxp(3,j1+1)=ldi3                                               
      ldndxp(4,j1+1)=ldi4                                               
 4000 continue                                                          
      lrep = lrepp                                                      
      return                                                            
      end                                                               
