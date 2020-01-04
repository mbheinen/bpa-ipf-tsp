C    %W% %G%
      subroutine gdifrd                                                 
C * * *                                                                 
C * * * THIS SUBROUTINE DECODES THE TYPE GDV OUTPUT DATA CARDS AND      
C * * * FORMS INITIAL TABLES FOR THE GENERATOR DIFFERENCE OUTPUT LOGIC. 
C * * * IS CALLED BY NOUT1.                                             
C * * *                                                                 
      include 'tspinc/params.inc' 
      include 'tspinc/gdif.inc' 
      include 'tspinc/prt.inc' 
*END  PRT                                                               
      include 'tspinc/reread.inc' 
*END  REREAD                                                            
      character*8 name1,name2                                           
      character*1 cde1,cde2,id1,id2                                     
      idifkt = idifkt + 1                                               
      if(idifkt .gt. 10) then                                           
         write(outbuf,50)                                               
  50     format('0 MORE THAN 10 GD CARDS HAVE BEEN ENTERED.  ONLY THE ',
     1          'FIRST TEN WILL BE USED.')                              
         call prtout(1)                                                 
         return                                                         
      endif                                                             
      read (buffer,200) name1,bkv1,id1                                    
 200  format (bz,3x,a8,f4.0,1x,a1)                                          
      kb1 = nambse(bkv1)                                                
      k1 = igensr(name1,kb1,id1)                                        
      if(k1 .eq. 0)then                                                 
         write(outbuf,300)name1,bkv1,id1                                
         call prtout(1)                                                 
 300     format('0 GENERATOR ',a8,1x,f5.1,1x,a1,' ON THE GD CARD',      
     1         ' CANNOT BE LOCATED.')                                   
         idifkt = idifkt-1                                              
         return                                                         
      endif                                                             
      read (buffer,400) name2,bkv2,id2                                    
 400  format (bz,63x,a8,f4.0,1x,a1)                                         
      kb2 = nambse(bkv2)                                                
      k2 = igensr(name2,kb2,id2)                                        
      if(k2 .eq. 0)then                                                 
         write(outbuf,500)name2,bkv2,id2                                
         call prtout(1)                                                 
 500     format('0 GENERATOR ',a8,1x,f5.1,1x,a1,' ON THE GD CARD',      
     1             ' CANNOT BE LOCATED.')                               
         idifkt = idifkt-1                                              
         return                                                         
      endif                                                             
      read (buffer,600) ioptan,ioptsp,ioptpw                              
 600  format (bz,19x,i1,2x,i1,14x,i1)                                       
      igdifc(1,idifkt) = 0                                              
      igdifc(2,idifkt) = 0                                              
      igdifc(3,idifkt) = 0                                              
      if(ioptan .ne. 0)igdifc(1,idifkt) = 1                             
      if(ioptsp .ne. 0)igdifc(2,idifkt) = 1                             
      if(ioptpw .ne. 0)igdifc(3,idifkt) = 1                             
      k1gdif(idifkt) = k1                                               
      k2gdif(idifkt) = k2                                               
      do 700 itrr = 1,600                                               
      gdifan(itrr,idifkt) = 0.0                                         
      gdifsp(itrr,idifkt) = 0.0                                         
  700 gdifpw(itrr,idifkt) = 0.0                                         
      return                                                            
      end                                                               
