C    %W% %G%
        subroutine piequi (brnch,gk1,bk1,gk2,bk2,g12,b12,g21,b21)       
C * * *                                                                 
C * * * THIS SUBROUTINE FORMS A SINGLE EQUIVALENT PI FOR A GIVEN        
C * * * ENTRY IN THE BRDATA TABLE.  IT IS CALLED BY INPUT2              
C * * *                                                                 
      include 'tspinc/params.inc' 
        dimension brnch(18)                                             
        complex y(2,2),cx                                               
        equivalence (rk1,k1), (rk2,k2), (rltype,ltype), (rlist,list)    
      include 'tspinc/namec.inc' 
      include 'tspinc/pointr.inc' 
C                                                                       
C       THIS SUBROUTINE COMPUTES THE 2-PORT ADMITTANCE MATRIX Y         
C       FOR BRNCH (1)                                                   
C                                                                       
        rltype=brnch(1)                                                 
        rk1=brnch(2)                                                    
        rk2=brnch(12)                                                   
        rlist=brnch(17)                                                 
        go to (240,240,100,240,100,100,240,100)ltype                    
C                                                                       
C       COMPUTE TRANSFER IMPEDANCE                                      
C                                                                       
  100   r=brnch(5)                                                      
        x=brnch(6)                                                      
        z=r**2 + x**2                                                   
  120   go to (240,280,130,280,150,180,280,140) ltype                   
C                                                                       
C       " L " BRANCH                                                    
C                                                                       
  130   y(1,2)=-cmplx (r/z, -x/z)                                       
        y(2,1) = y(1,2)                                                 
        y(1,1)=cmplx (brnch(7),brnch(8))-y(1,2)                         
        y(2,2)=y(1,1)                                                   
         go to 260                                                      
C                                                                       
C       " E " BRANCH                                                    
C                                                                       
  140   y(1,2)= -cmplx (r/z, -x/z)                                      
        y(2,1)=y(1,2)                                                   
        y(1,1)=cmplx(brnch(7),brnch(8))-y(1,2)                          
        y(2,2)=cmplx(brnch(9),brnch(10))-y(2,1)                         
        go to 260                                                       
C                                                                       
C       " T " BRANCH                                                    
C                                                                       
  150   i1=ixnamn(k1)                                                   
        i2=ixnamn(k2)                                                   
        tk=brnch(9)/basekv(i1)                                          
        tm=brnch(10)/basekv(i2)                                         
        tkm=1.0/(tk*tm)                                                 
        y(1,1)=cmplx(0.5*tkm*brnch(7),-0.5*tkm*abs(brnch(8)))           
        y(2,2)=y(1,1)                                                   
        y(1,2)=-cmplx(r/(z*tk*tm),-x/(z*tk*tm))                         
        y(2,1)=y(1,2)                                                   
        y(1,1)=y(1,1)+cmplx (r/(z*tk*tk),-x/(z*tk*tk))                  
        y(2,2)=y(2,2)+cmplx (r/(z*tm*tm), -x/(z*tm*tm))                 
        go to 260                                                       
C                                                                       
C       " TP " BRANCH                                                   
C                                                                       
  180   tk=brnch(9)                                                     
        i1=ixnamn(k1)                                                   
        i2=ixnamn(k2)                                                   
        if(list.eq.0) tm=brnch(10)/basekv(i1)                           
        if(list.eq.1) tm=brnch(10)/basekv(i2)                           
        if(tm.eq.0.0)tm=1.0                                             
        angle=0.0174532925*tk                                           
        y(1,2)=cmplx(0.5*brnch(7),-0.5*abs(brnch(8)))                   
        y(1,1)=y(1,2)+cmplx(r/z,-x/z)                                   
        y(2,2)=y(1,2)+cmplx(r/z,-x/z)/cmplx(tm**2,0.0)                  
        y(1,2)=-cmplx(cos(angle),sin(angle))*cmplx(r/(z*tm),-x/(z*tm))  
        y(2,1)=-cmplx(cos(angle),-sin(angle))*cmplx(r/(z*tm),-x/(z*tm)) 
        if (list.eq.0) go to 230                                        
        cx=y(2,2)                                                       
        y(2,2)=y(1,1)                                                   
        y(1,1)=cx                                                       
  230   continue                                                        
        go to 260                                                       
C                                                                       
C       " L "  EQUIVALENT BRANCH                                        
C                                                                       
  240   gk1=brnch(4)+brnch(6)                                           
        bk1=brnch(5)+brnch(7)                                           
        g12=-brnch(6)                                                   
        b12=-brnch(7)                                                   
        g21=-brnch(8)                                                   
        b21=-brnch(9)                                                   
         gk2=brnch(10)+brnch(8)                                         
        bk2=brnch(11)+brnch(9)                                          
        go to 280                                                       
  260   continue                                                        
        gk1 = real (y(1,1)+y(1,2))                                      
        bk1 = aimag (y(1,1)+y(1,2))                                     
        gk2 = real (y(2,2)+y(2,1))                                      
        bk2 = aimag (y(2,2)+y(2,1))                                     
        g12 = -real (y(1,2))                                            
        b12 = -aimag (y(1,2))                                           
         g21 = -real (y(2,1))                                           
        b21 = -aimag (y(2,1))                                           
  280   return                                                          
        end                                                             
