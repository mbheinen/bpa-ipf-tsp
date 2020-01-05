C    %W% %G%
      subroutine swap31 (m,n)                                           

      include 'tspinc/params.inc' 
      include 'tspinc/brakn.inc' 
      include 'tspinc/blkcom1.inc' 
      include 'tspinc/cntrl2.inc' 
      include 'tspinc/pointr.inc' 
      include 'tspinc/in1n.inc' 
      include 'tspinc/in1c.inc' 
      include 'tspinc/lnet.inc' 
      include 'tspinc/ecstbb.inc' 
      include 'tspinc/sort.inc' 

      common /lqiks/ lqiks                                            
      character*10 c10                                                
      common /areac2/ areac2(60)                                      
      character*10 areac2                                             
      common /ldnew/ basnew(MAXKV)
      character*8 c                                                     

      go to (100,120,140,160,180,200,220,240,260,280,300,320,340,360,380
     1      ,400,420,440,460,480,600),lqiks                             

  100 bx=basekv(n)                                                      
      basekv(n)=basekv(m)                                               
      basekv(m)=bx                                                      
      return                                                            

  120 continue                                                        
      do i = 1,2                                                  
        c = mach1c(i,m)                                              
        mach1c(i,m) = mach1c(i,n)                                    
        mach1c(i,n) = c                                              
      enddo
      do i = 1,4                                                  
        mx = mach1n(i,m)                                             
        mach1n(i,m) = mach1n(i,n)                                    
        mach1n(i,n) = mx                                             
      enddo
      do i = 1,2                                                  
        c = mach2c(i,m)                                              
        mach2c(i,m) = mach2c(i,n)                                    
        mach2c(i,n) = c                                              
      enddo
      mx = mach2n(m)                                                  
      mach2n(m) = mach2n(n)                                           
      mach2n(n) = mx                                                  
      return                                                            

  140 mx=lrep1(n)                                                       
      lrep1(n)=lrep1(m)                                                 
      lrep1(m)=mx                                                       
      c = lrep2c(m)                                                   
      lrep2c(m) = lrep2c(n)                                           
      lrep2c(n) = c                                                   
      mx = lrep2n(m)                                                  
      lrep2n(m) = lrep2n(n)                                           
      lrep2n(n) = mx                                                  
      return                                                            

  160 c = lnetc(m)                                                    
      lnetc(m) = lnetc(n)                                             
      lnetc(n) = c                                                    
                                                                       
      mx = lnetn(m)                                                   
      lnetn(m) = lnetn(n)                                             
      lnetn(n) = mx                                                   
      c=lnetcc(m)                                                     
      lnetcc(m)=lnetcc(n)                                             
      lnetcc(n)=c                                                     
      return                                                            
c
C     Underfrequency load shedding tables                             
C                                                                  
  180 c = lshd1c(m)                                                   
      lshd1c(m) = lshd1c(n)                                           
      lshd1c(n) = c                                                   
                                                                       
      mx = lshd1n(m)                                                  
      lshd1n(m) = lshd1n(n)                                           
      lshd1n(n) = mx                                                  
                                                                       
      c = lshd2c(m)                                                   
      lshd2c(m) = lshd2c(n)                                           
      lshd2c(n) = c                                                   
                                                                       
      mx=lshed2(m)                                                    
      lshed2(m)=lshed2(n)                                             
      lshed2(n)=mx                                                    
      return                                                            

  200 c = lsrc1c(m)                                                   
      lsrc1c(m) = lsrc1c(n)                                           
      lsrc1c(n) = c                                                   
                                                                       
      mx = lsrc1n(m)                                                  
      lsrc1n(m) = lsrc1n(n)                                           
      lsrc1n(n) = mx                                                  
                                                                       
      c = lsrc2c(m)                                                   
      lsrc2c(m) = lsrc2c(n)                                           
      lsrc2c(n) = c                                                   
                                                                       
      mx = lsrc2n(m)                                                  
      lsrc2n(m) = lsrc2n(n)                                           
      lsrc2n(n) = mx                                                  
                                                                       
      do i = 1,2                                                  
        c = lsrc3c(i,m)                                              
        lsrc3c(i,m) = lsrc3c(i,n)                                    
        lsrc3c(i,n) = c                                              
                                                                       
        mx = lsrc3n(i,m)                                             
        lsrc3n(i,m) = lsrc3n(i,n)                                    
        lsrc3n(i,n) = mx                                             
                                                                       
      enddo
      return                                                            

  220 c = lrly1c(m)                                                   
      lrly1c(m) = lrly1c(n)                                           
      lrly1c(n) = c                                                   
                                                                       
      c = lrly2c(m)                                                   
      lrly2c(m) = lrly2c(n)                                           
      lrly2c(n) = c                                                   
                                                                       
      do i = 1,2                                                  
        c = lrly3c(i,m)                                              
        lrly3c(i,m) = lrly3c(i,n)                                    
        lrly3c(i,n) = c                                              
      enddo
                                                                       
      mx = lrly1n(m)                                                  
      lrly1n(m) = lrly1n(n)                                           
      lrly1n(n) = mx                                                  
                                                                       
      mx = lrly2n(m)                                                  
      lrly2n(m) = lrly2n(n)                                           
      lrly2n(n) = mx                                                  
                                                                       
      do i = 1,2                                                  
        mx = lrly3n(i,m)                                             
        lrly3n(i,m) = lrly3n(i,n)                                    
        lrly3n(i,n) = mx                                             
      enddo
                                                                       
      return                                                            

  240 c = lrod1c(m)                                                   
      lrod1c(m) = lrod1c(n)                                           
      lrod1c(n) = c                                                   
                                                                       
      c = lrod2c(m)                                                   
      lrod2c(m) = lrod2c(n)                                           
      lrod2c(n) = c                                                   
                                                                       
      do i = 1,2                                                  
        c = lrod3c(i,m)                                              
        lrod3c(i,m) = lrod3c(i,n)                                    
        lrod3c(i,n) = c                                              
      enddo
                                                                       
      c = lrod4c(m)                                                   
      lrod4c(m) = lrod4c(n)                                           
      lrod4c(n) = c                                                   
                                                                       
      c = lrod5c(m)                                                   
      lrod5c(m) = lrod5c(n)                                           
      lrod5c(n) = c                                                   
                                                                       
      do i = 1,2                                                  
        c = lrod6c(i,m)                                              
        lrod6c(i,m) = lrod6c(i,n)                                    
        lrod6c(i,n) = c                                              
      enddo
                                                                       
      mx = lrod1n(m)                                                  
      lrod1n(m) = lrod1n(n)                                           
      lrod1n(n) = mx                                                  
                                                                       
      mx = lrod2n(m)                                                  
      lrod2n(m) = lrod2n(n)                                           
      lrod2n(n) = mx                                                  
                                                                       
      do i = 1,2                                                  
        mx = lrod3n(i,m)                                             
        lrod3n(i,m) = lrod3n(i,n)                                    
        lrod3n(i,n) = mx                                             
      enddo
                                                                       
      mx = lrod4n(m)                                                  
      lrod4n(m) = lrod4n(n)                                           
      lrod4n(n) = mx                                                  
                                                                       
      mx = lrod5n(m)                                                  
      lrod5n(m) = lrod5n(n)                                           
      lrod5n(n) = mx                                                  
                                                                       
      mx = lrod6n(m)                                                  
      lrod6n(m) = lrod6n(n)                                           
      lrod6n(n) = mx                                                  
                                                                       
      return                                                            

  260 c = tempc(m)                                                    
      tempc(m) = tempc(n)                                             
      tempc(n) = c                                                    
                                                                       
      c = tempc(m+ifcd)                                               
      tempc(m+ifcd) = tempc(n+ifcd)                                   
      tempc(n+ifcd) = c                                               
                                                                       
      c = tempc(m+2*ifcd)                                             
      tempc(m+2*ifcd) = tempc(n+2*ifcd)                               
      tempc(n+2*ifcd) = c                                             
                                                                       
      c = tempc(m+3*ifcd)                                             
      tempc(m+3*ifcd) = tempc(n+3*ifcd)                               
      tempc(n+3*ifcd) = c                                             
                                                                       
      mx = tempn(m)                                                   
      tempn(m) = tempn(n)                                             
      tempn(n) = mx                                                   
                                                                       
      mx = tempn(m+ifcd)                                              
      tempn(m+ifcd) = tempn(n+ifcd)                                   
      tempn(n+ifcd) = mx                                              
                                                                       
      mx = tempn(m+2*ifcd)                                            
      tempn(m+2*ifcd) = tempn(n+2*ifcd)                               
      tempn(n+2*ifcd) = mx                                            
                                                                      
      mx = tempn(m+3*ifcd)                                            
      tempn(m+3*ifcd) = tempn(n+3*ifcd)                               
      tempn(n+3*ifcd) = mx                                            
                                                                       
      return                                                            

  280 do i = 1,2                                                  
        c = dzonec(i,m)                                              
        dzonec(i,m) = dzonec(i,n)                                    
        dzonec(i,n) = c                                              
      enddo
                                                                       
      mx = idznen(m)                                                  
      idznen(m) = idznen(n)                                           
      idznen(n) = mx                                                  
                                                                       
      return                                                            

  300 c = arac1c(m)                                                   
      arac1c(m) = arac1c(n)                                           
      arac1c(n) = c                                                   
                                                                       
      do i = 1,14                                                 
        mx = iaracn(i,m)                                             
        iaracn(i,m) = iaracn(i,n)                                    
        iaracn(i,n) = mx                                             
      enddo
                                                                       
      c10 = areac2(m)                                                 
      areac2(m) = areac2(n)                                           
      areac2(n) = c10                                                 
                                                                       
      ic10 = irea1n(m)                                                
      irea1n(m) = irea1n(n)                                           
      irea1n(n) = ic10                                                
                                                                       
      return                                                            

  320 c = idcbuc(m)                                                   
      idcbuc(m) = idcbuc(n)                                           
      idcbuc(n) = c                                                   
                                                                       
      mx = idcbun(m)                                                  
      idcbun(m) = idcbun(n)                                           
      idcbun(n) = mx                                                  
                                                                       
      do i = 1,2                                                  
        mx = idcinn(i,m)                                             
        idcinn(i,m) = idcinn(i,n)                                    
        idcinn(i,n) = mx                                             
      enddo
                                                                       
      return                                                            

  340 c = lnetxc(m)                                                   
      lnetxc(m) = lnetxc(n)                                           
      lnetxc(n) = c                                                   
                                                                       
      mx = lnetxn(m)                                                  
      lnetxn(m) = lnetxn(n)                                           
      lnetxn(n) = mx                                                  
                                                                       
      return                                                            

  360 c = msortc(m)                                                   
      msortc(m) = msortc(n)                                           
      msortc(n) = c                                                   
                                                                       
      c = msortc(m+MAXLS)                                             
      msortc(m+MAXLS) = msortc(n+MAXLS)                               
      msortc(n+MAXLS) = c                                             
                                                                       
      c = msortc(m+2*MAXLS)                                           
      msortc(m+2*MAXLS) = msortc(n+2*MAXLS)                           
      msortc(n+2*MAXLS) = c                                           
                                                                       
      c = msortc(m+3*MAXLS)                                           
      msortc(m+3*MAXLS) = msortc(n+3*MAXLS)                           
      msortc(n+3*MAXLS) = c                                           
                                                                       
      mx = msortn(m)                                                  
      msortn(m) = msortn(n)                                           
      msortn(n) = mx                                                  
                                                                       
      mx = msortn(m+MAXLS)                                            
      msortn(m+MAXLS) = msortn(n+MAXLS)                               
      msortn(n+MAXLS) = mx                                            
                                                                       
      mx = msortn(m+2*MAXLS)                                          
      msortn(m+2*MAXLS) = msortn(n+2*MAXLS)                           
      msortn(n+2*MAXLS) = mx                                          
                                                                       
      mx = msortn(m+3*MAXLS)                                          
      msortn(m+3*MAXLS) = msortn(n+3*MAXLS)                           
      msortn(n+3*MAXLS) = mx                                          
                                                                       
      return                                                            

  380 bx=basnew(n)                                                      
      basnew(n)=basnew(m)                                               
      basnew(m)=bx                                                      
      return                                                            

  400 c = irbidc(m)                                                   
      irbidc(m) = irbidc(n)                                           
      irbidc(n) = c                                                   
                                                                       
      do i = 1,3                                                  
        mx = irbidn(i,m)                                             
        irbidn(i,m) = irbidn(i,n)                                    
        irbidn(i,n) = mx                                             
      enddo
                                                                       
      return                                                            

  420 c = ircidc(m)                                                   
      ircidc(m) = ircidc(n)                                           
      ircidc(n) = c                                                   
                                                                       
      do i = 1,3                                                  
        mx = ircidn(i,m)                                             
        ircidn(i,m) = ircidn(i,n)                                    
        ircidn(i,n) = mx                                             
      enddo
                                                                       
      return                                                            

  440 ka = kfreq(n)                                                     
      kfreq(n)=kfreq(m)                                                 
      kfreq(m)=ka                                                       
      c = ufreqc(m)                                                   
      ufreqc(m) = ufreqc(n)                                           
      ufreqc(n) = c                                                   
                                                                       
      mx = lufrqn(m)                                                  
      lufrqn(m) = lufrqn(n)                                           
      lufrqn(n) = mx                                                  
                                                                       
      return                                                            

  460 ka=kgenf(n)                                                       
      kgenf(n)=kgenf(m)                                                 
      kgenf(m)=ka                                                       
      do i = 1,2                                                  
        c = ugenfc(i,m)                                              
        ugenfc(i,m) = ugenfc(i,n)                                    
        ugenfc(i,n) = c                                              
      enddo
                                                                       
      mx = iugenn(m)                                                  
      iugenn(m) = iugenn(n)                                           
      iugenn(n) = mx                                                  
                                                                       
      return                                                            

  480 ra = reappx(n,1)                                                  
      reappx(n,1) = reappx(m,1)                                         
      reappx(m,1) = ra                                                  
      ra = reappx(n,2)                                                  
      reappx(n,2) = reappx(m,2)                                         
      reappx(m,2) = ra                                                  
      ra = reappx(n,3)                                                  
      reappx(n,3) = reappx(m,3)                                         
      reappx(m,3) = ra                                                  
      return                                                            

  600 c = izn1c(m)                                                    
      izn1c(m) = izn1c(n)                                             
      izn1c(n) = c                                                    
                                                                       
      mx = izn1n(m)                                                   
      izn1n(m) = izn1n(n)                                             
      izn1n(n) = mx                                                   
                                                                       
      c = izn2c(m)                                                    
      izn2c(m) = izn2c(n)                                             
      izn2c(n) = c                                                    
                                                                       
      mx = izn2n(m)                                                   
      izn2n(m) = izn2n(n)                                             
      izn2n(n) = mx                                                   
                                                                       
      do i = 1,2                                                  
        c = izn3c(i,m)                                               
        izn3c(i,m) = izn3c(i,n)                                      
        izn3c(i,n) = c                                               
                                                                       
        mx = izn3n(i,m)                                              
        izn3n(i,m) = izn3n(i,n)                                      
        izn3n(i,n) = mx                                              
                                                                       
      enddo
      return                                                            
      end                                                               
