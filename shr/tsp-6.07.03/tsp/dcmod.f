C    %W% %G%
      subroutine dcmod(i,imdp,iter)                                     
C * * *                                                                 
C * * * THIS SUBROUTINE SOLVES THE DIFFERENTIAL EQUATIONS REPRESENTING  
C * * * THE LOW LEVEL, HIGH LEVEL, AND DUAL FREQUENCY DC MODULATION     
C * * * MODELS.                                                         
      include 'tspinc/dcmodd.inc' 
      include 'tspinc/param.inc' 
      include 'tspinc/params.inc' 
      include 'tspinc/buskv.inc' 
      include 'tspinc/busvolt.inc' 
      include 'tspinc/pointr.inc' 
      include 'tspinc/vrgov.inc' 
      include 'tspinc/ectba.inc' 
      include 'tspinc/ecsind.inc' 
C * * *                                                                 
C * * * IMDP IS A CODE FOR THE TYPE OF MODULATION BEING SOLVED          
C * * * IMDP = 1 IS LOW LEVEL WITH BRANCH POWER INPUT                   
C * * * IMDP = 2 IS LOW LEVEL WITH BRANCH CURRENT INPUT                 
C * * * IMDP = 3 IS HIGH LEVEL WITH BRANCH POWER INPUT                  
C * * * IMDP = 4 IS HIGH LEVEL WITH BRANCH CURRENT INPUT                
C * * * IMDP = 6 IS DUAL FREQUENCY MODULATION                           
C * * *                                                                 
      if (iter.ne.1 .or. lppwr .ne. 0) go to 50                         
      if (idsw.eq.7) go to 30                                           
      xoomod(i) = x1nmod(i)                                             
      if(imdp .eq. 6) then                                              
         x1omod(i) = x1nmod(i)                                          
         y1omod(i) = y1omod(i)                                          
      endif                                                             
C                                                                       
C     UPDATE PAST VALUES OF VOLTAGE FOR W CALCULATION                   
C                                                                       
  30  if(idsw .eq. 3 .or. idsw .eq. 5) return                           
      ibf =idcbs1(i)                                                    
      eo = eyr(ibf)                                                     
      fo = eyi(ibf)                                                     
      dceo1(i) = eo                                                     
      dcfo1(i) = fo                                                     
      if(imdp .ne. 6) return                                            
      ibf = jdcbs1(i)                                                   
      eo = eyr(ibf)                                                     
      fo = eyi(ibf)                                                     
      dceo2(i) = eo                                                     
      dcfo2(i) = fo                                                     
      return                                                            
C * * *                                                                 
C * * *   PRECESS STATE VARIABLES IF THIS IS THE FIRST ITERATION        
C * * *   OF A NEW TIME STEP                                            
C * * *                                                                 
 50   if(idsw .eq. 7 .and. imdp .eq. 6) return                          
      if(lppwr .ne. 1) go to 100                                        
      x0omod(i) = x0nmod(i)                                             
      x1omod(i) = x1nmod(i)                                             
      x2omod(i) = x2nmod(i)                                             
      x3omod(i) = x3nmod(i)                                             
      x5omod(i) = x5nmod(i)                                             
      x6omod(i) = x6nmod(i)                                             
      x7omod(i) = x7nmod(i)                                             
      x8omod(i) = x8nmod(i)                                             
      if(idsw .eq. 3 .and. imdp .ne. 6)x1omod(i) = xoomod(i)            
      if(idsw .eq. 5 .and. imdp .ne. 6)x1omod(i) = xoomod(i)            
      if(imdp .eq. 6) then                                              
         y0omod(i) = y0nmod(i)                                          
         y1omod(i) = y1nmod(i)                                          
         y2omod(i) = y2nmod(i)                                          
         y3omod(i) = y3nmod(i)                                          
         y5omod(i) = y5nmod(i)                                          
         y6omod(i) = y6nmod(i)                                          
         y7omod(i) = y7nmod(i)                                          
         y8omod(i) = y8nmod(i)                                          
      endif                                                             
C * * *                                                                 
C * * * GET BRANCH P0WER OR CURRENT FOR HIGH                            
C * * * OR LOW LEVEL MODULATION INPUT                                   
C * * *                                                                 
 100  if(imdp .eq. 6) go to 200                                         
      ibs = idcbs1(i)                                                   
      jbs = jdcbs1(i)                                                   
      edc1 = eyr(ibs)                                                   
      fdc1 = eyi(ibs)                                                   
      edc2 =eyr(jbs)                                                    
      fdc2 = eyi(jbs)                                                   
      gij = dcgij(i)                                                    
      bij = dcbij(i)                                                    
      de=edc1-edc2                                                      
      df=fdc1-fdc2                                                      
      cr=de*gij-df*bij                                                  
      ci=de*bij+df*gij                                                  
      if(imdp .eq. 1 .or. imdp .eq. 3)then                              
C * * *                                                                 
C * * * BRANCH POWER INPUT                                              
C * * *                                                                 
         x1nmod(i) = bmva*(edc1*cr+fdc1*ci)                             
      else                                                              
C * * *                                                                 
C * * * BRANCH CURRENT INPUT                                            
C * * *                                                                 
         bkv = buskv(ibs)                                               
         x1nmod(i) = sqrt(cr*cr +ci*ci)*(bmva/bkv)*(1000./1.73203)      
      endif                                                             
      xoomod(i) = x1nmod(i)                                             
      go to 300                                                         
C * * *                                                                 
C * * *  SOLVE FOR W -- DUAL FREQUENCY INPUT                            
C * * *                                                                 
  200 wnow = x1omod(i)                                                  
      ibf = idcbs1(i)                                                   
      enew = eyr(ibf)                                                   
      fnew = eyi(ibf)                                                   
      efsq=enew*enew+fnew*fnew                                          
      if (efsq .eq. 0.0) then                                           
         wnew=wnow                                                      
      else                                                              
         wnew=(fnew*dceo1(i) - enew*dcfo1(i))/(efsq*edt)                
      endif                                                             
      x1nmod(i) = wnew                                                  
C * * *                                                                 
C * * * SECOND BLOCK OF EQUATIONS                                       
C * * *                                                                 
      ibf = jdcbs1(i)                                                   
      wnow = y1omod(i)                                                  
      enew = eyr(ibf)                                                   
      fnew = eyi(ibf)                                                   
      efsq = enew*enew+fnew*fnew                                        
      if(efsq .eq. 0.0) then                                            
         wnew = wnow                                                    
      else                                                              
         wnew = (fnew*dceo2(i) - enew*dcfo2(i))/(efsq*edt)              
      endif                                                             
      y1nmod(i) = wnew                                                  
C * * *                                                                 
C * * * DO NOT SOLVE DIFFERENTIAL EQUATIONS IF THIS IS A DISCONTINUITY  
C * * *                                                                 
 300  if(idsw .eq. 7) return                                            
C * * *                                                                 
C * * * SET UP TIME FACTORS                                             
C * * *                                                                 
      todt=2.0/edt                                                      
      twotp1=todt*camod1(i)                                             
      twotp2=todt*tdmod1(i)                                             
      tpp2=twotp2+1.0                                                   
      tpm2=twotp2-1.0                                                   
      twotp3=todt*emod1(i)                                              
      tpp3=twotp3+1.0                                                   
      tpm3=twotp3-1.0                                                   
C * * *                                                                 
C   PROCESSING TD BLOCK                                                 
C * * *                                                                 
      x0nmod(i) = (x0omod(i)*tpm2+x1nmod(i)+x1omod(i))/tpp2             
      x2nmod(i) = (1./tdmod1(i))*(x1nmod(i) - x0nmod(i))                
C * * *                                                                 
C    PROCESSING TF BLOCK                                                
C * * *                                                                 
      twotf = todt * tfmod1(i)                                          
      tfp = twotf + 1.                                                  
      tfm = tfp - 2.                                                    
      x3nmod(i) = (x3omod(i) * tfm + x2nmod(i) + x2omod(i) )/tfp        
C * * *                                                                 
C   PROCESSING E BLOCK                                                  
C * * *                                                                 
      tdot=todt                                                         
      x5nmod(i) = ( x5omod(i) * tpm3 + x3nmod(i) + x3omod(i) )/ tpp3    
      x6nmod(i) = x3nmod(i) -x5nmod(i)                                  
C * * *                                                                 
C   PROCESSING NOTCH FILTER                                             
C * * *                                                                 
      twoa=camod1(i)/tdot                                               
      tdsqiv=1./(tdot*tdot)                                             
      twob=cbmod1(i)*tdsqiv                                             
      twoc=ccmod1(i)/tdot                                               
      twod=cdmod1(i)*tdsqiv                                             
      cddt1=1.+twoc+twod                                                
      abdt1=1.+twoa+twob                                                
      bold1=x8omod(i)*(1.-twoc-twod)-x6omod(i)*(1.-twoa-twob)           
     1      +edt*x7omod(i)                                              
      x8nmod(i) = (x6nmod(i)*abdt1+bold1)/cddt1                         
      x7nmod(i) = x7omod(i)+(cbmod1(i)*(x6omod(i)+x6nmod(i))            
     1            -(x8omod(i)+ x8nmod(i))*cdmod1(i))/tdot               
      if(imdp .eq. 6) go to 400                                         
      signal=amax1(pnmod1(i),amin1(pxmod1(i),x8nmod(i)*ckmod1(i)))      
      if(imdp .eq. 1) siglom(i) = signal                                
      if(imdp .eq. 2) siglom(i) = signal                                
      if(imdp .eq. 3) sighim(i) = signal                                
      if(imdp .eq. 4) sighim(i) = signal                                
      return                                                            
C * * *                                                                 
C * * * SECOND SET OF EQUATIONS FOR DUAL FREQUENCY MODULATION           
C * * *                                                                 
C * * *                                                                 
C   PROCESSING TD BLOCK                                                 
C * * *                                                                 
  400 twotd = todt * tdmod2(i)                                          
      tdp = twotd + 1.0                                                 
      tdm = twotd - 1.0                                                 
      y0nmod(i) = (y0omod(i)*tdm+y1nmod(i)+y1omod(i))/tdp               
      y2nmod(i) = (y1nmod(i) - y0nmod(i))/tdmod2(i)                     
C * * *                                                                 
C                                                                       
C    PROCESSING TF BLOCK                                                
C * * *                                                                 
C                                                                       
      twotf = todt * tfmod2(i)                                          
      tfp = twotf + 1.0                                                 
      tfm = twotf - 1.0                                                 
      y3nmod(i) = (y3omod(i)*tfm+y2nmod(i)+y2omod(i))/tfp               
C * * *                                                                 
C                                                                       
C   PROCESSING E BLOCK                                                  
C * * *                                                                 
C                                                                       
      twot3 = todt * emod2(i)                                           
      tpp3 = twot3 + 1.0                                                
      tpm3 = twot3 - 1.0                                                
      y5nmod(i) = (y5omod(i)*tpm3+y3nmod(i)+y3omod(i))/tpp3             
      y6nmod(i) = y3nmod(i) - y5nmod(i)                                 
C * * *                                                                 
C * * *  PROCESSING NOTCH FILTER                                        
C * * *                                                                 
      twoa = camod2(i)/todt                                             
      twoc = ccmod2(i)/todt                                             
      tdsqiv = 1./(todt * todt )                                        
      twob = cbmod2(i)*tdsqiv                                           
      twod = cdmod2(i)*tdsqiv                                           
      cddt2 = 1.+twoc+twod                                              
      abdt2 = 1.+twoa+twob                                              
      bold2 = y8omod(i)*(1.-twoc-twod) - y6omod(i)*(1.-twoa-twob)       
     1        +edt*y7omod(i)                                            
      y8nmod(i) = (y6nmod(i)*abdt2+bold2)/cddt2                         
      y7nmod(i) = y7omod(i)+(cbmod2(i)*(y6omod(i)+y6nmod(i))-(y8omod(i) 
     1            +y8nmod(i))*cdmod2(i))/todt                           
      signal = x8nmod(i) * ckmod1(i) + y8nmod(i)*ckmod2(i)              
      sighim(i) = amax1(pnmod1(i),amin1(pxmod1(i),signal))              
      return                                                            
      end                                                               
