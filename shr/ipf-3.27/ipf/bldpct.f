C    @(#)bldpct.f	20.6 11/12/98
        subroutine bldpct (pctq)                                        
        dimension pctq(*)                                               
c                                                                       
c       builds default percentage var quantities.       
c                                                                       
        include 'ipfinc/parametr.inc'

        include 'ipfinc/alpha.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/cbus.inc'
        include 'ipfinc/komps.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/qksrt.inc'
        include 'ipfinc/tbx.inc'
	include 'ipfinc/xdata.inc'

c***KLN Local variables changed to double precision
c
	double precision qgen(25), pgen(25), percnt(25), ptot, qtot
 
        integer first                                                   
        dimension ktgen(25), jtgen(25)       
        external kmpsrt, swpsrt                         
c                                                                       
c       Build percentage var control array "KSORT"                      
c                                                                       
        if (ntotb .eq. 0) go to 190                                     
                                                                        
        npctq = 0                                                       
        do jt = 1,ntotb                                             
           ltyp = tbx(1,jt)                                            
           if (ltyp .eq. 3) then                                        
              np = tbx(2,jt)                                           
              nq = tbx(8,jt)                                           
              nq = iabs(nq)                                             
              npctq = npctq + 1                                         
              ksort(1,npctq) = nq                                       
              ksort(2,npctq) = np                                       
              ksort(3,npctq) = jt                                       
           endif                                                        
        enddo
                                                                        
        if (npctq .le. 1) go to 190                                     
        call qiksrt(1, npctq, kmpsrt, swpsrt)                           
c                                                                       
c       %var generators are now sorted consecutively in "NBSORT".       
c                                                                       
        first = 1                                                       
        do while (first .lt. npctq)
           mc = ksort(1,first)                                              
           pcntot = 0.0                                                 
           ptot = 0.0                                                   
           qtot = 0.0                                                   
           k = 0                                                        
           do j = first, npctq                                          
              nq = ksort(1,j)                                           
              np = ksort(2,j)                                           
              jt = ksort(3,j)                                           
              if (nq .eq. mc) then                                      
                 k = k + 1                                              
                 ktgen(k) = np                                          
                 jtgen(k) = jt                                          
                 percnt(k) = 0.01 * tbx(5,jt)                           
                 if (percnt(k) .ge. 0.0 .and.                           
     1               percnt(k) .le. 1.0) then                           
                    pcntot = pcntot + percnt(k)                         
                 else                                                   
                    pcntot = pcntot - 1.0e10                            
                 endif                                                  
                                                                        
                 kt = inp2opt(np)
                 pgen(k) = bmva * dmax1(0.0d0,pnetu(kt)+ploadu(kt))
                 ptot = ptot + pgen(k)                                  
                 qgen(k) = bmva * dmax1(tbx(3,jt),0.0d0)                  
                 qtot = qtot + qgen(k)                                  
              else                                                      
                 first = j                                              
                 go to 140                                              
              endif                                                     
           enddo
           first = npctq + 1                                            
c                                                                       
c          JPCTQ generators are controlling a single bus.               
c          Compute percentage factor.                                   
c                                                                       
  140      jpctq = k                                                    
           ipcttot = 0
           maxkpct = ktgen(1)
           maxjpct = jtgen(1)
           do k = 1, jpctq                                          
              np = ktgen(k)                                             
              jt = jtgen(k)
              pctx = 0.0                                                
              if (ptot .gt. 0.0) then                                   
                 pct1 = pgen(k)/ptot                                    
                 pctx = pct1                                            
              else                                                      
                 pct1 = 0.0                                             
              endif                                                     
              if (qtot .gt. 0.0) then                                   
                 pct2 = qgen(k)/qtot                                    
                 if (pctx .eq. 0.0 .and. ptot .eq. 0.0) pctx = pct2     
              else                                                      
                 pct2 = 0.0                                             
              endif                                                     
              pct3 = 1.0 / float(jpctq)                                 
              if (pctx .eq. 0.0 .and. ptot .eq. 0.0 .and. qtot .eq. 0.0)
     1           pctx = pct3                                            
              if (pcntot .le. 0.0) then                                 
                 pct = pctx                                             
              else if (abs(pcntot - 1.00) .le. 0.01) then               
                 pct = percnt(k)                                        
              else                                                      
                 pct = pctx
              endif                                                     
              pctq(np) = int(100.0 * pct + 0.5)                                    
              if (pctq(np) .eq. 0.0) pctq(np) = 1.0
              if (pctq(np) .gt. pctq(maxkpct)) then
                 maxkpct = np
                 maxjpct = jt
              endif
              ipcttot = ipcttot + int(pctq(np))
              busdta(14,np) = pctq(np)
              tbx(5,jt) = pctq(np)
           enddo
           if (ipcttot .ne. 100) then
             pctq(maxkpct) = pctq(maxkpct) + 100 - ipcttot
             busdta(14,maxkpct) = pctq(maxkpct)
             tbx(5,maxjpct) = pctq(maxkpct)
           endif

        enddo

  190   continue                                                        
        return                                                          
        end                                                             
