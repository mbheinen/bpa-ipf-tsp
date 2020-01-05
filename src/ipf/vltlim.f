C    @(#)vltlim.f	20.6 7/18/96
        subroutine vltlim(nb, vmin, vmax, vstrt)                          
	real vmin, vmax, vstrt
C                                                                       
C       This subroutine determines the voltage limits (VMIN and         
C        VMAX) for bus "NB" according to the following criteria:        
C                                                                       
C               1. Bus type and user-supplied data                      
C               2. Global voltage limits                                
C               3. Program default values                               
C
c	Subroutine changed to handle double precision.  This subroutine is
c	called from: btable, loadbus, mod_bus, mvchbu, and presln.for.  
c	All calls pass in vlimn and vlimx or assign output to vlimn vlimx.
c
      	include 'ipfinc/parametr.inc'

      	include 'ipfinc/blank.inc'
c	Global variables used:
c		vlimit, nvlim
      	include 'ipfinc/bus.inc'
c	Global variables used:
c		bus, zone, kbsdta, base, busdta, vstart
      	include 'ipfinc/prt.inc'
c	Global variables used:
c		errbuf
c                                                                       
c       blkdta-vltlim                                                   
c                                                                       
        common /vlt001/ ratio3, ratio4                                  
c
	double precision vtemp, vhi, vlo
c
	real ratio3, ratio4
c
	integer ktype
c
        character xbuf*120, zn*2                                        
                                                                        
c       ratio3 = option(23)                                             
c       ratio4 = option(24)                                             
                                                                        
        ktype=kbsdta(1,nb)                                              
        vstrt = 0.0                                                    
        go to (100,110,112,110,120,100,110,100,100,110,100,120,102,     
     1         102,110,110) ktype                                       
c                                                                       
  100   vmax = busdta(11,nb)
        vmin = busdta(12,nb)
        go to 130                                                       
c                                                                       
  102   vmax = 0.0
        vmin = 0.0
        vstrt = busdta(11,nb)                                          
        go to 130                                                       
c                                                                       
  110   vmax = busdta(11,nb)
        vmin = vmax                                                       
        vstrt = vmax
        go to 130                         
c                                         
  112   vstrt = busdta(11,nb)               
  120   vmax = 0.0
        vmin = 0.0
  130   continue                          
        do 150 i=1,nvlim                  
           if (base(nb) .ge. vlimit(1,i) .and. 
     &         base(nb) .le. vlimit(2,i)) then
              do 140 j=1,10
                 zn = vlimzn(j,i)              
                 if (zn .eq. ' ') then         
                    if (j .eq. 1) go to 170    
                    go to 150                  
                 endif                         
                 if (zone(nb) .eq. zn) go to 170 
  140         continue                           
           endif
  150   continue                                                        
  160   i = nvlim                                                       
  170   if (vmax .gt. 0.0 .and. vmin .gt. 0.0) go to 180                
        if (vmax .eq. 0.0) vmax = vlimit(4,i)
        if (vmin .eq. 0.0) vmin = vlimit(3,i)
        go to (180,174,172,174,180,174,174,180,180,174,180,180,180,     
     1         172,174,174) ktype                                       
  172   if (vstrt .gt. 0.0) go to 180                                  
  174   write (errbuf(1),176) vmax, vmin                                  
  176   format(' Missing voltage data, global values used ',
     &         ': VMAX, VMIN ',2f9.3,')')                   
        errbuf(2) = ' '                                     
        call bcdbus (nb,xbuf)                               
        write (errbuf(3),210) xbuf(1:80)                                
        call prterx ('W',3)                                             
  180   if (vmax .ge. vmin) go to 220                                   
        vtemp = vmax                                                      
        vmax = vmin                                                       
        vmin = vtemp                                                      
        write (errbuf(1),190)                                           
  190   format(' VMIN is greater than VMAX for the following bus. ',
     &         'Limits have been exchanged.')               
        call bcdbus (nb,xbuf)                               
        errbuf(2) = ' '                                                 
        write (errbuf(3),210) xbuf(1:80)                                
  210   format(13x,'(',a80,')')                                         
        busdta(11,nb) = vmax                                              
        busdta(12,nb) = vmin                                        
        call bcdbus (nb,xbuf)                               
        write (errbuf(4),210) xbuf(1:80)                                
        call prterx ('W',4)                                             
c                                                                       
  220   go to (235,230,225,235,235,235,230,240,240,235,240,235,225,     
     1         225,230,230) ktype                                       
c                                                                       
c       Type S,J,F                                                      
c                                                                       
  225   vstrt = busdta(11,nb)                                          
        if ( vstrt .eq. 0.0 ) then                                     
           vhi = amax1 (vmax, vlimit(4,i)) 
           vlo = amin1 (vmin, vlimit(3,i))                              
           vstrt = vhi - ratio4 * (vhi - vlo)
        else             
           vmax = amax1 (vmax, vstrt)                                  
           vmin = amin1 (vmin, vstrt)                                  
        endif                                                           
        go to 250                                                       
c                                                                       
c       Type E,Q,K,L                                                    
c                                                                       
  230   if( vstrt .eq. 0.0 ) vstrt = vmax
        go to 250                                                       
c                                                                       
c       Type B,V,C,T,D,M                                                
c                                                                       
  235   vhi = amax1 (vmax, vlimit(4,i))                                 
        vlo = amin1 (vmin, vlimit(3,i))                                 
        vstrt = vhi - ratio3 * (vhi - vlo) 
        go to 250                                                       
c                                                                       
c       Type G,X,O                                                      
c                                                                       
  240   vstrt = vmax - ratio4 * (vmax - vmin)
c                                                                       
  250   continue                                                        
        if (vstrt.gt.0.750.and.vstrt.le.1.333) go to 270              
        write (errbuf(1),260) bus(nb), base(nb), zone(nb), vmax,
     1                        vmin, vstrt
  260   format (' Infeasible starting voltage for bus ', a8, f6.1,
     1          ' ZONE ', a2, ' VMAX,VMIN,VSTART :', 3f9.3)    
        call prterx ('W',1)                                             
  270   continue                                                        
        return                                                          
        end                                                             
