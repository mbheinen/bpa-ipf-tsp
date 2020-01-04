C    @(#)opieqiv.f	20.4 2/13/96
      subroutine opieqiv (p, y, error)                                 
      complex y(2,2)
      integer p, error                                               
c                                                                       
c     This subroutine computes the 2-port admittance matrix y         
c     for obrnch(1,aq) from the ALTERNATE BASE CASE IN RESIDENCE!
c                                                                       
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alt_case.inc'
      include 'ipfinc/prt.inc'

c     set up record type code parameters...
      include 'ipfinc/brtype.inc'

      common /is_batch / is_batch

      complex cx
      integer q, aq
      character xbuf * 120
c                                                                       
      error = 0                                                       
      k1 = okx(p)
      k2 = oky(p)
      ltype = obrtype(p)
      q = obrnch_ptr(p)
      aq = iabs(q)


      if (ltype.eq.BRTYP_L .or. ltype.eq.BRTYP_T .or.
     &    ltype.eq.BRTYP_TP .or. ltype.eq.BRTYP_E) then
c                           
c        compute transfer impedance     
c                   
         r=obrnch(5,aq)  
         x=obrnch(6,aq)         
         z=r**2 + x**2        
         if (z .eq. 0.0) then
            write (errbuf(1),110) 
  110       format(' Branch has zero transfer impedance.') 
            errbuf(2) = ' '       
            call bcdbrn(p,xbuf)     
            write (errbuf(3), 120) xbuf(1:80) 
  120       format(13x,'(',a80,')')
            if (is_batch .eq. 0) then
               call prterx ('E',3)
            else
               call prterx ('F',3)
            endif
            error = 1           
            z=1.0             
         endif

         if (ltype .eq. BRTYP_L) then
c                                                                       
c           "L" branch 
c                   
            y(1,2)=-cmplx (r/z, -x/z)   
            y(2,1) = y(1,2)     
            y(1,1)=cmplx (obrnch(7,aq),obrnch(8,aq))-y(1,2)  
            y(2,2)=y(1,1)

         else if (ltype .eq. BRTYP_T) then
c                     
c           "T" branch      
c              
            if (q .gt. 0) then
               tk=obrnch(9,aq)/oldbase(k1)   
               tm=obrnch(10,aq)/oldbase(k2)    
            else
               tk=obrnch(10,aq)/oldbase(k1)    
               tm=obrnch(9,aq)/oldbase(k2)   
            endif
            if (amax1(tk,tm).ge.1.40 .or. amin1(tk,tm).le.0.60) then
               if (q .gt. 0) then
                  tk=obrnch(10,aq)/oldbase(k1) 
                  tm=obrnch(9,aq)/oldbase(k2) 
               else
                  tk=obrnch(9,aq)/oldbase(k1)     
                  tm=obrnch(10,aq)/oldbase(k2)    
               endif
               if (amax1(tk,tm).lt.1.40 .and. amin1(tk,tm).gt.0.60) 
     &            then       
                  write(errbuf(1),130) 
  130             format(' Backwards transformer taps are reversed.')
                  errbuf(2) = ' '   
                  call bcdbrn(p,xbuf)              
                  write (errbuf(3),120) xbuf(1:80)
                  temp = obrnch(9,aq)              
                  obrnch(9,aq) = obrnch(10,aq)      
                  obrnch(10,aq) = temp             
                  call bcdbrn(p,xbuf)             
                  write (errbuf(4),120) xbuf(1:80)
                  call prterx ('W',4)             
               else                               
                  write (errbuf(1),140) 
  140             format(' Transformer taps are beyond 40% of base kv.')
                  errbuf(2) = ' '                 
                  call bcdbrn(p,xbuf)             
                  write (errbuf(3),120) xbuf(1:80)
                  if (is_batch .eq. 0) then
                     call prterx ('E',3)
                  else
                     call prterx ('F',3)
                  endif
                  if (tk.eq.0.0) tk=1.0           
                  if (tm.eq.0.0) tm=1.0           
               endif                              
            endif
            tkm = 1.0 / (tk*tm)       
            y(1,1) = cmplx (0.5*tkm*obrnch(7,aq), 
     &                     -0.5*tkm*abs(obrnch(8,aq)))
            y(2,2) = y(1,1)        
            y(1,2) = -cmplx(r/(z*tk*tm),-x/(z*tk*tm)) 
            y(2,1) = y(1,2)     
            y(1,1) = y(1,1) + cmplx (r/(z*tk*tk),-x/(z*tk*tk)) 
            y(2,2) = y(2,2) + cmplx (r/(z*tm*tm), -x/(z*tm*tm))  

         else if (ltype .eq. BRTYP_TP) then 
c                      
c           "TP" branch             
c                       
            if (q .gt. 0) then
               tk=obrnch(9,aq)
               tm=obrnch(10,aq)/oldbase(k2)                        
            else
               tk=-obrnch(9,aq)
               tm=obrnch(10,aq)/oldbase(k1)                        
            endif
            if (tm .eq. 0.0) tm=1.0               
            if (tm .ge. 1.40 .or. tm .le. 0.60) then
               write (errbuf(1),150) 
  150          format(' Phase shifter tap2 is beyond 40% of base kv.')
               errbuf(2) = ' '                    
               call bcdbrn(p,xbuf)  
               write (errbuf(3),120) xbuf(1:80)   
               call prterx ('W',3)      
               tm=1.0                             
            endif
            if (tk .ge. 120.1 .or. tk .le. -120.1) then
               write (errbuf(1), 160) 
  160          format(' Phase shifter has excessive angle ',
     &                '(> 120 degrees)')
               errbuf(2) = ' '                    
               call bcdbrn(p,xbuf)   
               write (errbuf(3),120) xbuf(1:80)   
               call prterx ('W',3)     
            endif
            angle=0.0174532925*tk    
            y(1,2)=cmplx(0.5*obrnch(7,aq),-0.5*abs(obrnch(8,aq)))
            y(1,1)=y(1,2)+cmplx(r/z,-x/z)         
            y(2,2)=y(1,2)+cmplx(r/z,-x/z)/cmplx(tm**2,0.0) 
            y(1,2)=-cmplx(cos(angle),
     &                    sin(angle))*cmplx(r/(z*tm),-x/(z*tm))  
            y(2,1)=-cmplx(cos(angle),
     &                   -sin(angle))*cmplx(r/(z*tm),-x/(z*tm)) 
            if (q .lt. 0) then
               cx=y(2,2)      
               y(2,2)=y(1,1)    
               y(1,1)=cx       
            endif

         else 
c                                                                       
c           "E" branch   
c             
            y(1,2)= -cmplx (r/z, -x/z)   
            y(2,1)=y(1,2)          
            if (q .gt. 0) then
               y(1,1)=cmplx(obrnch(7,aq),obrnch(8,aq))-y(1,2) 
               y(2,2)=cmplx(obrnch(9,aq),obrnch(10,aq))-y(2,1) 
            else
               y(2,2)=cmplx(obrnch(7,aq),obrnch(8,aq))-y(2,1) 
               y(1,1)=cmplx(obrnch(9,aq),obrnch(10,aq))-y(1,2) 
            endif

         endif


      else if (ltype.eq.BRTYP_PEQ) then
c                                                                       
c        "pi-equivalent" branch  
c                                                    
         if (q .gt. 0) then
            y(1,1) = cmplx(obrnch(4,aq),obrnch(5,aq))  
            y(1,2) = cmplx(obrnch(6,aq),obrnch(7,aq))  
            y(2,1) = cmplx(obrnch(8,aq),obrnch(9,aq))  
            y(2,2) = cmplx(obrnch(10,aq),obrnch(11,aq))
         else
            y(2,2) = cmplx(obrnch(4,aq),obrnch(5,aq))   
            y(2,1) = cmplx(obrnch(6,aq),obrnch(7,aq))   
            y(1,2) = cmplx(obrnch(8,aq),obrnch(9,aq))   
            y(1,1) = cmplx(obrnch(10,aq),obrnch(11,aq)) 
         endif

      else
c                   
c        error-illegal branch type calling "pieqiv" 
c                                  
         write (errbuf(1), 170) 
  170    format(' Illegal branch type processed by "PIEQIV"') 
         errbuf(2) = ' '          
         call bcdbrn(p,xbuf)      
         write (errbuf(3),120) xbuf(1:80)
         call prterx ('W',3)    
         error = 1       
      endif
 
      return     
      end   
