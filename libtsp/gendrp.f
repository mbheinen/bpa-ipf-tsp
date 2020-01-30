C    %W% %G%
      subroutine gendrp(ig, pgdrop, igbn, igecs, idm)               
c
c     Debugged for new type GW governors only!
c
c     THIS SUBROUTINE CONTAINS LOGIC TO REDUCE OR DROP GENERATION     
c     ON A MACHINE.  IT IS CALLED BY MATMOD AND RELAY.                
c
      include 'tspinc/params.inc' 
      include 'tspinc/blkcom1.inc' 
      include 'tspinc/matrow.inc' 
      include 'tspinc/newton.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/vrgov.inc' 
      include 'tspinc/gentbla.inc' 
      include 'tspinc/gentblb.inc' 
      include 'tspinc/gentblc.inc' 
      include 'tspinc/dateq.inc' 
      include 'tspinc/pointr.inc' 
      include 'tspinc/param.inc' 
      include 'tspinc/ecsind.inc' 
      include 'tspinc/machd1.inc' 
      include 'tspinc/igentn.inc' 
      include 'tspinc/bname.inc' 
      include 'tspinc/buskv.inc' 
      include 'tspinc/gov.inc'
      include 'tspinc/turb.inc'

      character idm*1, id*1, nameg*8, gname*16  
      logical cross_compound
 
c     -     Begin     Begin     Begin     Begin     Begin     Begin  
c     L6=6
      ld=0
      lp=0
      id = idm                                                      
      if (id .eq. 'H') lp =1                                         
  300 n1 = igecs                                                    
      gvpwr1 = govpwr(ig)
      call redecs(datat,n1,33)
      mgen = igndta(1, ig)                                          
      mgov = govtyp(ig)
      mturb = turbtyp(ig)
      cross_compound = (mturb .ge. 4 .and. mturb .le. 7) 
      if (consm .eq. 0.0) go to 400
      if (consm .eq. 999999.) go to 400
      if (ld.ne.0) go to 320
      govp = govpwr(ig)
      pgo1=govp                                                        

      mgovh=mgov
      if (mgov .gt. 0) then
        igv = govindx(ig)
        if (cross_compound) then
          call genname (ig, gname)
          write (errbuf(1), 10306) gname
10306     format('0 Specified generation dropped on machine ', a, 
     &      ' cannot determine H/L ratio')
          call prterr ('E',1)
          write (*, 10306) gname
          pratio = datat(28)
          pgdrop=pgdrop*(1.+1./pratio)
        endif
        if (mgov .eq. 1) then
          pgo = govdat(govindx(ig)+17)
        else if (mgov .ge. 2 .and. mgov .le. 4) then
          pgo = govdat(govindx(ig)+17) 
        else if (mgov .eq. 5) then
          pgo = govdat(govindx(ig)+7)
        else
          pgo = govpwr(ig)
        endif
        pgo1=pgo                        
        if (mgov .eq. 2) pgo1 = pgo / govdat(igv+3)  
        if (lp .ne. 1 .and. mgov .eq. 1 .and. cross_compound) then
          pgdrop = fr * pgo1
        endif
      endif
      if (pgdrop .eq. 0.0) pgdrop = -pgo1
      fr = pgdrop/pgo1                                                 

      if (fr .lt. -1.001) then
        pgdrop = -pgdrop/fr
        call genname (ig, gname)
        write (errbuf(1),306) gname
  306   format('0 Specified generation drop on machine ', a, 
     &   ' is limited to 100%')
        write (*,306) gname
        write (outbuf, 290) pgdrop, pgo1                              
  290   format('0 Amount drop = ', e12.4, ' amount available = ', 
     &    e12.4)                                                 
        write (*, 290) pgdrop, pgo1                              
        call prterr ('E',2)
        call mpost('GENDRP')
      endif
      pgo1 = pgo1 + pgdrop

C     TEST IF 100 PERCENT GEN DUMP
  320 if (pgo1 .lt. 0.005) then
        fr=-1.0
        frmpy=0.0
        frmpyr =0.0
      else
        frmpy=1.0+fr
        frmpyr=1.0/frmpy
      endif
C
C     TEST IF EXTENDED MACH
      if (mgen.ne.1) go to 322
      gmod=0.0                                                         

      bmod=-fr /xdp                                                    

      go to 330                                                        

  322 if (mgen .eq. 2) go to 650                                     
      if (mgen .gt. 5) go to 323                                     
      xgen = 0.5*(xdp + xqp)
      zsqr = 1.0/(ra*ra + xdp*xqp)
      go to 324
  323 xgen = 0.5*(xdpp + xqpp)
      zsqr = 1.0/(ra*ra + xdpp*xqpp)
      xdpp= xdpp*frmpyr
      xqpp = xqpp*frmpyr
  324 gmod = fr*ra*zsqr
      bmod=- fr*xgen*zsqr                                              

      ra=ra*frmpyr                                                     

      xd=xd*frmpyr                                                     

      xq=xq*frmpyr                                                     

      xqp=xqp*frmpyr                                                   

      xp=xp*frmpyr                                                     

      xt=xt*frmpyr                                                     

      xdmod = xdmod*frmpyr
      xqmod = xqmod*frmpyr
  330 consm=frmpyr*consm                                               

      gvpwr1=gvpwr1*frmpy                                              

      xdp=xdp*frmpyr                                                   

      dmpfac=dmpfac*frmpy

C     UPDATE GEN DATA...                                            
      govpwr(ig) = gvpwr1
      if (mgen .eq. 1) go to 335
      call ritecs(datat, n1, 33)                                    
      go to 340                                                        

C
C     PROCESS INDUCTION MTR TRIP                                       

C
  650 zsqr = 1.0/(ra*ra + xdp*xdp)                                  
      gmod = fr*ra*zsqr                                             
      bmod = -fr*xdp*zsqr                                           
      ra = ra*frmpyr                                                
      xdp = xdp*frmpyr                                              
      xd = xd*frmpyr                                                
      consm = consm*frmpyr                                          
      all = all*frmpy                                               
      bl = bl*frmpy                                                 
      cl = cl*frmpy                                                 
      gvpwr1 = gvpwr1*frmpy                                         
      govpwr(ig) = gvpwr1                                           
      call ritecs(datat, n1, 33)                                    
      go to 340                                                     
  335 call ritecs ( datat,n1,7)                                        

C     MODIFY DIAG.ELEM DUE TO GEN DROP..
  340 ibus = igbn                                                   
      call getmat(ibus, ii)
      if (keybrd(30) .ne. 0) then                                   
         write (outbuf, 505) jbus, atrow(i + 1),                    
     1       atrow(i + 2), ibus, atrow(ii - 1), atrow(ii)       
  505    format('O GENDRP, S340+4, B4 ', 2(i10, e16.6, e16.6))         

         call prtout (1)                                            
      endif                                                          
C
C     YISOLN, NEWTON OPTION
C
      if (inewts .eq. 1) then
        atrow(ii-1)= atrow(ii-1)+ gmod
        atrow(ii)=atrow(ii) + bmod
      else
        gadmit(ibus) = gadmit( ibus) + gmod
        badmit(ibus) = badmit( ibus) + bmod
      endif
      call putmat(ibus, ii)

      if (keybrd(30) .ne. 0) then                                   
         write (outbuf, 510) jbus, atrow(i + 1),                    
     1        atrow(i + 2), ibus, atrow(ii - 1), atrow(ii)           
  510    format('O GENDRP, S343-5, B4 ', 2(i10, e16.6, e16.6))         

         call prtout (1)                                            
      endif                                                         
 
      pgdrp=pgdrop
      if (cross_compound) then
         if (id .eq. 'L') pgrdp = pgdrop/(1. + pratio)
         if (id .eq. 'H') pgdrp = pratio*pgdrop/(1. + pratio)
      endif
      pgdrp = -pgdrp
      call genname (ig, gname)                                         

      write (errbuf(1), 10306) gname
      if (id .ne. 'L') then
        gname(16:16) = 'H'
      endif
      write (outbuf,342) pgdrp, gname, to                         
  342 format('0', 5x, 'Generation dropping of ', f8.2, ' P(PU) AT ',
     &   a, ' at ', f7.2, ' cycles ')                                  

      call prtout (1)
      write (*, 342) pgdrp, gname, to                         

C     TEST IF GOV REP EXIST
      if (mgov.eq.0) go to 400
      if (mgov.eq.1) go to 365
      if (mgov.ne.3) go to 345
      a2p=a2p*frmpy
      go to 365
  345 velmax=velmax*frmpy
      velmin=velmin*frmpy
      if (cross_compound) then
         do i=26,27                                                    

           cgov(i)=cgov(i)*frmpy
         enddo
      endif
      pmin=pmin*frmpy
  365 sgkp1=sgkp1*frmpy
      pmax=pmax*frmpy
      pgo=pgo*frmpy
      do i=11,14                                                    
         cgov(i)=cgov(i)*frmpy
      enddo
c     call ritecs(cgov,mgvecs,32)
  400 continue
      if (keybrd(3) .gt. 0) then
         write (outbuf,550) nameg, bkvg, id, frmpy
  550    format('0 GENDRP',1x,a8,1x,f5.1,1x,a1,e13.6)
         call prtout (1)
         do jjj = 1,27,8                                               

            kkk = min0 (jjj+7,27)
            write (outbuf,551) (k,datat(k),k=jjj,kkk)
  551       format(1x,8(i3,e13.6))
            call prtout (1)
         enddo
         if (mgov .gt. 0) then
            do jjj = 1,32,8
               kkk = min0 (jjj+7,30)
               write (outbuf,560) (k,cgov(k),k=jjj,kkk)
  560          format(1x,8(i3,e13.6))
               call prtout (1)
            enddo
         endif
      endif

      if (lp.eq.0) go to 600

C     ARRANGE TO PROCESS THE LP MACHINE FOR CROSS COMPOUNDED UNITS
      id = 'L'                                                      
      igbn = igentn(1,ilow)                                         
      igecs = igentn(2,ilow)                                        
      ig=ilow
      lp=0
      if (mgov.eq.3) go to 300
      ld=1
      go to 300
  600 return
      end                                                              

