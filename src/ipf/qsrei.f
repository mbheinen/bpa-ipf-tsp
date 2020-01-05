C    @(#)qsrei.f	20.4 7/29/96
      subroutine qsrei (nb, kt, iflag, ngensw, nlodsw, nadmsw)

C     Permits multiple calls to the same node.
C     Status is always determined by the last call.   

      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/cbus.inc'
      include 'ipfinc/ikk.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/ordsta.inc'
      include 'ipfinc/qsreic.inc'
      include 'ipfinc/tbx.inc'
      include 'ipfinc/xdata.inc'

      character kode*1,kowner*3 

      if (tbx_loaded .ne. ordtbx) then
         do i = 1, ntot
            ptrtbx(i)  = 0
         enddo

         do i = 1, ntotb
            nb = tbx(2,i)
            if (nb .gt. 0) then
              if (ordtbx .eq. 2) nb = opt2inp(nb)
              ptrtbx(nb) = i
            endif
         enddo
         tbx_loaded = ordtbx
      endif

C     Determine status of candidate "KT"

      iflag = 0 
      ntyp = ntypu(kt)                             
      pgen = pnetu(kt) + ploadu(kt)                 
      qgen = qnetu(kt) + qloadu(kt)                 
      vk = e(kt)**2 + f(kt)**2  
      if (ngensw.eq.2) then
         if (abs(pgen)+abs(qgen).ge.sang/bmva) iflag = 1
      endif

      if (nlodsw.eq.2) then
         if (abs(ploadu(kt))+abs(qloadu(kt)).ge.sang/bmva) iflag = 1 
      endif

      if (iflag.eq.0) then
         if (nadmsw.ne.2) go to 410
      endif

C     Determine "QMAX" and "QMIN" from bus type  

      qmax=busdta(9,nb)
      qmin=busdta(10,nb) 

      go to (130,140,140,130,150,130,160,160,160,130,160,150,130), ntyp 

  130 if (qmin.ne.0.0) qmax=qgen*bmva 
      qmin=qmax 
      go to 160 

  140 qmax=1.0e05   
      qmin=-1.0e05  
      go to 160 

  150 qmax=0
      qmin=0

C     Determine shunt admittance

  160 gshunt=0  
      bshunt=0  
      badj=0
      if (ngensw.eq.0) qgen = 0 
      qmin = amin1 (qmin,qgen*bmva) 
      qmax = amax1 (qmax,qgen*bmva) 
      if (nadmsw.ne.2) go to 360
      if (ntyp.eq.5.or.ntyp.eq.12) go to 360
      gshunt=busdta(5,nb)/bmva   
      bshunt=busdta(6,nb)/bmva   
      ncb=kbsdta(15,nb)  
        
      do while (ncb .gt. 0)
         call getchr(1,kode,kbctbl(8,ncb)) 
         call getchr(3,kowner,kbctbl(10,ncb))  
         if (kode .eq. 'A' .and. kowner .eq. '***') then
         else
            gshunt = gshunt + bctbl(4,ncb)/bmva   
            bshunt = bshunt + bctbl(5,ncb)/bmva   
         endif
         ncb = bctbl_nxt(ncb)
      enddo
        
      if (gshunt.eq.0.0.and.bshunt.eq.0.0) go to 360
      iflag=1   
        
C     Determine adjustable shunt (if any)   
        
      if (ntyp .ne. 7 .and. ntyp .ne. 9 .and. ntyp .ne. 11 .and.
     1    ntyp .ne. 16) go to 360   
        
      jt = ptrtbx(nb)
      ltyp=tbx(1,jt)   
      ityp=tbx(7,jt)   
      badj=tbx(6,jt)
        
      go to (360,280,360,280,290), ltyp 
        
  280 go to (350,350,300,310), ityp 
        
  290 go to (350,300,310,330), ityp 
        
C     "BQ" OR "BX" BUS IN STATE "QMAX" OR "QMIN" -- RESTORE SHUNT   
        
  300 if (badj.le.0) go to 340  
      go to 320 
        
  310 if (badj.ge.0) go to 340  
        
  320 bkku(kt) = bkku(kt) + badj   
      qnetu(kt) = qnetu(kt) - badj*vk                  
      go to 340 
        
C     "BX" BUS IN STATE "QDIS" -- RESTORE SHUNT 
        
  330 jx=tbx(5,jt) 
      if (jx.eq.0) call erexit  
      total=xdata(3,jx)+xdata(4,jx) 
      used=xdata(5,jx)+xdata(6,jx)  
      ds=(total-used)/bmva  
      bkku(kt) =  bkku(kt) + ds
      qnetu(kt) = qnetu(kt) - ds*vk                    
      badj=total/bmva   
      xdata(5,jx)=xdata(3,jx)   
      xdata(6,jx)=xdata(4,jx)   
  340 tbx(7,jt)=1  
  350 continue  
        
C     Store injection data temporarily in "INET"
        
  360 if (iflag.eq.0) go to 410 
      if(nspar(kt) .eq.0) then                    
         iqsrei=iqsrei+1
         if(iqsrei .gt. 2000) then  
            write (errbuf(1),370)   
  370       format('MORE THAN 2000 ENTITIES IN REI SUBSYSTEM')  
            call prterx ('W',1) 
            iqsrei=1
         endif  
         nspar(kt) = iqsrei   
      else  
         iqsrei=nspar(kt)                         
      endif 
        
      qsdta(1,iqsrei)=qmin  
      qsdta(2,iqsrei)=qmax  
      qsdta(3,iqsrei)=gshunt*vk 
      qsdta(4,iqsrei)=-bshunt*vk
      qsdta(5,iqsrei)=-badj*vk
      qsdta(6,iqsrei)=100*ngensw+10*nlodsw+nadmsw
      if (kase1(33).ne.0) then
         write (dbug,400) kt, bus(nb), base(nb), pgen, qgen, ploadu(kt),
     &                    qloadu(kt), (qsdta(i,iqsrei),i=1,5), ngensw,
     &                    nlodsw, nadmsw
  400    format (' QSREI ',i4,2x,a8,f7.1,9e10.3,1x,3i2)
      endif
  
  410 continue
      return
      end   
