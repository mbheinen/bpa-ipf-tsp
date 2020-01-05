C    @(#)clustr.f	20.3 2/13/96
      subroutine clustr
C
C     optain the cluster population of the retained system.
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
c	Global variables used:
c		km, kmlen, ikmu, inp2opt, opt2inp,  
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
c	Global variables used:
c		bus, base, zone
      include 'ipfinc/ikk.inc'
      include 'ipfinc/intbus.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/red7.inc'
c	Global variables used:
c		None
c 
      common /scratch/ kolum(MAXBUS), net(2,2*MAXREI), mtrx(MAXBUS)
 
      integer  first
      external kmpclx, swpclx
C
C     Initialize clustering.
C
      do 150 nb = 1,ntot
         k = inp2opt(nb)  
         if(ikk(1,k) .eq. 1) then   
            kolum(nb) = 0   
         else   
            kolum(nb) = -1  
         endif  
  150 continue  
C       
C     Expand outwards in retained network, starting from a kernel node  
C       
      knt = 0   
      do 280 kernal = 1, ntot   
         if (kolum(kernal) .eq. 0) then 
            if (knt .ge. 2*MAXREI) then 
               write (errbuf(1)(1:120),230) MAXREI  
  230          format (' More than ',i4,' clusters in retained system.')
               call prterx ('W',1)  
*               ERROR = 1
               go to 900
            endif   
            knt = knt + 1   
            last=1  
            ngen = 0
            mtrx(last)=kernal   
            kolum(kernal)=knt   
            next=0  
  250       next = next + 1 
            if (next .le. last) then
               nb=mtrx(next)
               k = inp2opt(nb)
               ngen=ngen+1  
               if (kmlen(k) .gt. 0) then           
                  is = km(k) -1                    
                  ls = kmlen(k)                    
                  do 270 l=1,ls 
                     m = ikmu(is+l)                 
                     mb = opt2inp(m)  
                     if (kolum(mb) .eq. 0) then 
                        last = last + 1 
                        if (last .gt. MAXBUS) call erexit   
                        mtrx(last) = mb 
                        kolum(mb)=knt   
                     endif  
  270             continue  
               endif
               go to 250
            endif   
            net(1,knt) = kernal 
            net(2,knt) = ngen   
         endif  
  280 continue  
C       
C     Sort clusters from smallest population to largest.
C       
      do 282 i = 1, ntot+1  
  282 mtrx(i) = i   
        
      call qiksrt (1, ntot, kmpclx, swpclx) 
        
      first = 1 
  284 do 320 ix = first, ntot   
         i = mtrx(ix)   
         if (kolum(i) .gt. 0) then  
            kntlst = kolum(i)   
            write (outbuf, 290) kntlst, net(2,kntlst)   
  290       format ('0 Cluster ', i3, ' Population ', i5,   
     1         ' has the following composition: ')  
            call prtout (1) 
            call space (1)  
            do 292 j = ix+1, ntot   
               if (kolum(mtrx(j)) .ne. kolum(i)) then   
                  last = j - 1  
                  go to 294 
               endif
  292       continue
            last = ntot 
  294       do 310 j = ix, last, 6  
               k = min0 (j+5, last) 
               write (outbuf, 300) (bus(mtrx(l)),   
     1            base(mtrx(l)), zone(l), l = j, k) 
  300          format (6(2x,a8,f6.1,1x,a2)) 
               call prtout (1)  
  310       continue
            first = last + 1
            go to 284   
         endif  
  320 continue  
        
  900 continue  
      return
      end   
