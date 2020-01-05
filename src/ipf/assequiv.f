C    @(#)assequiv.f	20.7 11/11/97
      integer function assequiv (ntoto, ntotr, ltotr)
C       
C     Assemble network data from reduced y-matrix data:
C       
C     1.  Delete eliminated nodes
C     2.  Delete eliminated branches 
C     3.  Add equivalent branches
C     4.  Add distributed generations and loads to   
C         envelope nodes.
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/amtrx.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/branch.inc' 
      include 'ipfinc/bus.inc'
      include 'ipfinc/cbus.inc'
      include 'ipfinc/dc2t.inc'
      include 'ipfinc/dcmt.inc'
      include 'ipfinc/ikk.inc'
      include 'ipfinc/intbus.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/phase.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/qksrt.inc'
      include 'ipfinc/red4.inc'
      include 'ipfinc/red5.inc'
      include 'ipfinc/red6.inc'
      include 'ipfinc/red7.inc'
      include 'ipfinc/reic.inc'
      include 'ipfinc/sortbs.inc'
C
      common /eqarea/ iarea,kadata(MAXCAR)

      common /scratch/ nbr, array(2,100), 
     &                 ndel1, lindel1(MAXBRN), ndel2, 
     &                 lindel2(MAXBRN2), ndel3, cbdel(MAXBUS),
     &                 br_status(MAXBRN)
      integer array, cbdel, br_status, ierr, nadd2
c
      double precision yr, yi, eqbr_ymax, ysq, dgkk, dbkk, fk, ek,
     &                 vk, ai1(2,3), pgen, qgen, plod, qlod, pshun, 
     &                 qshun, vksq, dt
      complex * 16  oldy(2,2), y(2,2), v(2)
      real tol
      character id*1, ido*1, jid*1, tempc*10
      integer ptr, oldptr, status, compare, del_eqbrn, add_eqbrn,
     &        add_eqcbs, komparbr, eqbr_total, eqbr_added, count,
     &        kt, js, i, shift
      logical found, finished
      external komparbr, swap_br, kmpred, swpred  
c
      assequiv = 0          ! Initialize return status 'successful'

      ntotr = 0   
      ltotr = 0   
      ndel1 = 0
      ndel2 = 0
      ndel3 = 0
      idebug = kase1(33)

      read (chase1, '(bz, f10.5)') tol 
      if (tol .eq. 0.0) then  
         if (chase1(1) .eq. ' ') tol=0.02   
      endif
      ngensw=kase1(3)+1 
      nlodsw=kase1(5)+1 
      nadmsw=kase1(7)+1 
c
c     Begin loop assembling reduced equivalent system from reduced
c     Y-matrix data.
c
      do 2380 nbx = 1, ntotx
         nb = alf2inp(nbx)
         if (nb .eq. 0) go to 2380   
         krei = 0  
         if (nb .gt. ntoto) krei=1   
         kt=inp2opt(nb)  
         if(ikk(1,kt) .ne. 1) go to 2380   
         j2=ikk(2,kt)  
         j5=ikkind(1,kt)   
         j6=ikkind(2,kt)-j5+1  
         eqbr_total = 0
         eqbr_added = 0
         eqbr_ymax = 0.0d0
         ntotr=ntotr+1 
         ltotr=ltotr+j6/3-1
C       
C        Change voltage limits if controlled bus eliminated
C       
         if (kbsdta(1,nb) .eq. 8 .or. kbsdta(1,nb) .eq. 11) then   
            mt = kbsdta(13,nb) 
            if (mt .gt. 0) then
               mt = inp2opt(mt)   
               if (ikk(1,mt) .eq. 0) then   
                  vk=dsqrt(e(kt)**2+f(kt)**2)
                  busdta(11,nb) = sngl(vk)
                  busdta(12,nb) = sngl(vk)
                  kbsdta(13,nb) = nb
               endif
            endif  
         endif 
         if (j2 .eq. 0) go to 2380  ! Interior node, nothing changed
         if (kase1(36) .eq. 1) then ! Option to save envelope nodes
C                                   ! as type BE
            if (kbsdta(1,nb) .eq. 1 .or.
     &          kbsdta(1,nb) .eq. 4 .or.
     &          kbsdta(1,nb) .eq. 10) then
               kbsdta(1,nb) = 2
               vk = dsqrt (e(kt) ** 2 + f(kt) ** 2)
               busdta(11,nb) = sngl(vk)
               busdta(12,nb) = sngl(vk)
            endif
         endif
         ek=e(kt)  
         fk=f(kt)  
         vk=dsqrt(ek**2+fk**2)  
         pgen=(ek*ai(1,kt)+fk*ai(2,kt))*bmva   
         qgen=(fk*ai(1,kt)-ek*ai(2,kt))*bmva   
         plod=-(ek*ai(3,kt)+fk*ai(4,kt))*bmva  
         qlod=-(fk*ai(3,kt)-ek*ai(4,kt))*bmva  
         pshun=-(ek*ai(5,kt)+fk*ai(6,kt))*bmva 
         qshun=-(fk*ai(5,kt)-ek*ai(6,kt))*bmva 
         js=j6/3   

         do i=1,3 
            ai1(1,i)=0.0d0
            ai1(2,i)=0.0d0
         enddo

         do i = 1,js  
            j = j5 + 3*i - 3  
            k = amtrx(j)   
            k = opt2inp(k)
            yred(1,i) = inp2alf(k)
            yred(2,i) = amtrx(j+1)
            yred(3,i) = amtrx(j+2)
         enddo

         key=2 
         call qiksrt(1, js, kmpred, swpred )   

         kl = 0
         do i = 1,js   
            k = yred(1,i)
            yred(1,i) = alf2inp(k)
            if (yred(1,i) .eq. nb) kl = i
         enddo

         if (kl .eq. 0) then
            write (errbuf(1), 1680)
 1680       format (' Missing diagonal element in reduced y-matrix ', 
     &               i5)
            call prterx ('W', 1)
            go to 9000
         endif

         vksq=bmva*vk**2   
         if (ngensw .eq. 1) then
            ai1(1,1)=(1,1)+pgen
            ai1(2,1)=ai1(2,1)+qgen
         else if (ngensw .eq. 2) then
            ai1(1,3)=ai1(1,3)+ai(1,kt)
            ai1(2,3)=ai1(2,3)+ai(2,kt)
         else
            yred(2,kl)=yred(2,kl)-qgen/vksq   
            yred(3,kl)=yred(3,kl)+qgen/vksq   
            ai(1,kt)=0.0d0
            ai(2,kt)=0.0d0
         endif
         if (nlodsw .eq. 1) then
            ai1(1,2)=ai1(1,2)+plod
            ai1(2,2)=ai1(2,2)+qlod
         else if (nlodsw .eq. 2) then
            ai1(1,3)=ai1(1,3)+ai(3,kt)
            ai1(2,3)=ai1(2,3)+ai(4,kt)
         else
            yred(2,kl)=yred(2,kl)+plod/vksq   
            yred(3,kl)=yred(3,kl)-qlod/vksq   
            ai(3,kt)=0.0d0
            ai(4,kt)=0.0d0
         endif
         if (nadmsw .eq. 1) then
            ai1(1,2)=ai1(1,2)+pshun   
            ai1(2,2)=ai1(2,2)+qshun   
         else if (nadmsw .eq. 2) then
            ai1(1,3)=ai1(1,3)+ai(5,kt)
            ai1(2,3)=ai1(2,3)+ai(6,kt)
         else
            yred(2,kl)=yred(2,kl)+pshun/vksq  
            yred(3,kl)=yred(3,kl)-qshun/vksq  
            ai(5,kt)=0.0d0
            ai(6,kt)=0.0d0
         endif
C       
C        Convert current to constant power factor: 
C       
C        amag(v)*conjg(id) = v*conjg(i)
C       
         if (ai1(1,3)**2 + ai1(2,3)**2 .gt. 0.0) then
            dt = (ai1(1,3)*ek + ai1(2,3)*fk)/vk*bmva  
            ai1(2,3)=(-ai1(1,3)*fk+ai1(2,3)*ek)/vk*bmva   
            ai1(1,3)=dt   
         endif
         if (idebug .gt. 0) then
            write (dbug,1850) bus(nb),base(nb)
 1850       format ('0 REDUCED Y MATRIX FOR NODE ',a8,f7.1/)  
            do i=1,js
               jx=yred(1,i) 
               write (dbug,1870) bus(jx), base(jx), yred(2,i),
     &            yred(3,i)
 1870          format (8x,a8,f7.1,2e15.6)
            enddo
            write (dbug,1880) 
 1880       format ('0 ORIGINAL Y MATRIX '/)  
            write (dbug,1870) bus(nb), base(nb), gkku(kt), bkku(kt)
            kmluu = km(kt) - 1

            ls = kmlen(kt)
            do i = 1, ls
               array(1,i) = i
               jx = shift (shift (ikmu(i+kmluu), 16), -16)
               jx = opt2inp(jx)
               array(2,i) = inp2alf(jx)
            enddo
            i = shift (ikmu(ls+kmluu), -16)
            if (i .gt. 0) then
               ls = ls + 1
               jx = ikmu(i)
               jx = opt2inp(jx)
               array(1,ls) = ls
               array(2,ls) = inp2alf(jx)
            endif

            call shellsrt (1, ls, komparbr, swap_br)
  
            do i = 1, ls
               j = array(1,i)
               jx = alf2inp(array(2,i))
               write (dbug,1870) bus(jx), base(jx), gkmu(j+kmluu),
     &            bkmu(j+kmluu)
            enddo
         endif
C       
C        Perform a branch merge between YRED and GKMU, BKMU. The 
C        difference will be implemented into BRNCH array.   
C       
C        KSW   ---  Meaning   
C         1         normal
C         2         End-of-data for GKMU, BKMU
C         3         End-of-data for YRED  
C         4         End-of-data for both (GKMU, BKMU) and YRED   
C       
         if (krei .eq. 0) then 
            ksw = 1
            ptr = 0
            ksw = incr1red (ptr, oldptr, nb, k2, id, ksw)
         else  
            ksw = 2
c
c           The REI system at this point has no network data. Thus
c           the off-diagonal contributions to the diagonal must be
c           compensated directly, and not through the non-existant
c           BRNCH.
c
            kmluu = km(kt) - 1
            do l = 1, kmlen(kt)
               gkku(kt) = gkku(kt) + gkmu(l+kmluu)
               bkku(kt) = bkku(kt) + bkmu(l+kmluu)
            enddo
            ptr = 0
         endif 
         jt=0  
         ksw = incr2red (jt, js, nb, m1, m2, ksw)
C       
C        Begin branch merge   
C       
         do while (ksw .lt. 4)
c
c           ISW is status of merge:
C            -1 = delete branch in BRNCH
c             0 = topological match
c            +1 = add equivalent branch
c
            if (ksw .eq. 1) then

 1980          if (idebug .gt. 0) then
                  write (dbug,1990) bus(k2),base(k2),bus(m2),base(m2) 
 1990             format ('  BRANCH COMPARISON -- "YKMU" ',
     &               a8,f7.1,'  "YRED" ', a8,f7.1)  
               endif
               if (inp2alf(k2) .lt. inp2alf(m2)) then
                  isw = -1
               else if (inp2alf(k2) .eq. inp2alf(m2)) then
                  isw = 0
               else
                  isw = +1
                  iaddsw = 1
C       
C                 IADDSW = 1: add equivalent branch 
C                          2: add equivalent parallel   
C       
               endif
            else if (ksw .eq. 2) then
               isw = 1
            else if (ksw .eq. 3) then
               isw = -1
            endif
            if (isw .lt. 0) then
c
c              Delete branch elements one parallel at a time
c
               k2o = k2
               do while (ptr .gt. 0 .and. ky(ptr) .eq. k2o)
                  ido = brid(ptr)
                  if (idebug .gt. 0) then
                     write (dbug,2010) bus(k2),base(k2), ido
 2010                format ('  Deleting parallel ',a8,f7.1,' PAR ',a2)  
                  endif
                  if (brtype(ptr) .eq. 4) then
                     status = del_eqbrn (ptr, oldptr)
                     if (status .ne. 0) go to 9000
                     ksw = incr1red (ptr, oldptr, nb, k2, id, ksw)
                  endif
                  if (brtype(ptr) .ne. 4) then
c***KLN***Variable passed to this routine has changed(single to double)
                     call pieqiv(ptr, y, ierr)
                     gkku(kt) = gkku(kt) - dreal(y(1,1))
                     bkku(kt) = bkku(kt) - dimag(y(1,1))   
                  endif 
                  status = del_eqbrn (ptr, oldptr)
                  if (status .ne. 0) go to 9000
                  ksw = incr1red (ptr, oldptr, nb, k2, id, ksw)
c
c                 Delete section elements
c
                  do while (ptr .gt. 0 .and. ky(ptr) .eq. k2o .and.
     &                      brid(ptr) .eq. ido)
                     if (idebug .gt. 0) then
                        write(dbug,2060) brtype(ptr), brsect(ptr)
 2060                   format('  Deleting section TYPE ',i2,
     &                     ' Section ',i2)  
                     endif

                     status = del_eqbrn (ptr, oldptr)
                     if (status .ne. 0) go to 9000

                     ksw = incr1red (ptr, oldptr, nb, k2, id, ksw)
                  enddo
               enddo

            else if (isw .eq. 1) then
c
c              Add equivalent branches
C       
C              IADDSW = 1: add equivalent branch 
C                       2: add equivalent parallel   
C       
C       
C              Check G/B ratio of equivalent branches  
C       
               oldy(1,2) = dcmplx (yred(2,jt),yred(3,jt))   
               oldy(1,1) = -oldy(1,2)  
               oldy(2,1) = oldy(1,2)   
               oldy(2,2) = oldy(1,1)   
               do i = 1,2 
                  do j = 1,2 
                     y(i,j) = oldy(i,j)  
                  enddo
               enddo
               v(1) = dcmplx(e(kt),f(kt))   
               v(2) = dcmplx(e(m1),f(m1))
c***KLN***Variables passed to this routine changed(single to double)   
               call fxrxrt (y,v,tol)   
               if (y(1,2) .ne. oldy(1,2)) then 
                  yred(2,jt) = dreal (y(1,2))
                  yred(3,jt) = dimag(y(1,2))  
                  yred(2,kl) = yred(2,kl) + dreal (y(1,1) - oldy(1,1))  
                  yred(3,kl) = yred(3,kl) + dimag(y(1,1) - oldy(1,1)) 
                  if (idebug .gt. 1) then
                     write (dbug,734) intbus(kt),intbas(kt),   
     &                  intbus(m1),intbas(m1),oldy(1,1),oldy(1,2),
     &                  y(1,1),y(1,2),yred(2,i),yred(3,i),yred(2,jt),
     &                  yred(3,jt) 
  734                format (' Eliminate G on branch ',a8,f7.1,1x,a8,
     &                  f7.1, / 
     1                  '         2-port Y(old) ',4e12.5,/,   
     2                  '         2-port Y(new) ',4e12.5,/,   
     3                  '                Y      ',4e12.5) 
                  endif
               endif   
C
C              Ignore equivalent branch if high impedance
C
               eqbr_total = eqbr_total + 1
               eqbr_ymax = dmax1 (eqbr_ymax, cdabs(oldy(1,2)))
               if (cdabs(oldy(1,2)) .lt. tol) then
                  go to 2170
               endif
          
               eqbr_added = eqbr_added + 1
               yr = yred(2,jt)
               yi = yred(3,jt)

               jid = 'X'
               status = add_eqbrn (nb, m2, jid, nadd1, nadd2)
               if (status .ne. 0) go to 9000
               ptr = nadd2
               ksw = incr1red (ptr, oldptr, nb, k2, id, ksw)

               nbr = iabs (nadd1)
               call putchr(3,'***',kbrnch(3,nbr))   
               kbrnch(4,nbr)=0  
c
c***kln		brnch is not double precision.
c
               ysq = yr**2 + yi**2   
               brnch(5,nbr)=sngl(-yr/ysq)
               brnch(6,nbr)=sngl(yi/ysq)
c***KLN Variable passed to this routine has changed from single to double.
               call pieqiv(nadd2,y,ierr)  
               gkku(kt) = gkku(kt) - dreal(y(1,2))  
               bkku(kt) = bkku(kt) - dimag(y(1,2)) 
               if (idebug .gt. 0) then
                  write (dbug,2160) bus(nb),base(nb),bus(m2),
     &               base(m2),jid,(brnch(j,nbr),j=5,6)  
 2160             format ('  Added equivalent branch ',a8,f7.1,
     &               ' TO ',a8,f7.1, ' PAR ',a1,' R + JX =',2e13.5)  
               endif
 2170          continue
C       
C              Equivalent branch complete. Advance YRED pointers
C       
               ksw = incr2red (jt, js, nb, m1, m2, ksw)

            else if (isw .eq. 0) then
C       
C              Check for equivalent parallels
C       
               mt=inp2opt(k2)  
               k2o=k2
               ido=id
               ls = km(kt)-1+kmlen(kt)   
               do lt=km(kt), ls
                  if (shift (shift (ikmu(lt), 16), -16) .eq. mt) 
     &               go to 2210  
               enddo
               if (shift (ikmu(ls), -16) .gt. 0) then
                  lt = shift (ikmu(ls), -16)
                  if (shift (shift (ikmu(lt), 16), -16) .eq. mt) 
     &               go to 2210  
               endif
               write (errbuf(1), 2200) kt, mt
 2200          format (' Cannot find branch (', i5, ',', i5,
     &             ') in reduced y-matrix ')
               call prterx ('W', 1)
               go to 9000
 2210          iaddsw=2  
C       
C              Check G/B ratio of equivalent branch
C       
               oldy(1,2) = dcmplx(yred(2,jt)-gkmu(lt),
     &                           yred(3,jt)-bkmu(lt))
               oldy(1,1) = -oldy(1,2)  
               oldy(2,1) = oldy(1,2)   
               oldy(2,2) = oldy(1,1)   
               do i = 1,2 
                  do j = 1,2 
                     y(i,j) = oldy(i,j)  
                  enddo
               enddo
               v(1) = dcmplx(e(kt),f(kt))   
               v(2) = dcmplx(e(mt),f(mt))
c***kln Variables passed to this routine changed from single to double.   
               call fxrxrt (y,v,tol)   
               if (y(1,2) .ne. oldy(1,2)) then 
                  yred(2,jt) = yred(2,jt) + dreal (y(1,2) - oldy(1,2))  
                  yred(3,jt) = yred(3,jt) + dimag (y(1,2) - oldy(1,2)) 
                  yred(2,kl) = yred(2,kl) + dreal (y(1,1) - oldy(1,1))  
                  yred(3,kl) = yred(3,kl) + dimag (y(1,1) - oldy(1,1)) 
                  if (idebug .gt. 1) then
                     write (dbug,734) intbus(kt),intbas(kt),intbus(mt),
     &                  intbas(mt),oldy(1,1),oldy(1,2),y(1,1),y(1,2),
     &                  yred(2,i),yred(3,i),yred(2,jt),yred(3,jt) 
                  endif   
               endif
C
C              Ignore equivalent branch if high impedance
C
               if (cdabs(oldy(1,2)) .lt. tol) then
                  yr = 0.0d0
                  yi = 0.0d0
                  go to 2280
               endif

               yr = yred(2,jt) - gkmu(lt)
               yi = yred(3,jt) - bkmu(lt)
C       
C              Delete R records  
C       
               if (brtype(ptr) .eq. 4) then

                  status = del_eqbrn (ptr, oldptr)
                  if (status .ne. 0) go to 9000

               endif 
C       
C              Check for parallel equivalent branches. If found, 
C              consolidate its quantities with that from the new 
C              equivalent branch.  
C       
               do while (k2 .eq. k2o) 
                  if (id .eq. 'X') then
c***kln Variable passed to this routine has changed from single to double. 
                     call pieqiv(ptr,y,ierr) 
                     y(1,2) = y(1,2) + dcmplx(yr,yi)  
                     y(2,2) = dcmplx(1.0,0.0)/(-y(1,2))   
                     nbr = iabs (brnch_ptr(ptr))
                     brnch(5,nbr)=sngl(dreal(y(2,2)))
                     brnch(6,nbr)=sngl(dimag(y(2,2)))
                     if (idebug .gt. 0) then
                        k1 = kx(ptr)
                        k2 = ky(ptr)
                        write (dbug,2270) bus(k1),base(k1),bus(k2),
     &                     base(k2),id,yr,yi   
 2270                   format ('  Extended equivalent parallel ',
     &                     a8,f7.1,' TO ',a8,f7.1,' PAR ',a1,
     &                     ' R + JX =',2e13.5)
                     endif
                     gkku(kt) = gkku(kt) - yr 
                     bkku(kt) = bkku(kt) - yi   
                     yr = 0.0d0
                     yi = 0.0d0
                  endif 
                  ksw = incr1red (ptr, oldptr, nb, k2, id, ksw)
               enddo

               if (yr .ne. 0.0 .or. yi .ne. 0.0) then
  
                  jid = 'X'
                  status = add_eqbrn (nb, m2, jid, nadd1, nadd2)
                  if (status .ne.0) go to 9000
                  ptr = nadd2
                  ksw = incr1red (ptr, oldptr, nb, k2, id, ksw)

                  nbr = iabs (nadd1)
                  call putchr(3,'***',kbrnch(3,nbr))   
                  kbrnch(4,nbr)=0  
                  ysq = yr**2 + yi**2   
                  brnch(5,nbr)=sngl(-yr/ysq)
                  brnch(6,nbr)=sngl(yi/ysq)
c***kln Variable passed to this routine changed from single to double.
                  call pieqiv(nadd2,y,ierr)  
                  gkku(kt) = gkku(kt) - dreal(y(1,2))  
                  bkku(kt) = bkku(kt) - dimag(y(1,2)) 
                  if (idebug .gt. 0) then
                     write (dbug,2160) bus(nb),base(nb),bus(m2),
     &                  base(m2),jid,(brnch(j,nbr),j=5,6)  
                  endif
C       
C                 Equivalent parallel complete. Advance YRED and
C                 YKM pointers.
C       
               endif
 2280          continue
   
               do while (k2 .eq. k2o .and. ksw .eq. 1) 
                  ksw = incr1red (ptr, oldptr, nb, k2, id, ksw)
               enddo

               ksw = incr2red (jt, js, nb, m1, m2, ksw)
            endif
         enddo
C       
C        End of branch merge   
C       
         if (kl .eq. 0) then
            write (errbuf(1), 2290) kt
 2290       format (' Cannot find diagonal in reduced y-matrix row ', 
     &         i5)
            call prterx ('W', 1)
            go to 9000
         endif
         dgkk=yred(2,kl)-gkku(kt)                     
         dbkk=yred(3,kl)-bkku(kt)                     
c
c        Begin continuation bus merge
c
         ksw = 1
         finished = .false.
         do while (.not. finished)
C       
C           Search for existing equivalenced shunt and distributed 
C           injections 
C       
C           KSW = 1 : +A01 (constant current load)
C           KSW = 2 : +A02 (constand power load)
C       
            if (krei .eq. 0) then 
               ncb=kbsdta(15,nb)   
            else  
               ncb=0   
            endif 
            found = .false.
            tempc = ' '

            do while (ncb .gt. 0 .and. .not. found)
               call getchr (1, tempc(1:1), kbctbl(8,ncb))
               call getchr (3, tempc(2:4), kbctbl(10,ncb))
               call getchr (2, tempc(5:6), kbctbl(9,ncb))
               if (ksw .eq. 1) then
                  compare = kompr ('A***01', tempc, junk)
               else
                  compare = kompr ('A***02', tempc, junk)
               endif
               if (compare .lt. 0) then
                  ncb = 0
               else if (compare .eq. 0) then
                  found = .true.
               else
                  ncb = bctbl_nxt(ncb)
               endif
            enddo
            if (found) then
               if (ksw .eq. 1) then
                  if (ai1(1,3) .lt. 0.0) then
                     bctbl(2,ncb) = bctbl(2,ncb) - sngl(ai1(1,3))  
                  else
                     bctbl(6,ncb) = bctbl(6,ncb) + sngl(ai1(1,3))  
                  endif
                  if (ai1(2,3) .lt. 0.0) then
                     bctbl(3,ncb) = bctbl(3,ncb) - sngl(ai1(2,3))  
                  else
                     bctbl(11,ncb) = bctbl(11,ncb) + sngl(ai1(2,3))  
                  endif
                  bctbl(4,ncb) = bctbl(4,ncb) + sngl(dgkk)*bmva  
                  bctbl(5,ncb) = bctbl(5,ncb) + sngl(dbkk)*bmva  
                  if (idebug .gt. 0) then
                     write (dbug, 2328) (kbctbl(j,ncb),j=8,10),   
     &                  -ai1(1,3), -ai1(2,3), dgkk*bmva, dbkk*bmva,
     &                  0.0, 0.0
 2328                format('  Merged equivalent bus shunt ', a1, 2x,
     &                  a2,2x,a3,2x, 6e10.3)
                 endif
               else
                  if (ai1(1,2) .gt. 0.0) then
                     bctbl(2,ncb) = bctbl(2,ncb) + sngl(ai1(1,2))  
                  else
                     bctbl(6,ncb) = bctbl(6,ncb) - sngl(ai1(1,2))  
                  endif
                  if (ai1(2,2) .gt. 0.0) then
                     bctbl(3,ncb) = bctbl(3,ncb) + sngl(ai1(2,2))  
                  else
                     bctbl(11,ncb) = bctbl(11,ncb) - sngl(ai1(2,2))  
                  endif
                  bctbl(6,ncb) = bctbl(6,ncb) + sngl(ai1(1,1))   
                  bctbl(11,ncb) = bctbl(11,ncb) + sngl(ai1(2,1))  
               endif
               if (idebug .gt. 0) then
                  write (dbug, 2328) (kbctbl(j,ncb),j=8,10),   
     &               ai1(1,2), ai1(2,2), 0.0, 0.0, ai1(1,1), ai1(2,1)
               endif
            else
C       
C              Add equivalenced shunt and distributed injections 
C       
               if (ksw .eq. 1) then
                  status = add_eqcbs (nb, 'A', '***', '01', ncb)
               else
                  status = add_eqcbs (nb, 'A', '***', '02', ncb)
               endif
               if (status .ne. 0) go to 9000

               if (ksw .eq. 1) then  
                  if (ai1(1,3) .lt. 0.0) then
                     bctbl(2,ncb) = -sngl(ai1(1,3))  
                  else
                     bctbl(6,ncb) = sngl(ai1(1,3))  
                  endif
                  if (ai1(2,3) .gt. 0.0) then
                     bctbl(3,ncb) = sngl(ai1(2,3))  
                  else
                     bctbl(11,ncb) = - sngl(ai1(2,3))  
                  endif
                  bctbl(4,ncb) = sngl(dgkk)*bmva  
                  bctbl(5,ncb) = sngl(dbkk)*bmva  
               else  
                  if (ai1(1,2) .gt. 0.0) then
                     bctbl(2,ncb) = sngl(ai1(1,2))  
                  else
                     bctbl(6,ncb) = -sngl(ai1(1,2))  
                  endif
                  if (ai1(2,2) .gt. 0.0) then
                     bctbl(3,ncb) = sngl(ai1(2,2))  
                  else
                     bctbl(11,ncb) = -sngl(ai1(2,2))  
                  endif
                  bctbl(6,ncb) = sngl(ai1(1,1))  
                  bctbl(11,ncb) = sngl(ai1(2,1))  
               endif
               if (idebug .gt. 0) then
                  write (dbug,2330) (kbctbl(j,ncb),j=8,10),   
     1               (bctbl(j,ncb),j=2,6),bctbl(11,ncb) 
 2330             format('  Added equivalent bus shunt ', a1, 2x,
     &               a2,2x,a3,2x, 6e10.3)
               endif
            endif 
            if (ksw .eq. 1) then
               if (ai1(1,1) .ne. 0.0 .or. ai1(1,2) .ne. 0.0) then
                  ksw = 2  
               else if (ai1(2,1) .ne. 0.0 .or. ai1(2,2) .ne. 0.0) then
                  ksw = 2  
               else 
                  finished = .true.
               endif
            else
               finished = .true.
            endif
         enddo
c
c        End of continuation bus merge.
c        Begin branch connectivity check
c
         count = 0
         ptr = kbsdta(16,nb)
         k2 = 0
         do while (ptr .gt. 0)
            if (ky(ptr) .ne. k2) then
               count = count + 1
               k2 = ky(ptr)
            endif
            ptr = brnch_nxt(ptr)
         enddo
         if (count .eq. 0) then
            write (errbuf(1), 2340) bus(nb), base(nb), zone(nb)
 2340       format (' Border node ', a8, f7.1, ' zone ', a, 
     &              ' has no branches ')
            write (errbuf(2), 2350) eqbr_total, eqbr_added, eqbr_ymax, 
     &                              tol
 2350       format (' Total equivalent branches = ', i3, 
     &              ' retained = ', i3,
     &              ' max Y  = ', e10.3, 
     &              ' tol = ', e10.3)
            call prterx ('W', 2)
         endif

         if (idebug .gt. 0) then
            do i = 1, js  
               m = yred(1,i)
               yred(1,i) = inp2opt(m) 
            enddo
 
            do i = 1,3   
               ai1(1,i) = ai(2*i-1,kt)   
               ai1(2,i) = ai(2*i,kt) 
            enddo
c
c           Check reduced Y-matrix + distributed current after reduction
c
            call pkqk1 (kt, js, yred, ai1) 
c
c           Check assembled input data after reduction
c
            do i = 1, 3
               pnetu(kt) = pnetu(kt) + ek * ai1(1,i) + fk * ai1(2,i)
               qnetu(kt) = qnetu(kt) - ek * ai1(2,i) + fk * ai1(1,i)
            enddo
            call evdata (nb, perr, qerr)
         endif 

 2380 continue
c
c     End of loop assembling equivalent system from reduced data
c
      go to 9020

 9000 write (errbuf(1), 9010)
 9010 format ('Reduction aborted by fatal errors ')
      call prterx ('F', 1)
      assequiv = 1

 9020 return
      end
