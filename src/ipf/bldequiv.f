C    @(#)bldequiv.f	20.7 11/11/97
      integer function bldequiv()
 
C     This subroutine performs the y-matrix reduction.                 
 
      include 'ipfinc/parametr.inc'
 
      include 'ipfinc/alpha.inc'
      include 'ipfinc/amtrx.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/ikk.inc'
      include 'ipfinc/intbus.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/qksrt.inc'
      include 'ipfinc/red4.inc'
      include 'ipfinc/red5.inc'
      include 'ipfinc/red6.inc'
      include 'ipfinc/red7.inc'
      include 'ipfinc/reic.inc'
c
      double precision ai1(2,3), ra, rb, xa, xb,
     &		       pgen, qgen, c, cg, ek, fk, vk
      logical finished
      integer shift
      
      bldequiv = 0            ! Set default return status "success"
      idebug = kase1(33)
      ngensw = kase1(2) + 1
      nlodsw = kase1(4) + 1
      nadmsw = kase1(6) + 1
C
C     NETSW identifies reduction pass:                                 
C
C        0 -- eliminate and normalize all eliminated nodes             
C        1 -- perform partial elimination on all retained nodes        

      ik = 1
      do 1640 netsw = 0, 1
         do 1630 kt = 1, ntotx
            if (kmlen(kt) .le. 0 .or. ikk(1,kt) .ne. netsw) go to 1630
C
C        1. Build working row KT in arrays KOLUM, KORDER, G, and B.
C        2. convert distributed generations and loads of eliminated 
C           nodes into appropriate current and admittance. 
C
            ikkind(1,kt) = ik
            pgen = pnetu(kt) + ploadu(kt)
            qgen = qnetu(kt) + qloadu(kt)
            ek = e(kt)
            fk = f(kt)
            vk = ek * ek + fk * fk
            if (vk .gt. 0 .and. netsw .eq. 0) then
 
               if (ngensw .eq. 2) then
                  gkku(kt) = gkku(kt) - pgen / vk
                  bkku(kt) = bkku(kt) + qgen / vk
               else
                  ai(1,kt) = ai(1,kt) + (pgen*ek + qgen*fk) / vk
                  ai(2,kt) = ai(2,kt) + (pgen*fk - qgen*ek) / vk
               endif
               pnetu(kt) = pnetu(kt) - pgen 
               qnetu(kt) = qnetu(kt) - qgen 

               if (nlodsw .eq. 2) then
                  gkku(kt) = gkku(kt) + ploadu(kt) / vk
                  bkku(kt) = bkku(kt) - qloadu(kt) / vk
               else
                  ai(3,kt) = ai(3,kt) 
     &                     - (ploadu(kt) * ek + qloadu(kt) * fk) / vk
                  ai(4,kt) = ai(4,kt) 
     &                     - (ploadu(kt) * fk - qloadu(kt) * ek) / vk
               endif
               pnetu(kt) = pnetu(kt) + ploadu(kt)
               qnetu(kt) = qnetu(kt) + qloadu(kt)
               ploadu(kt) = 0.0
               qloadu(kt) = 0.0

            endif
 
            lp = 0
            kl = 0
            ks = kt
            if (ikk(1,kt) .eq. 0) ks = -kt
c
c           Build working row from y-matrix row in the following order:
c
c           1. all eliminated nodes (colunms < 0) 
c           2. all retained nodes (colums > 0)
c
            do lsw = 0, 1
               j = km(kt)
               finished = (j .eq. 0)
               ksw = 0
               do while (.not. finished)
                  mt = shift (shift (ikmu(j), 16), -16)
                  if (ikk(1,mt) .eq. lsw) then
                     ms = mt
                     if (ikk(1,mt) .eq. 0) ms = -mt
c
c                    Insert diagonal if necessary  
c
                     if (kl .eq. 0) then
                        if (isign(1,ks) .eq. isign(1,ms)) then
                           if (mt .gt. kt) then
                              lp = lp + 1
                              kl = lp
                              korder(lp) = lp + 1
                           endif
                        else if (ms .gt. ks) then
                           lp = lp + 1
                           kl = lp
                           korder(lp) = lp + 1
                        endif
                     endif
                     lp = lp + 1
                     kolum(lp) = ms
                     korder(lp) = lp + 1
                     g(lp) = gkmu(j)
                     b(lp) = bkmu(j)
                  endif
                  if (ksw .eq. 0) then
                     if (j .eq. km(kt)+kmlen(kt)-1) then
                        if (shift (ikmu(j), -16) .gt. 0) then
                           j = shift (ikmu(j), -16)
                           ksw = 1
                        else
                           finished = .true.
                        endif
                     else
                        j = j + 1
                     endif
                  else
                     finished = .true.
                  endif 
               enddo
            enddo
c
c           If diagonal is not encountered, insert at end of row             
c
            if (kl .eq. 0) then
               lp = lp + 1
               kl = lp
               ms = ks
            endif
            kolum(kl) = ks
            g(kl) = gkku(kt)
            b(kl) = bkku(kt)
C       
C           Check validity of assembled Y-matrix data   
C       
            if (idebug .gt. 0) then   
               do i = 1, 3
                  ai1(1,i) = 0.0
                  ai1(2,i) = 0.0
               enddo
               do i = 1, 3
                  ai1(1,i) = ai1(1,i) + ai(2*i-1,kt)
                  ai1(2,i) = ai1(2,i) + ai(2*i,kt)
               enddo
               do i = 1, lp
                  yred(1,i) = iabs(kolum(i))
                  yred(2,i) = g(i)
                  yred(3,i) = b(i)
               enddo
               call pkqk1 (kt, lp, yred, ai1) 
            endif 

            if (km(kt) .eq. 0) then
               b(kl) = 0.01      ! Add residual to diagonal
            endif  
            max = ms
            korder(lp) = 0
            mend = lp + 1
            ko = lp
            mel = 1
            if (idebug .gt. 1) then
               write (dbug,1380) ks, (ai(j,kt),j=1,6),
     1            (l,kolum(l),korder(l),g(l),b(l),l=1,lp)
 1380          format ('0 DUMP OF ROW ',i5,' I =',6e12.4, /,/,
     &           ' INDEX    KOLUM    KORDER            ',   
     &           'G              B    '//(3i8,2e15.6))
            endif
c
c           Elimination loop                                                 
c
            do while (mel .gt. 0)
               ms = kolum(mel)
               mt = iabs(ms)
               if (netsw .eq. 1) then
                  if (ms .gt. 0) go to 1540
               else
                  if (isign(1,ms) .ne. isign(1,ks)) then
                     write (errbuf(1), 1390) ks, ms
 1390                format (' Y-matrix column order error ', 2i5)
                     call prterx ('F', 1)
                     go to 9000
                  endif
                  if (mt .ge. kt) go to 1540
               endif
C
C              Eliminate column MS from working row.                            
C
               ik = ikkind(1,mt)
               ikstop = ikkind(2,mt)
               krw = mel
               ra = g(mel)
               rb = b(mel)
               if (idebug .gt. 1) then
                  write (dbug,1430) ms, ra, rb, ik, ikstop
 1430             format ('  Eliminating column ',i5,' YKK =',2e13.5,   
     &               '  ECS = ',2i6)
               endif
               do i = 1, 5, 2
                  ai(i,kt) = ai(i,kt) - ra * ai(i,mt) + rb * ai(i+1,mt)
                  ai(i+1,kt) = ai(i+1,kt) - ra * ai(i+1,mt) 
     &                       - rb * ai(i,mt)
               enddo
               do while (ik .le. ikstop)
                  ms = amtrx(ik)
                  mt = iabs(ms)
                  xa = amtrx(ik+1)
                  xb = amtrx(ik+2)
C
C                 ISW denotes search status in working row for column 
C                 MS.
C                 ISW = -1 : not found, continue search
C                        0 : found identify as MLC.
C                        1 : not in working row, add.
C                        2 : not in working row, add at end of row.
C
                  isw = -1
                  if (isign(1,ms) .eq. isign(1,max)) then
                     if (mt .gt. iabs(max)) isw = 2
                  else if (ms .gt. max) then
                     isw = 2
                  endif
                  if (isw .lt. 0) then
                     do while (krw .gt. 0 .and. isw .lt. 0)
                        ka = kolum(krw)
                        if (isign(1,ka) .eq. isign(1,ms)) then
                           if (mt .lt. iabs(ka)) then
                              isw = 1
                           else if (mt .eq. iabs(ka)) then
                              mlc = krw
                              isw = 0
                           else
                              ko = krw
                              krw = korder(krw)
                           endif
                        else
                           if (ms .lt. ka) then
                              isw = 1
                           else if (ms .eq. ka) then
                              mlc = krw
                              isw = 0
                           else
                              ko = krw
                              krw = korder(krw)
                           endif
                        endif
                     enddo
                  endif
                  if (isw .lt. 0) then
                     write (errbuf(1), 1440) ks, ms
 1440                format (' Y-matrix working row order error ', 2i5)
                     call prterx ('F', 1)
                     go to 9000
                  endif
                  if (isw .eq. 2) then
                     max = ms
                     ko = lp
                     lp = mend
                     isw = 1
                  endif
                  if (isw .eq. 1) then
                     korder(mend) = korder(ko)
                     kolum(mend) = ms
                     korder(ko) = mend
                     mlc = mend
                     ko = mend
                     mend = mend + 1
                     if (mend .gt. 2001) then
                        write (errbuf(1), 1450) ks, mend
 1450                   format (' Y-matrix working row overflow ', 2i5)
                        call prterx ('F', 1)
                        go to 9000
                     endif
                     g(mlc) = 0.0
                     b(mlc) = 0.0
                  endif
                  g(mlc) = g(mlc) - ra * xa + rb * xb
                  b(mlc) = b(mlc) - ra * xb - rb * xa
                  ik = ik + 3
               enddo
               mel = korder(mel)
            enddo
 1540       continue
C
C           Begin normalization of row if eliminated.  Otherwise, store
C           values of residual reduced elements.                       
C
            ik = ikkind(1,kt)
            if (netsw .eq. 0) then
               if (ms .ne. ks) then
                  write (errbuf(1), 1550) ks
 1550             format (' Y-matrix working row diagonal error ', i5)
                  call prterx ('F', 1)
                  go to 9000
               endif
               ra = g(mel)
               rb = b(mel)
               c = 1.0 / (ra**2+rb**2)
               ra = ra * c
               rb = -rb * c
               do i = 1, 5, 2
                  cg = ai(i,kt) * ra - ai(i+1,kt) * rb
                  ai(i+1,kt) = ai(i+1,kt) * ra + ai(i,kt) * rb
                  ai(i,kt) = cg
               enddo
               mel = korder(mel)
            endif
 
            do while (mel .gt. 0)
               amtrx(ik) = kolum(mel)
               if (netsw .eq. 0) then
                  amtrx(ik+1) = g(mel) * ra - b(mel) * rb
                  amtrx(ik+2) = b(mel) * ra + g(mel) * rb
               else
                  amtrx(ik+1) = g(mel)
                  amtrx(ik+2) = b(mel)
               endif
               ik = ik + 3
               mel = korder(mel)
            enddo
   
            ikstop = ik - 1
            ikstrt = ikkind(1,kt)
            ikkind(2,kt) = ikstop
            if (idebug .gt. 1) then
               write (dbug,1610) ks, (ai(i,kt),i=1,6), ikstrt, ikstop,
     1            (ifix(sngl(amtrx(i))),amtrx(i+1),amtrx(i+2),i=ikstrt,
     2             ikstop,3)
 1610          format ('  REDUCED ROW ',i5,' I :',6e12.4,' ECS :',
     &            2i6,/'           KOLUM           G               B  ',
     &            /, / ,(i10,2e15.5))
            endif
 1630    continue
 1640 continue

      go to 9020

 9000 write (errbuf(1), 9010)
 9010 format (' Reduction aborted by errors ')
      call prterx ('F', 1)
      bldequiv = 1

 9020 return
      end
