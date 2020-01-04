C    %W% %G%
      subroutine cmde2_ovld (jout)
 
C     THIS SUBROUTINE CHECKS FOR CURRENT AND VOLTAGE VIOLATIONS
C
C          JOUT       Common Mode Outage Index
C          IIB        RUNNING COUNTER ON ARRAYS BAD,AND IBAD
C          IPBAD(J)   BASE ADDRESS IN BAD AND IBAD CONTAINING
C                     PROBLEMS FOUND IN THE JTH OUTAGE
C          IBAD( )=-K MEANS THAT THE KTH LINE IN CLNO WAS OVERLOADED
C          IBAD( )=K  MEANS THAT VOLTAGE VIOLATION FOUND AT NODE K
C          IBAD( )=0  MEANS SYSTEM SEPARATION OR NON-CONVERGENCE
C          BAD( )     MAGNITUDE OF CURRENT OR VOLTAGE
C          BAD( )=0.0 WHEN IBAD( )=0 MEANS SYSTEM SEPARATION
C          BAD( )= N  WHEN IBAD( )=0 MEANS NON-CONVERGENCE
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/cmde_com.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/time1.inc'
      include 'ipfinc/comm_mode.inc'
      include 'ipfinc/pfstates.inc'
      include 'ipfinc/intrst.inc'
      include 'ipfinc/ecvar.inc'

      common /scratch/ kolum(MAXBUS), net(200), mtrx(MAXBUS), 
     &                 last_out, comm_out(MAXBUS)
      integer last_out, comm_out
     
      common /term_debug/ iterm

      integer chek_lcmd2, chek_bcmd2, ptr
      complex*16 y_16(2,2)
C
C     Test for failed solution

      ipbad(jout) = iib
      ibad_rs(jout) = 0
      if (lskp .eq. 3 .or. ostates .eq. 8 .or. ostates .eq. 9) then
        iflag = 10*ittot + 1 
        if (iib .lt. MXIBAD) then
          ibad(iib) = 0
          bad(iib) = iflag
          iib = iib + 1
        elseif (iib .eq. MXIBAD) then
          ibad(iib) = 0
          bad(iib) = iflag
          iib = iib + 1
          write (errbuf(1), 10010) jout, nout, iflag, iib
10010     format (' Overflow of "IBAD" array: ', 
     &      'JOUT,NOUT,IFLAG,IIB = ', 4i8)
          call prterx('E', 1)
        endif
        if (iterm .ne. 0) write (*, 10020) jout
        if (idebug .ne. 0) write (dbug, 10020) jout
10020   format ('0  Abnormal termination - Outage ', i4)

      else

        do ib = 1, ntot_alf
          kt = inp2opt(alf2inp(ib))
          vmag(kt) = dsqrt (e(kt) ** 2 + f(kt) ** 2)
          if (e(kt) .ne. 0.0) then
            vangle(kt) = atan2 (f(kt), e(kt))
          else
            vangle(kt) = sign (1.5707963268, sngl(f(kt)))
          endif
        enddo
C
C       COMPARE ANGLE DIFFERENCE TO LIMITS IN CLNO
C
        do 300 i = 1,novl
          ptr = klno(8, i)
c
c         Test for deleted or irrelevant branch items
c
          if (ptr .eq. 0) go to 300
          if (brtype(ptr) .eq. 0 .or. brnch_ptr(ptr) .eq. 0)
     &      go to 300
          if (inp2alf(kx(ptr)) .gt. ntot_alf .or.
     &        inp2alf(ky(ptr)) .gt. ntot_alf) go to 300
          if (chek_lcmd2 (jout, i) .ne. 0) go to 300
C
C         CHECK FORWARD ANGLE LIMITS
C
          k = klno(1,i)
          m = klno(2,i)
          kt = inp2opt(k)
          mt = inp2opt(m)
          torque = vangle(kt) - vangle(mt)
          if (torque .ge. 0.0 .and. 
     &        torque .le. clno(1,i)) go to 300
C
C         CHECK REVERSE ANGLE LIMITS
C
          if (torque .lt. 0.0 .and. 
     &        torque .ge. clno(1,i)) go to 300
C
C         A POSSIBLE OVERLOAD HAS BEEN DETECTED - CALCULATE CURRENT
C
          call pieqiv (ptr, y_16, error)
          ykr = dreal (y_16(1,1)) / clno(13,i)
          yki = dimag (y_16(1,1)) / clno(13,i)
          ykmr = dreal (y_16(1,2)) / clno(13,i)
          ykmi = dimag (y_16(1,2)) / clno(13,i)
C
          ckmr = e(kt)*ykr - f(kt)*yki + e(mt)*ykmr - f(mt)*ykmi
          ckmi = f(kt)*ykr + e(kt)*yki + f(mt)*ykmr + e(mt)*ykmi
          curm = ckmr**2 + ckmi**2
          if (idebug.gt.1) then
            write(dbug, 10040) i, k, m, ykr, yki, ykmr, ykmi, ckmr, 
     &        ckmi, curm, e(kt),f(kt), e(mt), f(mt)
10040       format(' CHECK',3i10,/,1x,11e12.4)
          endif
          if (curm .le. 1.0) go to 300
C
          if (idebug .gt. 0) write (dbug, 10050) i, k, m, curm
10050     format(' CURRENT VIOLATION: I,K,M,CURM', 3i5, e13.5)
C
C         ENTER TYPE OF VIOLATION - NEGATIVE FOR BRANCH OVERLOAD
C
          if (iib .lt. MXIBAD) then
            ibad(iib) = -i
            bad(iib) = curm
            iib = iib + 1
          else if (iib .eq. MXIBAD) then
            ibad(iib) = -i
            bad(iib) = curm
            iib = iib + 1
            write (errbuf(1), 10010) jout, i, k, m, iib
            call prterx ('E',1)
          endif
  300   continue
C
C       CHECK FOR BUS VOLTAGE VIOLATION
C
        do 400 ib = 1,ntot_alf
          nb = alf2inp(ib)
          kt = inp2opt(nb)
c
c         Note: integer function chek_bout() defines outaged buses in
c               common array comm_out().
c
          if (intrst(nb) .eq. 0) go to 400
          if (chek_bcmd2 (jout, nb) .ne. 0) go to 400
          if (comm_out(nb) .ne. 0) go to 400
          call glbvlt(nb,vmin,vmax)   
          dv = dim(vbase(nb)+0.005,vmax) - dim(vmin,vbase(nb)-0.005)  
          vmin = vmin + amin1(0.0,dv)  
          vmax = vmax + amax1(0.0,dv)  
          dv = dim (vmag(kt), vmax) - dim (vmin,vmag(kt))
          if (abs(dv) .le. 0.001) go to 400
C
C         ENTER TYPE OF VIOLATION - POSITIVE FOR BUS VOLTAGE VIOLATION
C
  360     if (iib .lt. MXIBAD) then
            ibad(iib) = nb
            bad(iib) = vmag(kt)
            iib = iib + 1
          else if (iib .eq. MXIBAD) then
            ibad(iib) = nb
            bad(iib) = vlimx(kt)
            iib = iib + 1
            write (errbuf(1), 10010) jout,nout,i,k,0,iib
            call prterx ('E',1)
          endif
          if (idebug .gt. 0) then
            write (dbug,10060) nb, vmag(kt), vlimn(kt),vlimx(kt)
10060       format(' VOLTAGE VIOLATION: I, V, VLOW, VHI',i5,3e13.5)
          endif
  400   continue
      endif
      ipbad(jout+1) = iib
      return
      end
