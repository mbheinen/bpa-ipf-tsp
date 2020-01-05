C    @(#)capvlt.f	20.4 5/27/98
        subroutine capvlt        ! ESTABLISHES VOLTAGE LIMITS   
 
        include 'ipfinc/parametr.inc'

        include 'ipfinc/alpha.inc'
        include 'ipfinc/apcom.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/cont.inc'
        include 'ipfinc/ikk.inc'
        include 'ipfinc/intbus.inc'
        include 'ipfinc/intrst.inc'
        include 'ipfinc/prt.inc'

        common /iflag/ iflag(MAXBUS)

        logical label, ovldk1, outgk1, ovldk2, outgk2
C       
        label = .false. 
        if (irect.eq.1) go to 900   
        ivlim = 0   
        do nbx = 1, ntot_alf  
          nb = alf2inp(nbx)
          kt = inp2opt(nb)  
          if (ikk(1,kt) .eq. 0) go to 140   
          v = dsqrt (e(kt)**2 + f(kt)**2)  
          call glbvlt(nb,vmin,vmax)   
          call finzon(zone(nb), base(nb), outgk1, ovldk1)
          if (.not. outgk1 .and. .not. ovldk1) then
c
c           Set artificially high and low limits for buses not in the
c           zones/bases of interest.
c
            vlimn(kt) = amin1 (vmin, v-0.50)
            vlimx(kt) = amax1 (vmax, v+0.50)
          else
C       
C           EXAMINE ONLY RETAINED NODES 
C       

            v = dsqrt (e(kt)**2 + f(kt)**2)  
            vlimn(kt) = vmin  
            vlimx(kt) = vmax  
            dv = dim(v+0.005,vmax) - dim(vmin,v-0.005)  
            if (dv .eq. 0.0) go to 140 
            vlimn(kt) = vmin + amin1(0.0,dv)  
            vlimx(kt) = vmax + amax1(0.0,dv)  
            if (.not. label) then
              call forbtm
              write (outbuf,100)
  100         format(' Summary of busses with relaxed ',
     &               'undervoltage/overvoltage limits')
              call shdlod(1)
              write(outbuf,110)
  110         format('0 Bus      Base           Base Limits   Violations
     &  relaxed limits')
              call shdlod(2)
              write(outbuf,120)
  120         format ('                         Vmin    Vmax            
     &    Vmin    Vmax')
              call shdlod(3)
              write(outbuf,121)
  121         format (23x,'(P.U.)  (P.U.)    (P.U.)    (P.U.)  (P.U.)')
              call shdlod(4)
              outbuf = '0'
              call shdlod(5)  
              call fortop 
              label = .true.  
        
             endif   
             ivlim = ivlim + 1   
             if (ivlim .lt. 201) then  
                write(outbuf,122) intbus(kt), intbas(kt), vmin, vmax, 
     &             dv, vlimn(kt), vlimx(kt)
  122           format (1x, a8, f6.1, 6x, 2f8.3, 2x, f8.3, 2x, 2f8.3) 
                if (intrst(kt) .eq. 0) then
                   write (outbuf(67:),124)   
  124              format (' (in external equivalent system)')   
                endif
                call prtout(1)   
             endif   
           endif
  140      continue
        enddo
        
        if (label) then 
           call forbtm  
           outbuf = ' ' 
           call shdlod(1)   
           call shdlod(2)   
           do i = 3,5   
             call shdlod(i)   
           enddo
           call fortop  
        endif   
        
  900   return  
        end 
