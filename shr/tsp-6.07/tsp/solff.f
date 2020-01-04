C    %W% %G%
      subroutine solff (a)                              !
C                                                                       
C     This subroutine calculates the field voltage for IEEE model FF            
C                                                                       
      include 'tspinc/vrgov.inc'			! lppwr	 
      include 'tspinc/params.inc'                       ! parameters
      include 'tspinc/gentbla.inc'                      ! datat 
      include 'tspinc/param.inc'
      include 'tspinc/comvar.inc'                       ! cfd,x7,x70,vtd,vtq,x3
      include 'tspinc/gentblb.inc'                      ! oid,oiq,satd 
      include 'tspinc/lnk12.inc'
      include 'tspinc/ecsind.inc'
      include 'tspinc/lnk1a.inc'

      dimension array(40), a(40)                                         
      equivalence          (array(1),    vrmax), (array(2),  vrmin),      
     1 (array(3),   vref), (array(4),    esatx), (array(5),  csatx),      
     2 (array(6),    hbr), (array(7),      cke), (array(8),    cka),      
     3 (array(9),     dr), (array(10),   cfldo), (array(11),   ca6),      
     4 (array(12),    df), (array(13),     akf), (array(14),   hbf),      
     5 (array(15),   hcb), (array(16),     ckc), (array(17),    db),      
     6 (array(18),    dc), (array(19),     hbe), (array(20),    de),      
     7 (array(21),  rcex), (array(22), cfldpst), (array(23),  xcex),      
     8 (array(24), x7pst), (array(25),      da), (array(26),   hba),      
     9 (array(27),   vco), (array(28),   vamax), (array(29),   efd),      
     * (array(30),   ckd), (array(31),   vcpst), (array(32),    a3),      
     * (array(33), vamin), (array(34),     veo), (array(35),   ckb),      
     * (array(36), spare), (array(37),      a5), (array(38),    a6),      
     * (array(39),vfemax), (array(40),     ckh)                           
                             
      do itr = 1,40                                     ! 
        array(itr) = a(itr)				! Transfer citer to
      enddo                                             ! array.

      zcd = rcex*oid - xcex*oiq                         ! ---
      zcq = rcex*oiq + xcex*oid                         !
      vcd = zcd + vtd                                   ! Calculate voltage
      vcq = zcq + vtq                                   ! transducer circuit
      vc  = sqrt(vcd**2 + vcq**2)                       ! ---
                                                                        
      cfld = vtq + ra*oiq + oid*(xp +  (xd - xp)        ! ---
     &                               /(1.0 + satd) )    ! Calculate
      if (mgen .ge. 6) cfld = cfd                       ! field current

      if (lppwr .eq. 0) then                            ! Start lppwr=0 case

        se = csatx * exp (esatx * abs(veo)) 		! ---
        ck1 = se * (1.0 + veo * esatx)                  !           
        ck2 = veo**2 * esatx * se                       !         
        vc1 = (hbr + vco)/dr                            !             
        vfe = (cke + ck1)*veo + ckd*cfldo - ck2         !              
        vf = (vfe*akf + hbf)/df                         ! Calculate states
        vai = x7o - vf + vref - vc1                     ! for previous time
        vaii = (dc*vai + hcb)/db                        ! using converged
        va = (cka*vaii + hba)/da                        ! values of veo, x7
        if (va.gt.vamax) va = vamax                     ! vc, and cfld.
        if (va.lt.vamin) va = vamin                     !         
        x3 = va                                         !             
        vr = ckb*va - ckh*ckb*vfe                       !             
        if (vr.gt.vrmax) vr = vrmax                     !         
        if (vr.lt.vrmin) vr = vrmin                     ! ---

        hbr = (dr - 2.0)*vc1 + vco                      ! ---   
        hcb = (db - 2.0)*vaii - (dc - 2.0)*vai          !             
        hba = (da - 2.0)*va + cka*vaii                  ! Update past values
        hbe = de*veo + vr - vfe                         !             
        hbf = (df - 2.0)*vf - akf*vfe                   ! ---   

        a2 = cka*ckb*dc/(da*db)                         ! ---
        a1 = hbe + ckb/da*(hba + cka/db*hcb) - a2/df*hbf! update constants
        a4 = (a1 + a3*ck2) / (de + a3*(ck1 + cke))      ! ---

        dtsk = 0.1                                      ! ---
        ndiv = edt/dtsk                                 ! Assign values to be
        delx7 = (x7o - x7pst)*dtsk/edt                  ! used in sub time
        delvc = (vco - vcpst)*dtsk/edt                  ! loop.
        delcfld = (cfldo - cfldpst)*dtsk/edt            ! ---

        do itr = 1, ndiv-1                              ! Start sub time loop

          if (idsw.ge.3.and.idsw.le.5) then
            x7n = x7                                    ! at network
            vcn = vc                                    ! discontinuities
            cfldn = cfld                                ! use new values
          else
            x7n = x7o + itr*delx7                       ! Extrapolate input 
            vcn = vco + itr*delvc                       ! from values at prev-
            cfldn = cfldo + itr*delcfld                 ! ious two time steps.
          end if

          se = csatx * exp (esatx * abs(veo )) 		! ---
          ck1 = se * (1.0 + veo  * esatx)               ! Calculate se,ck1,ck2
          ck2 = veo **2 * esatx * se                    ! ---     

          vc1 = (vcn + hbr)/dr                          ! Calculate
          veo = a4 + a5*(vref + x7n - vc1) - a6*cfldn   ! output
      
          vemax1 = (vfemax - ckd*cfldn)/(cke + se)      ! ---                  
          vemin1 = 0.0                                  ! Check limit on output
          if (veo.gt.vemax1) veo = vemax1               ! due to vfemax
          if (veo.lt.vemin1) veo = vemin1               ! ---

          at3    = ckh*ckb + 1.0                        ! ---
          denom  = de + at3*(ck1 + cke)                 ! 
          vemax  = (hbe + ckb*vamax + at3*ck2)          !
          vemin  = (hbe + ckb*vamin + at3*ck2)          ! Check limit on output
          vemax2 = (vemax - at3*ckd*cfldn) / denom      ! due to vamax/vamin
          vemin2 = (vemin - at3*ckd*cfldn) / denom      !
          if (veo.gt.vemax2) veo = vemax2               !
          if (veo.lt.vemin2) veo = vemin2               ! ---

          denom = de + cke + ck1                        ! ---
          vemax3 = (hbe - ckd*cfldn + ck2 + vrmax)/denom!
          vemin3 = (hbe - ckd*cfldn + ck2 + vrmin)/denom! check limit on output
          if (veo.gt.vemax3) veo = vemax3               ! due to vrmax/vrmin
          if (veo.lt.vemin3) veo = vemin3               ! ---

          vc1 = (hbr + vcn)/dr                          ! ---
          vfe = (cke + ck1)*veo + ckd*cfldn - ck2       !
          vf = (vfe*akf + hbf)/df                       !
          vai = x7n - vf + vref - vc1                   !
          vaii = (dc*vai + hcb)/db                      ! Calculate states
          va = (cka*vaii + hba)/da                      ! inside the sub time
          if (va.gt.vamax) va = vamax                   ! loop.
          if (va.lt.vamin) va = vamin                   ! 
          x3 = va                                       !
          vr = ckb*va - ckh*ckb*vfe                     !
          if (vr.gt.vrmax) vr = vrmax                   !
          if (vr.lt.vrmin) vr = vrmin                   ! ---

          hbr = (dr - 2.0)*vc1 + vcn                    ! ---
          hcb = (db - 2.0)*vaii - (dc - 2.0)*vai        ! Update past value
          hba = (da - 2.0)*va + cka*vaii                ! parameters in the
          hbe = de*veo + vr - vfe                       ! sub time loop
          hbf = (df - 2.0)*vf - akf*vfe                 ! ---

          a2 = cka*ckb*dc/(da*db)                       ! ---
          a1 = hbe + ckb/da*(hba+ cka/db*hcb)- a2/df*hbf! update coefficients
          a4 = (a1 + a3*ck2) / (de + a3*(ck1 + cke))    ! ---

        end do                                          ! end sub time loop

        efdo  = efd                                     ! ---
        vcpst = vco					!     
        vco   = vc                                      !
        x7pst = x7o                                     ! Store input values
        cfldpst = cfldo                                 ! 
        cfldo = cfld                                    ! ---

      endif                                             ! end lppwr=0 case
                                                                        
      se = csatx * exp (esatx * abs(veo))               ! ---
      ck1 = (1.0 + esatx*veo) * se                      ! Calculate se,ck1,ck2
      ck2 = veo**2 * esatx * se                         ! ---

      vc1 = (vc + hbr)/dr                               ! Calculate new
      veo = a4 + a5*(vref + x7 - vc1) - a6*cfld         ! output

      vemax1 = (vfemax - ckd*cfld)/(cke + se)           ! ---
      vemin1 = 0.0                                      ! Check limit on output
      if (veo.gt.vemax1) veo = vemax1                   ! due to vfemax
      if (veo.lt.vemin1) veo = vemin1                   ! ---

      at3    = ckh*ckb + 1.0                            ! ---
      denom  = de + at3*(ck1 + cke)                     ! 
      vemax  = (hbe + ckb*vamax + at3*ck2)              !
      vemin  = (hbe + ckb*vamin + at3*ck2)              ! Check limit on output
      vemax2 = (vemax - at3*ckd*cfld) / denom           ! due to vamax/vamin
      vemin2 = (vemin - at3*ckd*cfld) / denom           !
      if (veo.gt.vemax2) veo = vemax2                   !
      if (veo.lt.vemin2) veo = vemin2                   ! ---
                                                         
      denom = de + cke + ck1                            ! ---
      vemax3 = (hbe - ckd*cfld + ck2 + vrmax) / denom   !
      vemin3 = (hbe - ckd*cfld + ck2 + vrmin) / denom   ! Check limit on output
      if (veo.gt.vemax3) veo = vemax3                   ! due to vrmax/vrmin
      if (veo.lt.vemin3) veo = vemin3                   ! ---


      if (veo .gt. 0.0) then                            ! ---
        cin = abs (ckc*cfld/veo)                        !
      else                                              !
        cin = 0.0                                       !
      endif                                             !
      if (cin .le. 0.51) then                           !
         fex = 1.0 - 0.58*cin                           !
      else if (cin .lt. 0.715) then                     ! Calculate Fex
         fex = -0.865*(cin + 0.00826)**2 + 0.93233      !
      else if (cin .lt. 0.9802) then                    !
         fex = 1.68 - 1.714*cin                         !
      else                                              !
         fex = 0.0                                      !
      endif                                             ! ---

      efd = veo*fex                                     ! Calculate field Volt
  
      do itr = 1,40                                     !
        a(itr) = array(itr)                             ! Transfer array to
      enddo                                             ! citer

      return                                                            
      end                                                               
