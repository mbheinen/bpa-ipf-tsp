C    @(#)kmp_ptiz.f	20.14 5/3/00
        integer function kmp_ptiz (i,j)  
  
        include 'ipfinc/parametr.inc'  
        include 'ipfinc/bus.inc'   
        include 'ipfinc/arcntl.inc'   
        include 'ipfinc/pti_data.inc'   
        include 'ipfinc/qsdup.inc'  
        include 'ipfinc/qksrt.inc'   
  
        common /scratch/ nbr, array(4,MAXBUS)  
        integer array  
  
        i1 = array(1,i)  
        j1 = array(1,j)  
        i2 = array(2,i)  
        j2 = array(2,j)  
c           
c       key = sorts on the following fields:  
c             1. bus(zone)  
c             2. population  
c             3. area  
c             4. pti_znum  
c             5. pti_anum  
c             6. pti_znum, zone-area population, pti_anum
c             6. pti_onum
c  
        if (key .eq. 1) then  
          i3 = array(3,i)  
          j3 = array(3,j)  
          kmp_ptiz = kompr (zone(i1), zone(j1), junk)  
          if (kmp_ptiz .eq. 0) then  
            kmp_ptiz = i3 - j3  
          endif    
          if (kmp_ptiz .eq. 0) then  
            if (i2 .eq. 0 .or. j2 .eq. 0) then  
              kmp_ptiz = i2 - j2  
            else if (i2 .lt. 19999 .and. j2 .lt. 19999) then  
              kmp_ptiz = kompr (arcnam(i2), arcnam(j2), junk)  
            else  
              kmp_ptiz = i2 - j2  
            endif  
          endif  
        else if (key .eq. 2) then  
c           
c         key = 2 sorts on the following fields:  
c             ! 1. zone  
c             ! 2. area  
c  
          if (i1 .lt. 19999 .and. j1 .lt. 19999) then  
             kmp_ptiz = kompr (acznam(i1), acznam(j1), junk)  
             if (kmp_ptiz .eq. 0) then  
               i2 = array(2,i)  
               j2 = array(2,j)  
               if (i2 .eq. 0 .or. j2 .eq. 0) then  
                 kmp_ptiz = i2 - j2  
               else if (i2 .lt. 19999 .and. j2 .lt. 19999) then  
                 kmp_ptiz = kompr (arcnam(i2), arcnam(j2), junk)  
               else  
                 kmp_ptiz = i2 - j2  
               endif  
             endif  
          else  
             kmp_ptiz = i1 - j1  
             if (kmp_ptiz .eq. 0) then  
               kmp_ptiz = i2 - j2  
             endif  
          endif  
        else if (key .eq. 3) then  
          kmp_ptiz = i1 - j1  
          if (kmp_ptiz .eq. 0) then  
            kmp_ptiz = i2 - j2  
          endif  
        else if (key .eq. 4) then  
          kmp_ptiz = pti_znum(i1) - pti_znum(j1)  
        else if (key .eq. 5) then  
          kmp_ptiz = pti_anum(i1) - pti_anum(j1)  
        else if (key .eq. 6) then  
c           
c         key = 6 sorts on the following fields:  
c             ! 1. zone  
c             ! 2. zone-area population
c             ! 3. area  
c  
          kmp_ptiz = i1 - j1
          if (kmp_ptiz .eq. 0) kmp_ptiz = array(3,i) - array(3,j)
          if (kmp_ptiz .eq. 0) kmp_ptiz = array(2,i) - array(2,j)
        else if (key .eq. 7) then  
          kmp_ptiz = pti_onum(i1) - pti_onum(j1)  
        endif  
        if ((kmp_ptiz .eq. 0) .and. (i .ne. j)) dupsw = .true.  
        return  
        end  
