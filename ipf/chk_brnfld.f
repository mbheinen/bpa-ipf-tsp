C    @(#)chk_brnfld.f	20.1 2/13/96
        integer function chk_brnfld (text, ptr, count, out_buffer)
        integer ptr, count
        character text*(*), out_buffer(10)*120
c
c       Check branch fields for suspicious entitites
c
        include 'ipfinc/parametr.inc'

        include 'ipfinc/branch.inc'
        include 'ipfinc/prt.inc'

        character fieldfmt(57)*10, brn_type(9)*2, datein*3, 
     &            dateot*3
     
        integer fieldptr(10), field(4,57), type, gtbrtype, status,
     &          chk_brndta
        logical extrat 

        data brn_type /'E*','LM','L ','R ','T ','TP','LD','E ','RZ'/
        data fieldptr / 1, 1, 6, 13, 18, 26, 34, 43, 51, 58 /
        data field /
c
c          Type E* (1)  (NULL)
c
c          Type LM (2)
c
     &      1, 34, 37,  4, 1, 38, 43,  5, 1, 44, 49,  6, 1, 50, 55,  7,
     &      1, 71, 74, 18,
c
c          Type L (3)
c
     &      1, 34, 37,  4, 1, 38, 38, 16, 1, 39, 44,  5, 1, 45, 50,  6,
     &      1, 51, 56,  7, 1, 57, 62,  8, 1, 63, 66,  9,
c
c          Type R (4)
c
     &      1, 46, 50,  6, 1, 51, 55,  7, 1, 56, 57,  8, 1, 58, 62,  9,
     &      1, 63, 67, 10,
c
c          Type T (5)
c
     &      1, 34, 37,  4, 1, 38, 38, 16, 1, 39, 44,  5, 1, 45, 50,  6,
     &      1, 51, 56,  7, 1, 57, 62,  8, 1, 63, 67,  9, 1, 68, 72, 10,
c
c          Type TP (6)
c
     &      1, 34, 37,  4, 1, 38, 38, 16, 1, 39, 44,  5, 1, 45, 50,  6,
     &      1, 51, 56,  7, 1, 57, 62,  8, 1, 63, 67,  9, 1, 68, 72, 10,
c
c          Type LD (7)
c
     &      1, 34, 37,  4, 1, 38, 43,  5, 1, 44, 49,  6, 1, 50, 55,  7,
     &      1, 57, 61,  8, 1, 62, 66,  9, 1, 67, 70, 10, 1, 71, 74, 18,
     &      1, 75, 78, 16,
c
c          Type E (8)
c
     &      1, 34, 37,  4, 1, 38, 38, 16, 1, 39, 44,  5, 1, 45, 50,  6,
     &      1, 51, 56,  7, 1, 57, 62,  8, 1, 63, 68,  9, 1, 69, 74, 10,
c
c          Type RZ (9) 
c
     &      1, 35, 39,  5, 1, 40, 44,  6, 1, 45, 48,  7, 1, 49, 54,  8, 
     &      1, 55, 60,  9, 1, 61, 66, 10, 1, 67, 72, 18 /

        data fieldfmt /
c
c          Type E* (1)  (NULL)
c
c          Type LM (2)
c
     &      '(bz, f4.0)', '(bz, f6.2)', '(bz, f6.2)', '(bz, f6.2)', 
     &      '(bz, f4.0)',
c
c          Type L (3)
c
     &      '(bz, f4.0)', '(bz, f1.0)', '(bz, f6.5)', '(bz, f6.5)', 
     &      '(bz, f6.5)', '(bz, f6.5)', '(bz, f4.1)', 
c
c          Type R (4)
c
     &      '(bz, f5.2)', '(bz, f5.2)', '(bz, f2.0)', '(bz, f5.0)', 
     &      '(bz, f5.0)',
c
c          Type T (5)
c
     &      '(bz, f4.0)', '(bz, f1.0)', '(bz, f6.5)', '(bz, f6.5)', 
     &      '(bz, f6.5)', '(bz, f6.5)', '(bz, f5.2)', '(bz, f5.2)',
c
c          Type TP (6)
c
     &      '(bz, f4.0)', '(bz, f1.0)', '(bz, f6.5)', '(bz, f6.5)', 
     &      '(bz, f6.5)', '(bz, f6.5)', '(bz, f5.2)', '(bz, f5.2)',
c
c          Type LD (7)
c
     &      '(bz, f4.0)', '(bz, f6.2)', '(bz, f6.2)', '(bz, f6.2)', 
     &      '(bz, f5.1)', '(bz, f5.1)', '(bz, f4.1)', '(bz, f4.1)', 
     &      '(bz, f4.0)',
c
c          Type E (8)
c
     &      '(bz, f4.0)', '(bz, f1.0)', '(bz, f6.5)', '(bz, f6.5)', 
     &      '(bz, f6.5)', '(bz, f6.5)', '(bz, f6.5)', '(bz, f6.5)',
c
c          Type RZ (9) 
c
     &      '(bz, f5.0)', '(bz, f5.0)', '(bz, f4.0)', '(bz, f6.5)', 
     &      '(bz, f6.5)', '(bz, f6.5)', '(bz, f6.5)'  /

        type = gtbrtype(text(1:2))
        chk_brnfld = 0
        new_count = count + 1
        out_buffer(count+1) = ' '
c
c       Perform table driven field checks
c
        do i = fieldptr(type), fieldptr(type+1)-1

          i1 = field(1,i)
          i2 = field(2,i)
          i3 = field(3,i)
          i4 = field(4,i)

          if (text(i2:i3) .ne. ' ') then
            if (index(fieldfmt(i), 'f') .ne. 0) then
c
c             Floating point field
c
              if (text(i3:i3) .eq. ' ' .and.
     &            index(text(i2:i3), '.') .eq. 0) then
                out_buffer(count+1)(i2+1:i3+1) = '*********'
                chk_brnfld = 1
              endif
            else
c
c             Integer or character decoding
c  
              if (text(i3:i3) .eq. ' ') then
                out_buffer(count+1)(i2+1:i3+1) = '*********'
                chk_brnfld = 1
              endif
            endif
            status = chk_brndta (ptr, i2, new_count, out_buffer)
          endif
        enddo
c                                                                       
c       Check date 
c                                                                       
        if (type .ne. 1) then
          datein = text(75:77)        
          dateot = text(78:80)        
          yearin = energd (datein)       
          if (yearin .eq. -9999.0) then                                   
            out_buffer(count+1)(75:77) = '*********'
            chk_brnfld = 1
          endif                           
          yearot = denerg (dateot)    
          if (yearot .eq. -9999.0) then     
            out_buffer(count+1)(78:80) = '*********'
            chk_brnfld = 1
          endif                    
        endif
C 
C       Check extended branch ratings
C 
        extrat = .false.
        if (type .eq. 2 .or. type .eq. 3 .or.
     &      type .eq. 7 .or. type .eq. 8) then
          if (text(81:84) .ne. ' ' .or.
     &        text(85:88) .ne. ' ') then
            extrat = .true.
          endif
        else if (type .eq. 5 .or. type .eq. 6) then
          if (text(81:84) .ne. ' ' .or.
     &        text(85:88) .ne. ' ' .or.
     &        text(89:92) .ne. ' ') then
            extrat = .true.
          endif
        endif
        if (extrat) then
          do i = 1, 3
            j1 = 77 + 4*i
            j2 = j1 + 3
            if (text(j1:j2) .ne. ' ') then
c
c             Floating point field
c
              if (text(j2:j2) .eq. ' ' .and.
     &            index(text(j1:j2), '.') .eq. 0) then
                out_buffer(count+1)(j1:j2) = '*********'
                chk_brnfld = 1
              endif
              status = chk_brndta (ptr, j1, new_count, out_buffer)
            endif
          enddo
        endif 
        if (chk_brnfld .eq. 1) then
          count = new_count
        else if (new_count .gt. count+1) then
          do i = 1, new_count-1
            out_buffer(i) = out_buffer(i+1)
          enddo
          count = new_count - 1
        endif
 
        return
        end
