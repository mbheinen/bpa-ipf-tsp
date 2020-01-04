C    @(#)shellsrt.f	20.3 2/13/96
        subroutine shellsrt (m, n, kompar, swap)   

        external kompar, swap

        integer kompar
        logical sorted, done

        sorted = .false.   
        level = 0      

        do while (.not.sorted)
           num = n - m + 1       
           do while (num .gt. 1)
              num = num / 2     
              k = n - num     
              j = m          
              do while (j .le. k)
                 i = j    
                 done = .false.
                 do while ( i .ge. m  .and.  .not. done )
                    if (kompar(i,i+num) .gt. 0) then
                       call swap(i,i+num)   
                       i = i - num  
                    else
                       done = .true.
                    endif
                 enddo
                 j = j + 1     
              enddo
           enddo
           sorted = .true.
        end do
        return
        end 
