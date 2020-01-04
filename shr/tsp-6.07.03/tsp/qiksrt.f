C    %W% %G%
        subroutine qiksrt(mm, nn, kompar, swap)
        external kompar, swap
C
C       Algorithm 271 (QUICKERSORT BY R.S. SCOWEN,)
C               MAR. 1965 COMMUNICATIONS OF THE ACM
C
C
C       This procedure uses a method similar to that of QUICKSORT.
C       by C.A.R. Hoare (Algorithms 63,64, COMM.ACM 4 JULY 1961)
C
C       Modified to use shell sort on partitions of 15 or less.
C               (1/18/79 by A.H. Schmidt,Jr.)
C
        dimension mstack(20), nstack(20)
        logical sorted, exchg, cont, cont1, cont2
C
        m=mm
        n=nn
        sorted=.false.
        level=0
C
C       REPEAT-UNTIL(SORTED) PARTITION-AND-SORT
C
        do while (.not.sorted) 
C
C          TO PARTITION-AND-SORT
C
           num=n-m+1
C
           if (num .gt. 15) then

C             PARTITION-THE-ARRAY
C
              i = m
              j = n
              k = (m + n) / 2
C
              do while (i .lt. j)
C
C                LOCATE ITEM AT I END THAT SHOULD GO TO J END

                 cont = .true.
                 do while (i .lt. n .and. cont) 
                    if (kompar(i,k) .le. 0) then
                       i = i + 1
                    else
                       cont = .false.
                    endif
                 enddo
C
C                LOCATE ITEM AT J END THAT SHOULD GO TO I END
C
                 cont = .true.
                 do while (j .gt. m .and. cont) 
                    if (kompar(j,k) .ge. 0) then
                       j = j - 1
                    else
                       cont = .false.
                    endif
                 enddo
C
C                SWAP MISPOSITIONED ITEMS
C
                 if (i .lt. j) then
                    call swap(i,j)
                    j = j - 1
                    i = i + 1

                 else if (i .lt. k) then
                    call swap (i,k)
                    i = i + 1

                 else if (j .gt. k) then
                    call swap (j,k)
                    j = j - 1

                 endif
              enddo

C             PUSH-LARGEST-PARTITION-ON-STACK
C
              level = level + 1

              if (j-m .lt. n-i) then
                 mstack(level) = i
                 nstack(level) = n
                 n = j

              else
                 mstack(level) = m
                 nstack(level) = j
                 m = i

              endif

           else

C             SHELL-SORT-PARTITION-OF-15-OR-LESS

              do while (num .gt. 1)
                 num = num / 2
                 k = n - num
                 j = m
                 cont1 = .true.
                 do while (cont1)
                    i = j
                    cont2 = .true.
                    do while (cont2)
                       exchg = .false.
                       if (kompar(i,i+num) .gt. 0) then
                          call swap(i,i+num)
                          exchg = .true.
                          i = i - num
                       endif
                       cont2 = (exchg .and. i .ge. m)
                    enddo
                    j = j + 1
                    cont1 = (j .le. k)
                 enddo
              enddo

              if (level .gt. 0) then

C                POP-NEXT-PORTION-FROM-STACK

                 m = mstack(level)
                 n = nstack(level)
                 level = level - 1
              else
                 sorted = .true.
              endif
           endif
        enddo
        return
        end
