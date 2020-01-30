C    @(#)komcbx.f	20.3 2/13/96
        function komcbx(m,n)
C
C       Compare customer buses M and N
C
        common /scratch/ ntemp, ktemp(12,100), temp_index(100)
        real temp(12,100)
        integer temp_index
        equivalence (temp, ktemp)

        include 'ipfinc/qsdup.inc'
C
        character strng1 * 6, strng2 * 6
 
        call getchr(1,strng1(1:1),ktemp(8,m))
        call getchr(2,strng1(5:6),ktemp(9,m))
        call getchr(3,strng1(2:4),ktemp(10,m))
 
        call getchr(1,strng2(1:1),ktemp(8,n))
        call getchr(2,strng2(5:6),ktemp(9,n))
        call getchr(3,strng2(2:4),ktemp(10,n))
 
        if (kompr (strng1, strng2, komcbx) .eq. 0) then
           if (m .ne. n) dupsw = .true.
        endif
c
        return
        end
