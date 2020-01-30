C    @(#)cnvtwrd.f	20.3 2/13/96
        character*(*) function cnvtwrd (word)
C
        character*(*) word
        character capital * 80
c
        cnvtwrd = capital(word)
c
        do while (index (cnvtwrd, '_') .ne. 0)
           i = index (cnvtwrd, '_')
           cnvtwrd(i:) = cnvtwrd(i+1:) 
        enddo
c
        return
        end
