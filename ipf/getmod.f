C    @(#)getmod.f	20.3 2/13/96
        character*120 function getmod (text1, text2)
        implicit none
        character *(*) text1, text2
C
C       This character function performs data modification changes by
C       blanking matching data fields and utilizing non-blank data
C       fields in TEXT2.
C
        character errmsg*80, temp*120
        integer pointer1(7), pointer2(3), pointer3(7), field_1(2,67), 
     &          field_2(2,30), field_3(2,74), lenmin, i1, i2, i, type, 
     &          error
        
        data pointer1 / 1, 16, 17, 33, 43, 58, 68 /
        data pointer2 / 1, 20, 31 /
        data pointer3 / 1, 15, 28, 41, 50, 60, 75 /
        data field_1 / 
c
c         A records
c
     &    14,21,   22,25,   27,34,   36,37,  39,40,   42,43,   45,46,
     &    48,49,   51,52,   54,55,   57,58,  60,61,   63,64,   73,76,
     &    77,80,
c
c         I records
c
     &    27,34,
c
c         B records (a-c buses)
c
     &     2,2,     4, 6,   19,20,   21,25,  26,30,   31,34,   35,38,
     &    39,42,   43,47,   48,52,   53,57,  58,61,   62,65,   66,73,
     &    74,77,   78,80,
c
c         BD records
c
     &     4,6,    19,20,   24,25,  26,30,   31,35,   36,40, 41,45,   
     &    46,50,   51,58,   59,62,
c
c         BM records
c
     &     4, 6,   19,20,   24,25,  26,30,   31,35,   36,40,   41,45,   
     &    46,50,   51,58,   59,62,  63,63,   64,66,   67,69,   70,75,
     &    76,80,
c
c         + records
c
     &    19,20,   21,25,  26,30,   31,34,   35,38,  39,42,   43,47,   
     &    48,52,   53,57,  75,77 /

        data field_2 / 
c
c         X records
c
     &     4, 6,   21,28,   29,32,  33,33,   34,38,   39,39,  40,44,   
     &    45,45,   46,50,   51,51,  52,56,   57,57,   58,62,  63,63,   
     &    64,64,   69,69,   70,74,  75,75,   76,80,
c
c         Q records
c
     &     4, 6,   19,24,   25,30,  31,36,   37,42,   43,48,  49,54,   
     &    55,60,   61,66,   67,72,  73,78 /
                                                                               
      data field_3 /
c
c         E records (a-c buses)
c
     &     4, 6,   19,19,   34,37,   38,38,   39,44,   45,50,   51,56,
     &    57,62,   63,68,   69,74,   75,77,   78,80,   81,84,   84,88,
c
c         L records (a-c buses)
c
     &     4, 6,   19,19,   34,37,   38,38,   39,44,   45,50,   51,56,
     &    57,62,   63,66,   65,67,   68,80,   81,84,   84,88,
c
c         LD records
c
     &     4, 6,   34,37,   38,43,   44,49,   50,55,   56,56,  57,61,
     &    62,66,   67,70,   71,74,   75,80,   81,84,   84,88,
c
c         LM records
c
     &     4, 6,   34,37,   38,43,   44,49,   50,55,   71,74,   75,80,   
     &    81,84,   84,88,
c
c         R records
c
     &     2, 2,    4, 6,   19,19,   34,41,   42,45,   46,50,   51,55,   
     &    56,57,   58,62,   63,67,
c
c         T records
c
     &     4, 6,   19,19,   34,37,   38,38,   39,44,   45,50,   51,56,
     &    57,62,   63,67,   68,72,   75,77,   78,80,   81,84,   84,88,
     &    89,92 /
C
C       Initialize 
C
        temp = text2
        temp(3:3) = 'M'
        lenmin = min (len (text1), len (text2))
        if (text1(1:1) .ne. text2(1:1)) then
           errmsg = ' Illegal record type change ' // text1(1:40)
           write (*, '(a)') errmsg
        else if (index ('AIB+XQ', text1(1:1)) .ne. 0) then
           if (text1(1:2) .eq. 'BD') then
              type = 4
           else if (text1(1:2) .eq. 'BM') then
              type = 5
           else
              type = index ('AIB$$+XQ', text1(1:1))
           endif
           do i = pointer1(type), pointer1(type+1)-1
              i1 = field_1(1,i)
              i2 = field_1(2,i)
              if (i2 .le. lenmin) then
                 if (text1(i1:i2) .eq. text2(i1:i2)) then
                    temp(i1:i2) = ' '
                 else
                    temp(i1:i2) = text2(i1:i2)
                 endif
              endif
           enddo
        else if (index ('LERT', text1(1:1)) .ne. 0) then
           if (text1(1:2) .eq. 'LD') then
              type = 3
           else if (text1(1:2) .eq. 'LM') then
              type = 4
           else
              type = index ('EL$$RT', text1(1:1))
           endif
           do i = pointer1(type), pointer1(type+1)-1
              i1 = field_1(1,i)
              i2 = field_1(2,i)
              if (i2 .le. lenmin) then
                 if (text1(i1:i2) .eq. text2(i1:i2)) then
                    temp(i1:i2) = ' '
                 else
                    temp(i1:i2) = text2(i1:i2)
                 endif
              endif
           enddo
        else
           errmsg = ' Unrecognized record type ' // text1(1:40)
           write (*, '(a)') errmsg
        endif
  900   getmod = temp
        return
        end
