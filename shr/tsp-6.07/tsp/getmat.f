C    %W% %G%
        subroutine getmat (row, rowlen)
        integer row, rowlen
C                                                                       
C       This subroutine loads the admittance matrix row for bus i       
c       into table matrow.  It is called by BRAKE, CNTRL, GENDROP,      
C       INITL4, LLDROP, MATMOD, REDUCE, RELAY.                          
C                                                                       
        include 'tspinc/params.inc' 
        include 'tspinc/matrow.inc' 

        dimension imat(2,MAXBUS), mat(16*MAXBUS)
        integer count 

        save 

        data count / 0 /

        i1 = imat(1,row)                                                   
        i2 = imat(2,row)                                                   
        rowlen = 0
        do l = i1, i2
           rowlen = rowlen + 1
           matrow(rowlen) = mat(l)                                          
        enddo
        return                                                          

        entry putmat(row, rowlen)                                         
C                                                                       
c       Determine whether new row can overwrite old row
c
        if (imat(2,row) - imat(1,row) + 1 .ge. rowlen) then
           imat(2,row) = imat(1,row) + rowlen - 1
           ix = imat(1,row) - 1
           do l = 1, rowlen                                                    
              mat(ix+l) = matrow(l)                                          
           enddo
        else
           imat(1,row) = count + 1
           imat(2,row) = count + rowlen 
           do l = 1, rowlen                                                    
              count = count + 1
              mat(count) = matrow(l)                                          
           enddo
        endif
        return                                                          

        entry initmat
        do l = 1, MAXBUS
           imat(1,l) = 1
           imat(2,l) = 0
        enddo
        return

        end                                                             
