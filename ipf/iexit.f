C    @(#)iexit.f	20.3 2/13/96
      subroutine iexit (jk)
 
 
      include 'ipfinc/parametr.inc'
 
      include 'ipfinc/aref.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/smallp.inc'
 
 
 
 9001 format (1h0,'OPTIMUM')
 9002 format (1h0,'INFEASIBLE.')
 9003 format (1h0,'UNBOUNDED.')
 9004 format (1h0,'THE MAXIMUM SIZE OF THE INVERSE HAS BEEN EXCEEDED')
 9005 format (1h0,'THE MAXIMUM NUMBER OF ITERATIONS HAS BEEN REACHED')
 9006 format (1h0,'EITHER THE AA VECTOR IS FULL WITH',i6,
     1            ' ELEMENTS, OR THE NUMBER OF CONSTRAINTS IS ABOUT ',
     2            'TO EXCEED',i6,'.'/1x,'THE PROGRAM IS TRYING TO ',
     3            'COPY ROW',i6,' WHICH IS AS FOLLOWS...')
 9007 format (1h0,'STILL INACCURATE AFTER ',i5,' REINVERSIONS.')
 9008 format (1h0,'INTEGER PROGRAM OPTIMUM.')
 9009 format (1h0,'QUADRATIC PROGRAM OPTIMUM.')
 9011 format (1h0,'THE NEGINV ROW OF THE UPDATED A MATRIX IS AS ',
     1            'FOLLOWS...')
 9012 format (8(1x,f14.7))
 9013 format (1h0,'THE VECTOR GR IS AS FOLLOWS...')
 9014 format (1h0,'THE VECTOR G IS AS FOLLOWS...')
 9015 format (/////)
 
      if (jk .eq. 2 .or. jk .eq. 3) morepr = 1
      write (dbug,9015)
      isdone = 1
      go to (1,2,3,4,5,6,7,8,9),jk

    1 write (dbug,9001)
      go to 10

    2 write (dbug,9002)
      write (dbug,9011)
      write (dbug,9012)(piv(j),j=1,n)
      go to 10

    3 write (dbug,9003)
      write (dbug,9013)
      write (dbug,9012)(gr(k),k=1,size)
      write (dbug,9014)
      write (dbug,9012)(g(i),i=1,mnow)
      go to 10

    4 write (dbug,9004)
      go to 10

    5 write (dbug,9005)
      go to 10

    6 write (dbug,9006) MAXA,MAXM,mnow
      write (dbug,9012)(piv(j),j=1,n)
      go to 10

    7 write (dbug,9007) ir
      go to 10

    8 write (dbug,9008)
      go to 10

    9 write (dbug,9009)

   10 if (mnow .ge. m) call iprint
      return
      end
