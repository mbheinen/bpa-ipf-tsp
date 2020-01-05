C    @(#)abcd.f	20.3 2/13/96
        subroutine abcd (v,i,y)
C
C       This subroutine emulates the ABCD coefficients for a
C       section. It calculates v(2) and i(2) given v(1), i(1),
C       and y(*,*).
C
        double complex v(2), i(2), y(2,2)
        double complex v1, v2, i1, y11, y12, y21, y22
 
        v1 = dcmplx(dreal(v(1)) , dimag(v(1)))
        v2 = dcmplx(dreal(v(2)) , dimag(v(2)))
        i1 = dcmplx(dreal(i(1)) , dimag(i(1)))
        y11 = dcmplx(dreal(y(1,1)) , dimag(y(1,1)))
        y12 = dcmplx(dreal(y(1,2)) , dimag(y(1,2)))
        y21 = dcmplx(dreal(y(2,1)) , dimag(y(2,1)))
        y22 = dcmplx(dreal(y(2,2)) , dimag(y(2,2)))

        v(2) = ( i1 - y11 ) * v1 / y12
        i(2) = y21 * v1  +  y22 * v2
 
        return
        end
