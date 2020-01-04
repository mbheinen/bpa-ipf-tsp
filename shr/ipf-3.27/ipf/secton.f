C    @(#)secton.f	20.3 2/13/96
        subroutine secton (y,yd)
        complex y(2,2)
        double complex  yeq(3,3), z, yd(2,2)
        save
C
C*****************************************************
C
C
C       THIS SUBROUTINE CONSOLIDATES SECTIONS. THE FIRST SECTION IS
C       PROCESSED WITH
C
C               CALL FIRSEC (Y)
C
C       AND SUBSEQUENT SECTIONS WITH
C
C               CALL NEXSEC (Y)
C
C       "Y" IS THE COMPLEX 2-PORT ADMITTANCE MATRIX OF THE
C       SECTION BEING PROCESSED. IT IS NOT ALTERED BY EITHER OF
C       THE AFORE MENTIONED CALLS.
C
C       THE PI EQUIVALENT OF CUMULATIVE SECTIONS CAN BE RETRIEVED WITH
C
C               CALL FINSEC (Y)
C
C       WHERE Y NOW CONTAINS  THE COMPLEX 2-PORT ADMITTANCE
C       MATRIX.
C
c	New entry points were created to handle double precision.
c
C*****************************************************
C
C       PROCESS FIRST SECTION
C
        entry firsec (y)
        do 100 i=1,2
        do 100 j=1,2
  100   yeq(j,i) = y(j,i)
        do 110 i=1,3
        yeq(i,3) = (0.0,0.0)
        yeq(3,i) = (0.0,0.0)
  110   continue
        return
C
C       PROCESS SUBSEQUENT SECTIONS
C
        entry nexsec (y)
        do 120 i=2,3
        do 120 j=2,3
  120   yeq(j,i) = yeq(j,i) + y(j-1,i-1)
C
C       ELIMINATE "INTERIOR" NODE
C
        z=yeq(2,2)
        if (z .eq. 0.0) z = 1.0e-6
        do 130 j=1,3,2
  130   yeq(2,j)=yeq(2,j)/z
        do 140 i=1,3,2
        z=yeq(i,2)
        do 140 j=1,3,2
  140   yeq(i,j)=yeq(i,j)-z*yeq(2,j)
C
C       REORDER EQUIVALENT MATRIX
C
        yeq(1,2)=yeq(1,3)
        yeq(2,2)=yeq(3,3)
        yeq(2,1)=yeq(3,1)
        do 150 i=1,3
        yeq(3,i)=(0.0,0.0)
  150   yeq(i,3)=(0.0,0.0)
        return
C
C       RETRIEVE PI EQUIVALENT
C
        entry finsec (y)
        do 160 i=1,2
        do 160 j=1,2
  160   y(j,i)=yeq(j,i)
        return
C
C       PROCESS FIRST SECTION, DOUBLE PRECISION.
C
        entry firsecd (yd)
        do 170 i=1,2
        do 170 j=1,2
  170   yeq(j,i) = yd(j,i)
        do 180 i=1,3
        yeq(i,3) = (0.0,0.0)
        yeq(3,i) = (0.0,0.0)
  180   continue
        return
C
C       PROCESS SUBSEQUENT SECTIONS, DOUBLE PRECISION.
C
        entry nexsecd (yd)
        do 190 i=2,3
        do 190 j=2,3
  190   yeq(j,i) = yeq(j,i) + yd(j-1,i-1)
C
C       ELIMINATE "INTERIOR" NODE
C
        z=yeq(2,2)
        if (z .eq. 0.0) z = 1.0e-6
        do 200 j=1,3,2
  200   yeq(2,j)=yeq(2,j)/z
        do 210 i=1,3,2
        z=yeq(i,2)
        do 210 j=1,3,2
  210   yeq(i,j)=yeq(i,j)-z*yeq(2,j)
C
C       REORDER EQUIVALENT MATRIX
C
        yeq(1,2)=yeq(1,3)
        yeq(2,2)=yeq(3,3)
        yeq(2,1)=yeq(3,1)
        do 220 i=1,3
        yeq(3,i)=(0.0,0.0)
  220   yeq(i,3)=(0.0,0.0)
        return
C
C       RETRIEVE PI EQUIVALENT, DOUBLE PRECISION.
C
        entry finsecd (yd)
        do 230 i=1,2
        do 230 j=1,2
  230   yd(j,i)=yeq(j,i)
        return
        end
