C    @(#)intdte.f	20.3 2/13/96
        function intdte(month,year)
C
C       THIS FUNCTION TAKES THE MONTH AS ONE CHARACTER 1-9,O,N,D
C       TO REQUEST THE 12 MONTHS OF THE YEAR, AND THE YEAR AS 2 INTEGERS
C       AND RETURNS A COMBINED YRMM
C
C          IE,MONTH='O',YEAR=79
C
C             INTDTE('O',79) WILL EQUAL 7910
C
        character month *1
        integer year
C
        imo = index( '123456789OND',month )
        intdte=100*year+imo
        return
        end
