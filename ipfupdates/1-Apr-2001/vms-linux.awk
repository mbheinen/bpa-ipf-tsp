BEGIN {
        numrec = 0
	numinc = 0
      }

      {
        numrec++
	if ((i = index($0, "include 'ipfinc:")) > 0) {
          j = index($0, ":")
	  l = length ($0)
          buffer = substr($0, 1, j-1) "/" substr($0, j+1, l-j)  
#         printf (" i = %d j = %d l = %d \nOriginal %s\nRevised  %s\n", 
#	   i,j,l, $0, buffer)
          numinc++
	  printf ("%s\n", buffer)
	} else {
	  printf ("%s\n", $0)
	}
      }


END   {
#       printf (" %d records, %d include statements written to file %s \n", 
#	  numrec, numinc, newfile)
      }
  

