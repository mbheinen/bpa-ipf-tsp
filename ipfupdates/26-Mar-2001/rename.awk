{
  file1 = $9
  n = split ($9, array, ".")
  file2 = tolower (array[1]) ".f"
  printf ("gawk -f VMS-Linux.awk %s > %s\n", file1, file2)
}  
