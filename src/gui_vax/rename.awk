{
 n = split ($9, array, ";")
 file = tolower(array[1])
 printf ("mv \"%s\" %s\n", $9, file)
}
