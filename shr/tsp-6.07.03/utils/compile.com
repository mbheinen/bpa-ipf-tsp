$!
$! 	This procedure conditionally compiles a .FOR module in an arbitrary
$!      directory if (1) the source .FOR file is more recent than the target 
$!      .OBJ or if the target .OBJ does not exist.  The target .OBJ is 
$!      always in the default directory.
$!
$ file1 := 'p1'.for
$ file_name == f$search (file1,1)
$ if file_name .eqs. "" 
$ then 
$    write sys$output "  File ''file1' does not exist"
$    exit 1
$ endif
$!
$!      parse the file name out of file1
$!
$ name = f$parse(file1,,,"name")
$ file2 := 'name'.obj
$ file_name == f$search (file2,1)
$ if file_name .eqs. "" 
$ then 
$    write sys$output "  File ''file2' does not exist"
$    write sys$output "  Compiling ''file1'"
$    ftddl 'file1'
$ else
$    time1 = f$file_attributes(file1,"cdt")
$    time2 = f$file_attributes(file2,"cdt") 
$    time1 = f$cvtime(time1)
$    time2 = f$cvtime(time2)
$    if time1 .gts. time2 
$    then 
$       write sys$output "  File ''file2' is obsolete"
$       write sys$output "  Compiling ''file1'"
$ 	ftddl 'file1'
$    endif
$ endif
