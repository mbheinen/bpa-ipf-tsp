check_same()
{
	diff ${1} ${2}
	if [ "$?" -eq "0" ]; then
		git rm ${2}
	fi
}

check_same param.inc param.inc.0
check_same params.inc params.inc.0
check_same pf_branchtsp.inc pf_branchtsp.inc.0
check_same pf_parametr.inc pf_parametr.inc.0
