#include <stdio.h>
#include <Xm/Xm.h>

#include "dmgr.h"
#include "coord_data.h"

#define MAXLST 512
define_init()
{
	PSCoordFileRecord cmmt_rec;     /* storage for db_search key values    */
	PSCoordFileRecord *pcmmt_rec;   /* ptr to key for bus search       */
	PSCoordFileRecord *nextrec;     /* ptr assigned to address of next rec */
	int lcnt;
	XmString xlst[MAXLST];
	char *listname = "mylist";
	Widget id;

	DB_STAT stat;

	if( db_valid_db(&pscor_db ) != D_OKAY ) return;
	if (!HashLookup(listname, &id)) {
		printf("define_init: error no wid found for lst %s \n", listname);
		return;
	}
	pcmmt_rec = &cmmt_rec;
	strncpy(pcmmt_rec->comment.key, PSKEY_DEFINES,
		sizeof(pcmmt_rec->comment.key ) );
	pcmmt_rec->comment.idx = 0;
    stat = db_search( &pscor_db, &pcmmt_rec, &nextrec, COORD_KEY_IDX );
	if(stat == D_EOF) {
		/* no records for this type found so return */
		return;
	}
	if(stat == D_NOTFOUND) {
		/* ok so no record found with key == defines AND idx == 0 */
		/* so now see if some record was returned anyway */
		if(strncmp(nextrec->comment.key, pcmmt_rec->comment.key,
			sizeof(nextrec->comment.key)) == 0)
		{
			stat = D_FOUND;
		}
	}
	lcnt = 0;
	while(stat == D_FOUND && lcnt < MAXLST)
	{
		pcmmt_rec = nextrec;
		xlst[lcnt] = XmStringCreateLtoR(pcmmt_rec->cor.data,
			XmSTRING_DEFAULT_CHARSET);
		lcnt++;
		stat = db_next( &pscor_db, &pcmmt_rec, &nextrec, COORD_KEY_IDX );
		if( strncmp( nextrec->comment.key, pcmmt_rec->comment.key, 1 ) )
		{
			stat = D_NOTFOUND;
		}
	}
	XmListAddItems( id, xlst, lcnt, 1);
	for(lcnt; lcnt >= 0; lcnt--) {
		XmStringFree(xlst[lcnt]);
		xlst[lcnt] = NULL;
	}

} /* end define init */
