#include "splitting.h"

extern int verbose;

int isEmpty(Graph *g)
{

	return(g->s==0);
}

MFR *mfrEmpty(void)
{
	MFR *mfr;

	if(verbose>0){
	   Rprintf("Empty graph\n");
	}
	mfr=makeMFR(1,1);
	mfr->emptygraph = 1;
	return(mfr);
}
