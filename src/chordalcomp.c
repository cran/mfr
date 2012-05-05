#include "splitting.h"

MFR *chordalComp(Graph *g)
{
	int i,j,k;
   int s;
	SEXP res;
	SEXP call;
	SEXP x;
	int *z;
	MFR *m;
	int *out;

	s = g->s;
	PROTECT(x = allocVector(INTSXP,2*s));
	z = INTEGER(x);
	for(i=0;i<s;i++) z[i] = g->edges[i][0];
	for(i=0;i<s;i++) z[i+s] = g->edges[i][1];
	call = PROTECT(lang2(install("chordalComp1"),x));
	res = PROTECT(eval(call,R_GlobalEnv));
	out = INTEGER(res);
	if(out[0]==0) {
		m = makeMFR(0,0);
		m->notfound=999;
	} else {
		m = makeMFR(out[0],out[1]);
		m->emptygraph = 0;
		m->notfound = 0;
		for(i=1,k=0;i<=out[1];i++){
			for(j=1;j<=out[0]+1;j++,k++){
				m->graded[i][j] = out[k+2];
			}
		}
	}
	UNPROTECT(3);
	return(m);
}

