#include "splitting.h"

MFR *checkDataBase(Graph *g)
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
	call = PROTECT(lang2(install("checkDB1"),x));
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

void TESTDB(int *edges1, int *edges2, int *N, int *S, 
         double *graded, int *pd, int *reg, int *found)
{
	int i,j,k,s=*S,n=*N;
	MFR *mfr;
	int **edges;
	Graph *g;

	edges = Calloc(s,int *);
	for(i=0;i<s;i++){
	   edges[i] = Calloc(2,int);
		edges[i][0] = edges1[i];
		edges[i][1] = edges2[i];
	}
   g = makeGraph(edges,s,n);

	mfr = checkDataBase(g);
	if(mfr->notfound==999){
		*found = 0;
		freeGraph(&g);
		freeMFR(&mfr);
		return;
	}
	*found = 1;
	*pd = mfr->pd;
	*reg = mfr->reg;

	k=0;
	for(i=1;i<((mfr->reg))+1;i++){
	   for(j=1;j<mfr->pd+2;j++){
		   graded[k] = (double)(mfr->graded[i][j]);
			k++;
		}
	}

	freeGraph(&g);
	freeMFR(&mfr);

}
