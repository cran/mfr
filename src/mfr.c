#include "splitting.h"

int verbose;
extern int ATRANDOM;
int APPROXBETTIS;

void mfr(int *edges1, int *edges2, int *N, int *S, int *NC, int *V,
         double *graded, int *pd, int *reg, int *punted, int *NOCODE,
			int *seed, char **TEMPNAME)
{
	int i,j,k,s=*S,n=*N;
	MFR *mfr;
	int **edges;
	Graph *g;
	int nocode=*NOCODE;
	char *tempname;

	tempname=*TEMPNAME;
	APPROXBETTIS=0;

	if(nocode){
		if(*seed<=0){
		   ATRANDOM=0;
		}
		else{
			srandom(*seed);
			ATRANDOM=1;
		}
	}

	*NC=0;

	verbose = *V;

	edges = Calloc(s,int *);
	for(i=0;i<s;i++){
	   edges[i] = Calloc(2,int);
		edges[i][0] = edges1[i]-1;
		edges[i][1] = edges2[i]-1;
	}
   g = makeGraph(edges,s,n);

	mfr = mfrSplitting(g,0,NC,punted,nocode,tempname);
	*pd = mfr->pd;
	*reg = mfr->reg;

	k=0;
	for(i=1;i<mfr->reg+1;i++){
	   for(j=1;j<mfr->pd+2;j++){
		   graded[k] = (double)(mfr->graded[i][j]);
			k++;
		}
	}

	if(APPROXBETTIS>0) *punted=APPROXBETTIS;

	freeGraph(&g);
	freeMFR(&mfr);

}
