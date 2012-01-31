#include "splitting.h"

extern int verbose;

Graph *makeGraph(int **edges,int s,int n)
{
   int i;
	Graph *g;
	int *d;

	g = Calloc(1,Graph);

	d = Calloc(n,int);

	g->degrees=d;
	g->n = n;
	g->s = s;

	if(s>0){
		g->edges = edges;

		for(i=0;i<s;i++){
			d[edges[i][0]]++;
			d[edges[i][1]]++;
		}
	}
	else{
	   g->edges = Calloc(1,int *);
		g->edges[0] = Calloc(2,int);
	}

	return(g);
}

void freeGraph(Graph **G)
{
	Graph *g;
   int i,n,s;

	g = *G;

   n=g->n;
	s=g->s;


	Free(g->degrees);
	if(s>0){
		for(i=0;i<s;i++){
			Free(g->edges[i]);
		}
		Free(g->edges);
	}
	Free(g);
}

MFR *makeMFR(int pd, int reg)
{
	int i;
   MFR *mfr;
   
	mfr = Calloc(1,MFR);
	mfr->nrow = max(reg+1,3);
	mfr->ncol = max(pd+2,3);
	mfr->graded = Calloc(mfr->nrow,unsigned long *);
	for(i=0;i<mfr->nrow;i++){
		mfr->graded[i] = Calloc(mfr->ncol,unsigned long);
	}
	mfr->pd = pd;
	mfr->reg = reg;
	mfr->graded[1][1] = 1;

	return(mfr);
}

void fprintfMFR(MFR *mfr)
{
   int i,j;
	unsigned long *bettis;

	bettis = Calloc(mfr->ncol,unsigned long);

	for(i=0;i<mfr->nrow;i++){
		for(j=0;j<mfr->ncol;j++){
			bettis[j] += mfr->graded[i][j];
		}
	}
	Rprintf("Total Bettis:\n");
	for(i=1;i<mfr->pd+2;i++){
	   Rprintf("%ld ",bettis[i]);
	}
	Free(bettis);
	Rprintf("\nGraded Bettis:\n");
	for(i=1;i<mfr->reg+1;i++){
	   for(j=1;j<mfr->pd+2;j++){
			Rprintf("%ld ",mfr->graded[i][j]);
		}
		Rprintf("\n");
	}
	Rprintf("\n");
	Rprintf("pd: %d reg: %d\n",mfr->pd,mfr->reg);
}

void freeMFR(MFR **Mfr)
{
	int i;
	MFR *mfr;

	mfr = *Mfr;

	for(i=0;i<mfr->nrow;i++){
		Free(mfr->graded[i]);
	}
	Free(mfr->graded);
	Free(mfr);
}

