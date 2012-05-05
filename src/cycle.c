#include "splitting.h"

extern int verbose;

int nextVertex(Graph *g,int v)
{
   int i,s=g->s;

	for(i=0;i<s;i++){
	   if(g->edges[i][1]==v) return(g->edges[i][2]);
	   if(g->edges[i][2]==v) return(g->edges[i][1]);
	}
	return(-1);
}

int isCycle(Graph *g)
{
   int i,n=g->n;
	int *degrees=g->degrees;
	int m;

	if(degrees[0]!=2) return(0);
	i = 0;
	m = 1;
	while(m<n){
	   i = nextVertex(g,i);
		if(i==-1) return(0);
		if(degrees[i]!=2) return(0);
		if(i==0) return(m==n);
		m++;
	}
	return(0);
}

MFR *mfrCycle(int n)
{
	int i,j,d,l;
	static int pd,reg;
	MFR *mfr;
   unsigned long **M;


	if(verbose>0){
	   Rprintf("Cycle on %d vertices\n",n);
	}
	switch(n % 3){
	   case 0:
	      pd = 2*n/3;
		break;
	   case 1:
	      pd = (2*n+1)/3;
		break;
	   case 2:
	      pd = (2*n-1)/3;
		break;
	}
	reg = (int)floor((n+1)/3)+1;

	mfr = makeMFR(pd,reg);

	M = mfr->graded;
	for(l=2;l<=pd;l++){
		for(d=l/2;d<=pd+reg;d++){
			j = d-l+1;
			i = j-2;
			if(j>=2 && j<=reg){
				M[j][d-i] = (n*mychoose(d-l,2*l-d)*
				             mychoose(n-2*(d-l),d-l))/(n-2*(d-l));
			}
		}
	}
	switch(n % 3){
	   case 0:
		M[reg][pd+1] = 2;
		break;
	   case 1:
	   case 2:
		M[reg][pd+1] = 1;
		break;
	}
	M[1][1] = 1;
	M[2][2] = n;
	return(mfr);
}

void MFRCycle(int *N, double *graded, int *pd, int *reg)
{
	int i,j,k;
   MFR *mfr;

	mfr = mfrCycle(*N);
	*pd = mfr->pd;
	*reg = mfr->reg;

	k=0;
	for(i=1;i<mfr->reg+1;i++){
	   for(j=1;j<mfr->pd+2;j++){
		   graded[k] = (double)(mfr->graded[i][j]);
			k++;
		}
	}
	freeMFR(&mfr);
}
