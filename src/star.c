#include "splitting.h"

extern int verbose;

int isStar(Graph *g,int *d)
{
   int i,n=g->n,k,v,z;
	int *degrees=g->degrees;

	k=0;
	v=0;
	z=0;
	for(i=0;i<n;i++){
	   if(degrees[i]==1) k++;
		else if(degrees[i]==n-1) v++;
		else if(degrees[i]==0) z++;
		if(v>1) return(0);
	}
	*d=k;
	return(v==1 && k==n-z-1);
}

MFR *mfrStar(int d)
{
	int i;
	MFR *mfr;
   unsigned long **M;

	if(verbose>0){
	   Rprintf("Star graph on %d vertices\n",d+1);
	}
	mfr = makeMFR(d,2);

	M = mfr->graded;
	for(i=0;i<d;i++) M[2][i+2] = mychoose(d,i+1);
	return(mfr);
}

void MFRStar(int *D, double *graded, int *pd, int *reg)
{
	int i,j,k;
   MFR *mfr;

	mfr = mfrStar(*D);
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
