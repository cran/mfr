#include "splitting.h"

extern int verbose;

int isComplete(Graph *g)
{
   int i,n=g->n,*d=g->degrees;

	for(i=0;i<n;i++){
	   if(d[i] != n-1) return(0);
	}
	return(1);
}


MFR *mfrComplete(int n)
{
	int i;
	MFR *mfr;
   unsigned long **M;

	if(verbose>1){
	   fprintf(stderr,"Complete graph on %d vertices\n",n);
	}
	mfr = makeMFR(n-1,2);

	M = mfr->graded;
	for(i=0;i<n;i++) M[2][i+1] = i*mychoose(n,i+1);
	return(mfr);
}

void MFRComplete(int *N, double *graded, int *pd, int *reg)
{
	int i,j,k;
   MFR *mfr;

	mfr = mfrComplete(*N);
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
