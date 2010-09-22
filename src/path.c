#include "splitting.h"

extern int verbose;

int isPath(Graph *g)
{
   int i,n=g->n,k;
	int *degrees=g->degrees;

	k=0;
	for(i=0;i<n;i++){
		if(degrees[i]>2) return(0);
	   else if(degrees[i]==1) k++;
	}
	return(k==2);
}

MFR *mfrPath(int n)
{
	int i,j,d,l;
	int pd,reg;
	MFR *mfr;
   unsigned long **M;


	if(verbose>1){
	   fprintf(stderr,"Path graph on %d vertices\n",n);
	}
	switch(n % 3){
	   case 0:
	      pd = 2*n/3;
		break;
	   case 1:
	      pd = (2*n-2)/3;
		break;
	   case 2:
	      pd = (2*n-1)/3;
		break;
	}

	reg = pd/2+1+(pd % 2);

	mfr = makeMFR(pd,reg);

	M = mfr->graded;
	for(l=2;l<=pd;l++){
		for(d=l/2;d<=pd+reg;d++){
			j = d-l+1;
			i = j-2;
			if(j>=2 && j<=reg){
				M[j][d-i] = mychoose(d-l,2*l-d)*mychoose(n-2*d+2*l,d-l)+
							 mychoose(d-l-1,2*l-d)*mychoose(n-2*d+2*l,d-l-1);
			}
		}
	}
	M[2][2] = n-1;
	M[1][1] = 1;
	return(mfr);
}

void MFRPath(int *N, double *graded, int *pd, int *reg)
{
	int i,j,k;
   MFR *mfr;

	mfr = mfrPath(*N);
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
