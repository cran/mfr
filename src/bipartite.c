#include "splitting.h"

extern int verbose;

int isCompleteBipartite(Graph *g, int *N, int *M)
{
   int i,j,n=g->n,a=0,b=0;
	int *degrees=g->degrees;
	static int *part1,*part2;

	for(i=0;i<n;i++){
	   if(degrees[i]>0){
		   part1 = neighborhood(g,i);
			part1[i]=0;
			for(j=0;j<n;j++){
					if(part1[j]>0){
						part2 = neighborhood(g,j);
						part2[j] = 0;
						break;
					}
			}
			a=0;
			b=0;
			for(j=0;j<n;j++){
				a += part1[j];
				b += part2[j];
				if((part1[j]==1) & (part2[j]==1)){
				   free(part1);
					free(part2);
					return(0);
				}
			}
			break;
		}
	}
	*N=a;
	*M=b;
	for(i=0;i<g->s;i++){
	   if(((part1[g->edges[i][0]]==1) & (part1[g->edges[i][1]]==1))  |
	      ((part2[g->edges[i][0]]==1) & (part2[g->edges[i][1]]==1))){
			free(part1);
			free(part2);
			return(0);
		}
	}
	free(part1);
	free(part2);
	return(((a+b)==numberVerticesActive(g)) & ((a*b)==g->s));
}

MFR *mfrCompleteBipartite(int n,int m)
{
	int i,j;
	MFR *mfr;
   unsigned long **M;

	if(verbose>0){
	   Rprintf("Complete bipartite graph on %d+%d vertices\n",n,m);
	}
	mfr = makeMFR(m+n-1,2);

	M = mfr->graded;
	for(i=1;i<n+m;i++) {
		for(j=1;j<=i;j++) {
			M[2][i+1] += mychoose(n,j)*mychoose(m,i+1-j);
		}
	}
	return(mfr);
}

void MFRCompleteBipartite(int *N, int *M, double *graded, int *pd, int *reg)
{
	int i,j,k;
   MFR *mfr;

	mfr = mfrCompleteBipartite(*N,*M);
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
