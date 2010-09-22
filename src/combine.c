#include "splitting.h"

extern int verbose;

/* result is returned in a */
void combineGraded(MFR *a, MFR *b)
{
   int i,j,p,q,r,s;
	unsigned long **G;
	unsigned long **H;
	int N=a->nrow+b->nrow;
	int M=a->ncol+b->ncol;
	int pd=0,reg=0;

	G = Calloc(N,unsigned long *);
	for(i=0;i<N;i++){
	   G[i] = Calloc(M,unsigned long);
	}
	for(p=1;p<=a->nrow;p++){
		for(q=1;q<=b->nrow;q++){
			for(r=1;r<=a->ncol;r++){
				for(s=1;s<=b->ncol;s++){
				   G[p+q-2][r+s-2] += a->graded[p-1][r-1]*b->graded[q-1][s-1];
					if(G[p+q-2][r+s-2]>0){
					   reg = max(reg,p+q-1);
					   pd = max(pd,r+s-1);
					}
				}
			}
		}
	}
	pd--;
	for(i=0;i<a->nrow;i++){
	   Free(a->graded[i]);
	}
	Free(a->graded);
	pd -= 2;
	reg -= 2;
	H = Calloc(N,unsigned long *);
	for(i=0;i<N;i++){
	   H[i] = Calloc(M,unsigned long);
	}
	for(i=1;i<N;i++){
		for(j=1;j<M;j++){
		   H[i-1][j-1] = G[i][j];
		}
	}
	for(i=0;i<N;i++){
	   Free(G[i]);
	}
	Free(G);
	a->graded=H;
	a->nrow=N;
	a->ncol=M;
	a->pd=pd;
	a->reg=reg;
}
