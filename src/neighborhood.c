#include "splitting.h"

extern int verbose;

int *neighborhood(Graph *g,int v)
{
   int i;
	int *nbhd;

	nbhd = Calloc(g->n,int);
	nbhd[v] = 1;

	for(i=0;i<g->s;i++){
	   if(g->edges[i][0]==v) nbhd[g->edges[i][1]]=1;
	   if(g->edges[i][1]==v) nbhd[g->edges[i][0]]=1;
	}
	return(nbhd);
}

