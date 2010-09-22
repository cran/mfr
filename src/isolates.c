#include "splitting.h"

extern int verbose;

/* copy from h to g */
void copyGraph(Graph *g, Graph *h)
{
	int i;

	Free(g->degrees);
	g->degrees = Calloc(h->n,int);
	for(i=0;i<g->s;i++){
		Free(g->edges[i]);
	}
	Free(g->edges);
	g->edges = Calloc(h->s,int *);
	g->s = h->s;
	g->n = h->n;
	for(i=0;i<h->n;i++){
	   g->degrees[i] = h->degrees[i];
	}
	for(i=0;i<h->s;i++){
	   g->edges[i] = Calloc(2,int);
		g->edges[i][0] = h->edges[i][0];
		g->edges[i][1] = h->edges[i][1];
	}
}

void removeIsolates(Graph *g)
{
   int i,v,n=g->n,s=g->s;
	int *vertices;
	Graph *h;

	vertices = Calloc(n,int);

	for(i=0,v=0;i<n;i++){
	   if(g->degrees[i]!=0){
		   vertices[i] = v;
			v++;
		} 
		else{
			vertices[i] = -1;
		}
	}
	if(verbose>1){
	   fprintf(stderr,"Removed %d vertices\n",n-v);
	}
	if(v<n){
		g->n = v;
		Free(g->degrees);
		g->degrees = Calloc(v,int);
		for(i=0;i<s;i++){
		   v = vertices[g->edges[i][0]];
			g->degrees[v]++;
		   g->edges[i][0]=v;
		   v = vertices[g->edges[i][1]];
			g->degrees[v]++;
		   g->edges[i][1]=v;
		}
	}
	Free(vertices);
}

