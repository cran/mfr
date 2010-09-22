#include "splitting.h"

extern int verbose;

extern int APPROXBETTIS;

int numberVerticesActive(Graph *g)
{
   int i,m;
	m=0;
	for(i=0;i<g->n;i++) if(g->degrees[i]>0) m++;
	return(m);
}

int simplicialVertex(Graph *g, int start)
{
   int i,j,n=g->n,m;
	int *nbhd,s;
	Graph *h;

	if(start>=n) return(-1);
	if(g->s==0) return(-1);
	if(n==1) return(-1);
	for(i=start;i<n;i++){
		if(g->degrees[i]>0){
			nbhd = neighborhood(g,i);
			m=0;
			for(j=0;j<n;j++){
			   if(nbhd[j]==1) m++;
			}
			h = subgraph(g,nbhd);
			s = h->s;
			Free(nbhd);
			freeGraph(&h);
			if(s==mychoose(m,2)) {
				return(i);
			}
		}
	}

	return(-1);
}

Graph *subgraph(Graph *g, int *vertices)
{
	int i,j;
	int n=g->n,s=g->s,s1;
	int v,w;
	Graph *h;
   int **edges;

	edges = Calloc(1,int *);
	s1=0;
	for(i=0;i<s;i++){
	   v=g->edges[i][0];
	   w=g->edges[i][1];
		if(vertices[v]==1 && vertices[w]==1){
		   edges = Realloc(edges,(s1+1),int *); 
			edges[s1] = Calloc(2,int);
			edges[s1][0]=v;
			edges[s1][1]=w;
			s1++;
		}
	}

	h = makeGraph(edges,s1,n);
	return(h);
}

int NOCODEAPPROX;
int ATRANDOM;

int splittingEdge(Graph *g)
{
   int i,j,k;
   int n=g->n;
   int s=g->s;
	int *degrees = g->degrees;
	int d;
	int v,w;
	int **edges;
	int *N1,*N2;
	int best=-1;
	int edge;

	edges = g->edges;

	if(s==0){
	   return(-1);
	}

	edge=-1;
	best=-1;
	for(i=0;i<s;i++){
	   v=edges[i][0];
	   w=edges[i][1];
		if((d=max(degrees[v],degrees[w]))>best){
			k=0;
			N1 = neighborhood(g,v);
			N2 = neighborhood(g,w);
			for(j=0;j<n;j++){
				k += N1[j]*N2[j];
			}
			Free(N1);
			Free(N2);
			if(k==min(degrees[v],degrees[w])+1){
			   best=d;
				edge=i;
			}
		}
	}
	if(edge==(-1) && NOCODEAPPROX==1){
		APPROXBETTIS++;
		if(ATRANDOM==1){
			edge = random() % s;
		}
		else{
		   Graph *G;
			int *N;

			edge = 0;
			best = 0;
		   N = Calloc(s,int);
			for(i=0;i<s;i++){
				G = removeEdge(g,i);
				splittingEdges(G,N);
				freeGraph(&G);
				k = 0;
				for(j=0;j<n;j++){
				   k += N[j];
				}
				if(k>best){
				   best = k;
					edge = i;
				}
			}
			/* fprintf(stderr,"Best: %d, edge: %d (%d:%d)\n",best,edge,s,n);*/
			Free(N);
			/* fprintf(stderr,"Freed N\n");*/
		}
	}
	return(edge);
}

void splittingEdges(Graph *g, int *splitting)
{
   int i,j,k;
   int n=g->n;
   int s=g->s;
	int *degrees = g->degrees;
	int v,w;
	int **edges;
	int *N1,*N2;
	int edge;

	edges = g->edges;

	if(s==0){
	   return;
	}

	edge=-1;
	for(i=0;i<s;i++){
	   v=edges[i][0];
	   w=edges[i][1];
		splitting[i]=0;
		k=0;
		N1 = neighborhood(g,v);
		N2 = neighborhood(g,w);
		for(j=0;j<n;j++){
			k += N1[j]*N2[j];
		}
		Free(N1);
		Free(N2);
		if(k==min(degrees[v],degrees[w])+1){
			splitting[i]=1;
		}
	}
}

void SplittingEdge(int *e1, int *e2, int *N, int *S, int *edge)
{
   int i,n=*N,s=*S;
	int **edges;
	Graph *g;

	edges = Calloc(s,int *);
	for(i=0;i<s;i++){
	   edges[i] = Calloc(2,int);
		edges[i][0] = e1[i]-1;
		edges[i][1] = e2[i]-1;
	}
	g = makeGraph(edges,s,n);
	*edge = splittingEdge(g);

	freeGraph(&g);
}

void SplittingEdges(int *e1, int *e2, int *N, int *S, int *splitting)
{
   int i,n=*N,s=*S;
	int **edges;
	Graph *g;

	edges = Calloc(s,int *);
	for(i=0;i<s;i++){
	   edges[i] = Calloc(2,int);
		edges[i][0] = e1[i]-1;
		edges[i][1] = e2[i]-1;
	}
	g = makeGraph(edges,s,n);
	splittingEdges(g,splitting);

	freeGraph(&g);
}

int chooseEdge(Graph *g)
{
   int v,w;
	int i,s=g->s;
	int *degrees=g->degrees;
	int edge,best;

	best=0;
	v = simplicialVertex(g,0);
	if(v==-1) {
	   return(splittingEdge(g));
	}
	while(v>(-1)){
		for(i=0;i<s;i++){
			if(g->edges[i][0]==v){
			   w = g->edges[i][1];
				if(degrees[w]>best){
				   best=degrees[w];
					edge = i;
					break;
				}
			}
			else if(g->edges[i][1]==v){
			   w = g->edges[i][0];
				if(degrees[w]>best){
				   best=degrees[w];
					edge = i;
					break;
				}
			}
		}
		v = simplicialVertex(g,v+1);
	}
	return(edge);
}

Graph *removeEdge(Graph *g, int edge)
{
   int i,j,n=g->n,s=g->s;
	int **edges;
	Graph *h;

	edges = Calloc(s-1,int *);
	j=0;
	for(i=0;i<s;i++){
	   if(i != edge){
		   edges[j] = Calloc(2,int);
			edges[j][0] = g->edges[i][0];
			edges[j][1] = g->edges[i][1];
			j++;
		}
	}
   h = makeGraph(edges,s-1,n);
	return(h);
}

unsigned long mychoose(int n, int k)
{
   int i,j;
	unsigned long out;
	
	if(k<0) return(0);
	if(k==0) return(1);
	if(n<k) return(0);
	if(n==k) return(1);
	out = 1;
	j = 1;
	for(i=n;i>n-k;i--,j++) {
		out *= i;
		if(j<=k) out /= j;
	}
	if(j<k){
		for(i=j;i<=k;i++){
			out /= i;
		}
	}
	return(out);
}

int myaccess(unsigned long **A, int i, int j, int nrow, int ncol)
{
	int I,J;

	I = i+2;
	if(I<1) return(0);
	J = j-i;
	if(J<1) return(0);
	if(I>=ncol) return(0);
	if(J>=nrow) return(0);
	return(A[J][I]);
}

MFR *mfrSplitting(Graph *g, int depth, int *numCombs,int *punted, int nocode,
                  char *tempname)
{
	int i,j;
	int n;
	int s=g->s;
   MFR *mfr,*mfra,*mfrb;
	int edge;
	int v;
	Graph *G;
	int *N;
	int *comps;
	int n1,n2;
	int m;
	unsigned long **A,**B,**M;
	int pd,reg;
	unsigned long x;
	int kk,a,b,d;

	if(nocode==0) NOCODEAPPROX=0;
	else NOCODEAPPROX=1;

	if(verbose){
	   fprintf(stderr,"MFR: Depth = %d\n",depth);
		fprintf(stderr,"\tn = %d, s = %d\n",numberVerticesActive(g),g->s);
	}

	if(isEmpty(g)){
	   return(mfrEmpty());
	}
	removeIsolates(g);
	comps = Calloc(g->n,int);
	m = components(g,comps);
	if(m>1){
	   N = Calloc(g->n,int);
		for(i=0;i<g->n;i++){
		   if(comps[i]==1) N[i]=1;
			else N[i]=0;
		}
		G = subgraph(g,N);
		mfra = mfrSplitting(G, depth+1, numCombs,punted, nocode, tempname);
		freeGraph(&G);
		for(j=2;j<=m;j++){
			for(i=0;i<g->n;i++){
				if(comps[i]==j) N[i]=1;
				else N[i]=0;
			}
			G = subgraph(g,N);
			mfrb = mfrSplitting(G, depth+1, numCombs,punted, nocode, tempname);
			freeGraph(&G);
			combineGraded(mfra,mfrb);
			freeMFR(&mfrb);
		}
	   Free(N);
		Free(comps);
		return(mfra);
	}
	else{
		Free(comps);
		if(isComplete(g)){
			return(mfrComplete(g->n));
		}
		if(isStar(g,&d)){
			return(mfrStar(d));
		}
		if(isCycle(g)){
			return(mfrCycle(g->n));
		}
		if(isCompleteBipartite(g,&n1,&n2)){
			/*
			fprintf(stderr,"completebipartite %d %d\n",n1,n2);
			for(i=0;i<g->s;i++)
				fprintf(stderr,"%d %d\n",g->edges[i][0],g->edges[i][1]);
			*/
			return(mfrCompleteBipartite(n1,n2));
		}
		edge = chooseEdge(g);
		if(edge>(-1)){
			G = removeEdge(g,edge);
			if(verbose>2){
				fprintf(stderr,"Removed edge %d: (%d,%d)\n",edge,
						  g->edges[edge][0],
						  g->edges[edge][1]);
			}
			mfra = mfrSplitting(G,depth+1,numCombs,punted,nocode,tempname);
			if(verbose>2){
				fprintfMFR(stderr,mfra);
			}
			freeGraph(&G);
			if(g->degrees[g->edges[edge][0]]>
				g->degrees[g->edges[edge][1]]){
				v = g->edges[edge][0];
			}
			else{
				v = g->edges[edge][1];
			}
			if(verbose>2){
				fprintf(stderr,"Vertex %d chosen: degree = %d\n",v,g->degrees[v]);
			}
			N = neighborhood(g,v);
			m=g->degrees[v]+1;
			if(g->n==m){
				if(verbose>2){
					fprintf(stderr,"\tNeighborhood is the full graph\n");
				}
				Free(N);
				mfrb = mfrEmpty();
			}
			else{
				for(i=0;i<g->n;i++){
					if(N[i]==0) {
						N[i]=1;
					}
					else{
						N[i]=0;
					}
				}
				G = subgraph(g,N);
				Free(N);
				if(verbose>2){
					a = numberVerticesActive(g);
					b = numberVerticesActive(G);
					fprintf(stderr,"Removed %d vertices, %d remaining\n",
							  a-b,b);
				}
				mfrb = mfrSplitting(G,depth+1,numCombs,punted,nocode,tempname);
				if(verbose>2){
					fprintfMFR(stderr,mfrb);
				}
				freeGraph(&G);
			}
			if(verbose>1){
				fprintf(stderr,"Patching the MFR up at depth %d\n",depth);
			}
			A = mfra->graded;
			B = mfrb->graded;
			n = m-2;
			pd = max(mfra->pd,mfrb->pd+n+1);
			reg = max(2,max(mfra->reg,mfrb->reg+1));
			if(verbose>1){
				fprintf(stderr,"pd=%d, reg=%d\n",pd,reg);
				if(mfra->emptygraph==1)
					fprintf(stderr,"A is empty\n");
				if(mfrb->emptygraph==1)
					fprintf(stderr,"B is empty\n");
			}

			mfr = makeMFR(pd,reg);
			M = mfr->graded;

			(*numCombs)++;
			if(verbose>1){
				fprintf(stderr,"Node: %d\n",*numCombs); 
			}
			M[2][2] = s;
			if(pd>1){
				for(i=1;i<=pd-1;i++){
					for(j=(i+1);j<=reg+i;j++){
						x = 0;
						for(kk=0;kk<=i;kk++){
							a = i-1-kk;
							b = j-2-kk;
							if(a==(-1)){
								if(b==0){
									x += mychoose(n,kk);
								}
							}
							else if(b>0){
								x += mychoose(n,kk)*myaccess(B,a,b,mfrb->nrow,mfrb->ncol);
							}
						}
						M[j-i][i+2] = myaccess(A,i,j,mfra->nrow,mfra->ncol)+x;
					}
				}
			}
			M[1][1] = 1;
			if(verbose>2){
				fprintf(stderr,"Combined at depth %d:\n",depth);
				fprintfMFR(stderr,mfr);
			}
			freeMFR(&mfra);
			freeMFR(&mfrb);
			return(mfr);
		}
		else{
			if(verbose>0){
				fprintf(stderr,"No simplicial vertices (or splitting edges)\n");
			}
			if(nocode){
				return(mfrSplitting(g, depth, numCombs,punted, nocode,tempname));
			}
			else{
				(*punted)++;
				return(singular(g,tempname));
			}
		}
	}
}
