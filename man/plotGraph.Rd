\name{plotGraph}
\alias{plotAdjacency}
\alias{plotGraph}
\alias{plotMFR}
\alias{plotScanMFR}
\title{
Graph and MFR plotting
}
\description{
Routines to plot a graph, the minimal free resolution of a graph,
and the mean of the scan MFR.
}
\usage{
plotGraph(g, n, x, ...)
plotAdjacency(g, col=0:1,addLine=FALSE,...)
plotMFR(m,icolors=gray((c(255,0:255))/255),
		  tcolors=2,zeros=FALSE,log=FALSE,cex=1,box=FALSE)
plotScanMFR(ms,icolors=gray((c(255,0:255))/255),col=gray(0.8),
                      tcolors=2,zeros=FALSE,log=FALSE,cex=1,box=FALSE)
}
\arguments{
  \item{g}{
a graph.
}
  \item{n}{
order of the graph. If missing, uses \code{Order(g)}.
}
  \item{x}{
optional coordinates for the vertices.
}
  \item{\dots}{
arguments passed to plot, or in the case of plotAdjacency, image.
}
  \item{m}{
an MFR.
}
  \item{addLine}{
logical. Adds a line down the diagonal if TRUE.
}
  \item{ms}{
output of \code{scanMFR}.
}
  \item{icolors,tcolors,col}{
colors for the image, text and lines in the Betti numbers plots.
}
  \item{zeros}{
logical. If FALSE the zeros are not labeled on the image when \code{tcolors}>0.
}
  \item{log}{
logical. Should the log of the Betti numbers be plotted in the image.
}
  \item{cex}{
text cex.
}
  \item{box}{
logical. Box around the image.
}
}
\details{
Plots the graph with vertices in a circle, or in positions defined by 
\code{x}. \code{plotAdjacency} plots the adjacency matrix as an image.
}
\value{
Returns the coordinates of the vertices, invisibly.
}
\author{
David Marchette, dmarchette@gmail.com
}
\seealso{
\code{\link{mfr}}, \code{\link{scanMFR}}, \code{\link{adjacencyMatrix}}.
}
\keyword{ math }
