\name{simplespider}
\alias{simplespider}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{simplespider: spider diagram generator (for already sorted data)
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
"simplespider" is a useful function for geochemists to draw spider diagrams of trace-elements.
}
\usage{
simplespider(s, ymin, ymax, output)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}

\author{
%%  ~~who you are~~
Ryo Fukushima
}

\note{
%%  ~~further notes~~

}

\references{
%% ~put references to the literature/web site here ~

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
simplespider(s = example2_data_GCtools, ymin = 0.1, ymax = 1000, output = FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{REE}% use one of  RShowDoc("KEYWORDS")
\keyword{spider diagram}% __ONLY ONE__ keyword per line
