\name{spdg}
\alias{spdg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{spdg:spider diagram generator
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
"spdg" is a useful function for geochemists to draw spider diagrams of trace-elements.
}
\usage{
spdg(elist, s, nml, output)
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
spdg(elist = c("Cs","Rb","Ba","Th","U","Nb","K","La","Ce","Pb","Pr","Sr","P",
"Nd","Zr","Sm","Eu","Ti","Dy","Y","Yb","Lu"), s = example_data_GCtools,
nml = "CI_MS95", output = FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{REE}% use one of  RShowDoc("KEYWORDS")
\keyword{spider diagram}% __ONLY ONE__ keyword per line
