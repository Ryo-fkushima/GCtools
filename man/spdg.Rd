\name{spdg}
\alias{spdg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{spdg: spider diagram generator
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
"spdg" is a useful function for geochemists to draw spider diagrams of trace/REE elements. This function allows us to extract only which kinds of elements you need and to plot their concentrations.
}
\usage{
spdg(elist, s, nml, output)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{elist}{a vector which determines which kinds/order of elements you would like to adopt. You cannot set values other than "Cs", "Tl", "Rb", "Ba", "W", "Th", "U", "Nb", "Ta", "K", "La", "Ce", "Pb", "Pr", "Mo", "Sr", "P", "Nd", "F", "Sm", "Zr", "Hf", "Eu", "Sn", "Sb", "Ti", "Gd", "Tb", "Dy", "Li", "Y", "Ho", "Er", "Tm", "Yb", and "Lu". This is because the reference values, by which we normalize the sample data, consist only of concentrations of these elements.
}
\item{s}{a compositional dataframe. The first column should be a vector of elemental names, and the other columns should be their concentrations in your samples. You had better name each of the columns (i.e., each of the samples).
}
\item{nml}{a name of the reference dataset you would like to use: "NMORB_SM89", "EMORB_SM89", and "OIB_SM89": N-MORB, E-MORB, and OIB values (Sun and McDonough, 1989); "CI_MS95", "PM_MS95": chondritic and pyrolite mantle values (McDonough and Sun, 1995).
}
\item{output}{If output = TRUE, you can obtain a text file which includes the final dataset used for the plot in the working directory.
}

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
