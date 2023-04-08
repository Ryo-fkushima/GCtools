\name{spdg}
\alias{spdg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{spdg: spider diagram generator
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
"spdg" is a useful function to draw spider diagrams of trace/REE elements. This function allows us to extract only which kinds of elements you need from your dataset, and to plot their concentrations.
}
\usage{
spdg(elist, s, nml, plottype, output)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{elist}{a vector which determines which kinds/order of elements you would like to adopt. You cannot set values other than "Cs", "Tl", "Rb", "Ba", "W", "Th", "U", "Nb", "Ta", "K", "La", "Ce", "Pb", "Pr", "Mo", "Sr", "P", "Nd", "F", "Sm", "Zr", "Hf", "Eu", "Sn", "Sb", "Ti", "Gd", "Tb", "Dy", "Li", "Y", "Ho", "Er", "Tm", "Yb", and "Lu". This is because the reference values, by which we normalize the sample data, consist only of concentrations of these elements.
}
\item{s}{a compositional dataframe. The first column should be a vector of elemental names, and the other columns should be their concentrations (ppm) in your samples. It is recommended to name each of the columns (i.e., each of the samples).
}
\item{nml}{a name of the reference dataset you would like to use: "NMORB_SM89", "EMORB_SM89", and "OIB_SM89": N-MORB, E-MORB, and OIB values (Sun and McDonough, 1989); "CI_MS95", "PM_MS95": chondritic and pyrolite mantle values (McDonough and Sun, 1995).
}
\item{plottype}{the type of line plots. You can choose from "l", "o", "b", etc.}
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
> This function works well even if the dataset ("s") includes NA values and/or undefined elemental names (e.g., SiO2, MgO, etc.), and/or even if the dataset lacks elemental names which are defined in the "elist" vector.

> Even if you set a dataset for many samples, this function can process it. However, as we have prepared only 10 colors and 2 line types, too many sample data would be indistinguishable on the plot.

> If you already have "output.txt" in the working directory, this function overwrites it when output = TRUE. The output file can be readily read with the read.table() function. Note that the output file does not contain any information about the reference dataset which was used for the normalization.

}

\references{
%% ~put references to the literature/web site here ~
Sun SS, McDonough WF (1989) Chemical and isotopic systematics of oceanic basalts: implications for mantle composition and processes. Geological Society, London, Special Publications, 42:313-345

McDonough WF, Sun SS (1995) The composition of the Earth. Chemical geology, 120:223-253
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
spdg(elist = c("Cs","Rb","Ba","Th","U","Nb","K","La","Ce","Pb","Pr","Sr","P",
"Nd","Zr","Sm","Eu","Ti","Dy","Y","Yb","Lu"), s = example_data_GCtools,
nml = "CI_MS95", plottype = "o", output = FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{incompatible element}
\keyword{REE}% use one of  RShowDoc("KEYWORDS")
\keyword{spider diagram}% __ONLY ONE__ keyword per line
