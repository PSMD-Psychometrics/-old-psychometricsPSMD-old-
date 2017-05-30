\name{fnRound}
\alias{fnRound}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A function to round values in accordance with PSMD policy
%% ~~function to do ... ~~
}
\description{
fnRound will round numeric values in accordance with PSMD policy. The policy (as at August 2016) proposes the use of the asymmetric round-half-up method (arithmetic rounding). This method rounds values at or above the midpoint away from zero and values below the midpoint towards zero to the closest required decimal place.

For example, when rounded to two decimal places values of 1.000 to 1.004 become 1.00 and values of 1.005 to 1.009 become 1.01. This applies irrespective of a number's sign so that values of -1.000 to -1.004 become -1.00 and values of -1.005 to -1.009 become -1.01.

Adapted from an anonymous post at Statistically Significant; http://alandgraf.blogspot.co.uk/2012/06/rounding-in-r.html

}
\usage{
fnRound(x, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
x is the numeric value or variable to be rounded
}
  \item{n}{
n is the number of decimal places required. Trailing zeroes will be excluded. 
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Dr Daniel Zahra, e:psychometrics@plymouth.ac.uk
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
fnRound(13.436765,2) will return 13.44
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
