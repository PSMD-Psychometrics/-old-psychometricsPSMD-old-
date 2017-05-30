\name{fnUpdate}
\alias{fnUpdate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A function to automate updating the PSMD.Psychometrics package.
%% ~~function to do ... ~~
}
\description{
A function to automate updating the PSMD.Psychometrics package.  

}
\usage{
fnUpdate(PackageName, Reinstall)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{PackageName}{
Can be omitted if you want to update PSMD.Psychometrics as it defaults to this. Specifying any other name should update the named package, but ssee 'Note' section.
}
  \item{Reinstall}{
Reinstall determines whether the package is reinstalled and reattached regardless of its current status (e.g. When Reinstall==TRUE, the package named in PackageName will be reinstalled and reattached even if it is already installed and/or attached). Set Reinstall=TRUE for updating PSMD.Psychometrics.
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
DZ260417: The argument character.only = TRUE works for library(), but possibly not for install.packages. So although PackageName in fnUpdate() could be changed to install and attach any package, it might hit errors at the install.package() phase. PackageName currently defaults to PSMD.Psychometrics though, so unless specified otherwise, fnUpdate handles PSMD.Psychometrics
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
fnUpdate(Reinstall = TRUE) will force a clean install and reattachment of PSMD.Psychometrics.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
