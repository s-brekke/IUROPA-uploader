\name{cjeu_sql}
\alias{cjeu_sql}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Update CJEU SQL database
}
\description{
Function to upgrade observations in the IUROPA CJEU database: Decisions and decision-makers. 

Will update all data tables and variables associated with the provided ID code.
}
\usage{
cjeu_sql(id = NA, 
        user = "brekke", 
        password = NA,
        server = "GU", 
        from = NA, 
        silent = TRUE) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{id}{Case ID or ECLI of case(s) to upgrade. Can be a list.}
  \item{from}{Optional case number. If \code{id == NA} and \code{from \%in\% CJEU::Procedures$case}}
  \item{silent}{If \code{TRUE}, status reports will not be given}
  \item{server}{\code{GU} for Gothenburg server; \code{Johan} for Johan Lindholm's personal server}
  \item{user}{SQL server username, defaults to \code{brekke}}
  \item{password}{SQL server password}
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
2021 Stein Arne Brekke - \code{stein.brekke@eui.eu}
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x) 
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
