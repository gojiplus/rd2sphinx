\name{add_numbers}
\alias{add_numbers}
\title{Add Two Numbers}
\usage{
add_numbers(x, y = 1, ...)
}
\arguments{
\item{x}{A numeric value.}
\item{y}{Another numeric value. See \code{\link{demo_data}} for sample data.}
\item{\dots}{Additional values are ignored.}
}
\value{
The numeric sum of \code{x} and \code{y}.
}
\description{
Add two numbers while demonstrating a local semantic cross-reference.
Plain syntax such as snake_case, a * b, x | y, and `inline_code` must not
become reStructuredText markup.
Inline code from roxygen backticks arrives as \verb{man/*.Rd} and must be
literal, asterisk and all.
- This sentence begins with a dash but is prose, not a list.
}
\details{
This topic contains \emph{emphasis}, \strong{strong text}, and an itemized list:
\itemize{
  \item{The first item, with a second line
  that must stay inside the item.}
  \item{The second item.}
}
Roxygen writes items without braces, which must render the same way:
\itemize{
  \item An unbraced item with \code{code} in it.
  \item Another unbraced item.
}
And a numbered list:
\enumerate{
  \item Step one.
  \item Step two.
}
A definition list:
\describe{
  \item{alpha}{The first thing.}
  \item{beta}{The second thing.}
}
This closing paragraph must not run into the list above.
}
\examples{
add_numbers(1, 2)
}
\seealso{
\code{\link{demo_data}}
}
