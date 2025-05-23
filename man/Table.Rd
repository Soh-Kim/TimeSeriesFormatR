% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Table.R
\name{Table}
\alias{Table}
\title{R6 Table Class}
\description{
R6 class to develop convenient data frame to analyse time series data.
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{Table}}{Data Frame to store processed longitudinal data.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Table-new}{\code{Table$new()}}
\item \href{#method-Table-clone}{\code{Table$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Table-new"></a>}}
\if{latex}{\out{\hypertarget{method-Table-new}{}}}
\subsection{Method \code{new()}}{
Method to Create R6 Table object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Table$new(LongitudinalData, TargetTrait)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{LongitudinalData}}{R6 LongitudinalData Class object.}

\item{\code{TargetTrait}}{It indicate on which phenotype is used to constract the table.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Table-clone"></a>}}
\if{latex}{\out{\hypertarget{method-Table-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Table$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
