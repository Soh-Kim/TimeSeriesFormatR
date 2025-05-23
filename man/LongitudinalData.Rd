% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LongitudinalData.R
\name{LongitudinalData}
\alias{LongitudinalData}
\title{R6 LongitudinalData Class}
\description{
R6 class to store all data. Individual's data is stored with Indiv R6 class.
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{RawData}}{Data frame to store un-processed longitudinal data.}

\item{\code{Indivs}}{List of R6 Indiv Class.}

\item{\code{Record}}{Vector to store measurement data.}

\item{\code{NameOfPheno}}{Vector to store all of variables.}

\item{\code{PhenotypeTable}}{Field to store R6 Table Class.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-LongitudinalData-new}{\code{LongitudinalData$new()}}
\item \href{#method-LongitudinalData-IntegDuplication}{\code{LongitudinalData$IntegDuplication()}}
\item \href{#method-LongitudinalData-AddData}{\code{LongitudinalData$AddData()}}
\item \href{#method-LongitudinalData-CreateDataTable}{\code{LongitudinalData$CreateDataTable()}}
\item \href{#method-LongitudinalData-Plot}{\code{LongitudinalData$Plot()}}
\item \href{#method-LongitudinalData-clone}{\code{LongitudinalData$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LongitudinalData-new"></a>}}
\if{latex}{\out{\hypertarget{method-LongitudinalData-new}{}}}
\subsection{Method \code{new()}}{
Construct LongitudinalData object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LongitudinalData$new(
  RawData,
  ListOfData,
  SproutDate = NULL,
  UUID_Other = NULL,
  duplicate = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{RawData}}{one-row Data Frame with several variables as well as UUID}

\item{\code{ListOfData}}{List indicating with column of RawData is corresponding to UUID, TimeDependVar, TimeIndependVar and Date.}

\item{\code{SproutDate}}{Date of Sprout. Not mandatory.}

\item{\code{UUID_Other}}{Data Frame to connect old UUID and new UUID}

\item{\code{duplicate}}{Weather data has duplication or not. This parameter is not used in the function.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LongitudinalData-IntegDuplication"></a>}}
\if{latex}{\out{\hypertarget{method-LongitudinalData-IntegDuplication}{}}}
\subsection{Method \code{IntegDuplication()}}{
Method to eliminate duplication as well as integrate time-series data.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LongitudinalData$IntegDuplication()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LongitudinalData-AddData"></a>}}
\if{latex}{\out{\hypertarget{method-LongitudinalData-AddData}{}}}
\subsection{Method \code{AddData()}}{
Method to add new data.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LongitudinalData$AddData(
  RawData,
  ListOfData,
  SproutDate = NULL,
  UUID_Other = NULL,
  duplicate = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{RawData}}{one-row Data Frame with several variables as well as UUID}

\item{\code{ListOfData}}{List indicating with column of RawData is corresponding to UUID, TimeDependVar, TimeIndependVar and Date.}

\item{\code{SproutDate}}{Date of Sprout. Not mandatory.}

\item{\code{UUID_Other}}{Data Frame to connect old UUID and new UUID}

\item{\code{duplicate}}{Weather data has duplication or not. This parameter is not used in the function.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LongitudinalData-CreateDataTable"></a>}}
\if{latex}{\out{\hypertarget{method-LongitudinalData-CreateDataTable}{}}}
\subsection{Method \code{CreateDataTable()}}{
method to create data frame as we wish.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LongitudinalData$CreateDataTable()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LongitudinalData-Plot"></a>}}
\if{latex}{\out{\hypertarget{method-LongitudinalData-Plot}{}}}
\subsection{Method \code{Plot()}}{
method to visualize longitudinal data
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LongitudinalData$Plot()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LongitudinalData-clone"></a>}}
\if{latex}{\out{\hypertarget{method-LongitudinalData-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LongitudinalData$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
