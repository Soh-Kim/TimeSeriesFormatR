#' @title R6 Table Class
#' @description
#' R6 class to develop convenient data frame to analyse time series data.
#' @importFrom R6 R6Class
#' @export

Table <- R6::R6Class( "Table",
                  public = list(
                    #' @field Table Data Frame to store processed longitudinal data.
                    Table= NULL,
                    #' @description Method to Create R6 Table object.
                    #' @param LongitudinalData R6 LongitudinalData Class object.
                    #' @param TargetTrait It indicate on which phenotype is used to constract the table.
                    initialize = function( LongitudinalData, TargetTrait ){
                      colname <- unname(unlist( LongitudinalData$Record ) )
                      ListOfDf <- lapply( LongitudinalData$Indivs, private$extract, Target = TargetTrait, colname = colname )
                      self$Table <- do.call( rbind, ListOfDf )
                      self$Talbe[, LongitudinalData$Record$Date ] <- lapply( self$Talbe[, LongitudinalData$Record$Date ], as.numeric)
                    }
                  ),
                  private = list(
                    extract = function(Indivs, Target, colname ){
                      Df <-( Indivs$ExtractPheno(Target) )
                      colname <- c( "UUID", colname )
                      Df.expand <- data.frame( matrix( NA, nrow = 1, ncol=( length(colname ) ) ) )
                      colnames( Df.expand ) <- colname
                      rownames( Df.expand ) <- Indivs$UUID
                      Df.expand[ , names( Df ) ] <- unname( Df )
                      return( Df.expand )
                    }
                  )
)
