#' @title R6 Indiv Class
#' @description
#' R6 class to store individual longitudinal data. It can store unique ID, longitudinal data (time-dependent variables) as well as other time-independent variable.
#' @importFrom R6 R6Class

Indiv <- R6::R6Class( "Indiv",

                  public = list(

                    #' @field UUID Universally Unique Identifier (UUID). This is a mandatory field.
                    UUID = NULL,
                    #' @field UUID_Other Sometimes, the UUID is lost and another ID is provided. This field is used to store such information.
                    UUID_Other = NULL,
                    #' @field SproutDate Date of sprout. This must be specified is longitudinal data is recorded with un-absolute time scale.
                    SproutDate = NULL,
                    #' @field TimeDependVar Data.frame to store time dependent variable such as phenotype.
                    TimeDependVar = NULL,
                    #' @field TimeIndpendVar Data.frame to store time independent variable such as cultivation condition.
                    TimeIndpendVar = NULL,

                    #' @description Construct Indiv object.
                    #' @param RawData one-row Data Frame with several variables as well as UUID
                    #' @param ListOfData List indicating with column of RawData is corresponding to UUID, TimeDependVar, TimeIndependVar and Date.
                    #' @param SproutDate Date of Sprout. Not mandatory.
                    #' @param UUID_Other Data Frame to connect old UUID and new UUID
                    #' @param duplicate Weather data has duplication or not. This parameter is not used in the function.
                    initialize = function( RawData, ListOfData, SproutDate = NULL, UUID_Other = NULL, duplicate = FALSE  ){
                      self$UUID <- unname( RawData[ ListOfData$UUID ] )
                      self$TimeDependVar <- as.data.frame( as.numeric( RawData[ ListOfData$TimeDependVar ] ) )
                      colnames( self$TimeDependVar ) <- ( as.character( RawData[ ListOfData$Date ] ) )
                      rownames( self$TimeDependVar ) <- ListOfData$TimeDependVar
                      if( ! is.null( ListOfData$TimeIndependVar ) ) { self$TimeIndependVar <- RawData[ ListOfData$TimeIndependVar ] }
                      if( ! is.null( SproutDate ) ){ self$SproutDate <- SproutDate }
                      if( ! is.null( UUID_Other) ){ self$UUID_Other <- UUID_Other[ ( UUID_Other[ , 1] == RawData[ ListOfData$UUID ] ), 2 ] }
                    },

                    #' @description Method to create plot of individual growth. Parameters are same for plot() function
                    #' @param ... Arguments passed to the base plot function.
                    Plot = function( ... ){
                      if( is.null( self$TimeDependVar ) ) {
                        print( "No phenotype specified" )
                      } else {
                        Target <- nrow( self$TimeDependVar)
                        if( Target > 1 ){
                          cat( paste( "Following", nrow( self$TimeDependVar ),"phenotypes are recorded; \n " ) )
                          cat( paste( 1:Target, ".", rownames( self$TimeDependVar ) ), sep = "\t" )
                          Target <- as.numeric( readline( "Please indicate the number of phenotype you want to plot: " ) )
                          while ( ! Target %in% 1:nrow( self$TimeDependVar )  ) {
                            cat( "The indicated number is out of range" )
                            Target <- as.numeric( readline( "Please indicate the number of phenotype you want to plot: " ) )
                          }
                        }
                        plot( x = as.Date( colnames( self$TimeDependVar ) ), y = self$TimeDependVar[ Target, ], ... )
                      }
                    },

                    #' @description Method to create and display the summary of specific individual.
                    Summarize = function(){
                      cat( paste( "UUID:"), self$UUID )
                      cat( paste( "\nUUID_Other:", self$UUID_Other ) )
                      cat( paste( "\nSproutDate:", self$SproutDate ) )
                      cat( paste( "\nTimeIndependVar:", self$TimeIndependVar))
                      cat( "\nTimeDependVar:\n" )
                      print( self$TimeDependVar )
                    },

                    #' @description Method to display Phenotype of specific individual.
                    #' @param Target Name of variable to extract.
                    ExtractPheno = function( Target ){
                      ID <- data.frame( UUID = self$UUID )
                      Candidate <- list( ID, self$TimeDependVar[ Target, ,drop=FALSE ], self$TimeIndependVar )
                      ExtractCand <- Filter( function(x) ! is.null(x), Candidate )
                      return( unlist( ExtractCand ) )
                    },

                    #' @description Method to return unique variable.
                    UniqueValue = function() {
                      return( c( self$UUID, self$UUID_Other, self$SproutDate ) )
                    }

                  )
)
