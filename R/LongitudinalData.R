#' @title R6 LongitudinalData Class
#' @description
#' R6 class to store all data. Individual's data is stored with Indiv R6 class.
#' @importFrom R6 R6Class
#' @export

LongitudinalData <- R6::R6Class( "LongitudinalData",

                             public = list(
                               #' @field RawData Data frame to store un-processed longitudinal data.
                               RawData = NULL,
                               #' @field Indivs List of R6 Indiv Class.
                               Indivs = list(),
                               #' @field Record Vector to store measurement data.
                               Record = list( Date = NULL, Fixed = NULL ),
                               #' @field NameOfPheno Vector to store all of variables.
                               NameOfPheno = NULL,
                               #' @field PhenotypeTable Field to store R6 Table Class.
                               PhenotypeTable = list(),

                               #' @description Construct LongitudinalData object.
                               #' @param RawData one-row Data Frame with several variables as well as UUID
                               #' @param ListOfData List indicating with column of RawData is corresponding to UUID, TimeDependVar, TimeIndependVar and Date.
                               #' @param SproutDate Date of Sprout. Not mandatory.
                               #' @param UUID_Other Data Frame to connect old UUID and new UUID
                               #' @param duplicate Weather data has duplication or not. This parameter is not used in the function.
                               initialize = function( RawData, ListOfData, SproutDate = NULL, UUID_Other = NULL, duplicate = FALSE ){
                                 if( is.null( ListOfData$UUID ) ){
                                   print( "UUID must be specified in the Data Frame")
                                 } else if ( is.null( ListOfData$Date ) ) {
                                   print( "Date of measuremt must be specified in the Data Frame")
                                 } else if ( is.null( ListOfData$TimeDependVar ) ){
                                   print( "Phenotype must be specified in the Data Frame")
                                 } else {
                                   self$RawData <- RawData[,unname(unlist(ListOfData))]
                                   colnames( self$RawData )[ colnames( self$RawData ) == ListOfData$UUID ]  <- "UUID"
                                   self$Indivs  <- apply( RawData, 1, Indiv$new, ListOfData, SproutDate = NULL, UUID_Other = NULL, duplicate = FALSE )
                                   self$NameOfPheno <- unique( ListOfData$TimeDependVar )
                                   self$Record$Date <- unique( na.omit( names( summary( as.factor(RawData[, ListOfData$Date ] ) ) ) ) )
                                   if( !is.null(ListOfData$TimeIndependVar) ){
                                     self$Record$Fixed <- ListOfData$TimeIndependVar
                                   }
                                 }
                               },

                               #' @description Method to eliminate duplication as well as integrate time-series data.
                               IntegDuplication = function() {
                                 ID <- sapply( self$Indivs, function(x, name) x[[name]], "UUID" )
                                 DuplicatedID <- unique(  ID[ duplicated( ID ) ] )
                                 if( length(DuplicatedID) == 0 ) {
                                   print( "There is no duplication" )
                                 } else {
                                   DuplicatedList <- lapply( DuplicatedID, function(ID) {
                                     self$Indivs[ sapply( self$Indivs,  function( List, ID ){ return( List$UUID == ID ) }, ID ) ] } )
                                   n_Dupl <- length( DuplicatedList )
                                   for( i in 1:n_Dupl ){
                                     TargetList <- DuplicatedList[[i]]
                                     n_indivs <- length( TargetList )
                                     if( ! Reduce( function( x, y ) x && private$Check( y, TargetList[[1]] ), TargetList[-1], init=TRUE ) ){
                                       cat( paste( "The parameters of ", DuplicatedID[[i]], "doesn't match", sep = "" ) )
                                     }
                                   }
                                   IntegratedList <- lapply( DuplicatedList, function( TargetList ) Reduce( private$Integrate, TargetList ) )
                                   self$Indivs[ ! duplicated( ID ) & duplicated( ID, fromLast = TRUE ) ] <- IntegratedList
                                   self$Indivs <- self$Indivs[ ! duplicated( ID ) ]
                                   # names( self$Indivs ) <- sapply( self$Indivs, function(x, name) x[[name]], "UUID" )
                                 }
                                 names( self$Indivs ) <- sapply( self$Indivs, function(x, name) x[[name]], "UUID" )
                               },

                               # method to integrate original ID and changed ID
                               # InegrateChangedID = function(){},

                               #' @description Method to add new data.
                               #' @param RawData one-row Data Frame with several variables as well as UUID
                               #' @param ListOfData List indicating with column of RawData is corresponding to UUID, TimeDependVar, TimeIndependVar and Date.
                               #' @param SproutDate Date of Sprout. Not mandatory.
                               #' @param UUID_Other Data Frame to connect old UUID and new UUID
                               #' @param duplicate Weather data has duplication or not. This parameter is not used in the function.
                               AddData = function( RawData, ListOfData, SproutDate = NULL, UUID_Other = NULL, duplicate = FALSE ){
                                 NewData <- LongitudinalData$new( RawData, ListOfData, SproutDate = NULL, UUID_Other = NULL, duplicate = FALSE )
                                 self$Indivs <- c( self$Indivs, NewData$Indivs )
                                 self$RawData <- rbind( self$RawData, NewData$RawData )
                                 self$NameOfPheno <- unique( c( self$NameOfPheno, NewData$NameOfPheno ))
                                 self$Record$Date <- sort( unique( c( self$Record$Date, NewData$Record$Date ) ) )
                                 if( !is.null( NewData$Record$Fixed ) ){
                                   self$Record$Date <- unique( c( self$Record$Date, NewData$Record$Date ) )
                                 }
                               },

                               #' @description method to create data frame as we wish.
                               CreateDataTable = function( ) {
                                 Target <- length( self$NameOfPheno )
                                 if( Target > 1 ){
                                   cat( paste( "Following", length( self$NameOfPheno ),"phenotypes are recorded; \n " ) )
                                   cat( paste( 1:Target, ".",  self$NameOfPheno  ), sep = "\t" )
                                   Target <- as.numeric( readline( "Please indicate the number of phenotype you want to plot: " ) )
                                   while ( ! Target %in% 1:length( self$NameOfPheno )  ) {
                                     cat( "The indicated number is out of range" )
                                     Target <- as.numeric( readline( "Please indicate the number of phenotype you want to plot: " ) )
                                   }
                                 }
                                 TargetTrait <- self$NameOfPheno[ Target ]
                                 self$PhenotypeTable <- c( self$PhenotypeTable, Table$new( self, TargetTrait ) )
                                 names( self$PhenotypeTable )[length(self$PhenotypeTable)] <- TargetTrait
                               },
                               #' @description method to visualize longitudinal data
                               Plot = function(){
                                 l_list <- length(self$PhenotypeTable)
                                 l_target <- 1
                                 if(l_list > 1 ){
                                   cat( paste( "Following", l_list,"phenotypes are recorded; \n " ) )
                                   cat( paste( 1:l_list, ".",  names(Pheno$PhenotypeTable)  ), sep = "\t" )
                                   l_target <- as.numeric( readline( "Please indicate the number of phenotype you want to plot: " ) )
                                 }
                                 while ( ! l_target %in% 1:l_list  ) {
                                   cat( "The indicated number is out of range" )
                                   l_target <- as.numeric( readline( "Please indicate the number of phenotype you want to plot: " ) )
                                 }
                                 Target <- self$PhenotypeTable[[l_target]]
                                 Day <- self$Record$Date
                                 matplot( as.Date(Day), t( Target$Table[,Day]), type = "l", xlab="Time", ylab="Phenotype")
                               }

                               # method to create new variable such as TC/TC_R
                               # CreateNew = function( ){}

                             ),

                             private = list(
                               Check = function( Indivs1, Indivs2 ){
                                 ID <- ( Indivs1$UniqueValue() == Indivs2$UniqueValue() )
                                 TimeIndepend <- TRUE
                                 if( ! is.null( Indivs1$TimeIndpendVar ) && ! is.null( Indivs2$TimeIndpendVar ) ) {
                                   TimeIndepend <- identical( Indivs1$TimeIndpendVar, Indivs2$TimeIndpendVar )
                                 }
                                 return( ID && TimeIndepend )
                               },

                               Integrate = function( Indivs1, Indivs2 ){
                                 Indivs2$TimeDependVar <- private$IntegrateTable( Indivs1$TimeDependVar, Indivs2$TimeDependVar )
                                 return( Indivs2 )
                               },

                               IntegrateTable = function( Df1, Df2 ){
                                 CommonName <- intersect( colnames(Df1), colnames(Df2) )
                                 if( length( CommonName) >= 1 ){
                                   Df1.Common <- Df1[ , CommonName, drop=FALSE ]
                                   Df2.Common <- Df2[ , CommonName, drop=FALSE ]
                                   Df.Common.New <- ( Df1.Common + Df2.Common )/2

                                   Df1.Unique <- Df1[ ,! which( colnames( Df1 )  %in%  CommonName ), drop=FALSE ]
                                   Df2.Unique <- Df2[ ,! which( colnames( Df2 )  %in%  CommonName ), drop=FALSE  ]
                                   Df.List <- list( Df1.Unique, Df2.Unique, Df.Common.New )
                                   NotEmply <- Filter( function(x) ncol(x) > 0, Df.List )
                                   if( length(NotEmply) > 0 ){
                                     Df <- do.call( cbind, NotEmply )
                                   }
                                 } else{
                                   Df <- cbind( Df1, Df2 )
                                 }
                                 Df <- Df[ ,order( colnames(Df) ), drop = FALSE ]
                                 return( Df )
                               }
                             )
)
