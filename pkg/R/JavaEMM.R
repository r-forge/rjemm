## constructor
## possible measures: "Cosine", "Dice", "Euclidean", "Jaccard", "Overlap"
create_EMM <- function(measure="Cosine", threshold=0.2, location="") {    .jpackage("RJEMM", jars='*')

    EMM <- .jnew("jemm/EMM")
	if( location != "" ){
	      EMM <- .jcall(EMM, "Ljemm/EMM;","load",location)
	}
	simMeasure <- .jnew("java/lang/String", measure)
	threshold <- .jnew("java/lang/Double",threshold)
	EMMIncrement <- .jnew("jemm/EMMIncrement", simMeasure, threshold)
	structure(list(
	    EMM=EMM, 
            simMeasure = simMeasure, 
            threshold = threshold, 
            EMMIncrement = EMMIncrement,            
	    current = NULL
        ), class ="JavaEMM")
}

build <- function(JEMM ,newdata) {
    dataMatrix <- as.matrix(rbind(newdata))
    for(i in 1:nrow(dataMatrix)){
		vectorToAdd <- as.double(as.vector(dataMatrix[i,]))
		.jcall(JEMM$EMMIncrement,"V", "buildEMM", JEMM$EMM, vectorToAdd)
	}      
}

detectEvent <- function(JEMM, newdata, measure="Cosine", threshold=0.2 ){
	simMeasure <- .jnew("java/lang/String", measure)
	threshold <- .jnew("java/lang/Double",threshold)
	dataMatrix <- as.matrix(rbind(newdata))	
	EMMDetect <- .jnew("jemm/EMMDetection", simMeasure, threshold)
	for(i in 1:nrow(dataMatrix)){
		vectorToAdd <- as.double(as.vector(dataMatrix[i,]))
		.jcall(EMMDetect, "V", "detectEvent", JEMM$EMM, vectorToAdd )
	}  	
	.jcall(JEMM$EMM, "V", "printRareStateString")
}


display <-function(JEMM){
	graphDisplay <- new("graphNEL", edgemode="directed")
	numOfStates <- .jcall(JEMM$EMM, "I", "getNumOfState")
	a <- c(1:numOfStates)
 	toAddChar <- as.character(a)
	graphDisplay <- addNode( toAddChar ,graphDisplay)


	links <- .jcall( JEMM$EMM, "[D", "getLinksInArray")
	timesTraversed <- .jcall( JEMM$EMM, "[D", "getTimesTraversedInArray" )
	links <- as.character(links)
	for( i in 1 : ( (length(links) / 2) ) ){
		timesTraversed[i - 1]		
		graphDisplay <- addEdge( links[(i * 2) - 1], links[i * 2], graphDisplay, timesTraversed[i - 1] )  
	} 	

	plot(graphDisplay)

}	

save <- function(location="/EMMModel.bin", JEMM){
	.jcall(JEMM$EMM, "V", "save", location, JEMM$EMM )
} 

displayStates <-function(JEMM){
	.jcall(JEMM$EMM, "V", "printStatePoolR")
}

displayLinks <-function(JEMM){
	.jcall(JEMM$EMM, "V", "printLinkPoolR")
}




