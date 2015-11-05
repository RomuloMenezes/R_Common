prepareData<-function(inputMatrix, timestampIndex, valueIndex, colOrRow){
  if(colOrRow=="c"||colOrRow=="C"){
    matrizSaida=matrix(, nrow = 24, ncol = 0)
  }
  else{
    matrizSaida=matrix(, nrow = 0, ncol = 24)
  }
  dropVector=as.vector(c())
  vectorAuxCurrDateValues=as.vector(inputMatrix[1,valueIndex])
  vectorDataWithTimestamp=as.vector(c())
  matrixOutputWithTimestamps=matrix(ncol = 25)
  prevValue=substr(inputMatrix[1,timestampIndex],1,10)
  for(i in seq(2,length(inputMatrix[,timestampIndex]))){
    if(substr(inputMatrix[i,timestampIndex],1,10)==prevValue){
      vectorAuxCurrDateValues=append(vectorAuxCurrDateValues,inputMatrix[i,valueIndex])
    }
    else{
      canUse=TRUE
      for(j in seq(1,length(vectorAuxCurrDateValues))){
        if(is.na(vectorAuxCurrDateValues[j])){
          canUse=FALSE
          break()
        }
      }
      if(canUse){
        vectorDataWithTimestamp=c(prevValue,vectorAuxCurrDateValues)
        if(colOrRow=="c"||colOrRow=="C"){
          matrizSaida=cbind(matrizSaida,vectorAuxCurrDateValues)
          matrixOutputWithTimestamps=cbind(matrixOutputWithTimestamps,vectorDataWithTimestamp)
        }
        else{
          matrizSaida=rbind(matrizSaida,vectorAuxCurrDateValues)
          matrixOutputWithTimestamps=rbind(matrixOutputWithTimestamps,vectorDataWithTimestamp)
        }
      }
      else
        dropVector=append(dropVector,prevValue)
      vectorAuxCurrDateValues=as.vector(inputMatrix[i,valueIndex])
    }
    prevValue=substr(inputMatrix[i,timestampIndex],1,10)
  }
  
  # Treating the last date
  canUse=TRUE
  for(j in seq(1,length(vectorAuxCurrDateValues))){
    if(is.na(vectorAuxCurrDateValues[j])){
      canUse=FALSE
      break()
    }
  }
  if(canUse){
    vectorDataWithTimestamp=c(prevValue,vectorAuxCurrDateValues)
    if(colOrRow=="c"||colOrRow=="C"){
      matrizSaida=cbind(matrizSaida,vectorAuxCurrDateValues)
      matrixOutputWithTimestamps=cbind(matrixOutputWithTimestamps,vectorDataWithTimestamp)
    }
    else{
      matrizSaida=rbind(matrizSaida,vectorAuxCurrDateValues)
      matrixOutputWithTimestamps=rbind(matrixOutputWithTimestamps,vectorDataWithTimestamp)
    }
  }
  else
    dropVector=append(dropVector,prevValue)
  
  returnList=list(matrizSaida, dropVector, matrixOutputWithTimestamps)
  return(returnList)
}