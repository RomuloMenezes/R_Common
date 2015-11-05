elbow<-function(inputMatrix, maxK){
  ret=vector("numeric")
  for(i in seq(1,maxK)){
    cluster=kmeans(inputMatrix,i)
    ret[i]=cluster$tot.withinss
  }
  plot(1:length(ret), ret, type = "b")
}