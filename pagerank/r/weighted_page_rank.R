adjacencyMatrix<-function(pages){
 # n<-max(apply(pages,2,max))
  n<-max(mapply(max,pages$src,pages$dist))
  A <- matrix(0,n,n)
  for(i in 1:nrow(pages)) A[pages[i,]$dist,pages[i,]$src]<-pages[i,]$score
  A
}

probabilityMatrix<-function(G){
  cs <- colSums(G)
  cs[cs==0] <- 1
  n <- nrow(G)
  A <- matrix(0,nrow(G),ncol(G))
  for (i in 1:n) A[i,] <- A[i,] + G[i,]/cs
  A
}

pagerank<-function(m,a){
	m_nrow=dim(m)[1]
	v0 <- rep(1/m_nrow,m_nrow)
	U <- diag(m_nrow)
	#U <- matrix(rep(1/m_nrow,times=m_nrow*m_nrow),m_nrow)
	print(U)
	G <- a * m + ((1 - a)) * U	
	v <- v0
	repeat{
		v <- G %*% v
		d <- dist(matrix(c(v,v0),2,byrow=TRUE),method="euclidean")
		if ( d < 1e-6) break
		v0 <- v
	}
	return(v)
}

Args <- commandArgs()
cat( Args[1],"\n" )
cat( Args[2],"\n" )
cat( Args[3],"\n" )
cat( Args[4] ,"\n")
cat( Args[5],"\n" )
cat( Args[6],"\n" )
inputFileName <- Args[6]
outputFileName <- Args[7]

pages<-read.table(file=inputFileName,header=FALSE,sep=",")
names(pages)<-c("src","dist","score");
A<-adjacencyMatrix(pages)
G<-probabilityMatrix(A)
#print(A)
#print(pages)
#print(max(mapply(max,pages$src,pages$dist)))
#print(G)
a <- 0.85
v<- pagerank(G,a)
print(v)
write.table(v,file=outputFileName,row.names = F, quote = F, sep="\t")
