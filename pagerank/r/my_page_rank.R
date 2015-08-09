m <- matrix( c(0,1/3.0,1/3.0,1/3.0,0.5,0,0,0.5,1,0,0,0,0,0.5,0.5,0),nrow=4,ncol=4 )
a <- 0.85
U <- matrix(rep(1/4,times=16),4)
v <- rep(1/4,4)
G <- a * m + ((1 - a)) * U

print("start")
for (i in 1:5 )
{
	v <- G %*% v ;
	print(v)
}
