
m <- matrix( c(0,1/3.0,1/3.0,1/3.0,0.5,0,0,0.5,1,0,0,0,0,0.5,0.5,0),nrow=4,ncol=4 )
v <- matrix( c(0.25,0.25,0.25,0.25),nrow=4,ncol=1 )
a <- 0.85
b <- 0.15

bia <- b * v

print(m)
print(v)
print(bia)

print("start")
for (i in 1:5 )
{
	v <-  a * m %*% v + bia;
	print(v)
}
