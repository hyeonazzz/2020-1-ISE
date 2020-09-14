library(jmotif)
library(TSclust)

# PAA Chart
X <- c(-1.5, 0, 0, 2, 0.5, 0, -2, 0, -0.5, 0)
A <- 5
N <- length(X)
plot(X, type="l", col="royalblue", xlab ="Time index",
     xlim=c(0, N), ylim=c(-2.1, 2.1), xaxt="n")
axis(1, at = seq(1,10))
points(X, pch=16, lwd=5, col="royalblue")
abline(v=c(1,1+9/5,1+9/5*2,1+9/5*3,1+9/5*4, 10), lty=3, lwd=2, col="gray50")

x_paa = paa(X, A)

segments(1,x_paa[1],1+9/5,x_paa[1],lwd=1,col="red")
points(x=1+9/5/2,y=x_paa[1],col="red",pch=4,lwd=2)

segments(1+9/5,x_paa[2],1+9/5*2,x_paa[2],lwd=1,col="red")
points(x=1+9/5+9/5/2,y=x_paa[2],col="red",pch=4,lwd=2)

segments(1+9/5*2,x_paa[3],1+9/5*3,x_paa[3],lwd=1,col="red")
points(x=1+9/5*2+9/5/2,y=x_paa[3],col="red",pch=4,lwd=2)

segments(1+9/5*3,x_paa[4],1+9/5*4,x_paa[4],lwd=1,col="red")
points(x=1+9/5*3+9/5/2,y=x_paa[4],col="red",pch=4,lwd=2)

segments(1+9/5*4,x_paa[5],10,x_paa[5],lwd=1,col="red")
points(x=1+9/5*4+9/5/2,y=x_paa[5],col="red",pch=4,lwd=2)

# SAX Chart
N <- 100
W <- 20
A <- 10

# X = round(runif(N, min = -2, max = 2))
X = round(rnorm(N, sd = 2),1)
# X = c(-1.5, 0, 0, 2, 0.5, 0, -2, 0, -0.5, 0)

plot(X, type="l", col="royalblue", xlab = "Time index",
     xlim=c(0, N), ylim=c(-2.1, 2.1), xaxt="n")
# axis(1, at = seq(1,N))

seg <- c(1, 1+(N-1)/W * seq(1,W-1), N)
seg_cen <- c(1+(N-1)/W * seq(0,W-1) + (N-1)/W/2)
abline(v=seg, lty=3, lwd=2, col="gray50")

x_paa <- paa(X, W)
paa_char <- series_to_chars(x_paa, A)

for (i in 1:length(seg)){
  let_index <- match(paa_char[i], letters)
  seg_col <- rainbow(A)[let_index]
  
  segments(seg[i], x_paa[i], seg[i+1], x_paa[i], lwd=3, col=seg_col)
  text(x=seg_cen[i], y=x_paa[i]+0.2, paa_char[i], cex=1, col="black")   
}

# y <- seq(-2,2, length=100)
# x <- dnorm(y, mean=0, sd=1)
# lines(x,y, type="l", lwd=2, col="black")
# abline(h = alphabet_to_cuts(3)[2:3], lty=2, lwd=2, col="black")
# text(0.7,-1,"a",cex=1.5,col="black")
# text(0.7, 0,"b",cex=1.5,col="black")
# text(0.7, 1,"c",cex=1.5,col="black")

