#defining the function and its derivative, and plotting it.
f<-function(x) x^4-3*x^3+2*x^2+5*x-1
df<-function(x) 4*x^3-9*x^2+4*x+5
curve(f, from=-3, to=3)

#gradient descent to locate minimum initialization bits
x0=-1
lambda=0.1
tol=0.00001
maxsteps=1000
steps=0

#Gradient descent procedure
#Will stop after maxsteps steps or when |f'(xn)|<tol

while(abs(df(x=x0))>=tol & steps<=maxsteps) {
  x0<-x0-lambda*df(x=x0)
  steps<-steps+1
  print(paste(steps,x0,df(x=x0)))
}

#prints out summary of results
print(x=paste("The function attains its minimum value of ",
              round(f(x0),3)," at x = ",round(x0,3)))


## A function of two variables and its first-order partials
g<-function(x,y) x^4+y^4-x+2*y
gx<-function(x,y) 4*x^3-1
gy<-function(x,y) 4*y^3+2

#this produces plots of the function over a square
x<-seq(-2,2,0.1)
y<-seq(-2,2,0.1)
z<-outer(x,y,g)
persp(x,y,z,col="orange",shade=0.4,theta = 30, phi = 15)
contour(x,y,z,nlevels=15)

##gradient descent initialization
x0<-0
y0<-0
lambda<-0.1
maxsteps<-1000
steps<-0
tol<-0.0001

gnorm<-sqrt(gx(x0,y0)^2+gy(x0,y0)^2) #magnitude of gradient vector

#gradient descent loop

while(gnorm>tol & steps<maxsteps){
  x1<-x0-lambda*gx(x0,y0)
  y1<-y0-lambda*gy(x0,y0)
  x0<-x1
  y0<-y1
  steps<-steps+1
  gnorm<-sqrt(gx(x0,y0)^2+gy(x0,y0)^2)
  print(paste(steps,x0,y0,gnorm))
}

print(paste("The function attains its minimum value of ",
            round(g(x0,y0),4),
            " at (x,y) = (",round(x0,4),",",round(y0,4),")."))
