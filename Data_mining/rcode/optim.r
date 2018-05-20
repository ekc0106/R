### Optimization

f = function(x)
{
   f = (x[1] - 1)^2 + (x[2] - 1)^2 - x[1] * x[2]
}

df = function(x)
{
   df1 = 2 * (x[1] - 1) - x[2]
   df2 = 2 * (x[2] - 1) - x[1]
   df = c(df1, df2)
   return(df)
}

Norm = function(u)
{
   return(sqrt(sum(u^2)))
}

## Steepest decscent method
m = 100
par(mfrow=c(2,2), pty="s")
x1 = x2 = seq(-10.5, 10.5, length=m)
xg = expand.grid(x1, x2)
z = matrix(apply(xg, 1, f), m, m)
xh = NULL; fh = NULL
x0 = c(-10, -3); fx0 = f(x0); ni = 0
for (i in 1:10)
{  
   xh = rbind(xh, x0); fh = c(fh, fx0); ni = ni+1
   cat("iteration=", round(i,2))
   cat("  x0=", round(x0,2), "  f(x0)=", round(f(x0),3), "\n")
   d = df(x0)
   for (iters in 1:20)
   {
      x = x0 - d; fx = f(x)
      if (fx < fx0) break
      d = d / 2
   }
   x0 = x; fx0 = fx
}
contour(x1, x2, z, levels=round(fh, 2))
for (i in 1:(ni-1))
{
   points(xh[i,1], xh[i,2], pch=as.character(i))
   x1=xh[i,1]; y1=xh[i,2]; x2=xh[i+1,1]; y2=xh[i+1,2]
   arrows(x1, y1, x2, y2, length=0.1, col="red", lwd=0.5)
}
points(xh[ni,1], xh[ni,2], pch=as.character(ni))
x1 = x2 = seq(0.4, 3, length=m)
xg = expand.grid(x1, x2)
z = matrix(apply(xg, 1, f), m, m)
xh = NULL; fh = NULL
x0 = c(-10, -3); fx0 = f(x0); ni = 0
for (i in 1:10)
{  
   xh = rbind(xh, as.vector(x0)); fh = c(fh, fx0); ni = ni+1
   cat("iteration=", round(i,2))
   cat("  x0=", round(x0,2), "  f(x0)=", round(f(x0),3), "\n")
   d = df(x0)
   for (iters in 1:20)
   {
      x = x0 - d; fx = f(x)
      if (fx < fx0) break
      d = d / 2
   }
   if (abs(fx-fx0) < 1e-5) break
   x0 = x; fx0 = fx
}
contour(x1, x2, z, levels=round(fh, 2))
for (i in 1:(ni-1))
{
   points(xh[i,1], xh[i,2], pch=as.character(i))
   x1=xh[i,1]; y1=xh[i,2]; x2=xh[i+1,1]; y2=xh[i+1,2]
   arrows(x1, y1, x2, y2, length=0.1, col="red", lwd=0.5)
}
points(xh[ni,1], xh[ni,2], pch=as.character(ni))

## Newton-Raphson method
x1 = x2 = seq(-10.5, 10.5, length=m)
xg = expand.grid(x1, x2)
z = matrix(apply(xg, 1, f), m, m)
xh = NULL; fh = NULL
x0 = c(-10, -3); fx0 = f(x0); ni = 0
df2 = matrix(c(2,-1,-1,2),2,2); v = solve(df2)
for (i in 1:10)
{  
   xh = rbind(xh, as.vector(x0)); fh = c(fh, fx0); ni = ni+1
   cat("iteration=", round(i,2))
   cat("  x0=", round(x0,2), "  f(x0)=", round(f(x0),3), "\n")
#   d = df(x0)
   d = v %*% df(x0)
   for (iters in 1:20)
   {
      x = x0 - d; fx = f(x)
      if (fx < fx0) break
      d = d / 2
   }
   if (abs(fx-fx0) < 1e-5) break
   x0 = x; fx0 = fx
}
contour(x1, x2, z, levels=round(fh, 2))
for (i in 1:(ni-1))
{
   points(xh[i,1], xh[i,2], pch=as.character(i))
   x1=xh[i,1]; y1=xh[i,2]; x2=xh[i+1,1]; y2=xh[i+1,2]
   arrows(x1, y1, x2, y2, length=0.1, col="red", lwd=0.5)
}
points(xh[ni,1], xh[ni,2], pch=as.character(ni))
