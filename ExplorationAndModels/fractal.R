library(mandelbrot)
library(ggplot2)

mb <- mandelbrot()
df <- as.data.frame(mb)
head(df)
plot(df, col = mandelbrot_palette(c("white",grey.colors(50))), transform = c("none", "inverse", "log"), asp = 1)

mb <- mandelbrot(xlim = c(-0.805, -0.705),
                 ylim = c(-0.205, -0.105),
                 resolution = 2400L,
                 iterations = 2000)

# vaccination heatmap palette
cols <- c(
  colorRampPalette(c("#e7f0fa", "#c9e2f6", "#95cbee",
                     "#0099dc", "#4ab04a", "#ffd73e"))(10),
  colorRampPalette(c("#eec73a", "#e29421", "#e29421",
                     "#f05336","#ce472e"), bias=2)(90),
  "black")

df <- as.data.frame(mb)
ggplot(df, aes(x = x, y = y, fill = value)) +
  geom_raster(interpolate = TRUE) + theme_void() +
  scale_fill_gradientn(colours = cols, guide = "none")

## "Naive" implementation of Mandelbrot Set in R
# Myles Harrison
# http://www.everydayanalytics.ca

# parameters
cols=colorRampPalette(c("blue","yellow","red","black"))(11)
xmin = 0.25
xmax = 0.27
nx = 5
ymin = -0.01
ymax = 0.01
ny = 5
n=10

# variables
x <- seq(xmin, xmax, length.out=nx)
y <- seq(ymin, ymax, length.out=ny)
c <- outer(x,y*1i,FUN="+")
z <- matrix(0.0, nrow=length(x), ncol=length(y))
k <- matrix(0.0, nrow=length(x), ncol=length(y))

for (rep in 1:n) { 
  # print(rep)
  for (i in 1:nx) { 
    for (j in 1:ny) { 
      print(z[i,j])
      print(c[i,j])
      print(k[i,j])
      if(Mod(z[i,j]) < 2 && k[i,j] < n) {
        z[i,j] <- z[i,j]^2 + c[i,j]
        k[i,j] <- k[i,j] + 1
        print(i)
        print(j)
      }
    }
  }
}
zt <- 0
c <- 0.25000000001+0i 
for(i in 1:1000) {
  if(Mod(zt) < 2) {
    pzt <- zt
    zt <- zt^2 + c
  }
  else {
    print(i)
    break
  }
}
print(pzt)
print(zt)

image(x,y,k, col=cols)


  