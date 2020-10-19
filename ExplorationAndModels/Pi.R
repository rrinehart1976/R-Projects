library(tidyverse)
#Defining URL to download csv (100k digits)
url<-"https://www.angio.net/pi/digits/100000.txt"
#reading file
d <- readr::read_file(url)
dlist = list()
j <- 0
for(i in 0:99999)
{
  if(i>10000)
  { 
    sc <- str_count(d, as.character(i)) 
    if(sc > 5)
    {
      j <- j + 1
      z <- cbind.data.frame(x = sc, y = i)
      dlist[[j]] <- z
    }
  }
    
}

big_data = do.call(rbind, dlist)

#splitting string into single characters
data.vec<-strsplit(data.raw, "")


#vector to dataframe and 
data.df1<-data.frame(data.vec[1])

#removing the "." i.e. 2nd row
data.df2<-data.frame(data.df1[-c(2),])

#renaming column to digits
names(data.df2)[1]<-"digits"

##Dividing the digits into x and y 
#Adding a column of row number
data.df3<-data.df2%>%
  mutate(id=seq(1:nrow(data.df2)))

#Adding a column of coordinate (x or y) and reshuffling the columns
data.df4<-data.df3%>%
  mutate(cor=ifelse(id%%2==0,"y","x"))


#changing class of digits to integers. Earlier parsed as char

data.df4$digits<-as.integer(data.df4$digits)

#Creating data frame of x and y coordinates
x<-data.df4[c(T,F),1]
y<-data.df4[c(F,T),1]

data.cor<-data.frame(x,y)

head(data.cor)

#creating a column showing row position
data.cor$pos<-seq(1:nrow(data.cor))

#defining the size of each group
lot=1000

#Creating groups
data.plot<-data.cor%>%
  mutate(grp=floor(pos/lot)+1)


#Creating vectors with coordinates of first dot of each group
minx=NULL
miny=NULL
for(i in 0:(49999)){
  j=(floor(i/lot)*lot)+1
  minx[i+1]<-data.plot$x[j]
  miny[i+1]<-data.plot$y[j]
}

#creating columns from vector
data.plot$xend=minx
data.plot$yend=miny

#Removing rows containing coordinates of first point of each group

rowrmv<-c(which(data.plot$x==data.plot$xend & data.plot$y==data.plot$yend))

data.plotf<-data.plot[-rowrmv,]

#Plotting
art100k.1<-data.plotf%>%
  ggplot()+
  geom_curve(aes(x = x, y = y, xend = xend, yend = yend, colour = factor(x)), 
             curvature=0.2,alpha=0.2)+theme_void()+ scale_color_brewer(palette="Greys")+
  theme(legend.position = "none",panel.background = element_rect(fill="#000000"))+
  xlim(-1,10)+ylim(-1,10)

plot(art100k.1)
