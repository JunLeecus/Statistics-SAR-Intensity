library(RSpectra)
library(jpeg)
library(animation)
require(ggplot2)

ggplot(data=data.frame(x=c(0, 7)), aes(x=x)) +
  stat_function(fun=dexp, geom = "line", size=2, col="black", args = (mean=1)) +
  stat_function(fun=dexp, geom = "line", size=2, col="red", args = (mean=.5)) +
  stat_function(fun=dexp, geom = "line", size=2, col="blue", args = (mean=2)) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  xlab("x") + ylab("Exponential Densities")


ggplot(data=data.frame(x=seq(0, 7, length.out = 500)), aes(x=x)) +
  stat_function(fun=dexp, geom = "line", size=2, col="black", args = (mean=1)) +
  stat_function(fun=dexp, geom = "line", size=2, col="red", args = (mean=.5)) +
  stat_function(fun=dexp, geom = "line", size=2, col="blue", args = (mean=2)) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  xlab("x") + ylab("Exponential Densities") +
  coord_trans(y="log10")

ggplot(data=data.frame(x=seq(10^-3, 5, length.out = 500)), aes(x=x)) +
  stat_function(fun=dgamma, geom = "line", size=2, col="black", args = list(shape=1, scale=1)) +
  stat_function(fun=dgamma, geom = "line", size=2, col="red", args = list(shape=3, scale=1/3)) +
  stat_function(fun=dgamma, geom = "line", size=2, col="blue", args = list(shape=8, scale=1/8)) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  xlab("x") + ylab("Gamma Densities")

ggplot(data=data.frame(x=seq(10^-4, 5, length.out = 1000)), aes(x=x)) +
  stat_function(fun=dgamma, geom = "line", size=2, col="black", args = list(shape=1, scale=1)) +
  stat_function(fun=dgamma, geom = "line", size=2, col="red", args = list(shape=3, scale=1/3)) +
  stat_function(fun=dgamma, geom = "line", size=2, col="blue", args = list(shape=8, scale=1/8)) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  xlab("x") + ylab("Gamma Densities") +
  coord_trans(y="log10")

r <- 0.299
g <- 0.587
b <- 0.114
pic <- readJPEG("/Users/lijun/Desktop/pic.jpg")
R <- pic[,,1]
G <- pic[,,2]
B <- pic[,,3]
#通过灰度进行图像转化
new_pic <- r*R + g*G + b*B  
a <- hist(new_pic, 
          breaks = seq(0,1,0.02), 
          freq = F)
b <- matrix(unlist(a))
b <- b[101:150]
b <- rev(b)
inputdata <- data.frame(gray_value=seq(0,0.998,0.02),gray_frequency=b)
ggplot(data=inputdata,aes(x=gray_value,y=as.numeric(gray_frequency)/16)) + 
  geom_bar(stat="identity") +
  stat_function(fun=dgamma, geom = "line", size=2, col="black", args = list(shape=3, scale=1/9)) +
  stat_function(fun=dgamma, geom = "line", size=2, col="red", args = list(shape=3, scale=1/7)) +
  stat_function(fun=dgamma, geom = "line", size=2, col="blue", args = list(shape=3, scale=1/8)) +
  xlab("Grey Value") + ylab("Gray Histogram and Gamma Densities")

