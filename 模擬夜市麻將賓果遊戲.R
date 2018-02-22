
{
win <- c()
for(i in 1:100000){#模擬100000次
zero <- rep(0,36)
zero[c(sample(c(1:36),15,replace = F))] <- c(1)
bingo <- matrix(zero,6,6)

row<- apply(bingo,1,sum)
col<-apply(bingo,2,sum)
diagr <- sum(diag(bingo))
diagl <- sum(diag(t(bingo)))
iswin <- T

while(iswin ==T){#若聽牌則進入迴圈
  if(any(c(row,col,diagr,diagl)==5)){
    zero <- rep(0,36)
    zero[c(sample(c(1:36),15,replace = F))] <- c(1)
    bingo <- matrix(zero,6,6)
  
    row<- apply(bingo,1,sum)
    col<-apply(bingo,2,sum)
    diagr <- sum(diag(bingo))
    diagl <- sum(diag(t(bingo)))
  }else{
    iswin <- F
  }
}
win[i]<- length(c(row,col,diagr,diagl)[c(row,col,diagr,diagl)==6])
}

result<- c(
length(win[win==1])/100000,
length(win[win==2])/100000,
length(win[win==3])/100000)*100

names(result) <- c("連一條線","連兩條線","連三條線")

barplot(result,
        ylim=c(0,10),
        main="抽15張牌",
        col=c("red","blue","green"),
        legend.text =c(paste0("",length(win[win==1]),"/100,000"),
                       paste0("  ",length(win[win==2]),"/100,000"),
                       paste0("      ",length(win[win==3]),"/100,000")),
        axisnames=T,
        ylab = "probalility(%)",
        family="DFHei-W3-WINP-BF")
}
