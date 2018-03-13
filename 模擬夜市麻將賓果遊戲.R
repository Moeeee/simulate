#模擬賓果麻將程式原始碼

mahjong<- function(nmahjong=15,draw=3,games=6,simula=100000){
  
  cat("模擬中請稍後...")
  
  win <- c()
  for(i in 1:simula){#模擬100000次
    zero <- rep(0,36)
    position<- c(sample(c(1:36),nmahjong,replace = F))
    zero[position] <- c(1)
    bingo <- matrix(zero,6,6)
    
    
    row<- apply(bingo,1,sum)
    col<-apply(bingo,2,sum)
    diagr <- sum(diag(bingo))
    diagl <- sum(diag((bingo[c(6:1),])))
    
    if((draw!=0) & any(c(row,col,diagr,diagl)==5) & any(c(row,col,diagr,diagl)!=6)){#聽牌就再抽三支
      zero[sample(c(1:36)[-position],draw,replace = F)] <- c(1)
      bingo <- matrix(zero,6,6)
      
      row<- apply(bingo,1,sum)
      col<-apply(bingo,2,sum)
      diagr <- sum(diag(bingo))
      diagl <- sum(diag((bingo[c(6:1),])))
    }
    
    win[i]<- length(c(row,col,diagr,diagl)[c(row,col,diagr,diagl)==6])#判斷是否有贏
    if(i%%(simula/10)==0){
      cat(paste0(i/simula*100,"%..."))
    }
  }
  
  result<- c(
    length(win[win==1])/simula,
    length(win[win==2])/simula,
    length(win[win>=3])/simula)*100
  
  names(result) <- c("One line","Two lines","Three lines more")
  
  
  out <- data.frame(result[1],result[2],result[3]
                    ,sum(result),round((1-(1-length(win[win>=1])/simula)^games)*100,3),
                    nmahjong,draw,games)
  names(out) <- c(names(result),"每局勝率","100元勝率","nmahjong","draw","games")
  row.names(out) <- 1
  cat("\n")
  return(out)
}
mahjong(nmahjong = 15, draw = 3, games = 6, simula = 100000)
