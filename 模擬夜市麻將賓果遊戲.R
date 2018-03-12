#載下這份文件，打開R，輸入：source("檔案路徑")，enter執行。

{
  cat("設定夜市麻將bingo基本規則：\n")
  nmahjong <- c();draw <- c();games <- c();simula <- c()
  
  nmahjong <- as.numeric(readline(prompt="一局可以抽幾張牌(預設15張)： "))
  draw <- as.numeric(readline(prompt="聽牌可以抽幾張牌(預設3張，輸入0則無聽牌機制)： "))
  games <- as.numeric(readline(prompt="100元可以玩幾局(預設6局)： "))
  simula <- as.numeric(readline(prompt="要做幾次模擬(預設10萬，不建議超過百萬次)： "))
  
  if(is.na(nmahjong)){nmahjong <- 15
    }
  if(is.na(draw)){draw <- 3
    }
  if(is.na(games)){games <- 6
    }
  if(is.na(simula)){simula <- 100000
    }
  
  
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
    length(win[win==3])/simula)*100
  
  names(result) <- c("One line","Two lines","Three lines")
  
  yim<- ceiling(as.numeric(result[1]))*1.5
  if(yim>100){
    yim <- 100
  }
  options(scipen = 999)
  
  barplot(result,
          ylim=c(0,yim),
          main="Night Market Mahjong Simulation",
          col=c("red","blue","green"),
          legend.text =c(paste0(" ",length(win[win==1]),"/",as.character(simula)),
                         paste0("    ",length(win[win==2]),"/",as.character(simula)),
                         paste0("        ",length(win[win==3]),"/",as.character(simula))),
          axisnames=T,
          ylab = "probability(%)")
  
  cat(paste0("\n\n連成一線的機率為 ",as.numeric(result[1]),"%\n",
             "連成二線的機率為 ",as.numeric(result[2]),"%\n",
             "連成三線的機率為 ",as.numeric(result[3]),"%\n\n",
             "每局中獎機率共 ",sum(result),"%\n",
             "花100元有 ",round((1-(1-length(win[win>=1])/simula)^games)*100,3),"%的機率會中獎\n"))
}




