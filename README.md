# Ball_simulation

## ggplot2 로드하기 ###

library(ggplot2)

## 1~10까지 random 숫자를 x 좌표 1000개, y좌표 1000개를 뽑아서 a1, a2에 넣기 ####

#1000개의 구술에 x,y 좌표 이름 붙이기 (랜덤)
a1<-round(runif(1000)*9)+1
a2<-round(runif(1000)*9)+1

#1000개의 구술에 x,y 좌표 이름 붙이기 (일정)
a1<-rep(c(1:10),100)
a2<-rep(1:10,each=100)


##a1, a2를 각각 x,y 좌표로 해서 matrix로 만들어서 m에 넣기 ####

m<-data.frame(cbind(a1,a2))

## x, y에 각각 몇개씩의 구슬이 들어있는지 y로 구하고 이것을 data.frame으로 만들어서 z에 넣기 ####

y<-table(m$a1,m$a2)

z<-data.frame(y)

## 구슬의 개수만큼 원의 사이즈를 비례하게 만들어서 x,y matrix에 표시하기 ####

ggplot(z,aes(x=Var1,y=Var2,fill=z$Freq))+geom_point(colour="grey",shape=21,size=z$Freq)+ xlab('x')+ylab('y')+ggtitle('구슬 분포')+labs(fill='구슬수')

## 구슬의 개수 히스토그램 그리기 ####

hist(y,main="구슬 옮기기", ylim=c(0,30), xlab="구슬 갯수", ylab="빈도수",col="#80fd3d", breaks=seq(0, 30, 2))

## 한칸씩 욺직이며 구슬 옮기기, 옮기는 횟수를 100번하기 ###

for (l in c(1:100)){
  
  for (i in c(1:10)){
    
    for (j in c(1:10)) { 
      
      ## 구슬을 1개 이상일 때만 옮기기 ##
      
      if (y[i,j]>0){ 
        
        ### 옮기는 구슬의 개수 정하기 t ###
        
        t<-1
        
        ### 구슬을 옮길 때 상/하/좌/우 네가지 경우의 수를 임의로 정하기 k ###
        
        k<-(round(runif(1)*9)+1)%%4
        
        ## 옮기는 칸의 구슬은 t만큼 수를 줄이기
        
        y[i,j]<-y[i,j]-t    
        
        ## k=0 이면 왼쪽으로 구슬 t개를 주기 ##
        
        if(k==0){
          
          if(i==1){
            
            y[i+9,j]=y[i+9,j]+t }
          
          else {
            
            y[i-1,j] = y[i-1,j] + t
            
          }
          
        }
        
        ## k=1 이면 오른쪽으로 구슬 t개를 주기 ##
        
        else if(k==1){
          
          if(i==10){
            
            y[i-9,j]=y[i-9,j]+t }
          
          else {
            
            y[i+1,j] = y[i+1,j] + t
            
          }
          
        }
        
        ## k=2 이면 윗쪽으로 구슬 t개를 주기 ##
        
        else if(k==2){
          
          if(j==1){
            
            y[i,j+9]=y[i,j+9]+t }
          
          else {
            
            y[i,j-1] = y[i,j-1] + t
            
          }
          
        }
        
        ## k=3 이면 아래쪽으로 구슬 t개를 주기 ##
        
        else if(k==3){
          
          if(j==10){
            
            y[i,j-9]=y[i,j-9]+t }
          
          else {
            
            y[i,j+1] = y[i,j+1] + t
            
          }
          
        }
        
      }
      
    }
    
  }
  
  ## 구슬의 개수 히스토그램 그리기 ####

  hist(y,main="구슬 옮기기", ylim=c(0,30), xlab="구슬 갯수", ylab="빈도수",col="#80fd3d", breaks=seq(0, 50, 2)) 
  
  ## 히스토그램 변화를 보기 위하여 지연 시간 0.1초씩 주기 ###
  
  Sys.sleep(0.1) 
  
}




## 최종 결과 보여주기  히스토그램 ##

hist(y,main="구슬 옮기기", ylim=c(0,30), xlab="구슬 갯수", ylab="빈도수",col="#80fd3d", breaks=seq(0, 50, 2))

## 구슬의 개수만큼 원의 사이즈를 비례하게 만들어서 x,y matrix에 표시하기 ####

z<-data.frame(y)

ggplot(z,aes(x=Var1,y=Var2,fill=z$Freq))+geom_point(colour="grey",shape=21,size=z$Freq)+ xlab('x')+ylab('y')+ggtitle('구슬 분포')+labs(fill='구슬수')

## 정규성 검사하기 ##

shapiro.test(y)

