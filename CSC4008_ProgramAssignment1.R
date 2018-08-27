titanic1 <- read.table("/Users/bubble/Documents/Dataset.data", header=F)
names(titanic1) <- c("Class", "Age", "Sex", "Survived")
titanic <- read.table("/Users/bubble/Documents/Dataset.data",stringsAsFactors = FALSE, header=F)
names(titanic) <- c("Class", "Age", "Sex", "Survived")
c1 = list()
num1 = c()
#produce c1,num1
for(t in titanic){
  for(item in t){
    if(item %in% c1 == FALSE){
      c1=append(c1,list(item))
      num1[which(c1==item)]=1
    }else{
      num1[which(c1==item)]=num1[which(c1==item)]+1
    }
  }
}
#produce a del_list to contain those numbers smaller than min_sup
#lk=ck[-del_list] numk=numk[-del_list]
prune_lk <- function(ck,numk){
  del_list=c()
  for(i in 1:length(numk)){
     if(numk[i]<220){
       del_list=append(del_list,i)
    }
  }
  return(del_list)
}
#check whether a k-itemset is a frequent itemset:
#if its sub k-1-itemsets aren't in frequent (k-1)-itemsets, then it's not frequent 
is_apriori <- function(item,lj){
  judge=c()
  for(i in 1:length(item)){
    for(j in 1:length(lj)){
       if(all(item[[1]][-i] %in% lj[[j]]) == TRUE){
          judge=append(judge,TRUE)
       }
    }
  }
  if(all(judge)==TRUE){
    return(TRUE)
  }
}
create_ck <- function(lj){
  ck=list()
  for(m in 1:(length(lj)-1)){
    n = m+1 
    while(n <= length(lj)){
      if(setequal(lj[[m]][-1],lj[[n]][-1])==TRUE){
        item = list(union(lj[[m]],lj[[n]]))
        if(is_apriori(item,lj) ==TRUE){
           ck=append(ck,item)
        }
      }
      n = n+1
    }
  }
  return(ck)
}
#count frequent itemsets' support
count_num <- function(ck){
  numk=rep(0,length(ck))
  for(j in 1:nrow(titanic)){
    for(i in 1:length(ck)){
      if(all(c(ck[[i]]) %in% titanic[j,])==TRUE){
         numk[i]=numk[i]+1
      }
    }
  }
  return(numk)
}
#generate rules:item0 -> item1
generate_item0 <- function(l,num){
  item0=list()
  for(i in 1:(length(l)-1)){
    for(j in 1:length(l[[i]])){
      m=i+1
      while(m <= length(l)){
        for(n in 1:length(l[[m]])){
          if(all(is.element(l[[i]][[j]],l[[m]][[n]]))== TRUE){
            conf=num[[m]][[n]]/num[[i]][[j]]
            if(conf>0.8){
              item0=append(item0,list(l[[i]][[j]]))
            }
          }
        }
        m=m+1
      }
    }
  }
  return(item0)
}
generate_item1 <- function(l,num){
  item1=list()
  for(i in 1:(length(l)-1)){
    for(j in 1:length(l[[i]])){
      m=i+1
      while(m <= length(l)){
        for(n in 1:length(l[[m]])){
          if(all(is.element(l[[i]][[j]],l[[m]][[n]]))== TRUE){
            conf=num[[m]][[n]]/num[[i]][[j]]
            if(conf>0.8){
              item1=append(item1,list(setdiff(l[[m]][[n]],l[[i]][[j]])))
            }
          }
        }
        m=m+1
      }
    }
  }
  return(item1)
}
count_confidence <- function(l,num){
  confidence=c()
  for(i in 1:(length(l)-1)){
    for(j in 1:length(l[[i]])){
      m=i+1
      while(m <= length(l)){
        for(n in 1:length(l[[m]])){
          if(all(is.element(l[[i]][[j]],l[[m]][[n]]))== TRUE){
            conf=num[[m]][[n]]/num[[i]][[j]]
            if(conf>0.8){
              confidence=append(confidence,conf)
            }
          }
        }
        m=m+1
      }
    }
  }
  return(confidence)
}
count_lift <- function(l,num,conf){
  lift=c()
  for(i in 1:length(conf)){
       for(j in 1:length(l)){
         for(t in 1:length(l[[j]])){
           if(setequal(item1[[i]],l[[j]][[t]])==TRUE){
             li=conf[[i]]/num[[j]][[t]]*nrow(titanic)
             lift=append(lift,li)
           }
         }
       }
  }
  return(lift)
}
del_list1=prune_lk(c1,num1)
l1=c1[-del_list1]
num1=num1[-del_list1]
c2=create_ck(l1)
num2=count_num(c2)
del_list2=prune_lk(c2,num2)
l2=c2[-del_list2]
num2=num2[-del_list2]
c3=create_ck(l2)
num3=count_num(c3)
del_list3=prune_lk(c3,num3)
l3=c3[-del_list3]
num3=num3[-del_list3]
c4=create_ck(l3)
num4=count_num(c4)
del_list4=prune_lk(c4,num4)
l4=c4[-del_list4]
num4=num4[-del_list4]
#l5 is an empty list, so max k is 4 for frequent k-itemset
l=list(l1,l2,l3,l4)
num=list(num1,num2,num3,num4)
item0=generate_item0(l,num)
item1=generate_item1(l,num)
conf=count_confidence(l,num)
lift=count_lift(l,num,conf)
p_y=conf/lift
p_x=c()
p_xy=c()
#generate support of (x,y)
for(i in 1:(length(l)-1)){
  for(j in 1:length(l[[i]])){
    m=i+1
    while(m <= length(l)){
      for(n in 1:length(l[[m]])){
        if(all(is.element(l[[i]][[j]],l[[m]][[n]]))== TRUE){
          conf=num[[m]][[n]]/num[[i]][[j]]
          if(conf>0.8){
            p_x=append(p_x,num[[i]][[j]]/nrow(titanic))
            p_xy=append(p_xy,num[[m]][[n]]/nrow(titanic))
          }
        }
      }
      m=m+1
    }
  }
}
#question1
print("frequent 1 itemset:  support")
print(hash(keys=l1,values=num1))
print("frequent 2 itemset:  support")
print(hash(keys=l2,values=num2))
print("frequent 3 itemset:  support")
print(hash(keys=l3,values=num3))
print("frequent 4 itemset:  support")
print(hash(keys=l4,values=num4))
l=list(l1,l2,l3,l4)
num=list(num1,num2,num3,num4)
conf=count_confidence(l,num)
for(i in 1:length(conf)){
  cat(item0[[i]],"->",item1[[i]],"\n","support:",s_xy[[i]],"conf:",conf[[i]],"lift:",lift[[i]],"\n")
}
#question2
l=list(l1,l2,l3,l4)
num=list(num1,num2,num3,num4)
conf=count_confidence(l,num)
for(i in 1:length(conf)){
  if(all(item1[[i]]=="no")|all(item1[[i]]=="yes")){
    cat(item0[[i]],"->",item1[[i]],"\n","support:",p_xy[[i]],"conf:",conf[[i]],"lift:",lift[[i]],"\n")
  }
}
#question3
#calculate interesting measures
#to calculate the results more effectively:produce two functions to get maximum and minimum vectors
maximum<-function(itema,itemb){
  item=c()
  for(i in 1:length(itema)){
    if(is.na(itema[[i]])==TRUE|is.na(itemb[[i]])==TRUE){
      item=append(item,"NaN")
    }else{
      if(itema[[i]]>=itemb[[i]]){
        item=append(item,itema[[i]])
      }else{item=append(item,itemb[[i]])}
    }
  }
  return(item)
}
minimum<-function(itema,itemb){
  item=c()
  for(i in 1:length(itema)){
    if(is.na(itema[[i]])==TRUE|is.na(itemb[[i]])==TRUE){
      item=append(item,"NaN")
    }else{
      if(itema[[i]]<=itemb[[i]]){
        item=append(item,itema[[i]])
      }else{item=append(item,itemb[[i]])}
    }
  }
  return(item)
}
#calculate different probabilities for more effective calculations 
p_xbar=1-p_x
p_ybar=1-p_y
p_xybar=1-p_x-p_y+p_xy
p_xbary=p_y-p_xy
p_ybarx=p_x-p_xy
p_xIy=p_xy/p_y
p_yIx=p_xy/p_x
p_xbarIy=p_xbary/p_y
p_yIxbar=p_xbary/p_xbar
p_xIybar=p_ybarx/p_ybar
p_ybarIx=p_ybarx/p_x
p_xbarIybar=p_xybar/p_ybar
p_ybarIxbar=p_xybar/p_xbar
fai_coefficient=(p_xy-p_x*p_y)/(p_x*p_y*p_xbar*p_ybar)^0.5
goodman_kruskal=(maximum(p_xy,p_ybarx)+maximum(p_xbary,p_xybar)+maximum(p_xy,p_xbary)+maximum(p_ybarx,p_xybar)-maximum(p_x,p_xbar)-maximum(p_y,p_ybar))/(2-maximum(p_x,p_xbar)-maximum(p_y,p_ybar))
odds_ratio=p_xy*p_xybar/(p_xbary*p_ybarx)
yule_q=(p_xy*p_xybar-p_xbary*p_ybarx)/(p_xy*p_xybar+p_xbary*p_ybarx)
yule_y=((p_xy*p_xybar)^0.5-(p_xbary*p_ybarx)^0.5)/((p_xy*p_xybar)^0.5+(p_xbary*p_ybarx)^0.5)
kappa=(p_xy+p_xybar-p_x*p_y-p_xbar*p_ybar)/(1-p_x*p_y-p_xbar*p_ybar)
mutual_information=(p_xy*log(p_xy/(p_x*p_y))+p_ybarx*log(p_ybarx/(p_x*p_ybar))+p_xy*log(p_xbary/(p_xbar*p_y))+p_xybar*log(p_xy/(p_xbar*p_ybar)))/minimum(-(p_x*log(p_x)+p_xbar*log(p_xbar)),-(p_y*log(p_y)+p_ybar*log(p_ybar)))
j_measure=maximum(p_xy*log(p_yIx/p_y)+p_ybarx*log(p_ybarIx/p_ybar),p_xy*log(p_xIy/p_x)+p_xbary*log(p_xbarIy/p_xbar))
gini=maximum(p_x*(p_yIx^2+p_ybarIx^2)+p_xbar*(p_yIxbar^2+p_ybarIxbar^2)-p_y^2-p_ybar^2,p_y*(p_xIy^2+p_xbarIy^2)+p_ybar*(p_xIybar^2+p_xbarIybar^2)-p_x^2-p_xbar^2)
laplace=maximum((s_xy+1)/(s_x+2),(s_xy+1)/(s_y+2))
conviction=maximum(p_x*p_ybar/p_ybarx,p_y*p_xbar/p_xbary)
interest=p_xy/(p_x*p_y)
cosine=p_xy/((p_x*p_y)^0.5)
P_S=p_xy-(p_x*p_y)
certainty_F=maximum((p_yIx-p_y)/(1-p_y),(p_xIy-p_x)/(1-p_x))
added_V=maximum(p_yIx-p_y,p_xIy-p_x)
collective_S=(p_xy+p_xybar)/(p_x*p_y+p_xbar*p_ybar)*(1-p_x*p_y-p_xbar*p_ybar)/(1-p_xy-p_xybar)
jaccard=p_xy/(p_x+p_y-p_xy)
klosgen=p_xy^0.5*maximum(p_yIx-p_y,p_xIy-p_x)
conf=count_confidence(l,num)
for(i in 1:length(conf)){
  if(all(item1[[i]]=="no")|all(item1[[i]]=="yes")){
    cat(item0[[i]],"->",item1[[i]],"\n","support:",p_xy[[i]],"conf:",conf[[i]],"lift:",lift[[i]],"\n","jaccard:",jaccard[[i]],"fai_coefficient:",fai_coefficient[[i]],"goodman kruskal:",goodman_kruskal[[i]],"odds_ratio:",odds_ratio[[i]])
    cat("yule_q:",yule_q[[i]],"yule_y:",yule_y[[i]],"kappa:",kappa[[i]],"mutual information:",mutual_information[[i]],"j_measure:",j_measure[[i]],"gini index:",gini[[i]],"laplace:",laplace[[i]],"conviction:",conviction[[i]],"interest:",interest[[i]])
    cat("cosine:",cosine[[i]],"piatetsky-shapiro:",P_S[[i]],"certainty factor:",certainty_F[[i]],"added value:",added_V[[i]],"collective strength:",collective_S[[i]],"klosgen:",klosgen[[i]],"\n")
  }
}






