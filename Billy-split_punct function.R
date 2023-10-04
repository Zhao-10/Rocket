# Billy Q1-7

## Pre-possessing
a = scan("/Users/billy/Downloads/Edin/Edin-study/Sem1/Extended\ Statistical\ Programming/Assignments/Practical1/4300-0.txt", what="character", skip=73,nlines=32858-73)
a = gsub(")_","",gsub("_(","",a,fixed=TRUE),fixed=TRUE) ## remove "_(" and ")_"
a = gsub("\\_$","",a)
a = gsub("\\.{4,}$","",a)

a = gsub("^[[:punct:]]+$","",a)
a = gsub("^[[:punct:]]+","",a)

a = a[a != ""]


## Function 
split_punct = function(billy){
  vec = c()
  lv = 0 
  for (i in 1:length(billy)){
    if (length(grep("[[:alnum:]]+$", billy[i])) != 0){
      vec[lv+1] = billy[i]
      lv = lv + 1
    }
    
    if (length(grep("[[:alnum:]]+$", billy[i])) == 0) {
      bi1 = unlist(strsplit(billy[i], ""))
      last_bi = bi1[length(bi1)]
      word_bi = paste(bi1[-length(bi1)], collapse = "")
      vec[lv+1] = word_bi
      vec[lv+2] = last_bi
      lv = lv + 2
    }
    
    
    
  }
  
  return(vec)
}




## Question 6-9

### 6(a)
an1 = tolower(an)
an2 = unique(an1)
# 6(b)
an3 = match(an1, an2)
### 6(c)
tabulate(an3)

### 6(d)
sort(tabulate(an3), decreasing = TRUE)[1000] ### result=27
b_len = table(tabulate(an3)>=27)["TRUE"]

### 6(e)
an2_index = c(1:length(an2))
df = data.frame(an2_index, tabulate(an3)
)
newdf = df[order(-tabulate(an3)),]
newdf = newdf[1: b_len,]
b = an2[newdf$an2_index]


### 7(a)
b1 = match(an1,b)
### 7(b+c)
T = cbind(b1[1:(length(b1)-2)], b1[2:(length(b1)-1)], b1[3:length(b1)])
T = T[rowSums(is.na(T)) == 0, ]
### 7(d)
P = cbind(b1[1:(length(b1)-1)], b1[2:(length(b1))])
P = P[rowSums(is.na(P)) == 0, ]

### 8
