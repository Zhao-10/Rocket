# Billy Practical1

## Pre-possessing

## Use "scan" function to split the txt according to blank space
## a = scan(file directory, what="character", skip=73,nlines=32858-73) 

## replace _( and )_ with nothing
a = gsub(")_","",gsub("_(","",a,fixed=TRUE),fixed=TRUE) 
## replace _ appearing at the end of the entry with nothing
a = gsub("\\_$","",a)

## replace . appearing no less than 4 times at the end of the entry with nothing
a = gsub("\\.{4,}$","",a)

## replace entries containing only punct with nothing
a = gsub("^[[:punct:]]+$","",a)
## replace puncts starting the entries with nothing
a = gsub("^[[:punct:]]+","",a)
## eliminate ""
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

an = split_punct(a)



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


b_tf = sum(newdf$tabulate.an3.)
b_sf = newdf$tabulate.an3./b_tf

model_sim = function(b_p){
  sim = rep(NA, 52)
  sim[1:2] = sample(1:1007,2)
  for (i in 3:52){
    Mat1 = T[(T[,1] == sim[i-2]) & (T[,2] == sim[i-1]),]
    Mat2 = P[(P[,2] == sim[i-1]),]
    if (isTRUE(nrow(Mat1)) != 0) {
      bdf1 = as.data.frame(table(Mat1[,3]))
      accumulate_b_p1 = b_p[bdf1$Var1]*bdf1$Freq
      conditional_b_p1 = accumulate_b_p1/sum(accumulate_b_p1)
      sim[i] = sample(bdf1$Var1, size = 1, prob = conditional_b_p1)
    }
    
    else if (isTRUE(nrow(Mat2)) != 0){
      bdf2 = as.data.frame(table(Mat2[,2]))
      accumulate_b_p2 = b_p[bdf2$Var1]*bdf2$Freq
      conditional_b_p2 = accumulate_b_p2/sum(accumulate_b_p2)
      sim[i] = sample(bdf2$Var1, size = 1, prob = conditional_b_p2)
    }
    
    else {sim[i] = sample(1:1007, size = 1, prob = b_p)}
  }
  sim = b[sim[3:52]]
  return(sim)
}

#### Testing
b_p = runif(1007); b_p = b_p/sum(b_p)
cat(model_sim(b_p)," ")


### 9
sample(b, size = 50, replace = TRUE ,prob = b_p)

### 10 



