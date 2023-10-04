# Billy: split_punct function

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