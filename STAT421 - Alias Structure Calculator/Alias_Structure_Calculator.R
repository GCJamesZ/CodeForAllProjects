rm(list=ls(all = TRUE))

find_comb = function (factor){
  res = c()
  for(i in 1:length(factor)) {
    a = combn(factor, m=i)
    for ( j in 1:choose(length(factor), i)){
      res[length(res)+1] = print(paste(a[,j], collapse = ''))
    }
  }
  return(res)
}

remove_dupl = function (str) {
  library(stringr)
  newstr = ""
  if(length(str) == 0){
    return(newstr)
  }
  
  letters = as.character(strsplit(str, split = "")[[1]])
  for(i in 1:length(letters)){
    if(((str_count(str, letters[i]) %% 2) == 1) & 
       (!grepl(letters[i], newstr, fixed = TRUE))){
      newstr = paste0(newstr, letters[i], collapse = "")
    }
  }
  return(newstr)
}

find_alias_structure = function(fctr, DR) {
  FR = list()

  Def_relation = c()
  for(i in 1:length(DR)) {
    a = combn(DR, m=i)
    for (j in 1:ncol(a)){
      Def_relation[length(Def_relation)+1] = remove_dupl(paste0(a[,j], collapse = ''))
    }
  }
  
  for(i in 1:(2^(length(fctr)-length(Def_relation)-1))) {
    FR[i] = c(0)
  }
  FR.size = 0
  while(length(comb) != 0){
    cr = rep(0, length(Def_relation)+1)
    pr = rep(0, length(Def_relation)+1)
    cr[1] = comb[1]
    # pr[1] = comb[1]
    for (i in 1:length(Def_relation)){
      cr[i+1] = remove_dupl(paste0(comb[1], Def_relation[i]))
      cr[i+1]=paste(sort(as.character(strsplit(cr[i+1], split = "")[[1]])),collapse='')
    }
    comb=setdiff(comb,cr)
    if(length(FR) == 0){
      FR.size = 1
      FR[FR.size] = unique(as.vector(cr))
    } else {
      Notadd = TRUE
      for (f in 1:length(FR)){
        if(length(intersect(FR[f][[1]], cr)) != 0){
          FR[f][[1]] = c(FR[f][[1]], cr)
          Notadd = FALSE
          FR[f][[1]] = unique(FR[f][[1]])
        }
      }
      if(Notadd){
        FR.size = FR.size + 1
        FR[FR.size][[1]] = unique(as.vector(cr))
      }
    }
  }
  
  for(i in 1:length(FR)){
    tem = unique(FR[i][[1]])
    if ("" %in% tem) {
      tem[match("",tem)] = "I"
    }
    print(paste(as.character(tem),collapse="=",sep=""))
  }
  return(FR)
}


find_effects_comb = function(DR){
  Def_relation = c()
  for(i in 1:length(DR)) {
    a = combn(DR, m=i)
    for (j in 1:ncol(a)){
      Def_relation[length(Def_relation)+1] = remove_dupl(paste0(a[,j], collapse = ''))
    }
  }
  print(paste(as.character(Def_relation),collapse="=",sep=""))
}


# input
fctr = c("A","B","C","D","E","F","G")
comb = find_comb(fctr)

DR = c("ACE","BCG")
FR = find_alias_structure(fctr, DR)


Eff = c("ABD","ACE","BCF")
ES = find_effects_comb(Eff)


