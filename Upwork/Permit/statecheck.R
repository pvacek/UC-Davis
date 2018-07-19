#State Check

library(stringr)

pdf_read<-function(pdf){
  return(paste0(pdf_text(pdf),collapse=" "))
}

state_regex<-toupper(paste0(state.abb,"|",state.name))

state_check<-function(pdf){
  guess1<-str_extract_all(pdf,paste0(state_regex,collapse="|"))[[1]]
  raw<-pdf_read(pdf)
  text_raw<-toupper(unlist(strsplit(raw," ")))
  text_lean<-text_raw[nchar(text_raw)>0]
  regex<-regex_test(text_lean)
  guess2<-state_test(raw,regex)
  guess3<-state_test2(text_lean,regex)
  return(names(which.max(table(c(guess1,guess2,guess3)))))
}

regex_test<-function(lean){
  DOT<-gsub("DOT","",str_extract(lean[grep("[A-Z]{1,3}DOT",lean)],"[A-Z]{1,3}DOT"))
  if(length(DOT)>0){
    DOT.table<-table(DOT)
    new_regex<-state_regex[grep(paste0(names(which(DOT.table==max(DOT.table))),collapse="|"),state.abb)]
    if(length(new_regex)==0){
      return(state_regex)
    }
    return(new_regex)
  }
  else{
    return(state_regex)
  }
}

state_test<-function(raw,regex=state_regex){
  states<-gsub(" |,","",str_extract_all(raw,paste0("[ ,]",regex,collapse="|"))[[1]])
  matches<-cbind(match(states,state.abb),match(states,toupper(state.name)))
  match_tbl<-table(state.abb[apply(matches,1,max,na.rm=TRUE)])
  return(names(which.max(match_tbl)))
}

state_test2<-function(lean,regex=state_regex,k=10){
  transport<-grep("STATE|TRANSPORT|DEPARTMENT|PUBLIC",lean)
  state<-grep(paste0("[ ,]",regex,collapse="|"),lean)
  if(length(transport)==0 || length(state)==0){
    return(NA)
  }
  pairs<-cbind(state,sapply(state,function(x)transport[min(which(transport>=x))]))
  choice<-gsub(" |,","",lean[pairs[which.min(apply(pairs,1,diff)),1]])
  if(nchar(choice)>2){
    state.abb[match(choice,toupper(state.name))]
  }
  else{
    choice
  }
}