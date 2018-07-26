
vector =c(1,0,1,1,1,1,1,1,0,0,0)

codificar<-function(variable, digitos){
  
  len =length(variable)
  code<-vector("character",len)
  
  for (i in 1:len){
    code[i]<-paste(round(runif(min = 0, max = 7, n = digitos)), collapse = "")
    
    if (variable[i] == 0) {
      substr(code[i], 55,55)<-sample(c("0","1","2","3"), 1,replace =TRUE)
    }else {
      substr(code[i], 55,55)<-sample(c("4","5","6","7"), 1,replace =TRUE)
    }
  }
  return(code)
}


variable =codificar(vector,55)
