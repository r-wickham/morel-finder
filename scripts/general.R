#'
#' General functions
#'

waterYear <-  function(dateFun){
  require(lubridate)
  wy <- as.numeric(format(x=dateFun, format="%Y"))
  wy[month(dateFun) >= 10] <- wy[month(dateFun) >= 10] + 1
  return(wy)
}

#Extracts from right side of character (similar to Excel function)
right <- function(text, nchars)   substring(text, nchar(text)-nchars+1, nchar(text))

#Extracts from left side of character(similar to Excel function)
left <- function(text, nchars)  substring(text, 1, nchars)

#Extracts %Y-%m-%d as string from all string elements
datesFromString <- function(strings)str_extract_all(strings,"\\d{4}-\\d{2}-\\d{2}")


is.consecutive <- function(vec,allowGaps=T){
  #returns logical indicating which elements in the input vector are consecutive
  #  with adjacent vector elements. If allowGaps=F, then will only return true for the
  #  first series of consecutive numbers
  # e.g., allowGaps=T; vec=c(1,2,3,5,6,7) -> c(T,T,T,T,T,T)
  #       allowGaps=F; vec=c(1,2,3,5,6,7) -> c(T,T,T,F,F,F)
  out <- rep(T,length(vec))
  vecDiff <- diff(vec)==1
  foundGap <- F
  for( k in 1:length(vec) )
    if(allowGaps){
      if( !any(vecDiff[ pmin(pmax(c(k,k-1),1),length(vecDiff)) ]) ) out[k] <- F
    }else{
      if( !any(vecDiff[ pmin(pmax(c(k,k-1),1),length(vecDiff)) ]) | foundGap ) {
        out[k] <- F
      }
      if( any(!vecDiff[ pmin(pmax(c(k,k-1),1),length(vecDiff))]) ) foundGap=T
    }
  out
}