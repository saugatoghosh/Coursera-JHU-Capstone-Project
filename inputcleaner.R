#Clean the input before applying algorithm


require(readr)
profanewords <- read_lines("data/bad-words.txt")

toSpace <- function(x) {
  out <- gsub("[^[:alnum:][:space:]']", "", x)
  return(out)
}


clean_input <- function(input){
  require(tm)
  input <- toSpace(input)
  input <- removeNumbers(input)
  input <- tolower(input)
  input <- removeWords(input,profanewords)
  input <- stripWhitespace(input)
  input <- unlist(strsplit(input, split = " "))
  if (length(input) > 5){
    input <- tail(input,5)
  }
  input <- input[nzchar(x=input)]

  return(input)
}



