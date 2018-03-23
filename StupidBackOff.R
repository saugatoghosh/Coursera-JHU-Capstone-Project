hexagramdata <- readRDS("data/allHexagrams10_2.rds")
pentagramdata <- readRDS("data/allPentagrams10_2.rds")
quadgramdata <- readRDS("data/allQuadgrams10_2.rds")
trigramdata <- readRDS("data/allTrigrams10_2.rds")
bigramdata <- readRDS("data/allBigrams10_2.rds")
unigramdata <- readRDS("data/allUnigrams10_2.rds")


# Stupid BackOff algorithm to predict the next word based on unsmoothed n-gram probabilities

hexagrampred <- function(input, unigram, bigram, trigram, quadgram, pentagram, hexagram){
  hexagramPred <- hexagram[ngram1 ==input[1] & ngram2 == input[2] & ngram3 == input[3] & ngram4 == input[4] & ngram5 == input[5]]
  if(nrow(hexagramPred) > 0){
    output <- hexagramPred
  }
  else{
  pentagramPred <- pentagram[ngram1 ==input[2] & ngram2 == input[3] & ngram3 == input[4] & ngram4 == input[5]]
  if(nrow(pentagramPred) >0){
    output <- pentagramPred
  }
  else{
    quadgramPred <- quadgram[ngram1 ==input[3] & ngram2 == input[4] & ngram3 == input[5]]
    if(nrow(quadgramPred) > 0){
      output <- quadgramPred
    }
    else{
      trigramPred <- trigram[ngram1 == input[4] & ngram2 == input[5]]
      if (nrow(trigramPred) >0){
        output <- trigramPred
      }
      else {
        bigramPred <- bigram[ngram == input[5]]
        if (nrow(bigramPred) >0){
          output <- bigramPred
        }
        else{
          output <- unigram
        }
      }
    }
   }
  }
  return(output)
}



pentagrampred <- function(input, unigram, bigram, trigram, quadgram, pentagram){
  pentagramPred <- pentagram[ngram1 ==input[1] & ngram2 == input[2] & ngram3 == input[3] & ngram4 == input[4]]
  if(nrow(pentagramPred) >0){
    output <- pentagramPred
  }
  else{
  quadgramPred <- quadgram[ngram1 ==input[2] & ngram2 == input[3] & ngram3 == input[4]]
  if(nrow(quadgramPred) > 0){
    output <- quadgramPred
  }
  else{
    trigramPred <- trigram[ngram1 == input[3] & ngram2 == input[4]]
    if (nrow(trigramPred) >0){
      output <- trigramPred
    }
    else {
      bigramPred <- bigram[ngram == input[4]]
      if (nrow(bigramPred) >0){
        output <- bigramPred
      }
      else{
        output <- unigram
      }
    }
  }
}
  return(output)
}



quadgrampred <- function(input, unigram, bigram, trigram, quadgram){
  quadgramPred <- quadgram[ngram1 ==input[1] & ngram2 == input[2] & ngram3 == input[3]]
  if(nrow(quadgramPred) > 0){
    output <- quadgramPred
  }
  else{
    trigramPred <- trigram[ngram1 == input[2] & ngram2 == input[3]]
    if (nrow(trigramPred) >0){
      output <- trigramPred
    }
    else {
      bigramPred <- bigram[ngram == input[3]]
      if (nrow(bigramPred) >0){
        output <- bigramPred
      }
      else{
      output <- unigram
      }
    }
  }
  return(output)
}



trigrampred <- function(input, unigram, bigram, trigram){
  trigramPred <- trigram[ngram1 == input[1] & ngram2 == input[2]]
  if (nrow(trigramPred) >0){
    
    output <- trigramPred
  }
  else {
    bigramPred <- bigram[ngram ==input[2]]
    if (nrow(bigramPred) >0){
      output <- bigramPred
      
    }
    else{
      output <- unigram
      
    }
    
  }
  return(output)
}

bigrampred <- function(input, unigram, bigram) {
  bigramPred <- bigram[ngram ==input]
  if (nrow(bigramPred) >0){
    output <- bigramPred
    
  }
  else{
    output <- unigram
    
  }
  return(output)
}

predictSimple <- function(input, unigram, bigram, trigram, quadgram, pentagram, hexagram){
  if(length(input) == 5){
    pred <- hexagrampred(input, unigram, bigram, trigram, quadgram, pentagram, hexagram)
  }
  else{
   if(length(input) == 4){
     pred <- pentagrampred(input, unigram, bigram, trigram, quadgram, pentagram)
   }
  else{
    if(length(input) == 3){
      pred <- quadgrampred(input, unigram, bigram, trigram, quadgram)
    }
    else{
      if (length(input) ==2){
        pred <- trigrampred(input,unigram, bigram,trigram)
      }
      else{
        pred <- bigrampred(input,unigram, bigram)
          }
      }
    }
  }
    
    if(is.null(pred$nextword)){
      pred$nextword2 <- pred$ngram
    }
    else{
      pred$nextword2 <- pred$nextword
    }
    pred2 <- pred[1:3]
    pred2 <- pred2[!is.na(nextword2)]
  return(pred2)
  
}


