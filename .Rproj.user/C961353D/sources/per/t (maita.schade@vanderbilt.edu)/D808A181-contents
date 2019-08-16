#' Stratified sample of dataframe with weights
#'
#' \code{strat_weight_sample} returns a dataframe of records sampled in the strata according to their weight
#'
#' @param df A dataframe of records to sample from
#' @param strats A vector or list of variables to use for stratification
#' @param weights A character string indicating the variable in the dataframe containing the weights
#' @param N The overall number of the resulting sample
#'
#' @return A dataframe of length N. Subset of \code{df} selected by drawing a number proportional to the
#' stratum's total weight in each stratum, with selection probabilities equal to each entry's name.

strat_weight_sample <- function(df, strats, weights, N){
  census <- df

  #First, we'll make a table of strata, and their sum of weights.
  blocks <- aggregate(as.formula(paste0(weights, " ~ ", paste(strats, collapse=' + '))), data=census, sum)
  print(as.formula(paste0(weights, " ~ ", paste(strats, collapse=' + '))))

  #Then we determine how many individuals to randomly draw from each stratum.
  blocks$nblock <- round(blocks[[weights]] * N/sum(blocks[[weights]]))

  #Then we do a weighted random sample from each stratum.
  weighted_sample_helper <- function(r){
    s1 <- r[[strats[1]]]
    if (length(strats)>1){
      s2 <- r[[strats[2]]]
      block <- census[(census[strats[1]]==s1) & (census[strats[2]]==s2),]
    }
    else{
      block <- census[(census[strats[1]]==s1),]
    }
    # block <- merge(t(r[strats]), census, by=c('URBAN','GEO1_CO2005'))
    n = as.double(r['nblock'])
    index = sample(nrow(block),
                   size = n,
                   replace = TRUE,
                   prob = block[[weights]]
    )
    return(block[index,])
  }

  #...and bind them together
  sample <- do.call(rbind, apply(blocks,1,weighted_sample_helper))
  return(sample)
}
