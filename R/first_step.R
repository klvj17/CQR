
pt <- function(x, n){x^n}


inter_party <- function(df){

  nc <- NCOL(df) - 1
  rdf <- 1:NROW(df)
  cn <- c("num")
  for(i in 1:nc){

    init <- i + 1

    for(j in init:NCOL(df)){

      add <- df[,i] * df[,j]
      cn <- c(cn, paste(colnames(df)[i], colnames(df)[j], sep = "_"))
      rdf <- cbind(rdf, add)

    }

  }
  colnames(rdf) <- cn
  return(rdf[,-1])
}


# list of c and percentile(q)
cp_rate <- function(q1, step_one, tau){

  nocp_flag <- ifelse(step_one > (1 - tau), T, F)

  cp <- step_one[nocp_flag] %>% quantile(q1)

  return(cp)
}

# list of c and percentile(q) and tau
cp_rate_list <- function(taus, step_one, q1){

  cp <- lapply(taus, cp_rate, step_one = step_one, q1 = q1) %>% sapply("[[", 1)

  return(data.frame(tau = taus, cp))

}
