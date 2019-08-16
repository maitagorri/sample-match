matchRatio <- function(data, metric, n, exact = c(), id='id',treat='treat',panelid='Fakeid'){

  # assign the dataframe to hold the matching results
  df <- data.frame(matrix(ncol=1, nrow=sum(data[[treat]]==1)))
  names(df) <- c("targetId")
  df$targetId <- data$id[data[[treat]]==1]

  # assign the object to hold all the matching information
  matches <- vector("list",n)

  # make a copy of the passed-in data
  data.copy <- data.frame(data)

  # only while I don't have an internal ID assigned to NQ panelists: create a fake Fakeid
  # data.copy['Fakeid'] <- rep(999999, length(data.copy$treat))
  # data.copy['Fakeid'][data.copy$treat==0,] <- 1:sum(data.copy$treat==0)

  # loop over the number of respondents per target
  # if there are issues, can I relax the age groups?
  for(i in 1:n){
    print(paste('i = ',to_character(i)))
    m <- matchit(matching.form,
                 data = data.copy, exact=exact, method = "nearest", distance = metric)

    try({matches[[i]] <- m
    targetids <- data.copy[row.names(m$match.matrix), id]
    Fakeids <- as.data.frame(data.copy[m$match.matrix,panelid])
    names(Fakeids)<-c(panelid)
    Fakeids['targetId']<-targetids
    df <- merge(x=df, y=Fakeids, by="targetId", all.x = TRUE, suffixes=c("",to_character(i)))
    } #... maybe merge instead might be safer?
    )

    controls <- match.data(m, group='control')
    data.copy <- data.copy[!data.copy$jointId %in% controls$jointId,] # not relying on rownames

  }
  # final <- cbind(alldata[row.names(match.list$matches[[1]]$match.matrix),"id"],
  #     as.data.frame(do.call(cbind,(lapply(match.list$matches, function(m){alldata[m$match.matrix,"Fakeid"]}))))
  #  )
  return(list("ids"=df, "matches"=matches))
}
