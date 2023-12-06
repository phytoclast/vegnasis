wt.quantile <- function(x,wt,q){
  df <-  data.frame(x=x,wt=wt)

  if(length(x)==1){
    v <- x
  }else{
    df <- df[order(x),]
    wtsum = sum(df$wt)
    nrec = length(df$x)
    df$s <- df$wt/wtsum
    df$ct <- 0
    for(i in 1:nrec){
      if(i==1){
        df[i,]$ct = df[i,]$s
      }else{
        df[i,]$ct = df[i,]$s+df[i-1,]$ct
      }
    }
    smin = min(df$ct)
    df$ct <- (df$ct - smin)/(1-smin)

    q1 <- subset(df, ct >= q)
    q2 <- subset(df, ct <= q)
    q1 <- subset(q1, ct %in% min(q1$ct))
    q2 <- subset(q2, ct %in% max(q2$ct))

    if(q1$ct[1] %in% q2$ct[1]){
      v = q1$x[1]
    }else{
      v <- q1$x[1] * (1-(abs(q1$ct[1]-q)/abs(q1$ct[1]-q2$ct[1])))+
        q2$x[1] * (1-(abs(q2$ct[1]-q)/abs(q1$ct[1]-q2$ct[1])))
    }}
  return(v)
}
