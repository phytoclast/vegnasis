#This function exports dendrogram colored according to group.
export.dendro <- function(a,d,t,groups, folder=NA){
  require(ape)
  require(cluster)
  require(vegan)
  require(dendextend)
  if(is.na(folder)){filename <- paste0('Vegplot_',a,'.png')}else{filename <- paste0(folder,'/Vegplot_',a,'.png')}

  t <- as.hclust(t)
  #make cuts and reformat dendrogram
  groups <- groups

  soilplot <- names(groups)
  clust <- unname(groups)
  groupdf <- as.data.frame(cbind(soilplot, clust))
  groupdf$clust <- (as.numeric(as.character(groupdf$clust)))
  # maxcluster <- max(groupdf$clust)
  # numberzeros <- nrow(groupdf[(groupdf$clust == 0),])
  # whichrecords <- which(groupdf$clust == 0)
  # if (nrow(groupdf[groupdf$clust == 0,]) != 0){
  #   for (i in 1:numberzeros){ #assign all zero clusters to unique cluster number.
  #     groupdf[whichrecords[i],]$clust <- maxcluster+i}}

  newlabels <- t$labels
  newlabels <- as.data.frame(newlabels)
  newlabels$row <- row(newlabels)[,1]
  newlabels <- merge(newlabels, groupdf, by.x='newlabels', by.y ='soilplot')
  newlabels$newlabels <- paste(newlabels$clust, newlabels$newlabels)
  newlabels <- newlabels[order(newlabels$row),1]
  newtree <- t
  newtree$labels <- newlabels

  dend1 <- color_branches(as.dendrogram(as.hclust(newtree)), clusters = groups[order.dendrogram(as.dendrogram(t))])
  dend1 <- color_labels(dend1, col = get_leaves_branches_col(dend1))

  #output file

  w <- 800
  h <- nrow(groupdf)*12+80
  u <- 12
  png(filename=filename,width = w, height = h, units = "px", pointsize = u)

  par(mar = c(2,0,1,13))
  plot(dend1, horiz = TRUE, main=paste(a), font=1, cex=0.84)
  #rect.dendrogram(dend1, k = ngroups, horiz = TRUE)
  dev.off()

}
#This function reorders branches and groups based on order of nestedness.
dendrogrouporder <- function(t,groups){
  require(ape)
  require(cluster)
  require(vegan)

  t=as.hclust(t)
  soilplot <- names(groups)
  clust <- unname(groups)
  groupdf <- as.data.frame(cbind(soilplot, clust))
  groupdf$clust <- (as.numeric(as.character(groupdf$clust)))
  torder <- as.data.frame(cbind(trow=t$order))
  torder$torder <- row(torder)[,1]
  newlabels <- as.data.frame(cbind(labels=t$labels))
  newlabels$trow <- row(newlabels)[,1]
  newlabels <- merge(newlabels, groupdf, by.x='labels', by.y ='soilplot')
  newlabels <- merge(newlabels, torder, by='trow')

  grouporder <- newlabels %>% group_by(clust)  %>%  summarise(thisorder = min(torder))
  grouporder$newgroup <- order(order(grouporder$thisorder, decreasing=T))
  newlabels <- merge(newlabels, grouporder, by='clust')
  newlabels <- newlabels[order(newlabels$trow),]
  newgroups <- newlabels$newgroup
  names(newgroups) <- newlabels$labels
  return(newgroups)
}
