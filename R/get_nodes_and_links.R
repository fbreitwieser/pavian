get.nodes.and.links <- function(krakenres,max.per.level=10) {


  nodes_n_links <- res.createlinks(krakenres,NA,depth.pos=1,depth.to=15)
  nodes_n_links <- nodes_n_links[!is.na(nodes_n_links[,'source.name']),]
    
  node.names <- sort(unique(c(nodes_n_links[,'source.name'],nodes_n_links[,'target.name'])))
  nodes <- data.frame(name=node.names,stringsAsFactors=FALSE)
  node.to.id <- setNames(seq_along(node.names),node.names)
  
  links <- data.frame(source=node.to.id[nodes_n_links[,'source.name']]-1,
                       target=node.to.id[nodes_n_links[,'target.name']]-1,
                       value=as.numeric(nodes_n_links[,'value']))
  
  return(list(nodes,links))
}

res.createlinks <- function(krakenres,source.name,depth.pos,depth.to=depth.to) {
  
max.per.level <- 10

  if (length(krakenres) == 0 || nrow(krakenres) == 0 || depth.pos>depth.to || max(krakenres$depth) < depth.pos) {
    # we reached the depthachy level we want, or no data is left
    return()
  } 
      
  # get positions corresponding to the current level in the depthachy
  depth.sel <- krakenres$depth == depth.pos
    
  if (sum(depth.sel) > 0) {

      res.skipped <- NULL
      # check if we have too many nodes at current depth level
      if (sum(depth.sel) > max.per.level) {
          message("got ",sum(depth.sel)," at level ",depth.pos,", but I only want ",max.per.level)
          order.depth <- order(krakenres[depth.sel,"reads"],decreasing=TRUE)
          skip_them <- order.depth[seq(from=max.per.level+1,to=length(order.depth))]
          cumsum_depth <- cumsum(depth.sel)
          message("cumsum_depth")
          print(cumsum_depth)
          message("skip_them (",length(skip_them),")")
          print(head(skip_them))          
          krakenres <- krakenres[!cumsum_depth %in% skip_them,]
          res.skipped <- cbind(source.name=source.name,target.name=paste("and",length(skip_them),"more"),
                   value=sum(krakenres[depth.sel,"reads"][skip_them]))
          depth.sel <- krakenres$depth == depth.pos
      }


      # put a connection from source to the nodes of the current level 
      res <- cbind(source.name=source.name,target.name=krakenres$name[depth.sel],
                   value=krakenres$reads[depth.sel])
      res <- rbind(res,res.skipped)
      
      # split dataframe by the position of the depth
      split.depth <- split(krakenres,cumsum(depth.sel))
      
      for (mydf in split.depth) {
        if (nrow(mydf) > 0) {
          # save the source id of the first row
          source.name <- mydf$name[1]
          res <- rbind(res,
                       res.createlinks(mydf[-1,,drop=F],source.name,depth.pos+1,depth.to=depth.to))
          }
        } 
      
      return(res)
    } else  {
      return(
        res.createlinks(krakenres,source.name=source.name,depth.pos=depth.pos+1,depth.to=depth.to+1))
    }
}


