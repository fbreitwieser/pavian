#' @export

get_nodes_and_links <- function(krakenres,max_per_level=10) {

  max_per_level <- 10
  current_depth <- min(krakenres$depth)

  while(current_depth <= max(krakenres$depth)) {
    at_current_depth <- krakenres$depth==current_depth
    if (sum(at_current_depth) > max_per_level) {

      at_current_depth_order <- order(krakenres[at_current_depth,"reads"],decreasing=TRUE)
      skip_those_indices <- at_current_depth_order[seq(from=max_per_level+1,to=length(at_current_depth_order))]

      ## with cumsum we get the indices partitioned by the current depth level
      split_by_depth <- cumsum(at_current_depth)
      krakenres[at_current_depth,"name"][skip_those_indices] <- paste("Skipped",current_depth,skip_those_indices)

      skip_those_kids <-
        (split_by_depth %in% skip_those_indices) &
        (krakenres$depth >= current_depth) # & (!krakenres$name %in% paste("Skipped",current_depth,skip_those_indices))

      message("skipping ",sum(skip_those_kids)," at depth ",current_depth)

      #print (krakenres[skip_those_kids,])
      #stop()
      krakenres <- krakenres[!skip_those_kids,]
    }
    current_depth = current_depth + 1
  }

  nodes_n_links <- create_links(krakenres,NA,current_depth=0,max_depth=15)
  nodes_n_links <- unique(nodes_n_links)
  nodes_n_links <- nodes_n_links[!is.na(nodes_n_links[,'source.name']),]

  node.names <- sort(unique(c(nodes_n_links[,'source.name'],nodes_n_links[,'target.name'])))
  nodes <- data.frame(name=node.names,stringsAsFactors=FALSE)
  node.to.id <- setNames(seq_along(node.names),node.names)

  links <- data.frame(source=node.to.id[nodes_n_links[,'source.name']]-1,
                       target=node.to.id[nodes_n_links[,'target.name']]-1,
                       value=as.numeric(nodes_n_links[,'value']))

  return(list(nodes,links))
}

create_links <- function(krakenres,source.name,current_depth,max_depth=Inf) {
  max_per_level <- 10

  if (!isTRUE(nrow(krakenres) > 0) ||
      current_depth > max_depth || current_depth > max(krakenres$depth)) {
    # we reached the maximum depth level, or no data is left
    return()
  }

  # get positions corresponding to the current level in the depthachy
  at_current_depth <- krakenres$depth == current_depth

  res <- NULL
  if (sum(at_current_depth) > 0) {

      # make a connection from source to the nodes of the current level
      res <- cbind(source.name=source.name,
                   target.name=krakenres$name[at_current_depth],
                   value=krakenres$reads[at_current_depth],
                   depth=current_depth)

      # split dataframe by the position of the depth
      split.depth <- split(krakenres,cumsum(at_current_depth))

      for (mydf in split.depth) {
        if (nrow(mydf) > 0) {
          # save the source id of the first row
          source.name <- mydf$name[1]
          res <- rbind(res,
                       create_links(mydf[-1,,drop=F],source.name,current_depth+1,max_depth=max_depth))
        }
      }

      return(res)
    } else  {
      return(
        create_links(krakenres,source.name=source.name,
                     current_depth=current_depth+1,max_depth=max_depth+1))
    }
}


