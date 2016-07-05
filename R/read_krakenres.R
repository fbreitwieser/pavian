# vim: noai:ts=2:sw=2

## recursive function that transforms the kraken dataframe into a cascading list
build_kraken_tree <- function(krakenres) {
  if (nrow(krakenres) == 0 || nrow(krakenres) == 1) {
    # this should only happen if the original input to the function has a size <= 1
    return(list(krakenres))
  }

  ## select the current depth as the one of the topmost data.frame row
  sel_depth <- krakenres[,'depth'] == krakenres[1,'depth']

  ## partition the data.frame into parts with that depth
  depth_partitions <- cumsum(sel_depth)

  ## for each depth partition
  res <- lapply(unique(depth_partitions),
                function(my_depth_partition) {
                  sel <- depth_partitions == my_depth_partition

                  ## return the data.frame row if it is only one row (leaf node, ends recursion)
                  if (sum(sel) == 1)
                    return(krakenres[sel,,drop=F])

                  ## otherwise: take first row as partition descriptor ..
                  first_row <- which(sel)[1]
                  ##  and recurse deeper into the depths with the remaining rows
                  dres <- build_kraken_tree(krakenres[which(sel)[-1],,drop=F])

                  attr(dres,"row") <- krakenres[first_row,,drop=F]
                  dres
                })
  names(res) <- krakenres$name[sel_depth]
  res
}


## Collapse taxonomic levels to only those mentioned in keep_levels
collapse.levels <- function(krakenlist,keep_levels=LETTERS,filter_taxon=NULL) {
  ## input: a list, in which each element is either a
  ##            a list or a data.frame (for the leafs)
  ##   the input has an attribute row that gives details on the current level

  ## columns whose values are added to the next level when
  ##  a level is deleted
  cols <- c("reads_stay","n_unique_kmers","n_kmers")
  if (length(krakenlist) == 0 || is.data.frame(krakenlist)) {
    return(krakenlist)
  }

  parent_row <- attr(krakenlist,"row")
  all.child_rows <- c()

  if (is.null(parent_row)) {
    return(do.call(rbind,lapply(krakenlist,collapse.levels,keep_levels=keep_levels,filter_taxon=filter_taxon)))
  }

  ## rm.reads captures the number of reads that are deleted.
  ##  this has to be propagated to the higher level
  rm.reads <- 0

  for (kl in krakenlist) {
    if (is.data.frame(kl)) {  ## is a leaf node?
      child_rows <- kl
    } else {                 ## recurse deeper into tree
      child_rows <- collapse.levels(kl,keep_levels,filter_taxon=filter_taxon)
      if ('rm.reads' %in% names(attributes(child_rows))) {
        rm.reads <- rm.reads + attr(child_rows,'rm.reads')
      }
    }

    ## check if this level and the levels below should be removed
    delete.taxon <- child_rows[1,'name'] %in% filter_taxon
    if (delete.taxon) {
      rm.reads <- rm.reads + child_rows[1,'reads']
      message(sprintf("removed %7s reads, including %s childs, for %s",child_rows[1,'reads'],nrow(child_rows)-1,child_rows[1,'name']))

      ## remove all children
      child_rows <- NULL

    } else {

      ## check if the last (top-most) row should be kept
      keep_last.child <- child_rows[1,'level'] %in% keep_levels

      if (!keep_last.child) {
        cols <- cols[cols %in% colnames(parent_row)]

        ## save the specified colum information to the parent
        parent_row[,cols] <- parent_row[,cols] + child_rows[1,cols]

        ## remove row
        child_rows <- child_rows[-1,,drop=FALSE]

        ## decrease depths of rows below child row
        if (nrow(child_rows) > 0)
          child_rows[,'depth'] <- child_rows[,'depth'] - 1

      }
    }
    all.child_rows <- rbind(all.child_rows,child_rows)
  }

  ## subtract deleted read count from parent row
  parent_row[,'reads'] <- parent_row[,'reads'] - rm.reads
  res <- rbind(parent_row,all.child_rows)

  if (parent_row[,'reads'] < 0)
    stop("mistake made in removing reads")
  #if (parent_row[,'reads'] == 0)
  #  res <- c()

  if (rm.reads > 0)
    attr(res,'rm.reads') <- rm.reads
  return(res)
}

delete_levels_below <- function(krakenres,level="S") {
  del_level <- 0
  do_del <- FALSE
  del_row <- 0

  cols <- c("reads_stay","n_unique_kmers","n_kmers")
  sub.sums <- c(0,0,0)

  rows_to_delete <- c()
  for (i in seq_len(nrow(krakenres))) {
    if (krakenres[i,'level'] %in% level) {
      del_depth <- krakenres[i,'depth']
      do_del <- TRUE
      del_row <- i
      sub.sums <- c(0,0,0)
    } else {
      if (do_del) {
        if (krakenres[i,'depth'] > del_level) {
          rows_to_delete <- c(rows_to_delete,i)
          sub.sums <- sub.sums + krakenres[i,cols]
        } else {
          krakenres[del_row,cols] <- krakenres[del_row,cols]+sub.sums
          sub.sums <- c(0,0,0)
          do_del <- FALSE
        }
      }
    }
  }
  krakenres[-rows_to_delete,]
}


#' Read kraken report
#'
#' @param myfile kraken report file
#' @param collapse  should the results be collapsed to only those levels specified in keep_levels?
#' @param keep_levels levels to keep when collapse is TRUE
#' @param min.depth minimum depth
#' @param filter_taxon filter certain taxon names
#' @param has_header if the kraken report has a header or not
#' @param add_level_columns if TRUE, for each level columns are added
#'
#' @return report data.frame
#' @export
#'
read_krakenres <- function(myfile,collapse=TRUE,keep_levels=c("D","K","P","C","O","F","G","S"),min.depth=0,filter_taxon=NULL,
                           has_header=NULL,add_level_columns=FALSE) {
  if (is.null(myfile))
    file = "~/work/support/Patient_29Jan_CSF-identify-pathogen/JH64778849.report"
  else
    file <- myfile

  if (is.null(has_header)) {
    first.line <- readLines(myfile,n=1)
    has_header <- grepl("^[a-zA-Z]",first.line)
  }

  if (has_header) {
    krakenres <- utils::read.table(file,sep="\t",header = T,
                            quote = "",stringsAsFactors=FALSE)
    #colnames(krakenres) <- c("percentage","reads","reads_stay","level","taxonid","n_unique_kmers","n_kmers","perc_uniq_kmers","name")

    ## harmonize column names. TODO: Harmonize them in the scripts!
    colnames(krakenres)[colnames(krakenres)=="clade_perc"] <- "percentage"
    colnames(krakenres)[colnames(krakenres)=="perc"] <- "percentage"

    colnames(krakenres)[colnames(krakenres)=="n_reads_clade"] <- "reads"
    colnames(krakenres)[colnames(krakenres)=="n.clade"] <- "reads"

    colnames(krakenres)[colnames(krakenres)=="n_reads_taxo"] <- "reads_stay"
    colnames(krakenres)[colnames(krakenres)=="n.stay"] <- "reads_stay"

    colnames(krakenres)[colnames(krakenres)=="tax_rank"] <- "level"
    colnames(krakenres)[colnames(krakenres)=="tax"] <- "taxonid"

  } else {
    krakenres <- utils::read.table(file,sep="\t",header = F,
                            col.names = c("percentage","reads","reads_stay","level","taxonid","name"),
                            quote = "",stringsAsFactors=FALSE)
  }

  krakenres$depth <- nchar(gsub("\\S.*","",krakenres$name))/2
  krakenres$name <- gsub("^ *","",krakenres$name)
  krakenres$name <- paste(tolower(krakenres$level),krakenres$name,sep="_")


  ## Only stop at certain levels
  ## filter taxon and further up the tree if 'filter_taxon' is defined
  kraken.tree <- build_kraken_tree(krakenres)
  krakenres <- collapse.levels(kraken.tree,keep_levels=keep_levels,filter_taxon=filter_taxon)

  ## Add a metaphlan-style taxon string
  if (add_level_columns) {
    krakenres[,keep_levels] <- NA
  }
  krakenres$taxonstring = krakenres$name
  rows_to_consider <- rep(FALSE,nrow(krakenres))

  for (i in seq_len(nrow(krakenres))) {
    ## depth > 2 correspond to levels below 'D'
    if (i > 1 && krakenres[i,"depth"] > min.depth) {
      ## find the maximal index of a row below the current depth
      idx <- krakenres$depth < krakenres[i,"depth"] & rows_to_consider
      if (!any(idx)) { next() }

      current.level <- krakenres[i,'level']
      my_row <- max(which(idx))
      krakenres[i,'taxonstring'] <- paste(krakenres[my_row,'taxonstring'],krakenres[i,'taxonstring'],sep="|")

      if (add_level_columns) {
        if (krakenres[my_row,'level'] %in% keep_levels) {
          levels.cp <- keep_levels[seq(from=1,to=which(keep_levels == krakenres[my_row,'level']))]
          krakenres[i,levels.cp] <- krakenres[my_row,levels.cp]
        }

        krakenres[i,krakenres[i,'level']] <- krakenres[i,'name']
      }
    }
    rows_to_consider[i] <- TRUE
  }

  krakenres <- krakenres[krakenres$depth >= min.depth,]

  krakenres$percentage <- round(krakenres$reads/sum(krakenres$reads_stay),6) * 100
  if ('n_unique_kmers'  %in% colnames(krakenres))
    krakenres$kmerpercentage <- round(krakenres$n_unique_kmers/sum(krakenres$n_unique_kmers,na.rm=T),6) * 100
  #krakenres$rankperc <- 100/rank(krakenres$reads)

  rownames(krakenres) <- NULL

  krakenres
}



#' Filter lines from a kraken report result based on the taxonomy name
#'
#' It updates the read_stay counts, and removes any children below the
#' entry, and any parent entries that have no reads that stay
#'
#' @param krakenres Report \code{data.frame}.
#' @param filter_taxon Name of entry to remove.
#' @param rm_clade If \code{TRUE}, remove all reads at and below clade, otherwise just set the number of reads that stay at taxon to zero.
#' @param do_message If \code{TRUE}, report how many rows and reads were deleted.
#'
#' @return filtered krakenres
#' @export
filter_taxon <- function(krakenres, filter_taxon, rm_clade = TRUE, do_message=FALSE) {
  taxon_depth <- NULL
  taxon_reads <- 0

  pos.taxons <- which(sub("._","",krakenres$name) %in% filter_taxon)
  #pos.taxon <- which(krakenres$name := filter_taxon)
  if (length(pos.taxons) == 0) {
    return(krakenres)
  }

  row_seq <- seq_len(nrow(krakenres))
  rows_to_delete <- rep(FALSE,nrow(krakenres))

  taxon_depths <- krakenres[pos.taxons,"depth"]
  if (isTRUE(rm_clade)) {
    taxon_readss <- krakenres[pos.taxons,"reads"]
  } else {
    taxon_readss <- krakenres[pos.taxons,"reads_stay"]
    krakenres[pos.taxons,"reads_stay"] <- 0
  }


  for (i in seq_along(pos.taxons)) {
    pos.taxon <- pos.taxons[i]
    if (pos.taxon == 1) {
      rows_to_delete[1] <- TRUE
      next
    }
    taxon_depth <- taxon_depths[i]
    taxon_reads <- taxon_readss[i]

    if (rm_clade) {
      tosum_below <-  row_seq >= pos.taxon & krakenres$depth <= taxon_depth
      taxons_below <- cumsum(tosum_below) == 1
      rows_to_delete[taxons_below] <- TRUE
    }
    rows_to_update <- c(pos.taxon)

    taxons_above <- seq_len(nrow(krakenres)) < pos.taxon & krakenres$depth == taxon_depth

    any_stays <- FALSE
    prev_taxon_depth <- taxon_depth
    taxons_above <- c()
    for (i in seq(from=(pos.taxon-1),to=1)) {
      curr_taxon_depth <- krakenres[i,"depth"]
      if (curr_taxon_depth < prev_taxon_depth) {
        if (!any_stays) {
          if (krakenres[i,"reads"] == taxon_reads) {
            rows_to_delete[i] <- TRUE
            if (do_message)
              message("Deleting ",krakenres[i,"name"])
          } else {
            any_stays <- TRUE
          }
        }
        if (!rows_to_delete[i]) {
          rows_to_update <- c(rows_to_update, i)
          if (do_message)
            message("Updating ",krakenres[i,"name"])
        }
        prev_taxon_depth <- curr_taxon_depth
      } else {
        any_stays <- TRUE
      }
    }
    krakenres[rows_to_update, "reads"] <- krakenres[rows_to_update, "reads"] - taxon_reads
  }

  #if (rm_clade)
    krakenres[!rows_to_delete,]
  #else
  #  krakenres
}

