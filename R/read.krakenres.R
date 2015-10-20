# vim: noai:ts=2:sw=2

## recursive function that transforms the kraken dataframe into a cascading list
build.kraken.tree <- function(krakenres) {
  if (nrow(krakenres) == 0 || nrow(krakenres) == 1) {
    # this should only happen if the original input to the function has a size <= 1
    return(list(krakenres))
  }

  ## select the current depth as the one of the topmost data.frame row
  sel.depth <- krakenres[,'depth'] == krakenres[1,'depth']

  ## partition the data.frame into parts with that depth
  depth.partitions <- cumsum(sel.depth)

  ## for each depth partition
  res <- lapply(unique(depth.partitions),
                function(my.depth.partition) {
                  sel <- depth.partitions == my.depth.partition

                  ## return the data.frame row if it is only one row (leaf node, ends recursion)
                  if (sum(sel) == 1)
                    return(krakenres[sel,,drop=F])

                  ## otherwise: take first row as partition descriptor ..
                  first.row <- which(sel)[1]
                  ##  and recurse deeper into the depths with the remaining rows
                  dres <- build.kraken.tree(krakenres[which(sel)[-1],,drop=F])

                  attr(dres,"row") <- krakenres[first.row,,drop=F]
                  dres
                })
  names(res) <- krakenres$name[sel.depth]
  res
}


## Collapse taxonomic levels to only those mentioned in keep.levels
collapse.levels <- function(krakenlist,keep.levels=LETTERS,filter_taxon=NULL) {
  ## input: a list, in which each element is either a
  ##            a list or a data.frame (for the leafs)
  ##   the input has an attribute row that gives details on the current level

  ## columns whose values are added to the next level when
  ##  a level is deleted
  cols <- c("reads_stay","n_unique_kmers","n_kmers")
  if (length(krakenlist) == 0 || is.data.frame(krakenlist)) {
    return(krakenlist)
  }

  parent.row <- attr(krakenlist,"row")
  all.child.rows <- c()

  if (is.null(parent.row)) {
    return(do.call(rbind,lapply(krakenlist,collapse.levels,keep.levels=keep.levels,filter_taxon=filter_taxon)))
  }

  ## rm.reads captures the number of reads that are deleted.
  ##  this has to be propagated to the higher level
  rm.reads <- 0

  for (kl in krakenlist) {
    if (is.data.frame(kl)) {  ## is a leaf node?
      child.rows <- kl
    } else {                 ## recurse deeper into tree
      child.rows <- collapse.levels(kl,keep.levels,filter_taxon=filter_taxon)
      if ('rm.reads' %in% names(attributes(child.rows))) {
        rm.reads <- rm.reads + attr(child.rows,'rm.reads')
      }
    }

    ## check if this level and the levels below should be removed
    delete.taxon <- child.rows[1,'name'] %in% filter_taxon
    if (delete.taxon) {
      rm.reads <- rm.reads + child.rows[1,'reads']
      message(sprintf("removed %7s reads, including %s childs, for %s",child.rows[1,'reads'],nrow(child.rows)-1,child.rows[1,'name']))

      ## remove all children
      child.rows <- NULL

    } else {

    ## check if the last (top-most) row should be kept
    keep.last.child <- child.rows[1,'level'] %in% keep.levels

    if (!keep.last.child) {
      cols <- cols[cols %in% colnames(parent.row)]

      ## save the specified colum information to the parent
      parent.row[,cols] <- parent.row[,cols] + child.rows[1,cols]

      ## remove row
      child.rows <- child.rows[-1,,drop=FALSE]

      ## decrease depths of rows below child row
      if (nrow(child.rows) > 0)
        child.rows[,'depth'] <- child.rows[,'depth'] - 1

    }
    }
    all.child.rows <- rbind(all.child.rows,child.rows)
  }

  ## subtract deleted read count from parent row
  parent.row[,'reads'] <- parent.row[,'reads'] - rm.reads
  res <- rbind(parent.row,all.child.rows)

  if (parent.row[,'reads'] < 0)
    stop("mistake made in removing reads")
  #if (parent.row[,'reads'] == 0)
  #  res <- c()

  if (rm.reads > 0)
    attr(res,'rm.reads') <- rm.reads
  return(res)
}

delete.levels.below <- function(krakenres,level="S") {
  del.level <- 0
  do.del <- FALSE
  del.row <- 0

  cols <- c("reads_stay","n_unique_kmers","n_kmers")
  sub.sums <- c(0,0,0)

  rows.to.delete <- c()
  for (i in seq_len(nrow(krakenres))) {
    if (krakenres[i,'level'] %in% level) {
      del.depth <- krakenres[i,'depth']
      do.del <- TRUE
      del.row <- i
      sub.sums <- c(0,0,0)
    } else {
      if (do.del) {
        if (krakenres[i,'depth'] > del.level) {
          rows.to.delete <- c(rows.to.delete,i)
          sub.sums <- sub.sums + krakenres[i,cols]
        } else {
          krakenres[del.row,cols] <- krakenres[del.row,cols]+sub.sums
          sub.sums <- c(0,0,0)
          do.del <- FALSE
        }
      }
    }
  }
  krakenres[-rows.to.delete,]
}


#' Read kraken report
#'
#' @param myfile kraken report file
#' @param collapse  should the results be collapsed to only those levels specified in keep.levels?
#' @param keep.levels levels to keep when collapse is TRUE
#' @param min.depth minimum depth
#' @param filter_taxon filter certain taxon names
#' @param v2 report format is in v2, with kmer frequencies
#' @param add.level.columns if TRUE, for each level columns are added
#'
#' @return report data.frame
#'
read.krakenres <- function(myfile,collapse=TRUE,keep.levels=c("D","K","P","C","O","F","G","S"),min.depth=0,filter_taxon=NULL,
has.header=NULL,add.level.columns=FALSE) {
  if (is.null(myfile))
    file = "~/work/support/Patient_29Jan_CSF-identify-pathogen/JH64778849.report"
  else
    file <- myfile

	if (is.null(has.header)) {
		first.line <- readLines(myfile,n=1)
		has.header <- grepl("^[a-zA-Z]",first.line)
	}

  if (has.header) {
  krakenres <- read.table(file,sep="\t",header = T,
                          quote = "",stringsAsFactors=FALSE)
    #colnames(krakenres) <- c("coverage","reads","reads_stay","level","taxonid","n_unique_kmers","n_kmers","perc_uniq_kmers","name")

    ## harmonize column names. TODO: Harmonize them in the scripts!
    colnames(krakenres)[colnames(krakenres)=="clade_perc"] <- "coverage"
    colnames(krakenres)[colnames(krakenres)=="perc"] <- "coverage"

    colnames(krakenres)[colnames(krakenres)=="n_reads_clade"] <- "reads"
    colnames(krakenres)[colnames(krakenres)=="n.clade"] <- "reads"

    colnames(krakenres)[colnames(krakenres)=="n_reads_taxo"] <- "reads_stay"
    colnames(krakenres)[colnames(krakenres)=="n.stay"] <- "reads_stay"

    colnames(krakenres)[colnames(krakenres)=="tax_rank"] <- "level"
    colnames(krakenres)[colnames(krakenres)=="tax"] <- "taxonid"

  } else {
  krakenres <- read.table(file,sep="\t",header = F,
                          col.names = c("coverage","reads","reads_stay","level","taxonid","name"),
                          quote = "",stringsAsFactors=FALSE)
  }

  krakenres$depth <- nchar(gsub("\\S.*","",krakenres$name))/2
  krakenres$name <- gsub("^ *","",krakenres$name)
  krakenres$name <- paste(tolower(krakenres$level),krakenres$name,sep="_")


  ## Only stop at certain levels
  ## filter taxon and further up the tree if 'filter_taxon' is defined
  kraken.tree <- build.kraken.tree(krakenres)
  krakenres <- collapse.levels(kraken.tree,keep.levels=keep.levels,filter_taxon=filter_taxon)

  ## Add a metaphlan-style taxon string
	if (add.level.columns) {
		krakenres[,keep.levels] <- NA
	}
  krakenres$taxonstring = krakenres$name
  rows.to.consider <- rep(FALSE,nrow(krakenres))

  for (i in seq_len(nrow(krakenres))) {
      ## depth > 2 correspond to levels below 'D'
      if (i > 1 && krakenres[i,"depth"] > min.depth) {
        ## find the maximal index of a row below the current depth
        idx <- krakenres$depth < krakenres[i,"depth"] & rows.to.consider
        if (!any(idx)) { next() }

				current.level <- krakenres[i,'level']
        my.row <- max(which(idx))
        krakenres[i,'taxonstring'] <- paste(krakenres[my.row,'taxonstring'],krakenres[i,'taxonstring'],sep="|")

				if (add.level.columns) {
					if (krakenres[my.row,'level'] %in% keep.levels) {
						levels.cp <- keep.levels[seq(from=1,to=which(keep.levels == krakenres[my.row,'level']))]
						krakenres[i,levels.cp] <- krakenres[my.row,levels.cp]
					}

				  krakenres[i,krakenres[i,'level']] <- krakenres[i,'name']
				}
    }
    rows.to.consider[i] <- TRUE
  }

  krakenres <- krakenres[krakenres$depth >= min.depth,]

  krakenres$coverage <- round(krakenres$reads/sum(krakenres$reads_stay),6) * 100
  if ('n_unique_kmers'  %in% colnames(krakenres))
    krakenres$kmercoverage <- round(krakenres$n_unique_kmers/sum(krakenres$n_unique_kmers,na.rm=T),6) * 100
  krakenres$rankperc <- 100/rank(krakenres$reads)

  rownames(krakenres) <- NULL

  krakenres
}



#' Filter lines from a kraken report result based on the taxonomy name
#'
#' It updates the read_stay counts, and removes any children below the
#' entry, and any parent entries that have no reads that stay
#'
#' @param krakenres kraken report.
#' @param filter_taxon name of entry to remove.
#' @param do_message If TRUE, report how many rows and reads were deleted.
#'
#' @return filtered krakenres
#' @export
#'
#' @examples
#' \donotrun{
#'   filter_taxon(krakenres, 's_Homo sapiens')
#' }
filter_taxon <- function(krakenres,filter_taxon, do_message=TRUE) {
    taxon.depth <- NULL
    taxon.reads <- 0

    pos.taxon <- which(krakenres$name == filter_taxon)
    if (length(pos.taxon) == 0) {
      return(krakenres)
    } else if (length(pos.taxon) > 1) {
      stop("More than one position matches to ",filter_taxon)
    }

    taxon.depth <- krakenres[pos.taxon,"depth"]
    taxon.reads <- krakenres[pos.taxon,"reads"]
    del.taxon <- filter_taxon

    sseq <- seq(from=1,to=nrow(krakenres))

    rows.to.delete <- c(pos.taxon)
    if (pos.taxon < nrow(krakenres)) {
      for (i in seq(from=pos.taxon+1,to=nrow(krakenres))) {
         if (krakenres[i,"depth"] > taxon.depth) {
           rows.to.delete <- c(rows.to.delete,i)
         } else {
           break()
         }
      }
    }

    ## go to the top in the data.frame and update reads
    if (pos.taxon > 1) {
    delete.no.more.rows <- FALSE
    for (i in seq(from=(pos.taxon-1),to=1)) {

        ##   and the depth is higher than the delete depth
        if (krakenres[i,'depth'] >= taxon.depth) {
          delete.no.more.rows <- TRUE
          next()
        }

        if (krakenres[i,'reads'] > (taxon.reads + krakenres[i,'reads_stay'])) {
          ## just update the read count of the current row,
          ##   if not all reads can be accounted for by the deleted levels (plus those that stay)
          krakenres[i,'reads'] <- krakenres[i,'reads'] - taxon.reads

          if (krakenres[i,'depth'] == 0)
            break

        } else if (!delete.no.more.rows && krakenres[i,'reads'] == (taxon.reads + krakenres[i,'reads_stay'])) {
          ## if all reads can be accounted for by the deleted levels, plus those that stay, delete this level
          del.taxon <- krakenres[i,'name']
          taxon.depth <- krakenres[i,'depth']
          taxon.reads <- krakenres[i,'reads']
          rows.to.delete <- c(rows.to.delete,i)
        }
      }
    }
    krakenres <- krakenres[-rows.to.delete,]
    if (do_message)
      message(sprintf("Deleted %10s reads, %s rows from %s up to %s",taxon.reads,length(rows.to.delete),filter_taxon,del.taxon))
    krakenres
}

