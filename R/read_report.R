# vim: noai:ts=2:sw=2

## recursive function that transforms the kraken dataframe into a cascading list
build_kraken_tree <- function(report) {
  if (nrow(report) == 0 || nrow(report) == 1) {
    # this should only happen if the original input to the function has a size <= 1
    return(list(report))
  }

  ## select the current depth as the one of the topmost data.frame row
  sel_depth <- report[,'depth'] == report[1,'depth']

  ## partition the data.frame into parts with that depth
  depth_partitions <- cumsum(sel_depth)

  ## for each depth partition
  res <- lapply(unique(depth_partitions),
                function(my_depth_partition) {
                  sel <- depth_partitions == my_depth_partition

                  ## return the data.frame row if it is only one row (leaf node, ends recursion)
                  if (sum(sel) == 1)
                    return(report[sel,,drop=F])

                  ## otherwise: take first row as partition descriptor ..
                  first_row <- which(sel)[1]
                  ##  and recurse deeper into the depths with the remaining rows
                  dres <- build_kraken_tree(report[which(sel)[-1],,drop=F])

                  attr(dres,"row") <- report[first_row,,drop=F]
                  dres
                })
  names(res) <- report$name[sel_depth]
  res
}


## Collapse taxonomic ranks to only those mentioned in keep_ranks
collapse.ranks <- function(krakenlist,keep_ranks=LETTERS,filter_taxon=NULL) {
  ## input: a list, in which each element is either a
  ##            a list or a data.frame (for the leafs)
  ##   the input has an attribute row that gives details on the current rank

  ## columns whose values are added to the next rank when
  ##  a rank is deleted
  cols <- c("reads_stay","n_unique_kmers","n_kmers")
  if (length(krakenlist) == 0 || is.data.frame(krakenlist)) {
    return(krakenlist)
  }

  parent_row <- attr(krakenlist,"row")
  all.child_rows <- c()

  if (is.null(parent_row)) {
    return(do.call(rbind,lapply(krakenlist,collapse.ranks,keep_ranks=keep_ranks,filter_taxon=filter_taxon)))
  }

  ## rm.reads captures the number of reads that are deleted.
  ##  this has to be propagated to the higher rank
  rm.reads <- 0

  for (kl in krakenlist) {
    if (is.data.frame(kl)) {  ## is a leaf node?
      child_rows <- kl
    } else {                 ## recurse deeper into tree
      child_rows <- collapse.ranks(kl,keep_ranks,filter_taxon=filter_taxon)
      if ('rm.reads' %in% names(attributes(child_rows))) {
        rm.reads <- rm.reads + attr(child_rows,'rm.reads')
      }
    }

    ## check if this rank and the ranks below should be removed
    delete.taxon <- child_rows[1,'name'] %in% filter_taxon
    if (delete.taxon) {
      rm.reads <- rm.reads + child_rows[1,'reads']
      message(sprintf("removed %7s reads, including %s childs, for %s",child_rows[1,'reads'],nrow(child_rows)-1,child_rows[1,'name']))

      ## remove all children
      child_rows <- NULL

    } else {

      ## check if the last (top-most) row should be kept
      keep_last.child <- child_rows[1,'rank'] %in% keep_ranks

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

delete_ranks_below <- function(report,rank="S") {
  del_rank <- 0
  do_del <- FALSE
  del_row <- 0

  cols <- c("reads_stay","n_unique_kmers","n_kmers")
  sub.sums <- c(0,0,0)

  rows_to_delete <- c()
  for (i in seq_len(nrow(report))) {
    if (report[i,'rank'] %in% rank) {
      del_depth <- report[i,'depth']
      do_del <- TRUE
      del_row <- i
      sub.sums <- c(0,0,0)
    } else {
      if (do_del) {
        if (report[i,'depth'] > del_rank) {
          rows_to_delete <- c(rows_to_delete,i)
          sub.sums <- sub.sums + report[i,cols]
        } else {
          report[del_row,cols] <- report[del_row,cols]+sub.sums
          sub.sums <- c(0,0,0)
          do_del <- FALSE
        }
      }
    }
  }
  report[-rows_to_delete,]
}


#' Read kraken or centrifuge-style report
#'
#' @param myfile kraken report file
#' @param collapse  should the results be collapsed to only those ranks specified in keep_ranks?
#' @param keep_ranks ranks to keep when collapse is TRUE
#' @param min.depth minimum depth
#' @param filter_taxon filter certain taxon names
#' @param has_header if the kraken report has a header or not
#' @param add_rank_columns if TRUE, for each rank columns are added
#'
#' @return report data.frame
#' @export
#'
read_report2 <- function(myfile,collapse=TRUE,keep_ranks=c("D","K","P","C","O","F","G","S"),min.depth=0,filter_taxon=NULL,
                         has_header=NULL,add_rank_columns=FALSE) {

  first.line <- readLines(myfile,n=1)
  isASCII <-  function(txt) all(charToRaw(txt) <= as.raw(127))
  if (!isASCII(first.line)) {
    message(myfile," is no valid report")
    return(NULL)
  }
  if (is.null(has_header)) {
    has_header <- grepl("^[a-zA-Z]",first.line)
  }

  if (has_header) {
    report <- utils::read.table(myfile,sep="\t",header = T,
                                quote = "",stringsAsFactors=FALSE)
    #colnames(report) <- c("percentage","reads","reads_stay","rank","taxonid","n_unique_kmers","n_kmers","perc_uniq_kmers","name")

    ## harmonize column names. TODO: Harmonize them in the scripts!
    colnames(report)[colnames(report)=="clade_perc"] <- "percentage"
    colnames(report)[colnames(report)=="perc"] <- "percentage"

    colnames(report)[colnames(report)=="n_reads_clade"] <- "reads"
    colnames(report)[colnames(report)=="n.clade"] <- "reads"

    colnames(report)[colnames(report)=="n_reads_taxo"] <- "reads_stay"
    colnames(report)[colnames(report)=="n.stay"] <- "reads_stay"

    colnames(report)[colnames(report)=="tax_rank"] <- "rank"
    colnames(report)[colnames(report)=="tax"] <- "taxonid"

  } else {
    report <- utils::read.table(myfile,sep="\t",header = F,
                                col.names = c("percentage","reads","reads_stay","rank","taxonid","name"),
                                quote = "",stringsAsFactors=FALSE)
  }

  report$depth <- nchar(gsub("\\S.*","",report$name))/2
  report$name <- gsub("^ *","",report$name)
  report$name <- paste(tolower(report$rank),report$name,sep="_")


  ## Only stop at certain ranks
  ## filter taxon and further up the tree if 'filter_taxon' is defined
  kraken.tree <- build_kraken_tree(report)
  report <- collapse.ranks(kraken.tree,keep_ranks=keep_ranks,filter_taxon=filter_taxon)

  ## Add a metaphlan-style taxon string
  if (add_rank_columns) {
    report[,keep_ranks] <- NA
  }
  report$taxonstring = report$name
  rows_to_consider <- rep(FALSE,nrow(report))

  for (i in seq_len(nrow(report))) {
    ## depth > 2 correspond to ranks below 'D'
    if (i > 1 && report[i,"depth"] > min.depth) {
      ## find the maximal index of a row below the current depth
      idx <- report$depth < report[i,"depth"] & rows_to_consider
      if (!any(idx)) { next() }

      current.rank <- report[i,'rank']
      my_row <- max(which(idx))
      report[i,'taxonstring'] <- paste(report[my_row,'taxonstring'],report[i,'taxonstring'],sep="|")

      if (add_rank_columns) {
        if (report[my_row,'rank'] %in% keep_ranks) {
          ranks.cp <- keep_ranks[seq(from=1,to=which(keep_ranks == report[my_row,'rank']))]
          report[i,ranks.cp] <- report[my_row,ranks.cp]
        }

        report[i,report[i,'rank']] <- report[i,'name']
      }
    }
    rows_to_consider[i] <- TRUE
  }

  report <- report[report$depth >= min.depth,]

  report$percentage <- round(report$reads/sum(report$reads_stay),6) * 100
  if ('n_unique_kmers'  %in% colnames(report))
    report$kmerpercentage <- round(report$n_unique_kmers/sum(report$n_unique_kmers,na.rm=T),6) * 100
  #report$rankperc <- 100/rank(report$reads)

  rownames(report) <- NULL

  report
}



#' Filter lines from a kraken report result based on the taxonomy name
#'
#' It updates the read_stay counts, and removes any children below the
#' entry, and any parent entries that have no reads that stay
#'
#' @param report Report \code{data.frame}.
#' @param filter_taxon Name of entry to remove.
#' @param rm_clade If \code{TRUE}, remove all reads at and below clade, otherwise just set the number of reads that stay at taxon to zero.
#' @param do_message If \code{TRUE}, report how many rows and reads were deleted.
#'
#' @return filtered report
#' @export
filter_taxon <- function(report, filter_taxon, rm_clade = TRUE, do_message=FALSE) {
  taxon_depth <- NULL
  taxon_reads <- 0

  pos.taxons <- which(sub("._","",report$name) %in% filter_taxon)
  #pos.taxon <- which(report$name := filter_taxon)
  if (length(pos.taxons) == 0) {
    return(report)
  }

  row_seq <- seq_len(nrow(report))
  rows_to_delete <- rep(FALSE,nrow(report))

  taxon_depths <- report[pos.taxons,"depth"]
  if (isTRUE(rm_clade)) {
    taxon_readss <- report[pos.taxons,"reads"]
  } else {
    taxon_readss <- report[pos.taxons,"reads_stay"]
    report[pos.taxons,"reads_stay"] <- 0
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
      tosum_below <-  row_seq >= pos.taxon & report$depth <= taxon_depth
      taxons_below <- cumsum(tosum_below) == 1
      rows_to_delete[taxons_below] <- TRUE
    }
    rows_to_update <- c(pos.taxon)

    taxons_above <- seq_len(nrow(report)) < pos.taxon & report$depth == taxon_depth

    any_stays <- FALSE
    prev_taxon_depth <- taxon_depth
    taxons_above <- c()
    for (i in seq(from=(pos.taxon-1),to=1)) {
      curr_taxon_depth <- report[i,"depth"]
      if (curr_taxon_depth < prev_taxon_depth) {
        if (!any_stays) {
          if (report[i,"reads"] == taxon_reads) {
            rows_to_delete[i] <- TRUE
            if (do_message)
              message("Deleting ",report[i,"name"])
          } else {
            any_stays <- TRUE
          }
        }
        if (!rows_to_delete[i]) {
          rows_to_update <- c(rows_to_update, i)
          if (do_message)
            message("Updating ",report[i,"name"])
        }
        prev_taxon_depth <- curr_taxon_depth
      } else {
        any_stays <- TRUE
      }
    }
    report[rows_to_update, "reads"] <- report[rows_to_update, "reads"] - taxon_reads
  }

  #if (rm_clade)
  report[!rows_to_delete,]
  #else
  #  report
}


#' Read Kraken-style and MetaPhlAn reports
#'
#' @param myfile Kraken-style or MetaPhlAn report file.
#' @param has_header If the kraken report has a header or not.
#' @param check_file If TRUE, only the first 5 lines of the file are loaded.
#'
#' @return report data.frame
#' @export
#'
read_report <- function(myfile, has_header=NULL, check_file = FALSE) {

  #myfile <- file(myfile)
  #file_class <- summary(myfile)$class
  #if (file_class == "gzfile")
  #  myfile <- gzcon(myfile)

  first.line <- tryCatch( readLines(myfile,n=1, warn=FALSE),
                          error = function(e) { warning("Error reading ",myfile); return() })
  isASCII <-  function(txt) {
    if (length(txt) == 0)
      return(FALSE)
    raw <- charToRaw(txt)
    all(raw <= as.raw(127) && (raw >= as.raw(32) | raw == as.raw(9)))
  }
  if (!isTRUE(isASCII(first.line))) {
    message(myfile," is not a ASCII file")
    return(NULL)
  }

  if (is.null(has_header)) {
    has_header <- grepl("^[a-zA-Z#\"]",first.line)
  }

  nrows <- ifelse(check_file, 5, -1)
  if (has_header) {
    report <- tryCatch({
      utils::read.table(myfile,sep="\t",header = T,
                        quote = "",stringsAsFactors=FALSE,
                        comment.char = "", nrows = nrows)
    }, error = function(x) NULL, warning = function(x) NULL)
    if (is.null(report)) { return(NULL); }
    #colnames(report) <- c("percentage","reads","reads_stay","rank","taxonid","n_unique_kmers","n_kmers","perc_uniq_kmers","name")

    ## harmonize column names. TODO: Harmonize them in the scripts!
    colnames(report)[colnames(report)=="clade_perc"] <- "percentage"
    colnames(report)[colnames(report)=="perc"] <- "percentage"

    colnames(report)[colnames(report)=="n_reads_clade"] <- "reads"
    colnames(report)[colnames(report)=="n.clade"] <- "reads"

    colnames(report)[colnames(report)=="n_reads_taxo"] <- "reads_stay"
    colnames(report)[colnames(report)=="n.stay"] <- "reads_stay"

    colnames(report)[colnames(report)=="tax_rank"] <- "rank"
    colnames(report)[colnames(report)=="level"] <- "rank"
    colnames(report)[colnames(report)=="tax"] <- "taxonid"

  } else {
    report <- tryCatch({
      utils::read.table(myfile,sep="\t",header = F,
                        col.names = c("percentage","reads","reads_stay","rank","taxonid","name"),
                        quote = "",stringsAsFactors=FALSE,
                        nrows = nrows)
    }, error=function(x) NULL, warning=function(x) NULL)
    if (is.null(report)) { return(NULL); }
  }

  if (colnames(report)[1] == "X.SampleID") {
    ## Metaphlan report
    colnames(report) <- c("taxonstring", "reads")
    report <- report[order(report$taxonstring), ]
    report$taxonstring <- gsub("_"," ",report$taxonstring)
    report$taxonstring <- gsub("  ","_",report$taxonstring)
    report$taxonstring <- paste0("-_root|", report$taxonstring)

    report <- rbind(
      data.frame(taxonstring=c("u_unclassified","-_root"),reads=c(0,100), stringsAsFactors = F),
      report)
  }

  if (all(c("name","rank") %in% colnames(report)) && !"taxonstring" %in% colnames(report)) {
    ## Kraken report
    report$depth <- nchar(gsub("\\S.*","",report$name))/2
    report$name <- gsub("^ *","",report$name)
    report$name <- paste(tolower(report$rank),report$name,sep="_")

    rownames(report) <- NULL

    ## make taxonstring path
    report$taxonstring <- report$name
    n <- nrow(report)
    depths <- report$depth
    taxonstrings <- report$name

    prev_row <- 2

    if (nrow(report) < 3) {
      return(report)
    }
    for (current_row in seq(from=3, nrow(report))) {
      while (depths[current_row] != depths[prev_row] + 1) {
        # find previous row with correct depth
        prev_row <- prev_row - 1
      }
      taxonstrings[current_row] <- paste0(taxonstrings[prev_row], "|", taxonstrings[current_row])

      prev_row <- current_row
    }

    report$taxonstring <- taxonstrings

  } else if ("taxonstring" %in% colnames(report)) {
    taxonstrings <- strsplit(report$taxonstring, "|", fixed=TRUE)

    if (!"name" %in% colnames(report))
      report$name <- sapply(taxonstrings, function(x) x[length(x)])

    if (!"depth" %in% colnames(report)) {
      report$depth <- sapply(taxonstrings, length) - 1
    }
    if (!"rank" %in% colnames(report))
      report$rank <- toupper(substr(report$name, 0, 1))
  }


  if (!all(c("name","rank") %in% colnames(report)) ||
      nrow(report) < 2 ||
      report[1,"name"] != "u_unclassified" ||
      report[2,"name"] != "-_root") {
    return(NULL)
  }


  if (!"reads_stay" %in% colnames(report)) {
    taxonstrings <- strsplit(report$taxonstring, "|", fixed=TRUE)
    ## fix reads_stay
    report$parent <- sapply(taxonstrings, function(x) x[length(x) - 1])
    report$reads_stay <- report$reads - sapply(report$name, function(x) sum(report$reads[report$parent == x]))
    #report$reads_stay[sapply(report$reads_stay, function(x) isTRUE(all.equal(x, 0)))] <- 0
    report$reads_stay[report$reads_stay <= 0.00001] <- 0  # fix for rounding in percentages by MetaPhlAn
  }

  report$percentage <- signif(report$reads/sum(report$reads_stay),6) * 100
  if ('n_unique_kmers'  %in% colnames(report))
    report$kmerpercentage <- round(report$n_unique_kmers/sum(report$n_unique_kmers,na.rm=T),6) * 100
  #report$rankperc <- 100/rank(report$reads)

  #report$depth <- NULL

  if ("taxonid" %in% colnames(report)) {
    std_colnames <- c("percentage","reads","reads_stay","rank", "taxonid","name")
  } else {
    std_colnames <- c("percentage","reads","reads_stay","rank","name")
  }
  stopifnot(all(std_colnames %in% colnames(report)))
  report[, c(std_colnames, setdiff(colnames(report), std_colnames))]
}

