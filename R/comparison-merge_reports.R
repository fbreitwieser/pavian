REPORTATTR_vec <- c(
  data_column_start = "data_column_start",
  data_columns = "data_columns",
  reads_columns = "read_columns",
  taxonReads_columns = "taxonReads_columns",
  stat_column = "stat_column",
  taxID_column = "taxID_column"
)

REPORTATTR <- function(name) {
  if (!name %in% REPORTATTR_vec) {
    stop(name, " is not a report attribute")
  }
  REPORTATTR_vec[[name, exact=TRUE]]
}

COLNAMES <- list(
  name = "Name",
  taxRank = "Rank",
  taxID = "TID",
  taxLineage = "Lineage"
)

req_attr <- function(mydf, myattr) {
  validate(need(myattr), "Need valid attribute definition!")
  validate(need(attr(mydf,myattr, exact=TRUE)), paste("Attribute", myattr, "is not truthy!"))
  return(attr(mydf,myattr, exact=TRUE))
}

normalize_data_cols <- function(merged_reports, normalize_col = REPORTATTR("taxonReads_columns"), sum_reads = NULL) {
  data_columns <- req_attr(merged_reports, REPORTATTR("data_columns"))
  normalize_columns <- req_attr(merged_reports, normalize_col)
  
  validate(need(data_columns, message="data_columns is NULL"),
           need(normalize_columns, message=paste(normalize_col,"is NULL")))
  
  if (is.null(sum_reads)) {
    sum_reads <- colSums(merged_reports[, normalize_columns, drop=F], na.rm = T)
  }
  
  if(length(sum_reads) != length(normalize_columns))
    stop("length(sum_reads) != length(normalize_columns)")
  
  sum_reads <- rep(sum_reads, each = length(data_columns) / length(normalize_columns))
  
  merged_reports[, data_columns] <- 100*scale(merged_reports[, data_columns],
                                              center = rep(0, length(sum_reads)),
                                              scale = sum_reads)
  
  merged_reports
}

log_data_cols <- function(merged_reports) {
  data_columns <- req_attr(merged_reports, REPORTATTR("data_columns"))
  merged_reports[, data_columns] <- log10(merged_reports[, data_columns, drop=F] + 1)
  
  merged_reports
}

#' Calculate robust z-score on matrix
#'
#' @param m matrix
#' @param min_scale minimum scale
#'
#' @return robust z-score of matrix
#' @export
robust_zscore <- function(m, min_scale = 1) {
  m1 <- m
  m1[is.na(m1)] <- 0
  t(scale(t(m),
          center = apply(m1,1,stats::median),
          scale  = pmax(apply(m1,1,stats::mad), min_scale)))
}

calc_robust_zscore <- function(merged_reports, min_scale = 1) {
  data_columns <- req_attr(merged_reports, REPORTATTR("data_columns"))
  stopifnot(!is.null(data_columns))
  
  dp2 <- merged_reports[, data_columns, drop=F]
  dp2[is.na(dp2)] <- 0
  merged_reports[, data_columns] <- t(scale(t(merged_reports[, data_columns, drop=F]),
                                            center = apply(dp2,1,stats::median),
                                            scale  = pmax(apply(dp2,1,stats::mad), min_scale)))
  
  merged_reports
}

filter_reports_to_taxRank <- function(my_reports, classification_taxRank) {
  lapply(my_reports, function(my_report) {
    
    ## subset report to the requested taxRank
    if (!is.null(classification_taxRank) && !(any(classification_taxRank == "-"))) {
      my_report[my_report$Rank == classification_taxRank, ]
    }
  })
}


#' Merge report files into a wide format, with column(s) for each
#' report.
#'
#' @param my_reports Report data.frames.
#' @param numeric_col Numeric columns to keep.
#'
#' @return Combined data.frame
#' @export
merge_reports <- function(my_reports, numeric_col = c("cladeReads","taxonReads")) {
  ## generate data.frame which has a name column (species name) and a further "cladeReads" column for each sample
  id_cols_before <- c("name", "taxRank")
  
  if (all(sapply(my_reports, function(x) "taxID" %in% colnames(x) ))) {
    id_cols_before <- c(id_cols_before, "taxID")
  }
  
  id_cols_after <- c("taxLineage")
  id_cols <- c(id_cols_before, id_cols_after)
  
  if (is.null(my_reports) || length(my_reports) == 0) {
    return(NULL)
  }
  idx_of_numeric_col <- seq(from = length(id_cols) + 1, to = length(id_cols) + length(numeric_col))
  
  my_reports <- lapply(names(my_reports), function(report_name) {
    
    my_report <- my_reports[[report_name]][ ,c(id_cols, numeric_col), drop=FALSE]
    my_report <- my_report[rowSums(my_report[, numeric_col, drop=F], na.rm=T)>0, , drop=FALSE]
    
    if (nrow(my_report) > 0) {
      ## set zeros to NA
      my_report[numeric_col][!is.na(my_report[numeric_col]) & my_report[numeric_col] == 0] <- NA
    }
    
    ## set the basename of the report file as name for the numeric column
    colnames(my_report)[idx_of_numeric_col] <- sub(".*/(.*).report", "\\1", report_name)
    if (length(numeric_col) > 1) {
      colnames(my_report)[idx_of_numeric_col] <-
        paste(colnames(my_report)[idx_of_numeric_col], numeric_col, sep = "\n")
    }
    my_report
  })
  
  idx_of_numeric_col_merged <- seq(from = length(id_cols) + 1, length.out = length(my_reports)*length(numeric_col))
  
  ## merge all the data.frames in the my_reports list, and add additional info (sparkline and mean)
  merged_reports <-
    Reduce(function(x, y)
      merge(x, y, all = TRUE, by = id_cols), my_reports)
  
  merged_reports <- cbind(
    beautify_colnames(merged_reports[, id_cols_before, drop = FALSE]),
    OVERVIEW = rep(NA, nrow(merged_reports)),
    STAT = rep(NA, nrow(merged_reports)),  ## Placeholder
    merged_reports[, idx_of_numeric_col_merged, drop = FALSE],
    beautify_colnames(merged_reports[, id_cols_after, drop = FALSE])
  )
  
  ## make a link to NCBI genome browser in the taxonID column
  if (!"Taxonid" %in% colnames(merged_reports)) {
    taxID_column <- NA
  } else {
    taxID_column <- which(colnames(merged_reports) == "Taxonid")
    stopifnot(length(taxID_column) == 1)
    merged_reports[, taxID_column] <- sub("^  *", "", merged_reports[, taxID_column])
  }
  ## remove s_, g_, etc
  merged_reports[, 1] <- sub("^[a-z-]_", "", merged_reports[, 1])
  
  for (col in numeric_col) {
    attr(merged_reports, paste0(col,"_columns")) <-
      seq(from = length(id_cols) + which(numeric_col == col),
          length.out = length(my_reports), by = length(numeric_col)) + 1
  }
  
  
  attr(merged_reports, REPORTATTR("data_column_start")) <- length(id_cols) + 2
  attr(merged_reports, REPORTATTR("data_columns")) <- idx_of_numeric_col_merged + 1
  attr(merged_reports, REPORTATTR("stat_column")) <- which(colnames(merged_reports) == "STAT")
  attr(merged_reports, REPORTATTR("taxID_column")) <- taxID_column
  
  class(merged_reports) <- append(class(merged_reports),"merged_reports")
  
  merged_reports
}

assayData <- function(x, ...) UseMethod("assayData", x)
assayData.merged_reports <- function(x)
  list("cladeReads"=x[, req_attr(x, REPORTATTR("reads_columns"))],
       "taxonReads"=x[, req_attr(x, REPORTATTR("taxonReads_columns"))])



#' Merge report files into a wide format, with column(s) for each
#' report.
#'
#' @param my_reports Report data.frames.
#' @param col_names Column names.
#' @param fix_taxnames Check if there are differences in the taxonomies of the reports.
#' @param update_progress Update shiny progress
#' @param id_cols Columns for taxdata
#' @param numeric_cols Numeric data
#'
#' @return Combined data.frame
#' @export
#' @examples
#' report_dir <- system.file("shinyapp", "example-data", "brain-biopsies", package="pavian")
#' reports <- read_reports(report_dir)
merge_reports2 <- function(my_reports, col_names = NULL, fix_taxnames = TRUE, update_progress = FALSE,
                           id_cols = c("name", "taxRank", "taxID", "taxLineage"),
                           numeric_cols = c("cladeReads","taxonReads")) {
  common_colnames <- Reduce(intersect, lapply(my_reports, colnames))
  
  if (is.null(my_reports) || length(my_reports) == 0)
    return(NULL)
  
  if (!"taxID" %in% common_colnames)
    id_cols <- c("name", "taxRank", "taxLineage")
  
  if (!all(c(id_cols,numeric_cols) %in% common_colnames)) {
    stop("Not all required columns are present Required: ",
         paste0(sort(c(id_cols,numeric_cols)), collapse=", "),". Present: ",
         paste0(sort(common_colnames), collapse=", "))
  }

  if (fix_taxnames && length(my_reports) > 1) {
    if (!"taxID" %in% common_colnames) {
      dmessage("Can't fix taxnames without taxID!")
    } else {
      c_id_to_name1 <- lapply(my_reports, function(r) {
        r[,c("taxID","name")]
      }) %>% do.call(rbind, .)
      rownames(c_id_to_name1) <- NULL
      c_id_to_name <- unique(c_id_to_name1[order(c_id_to_name1$taxID),])
      rownames(c_id_to_name) <- NULL
      sel_diff_taxid <- c_id_to_name$name %in% c_id_to_name$name[duplicated(c_id_to_name$name)]
      #if (any(sel_diff_taxid)) {
      #  dmessage("The following tax names have differing taxIDs:")
      #  print(c_id_to_name[sel_diff_taxid,])
      #}
      sel_diff_name <- c_id_to_name$taxID %in% c_id_to_name$taxID[duplicated(c_id_to_name$taxID)]
      if (any(sel_diff_name)) {
        dmessage("The following taxons have the same taxIDs but differing names:")
        print(c_id_to_name[sel_diff_name,])
      }
    }
  }
  
  my_reports <- lapply(seq_along(my_reports), function(i) {
    mm <- my_reports[[i]][,c(id_cols, numeric_cols)]
    colnames(mm)[colnames(mm) %in% numeric_cols] <- sprintf("%s.%s", colnames(mm)[colnames(mm) %in% numeric_cols], i)
    mm
  })
  
  if (length(my_reports) == 1) {
    merged_reports <- my_reports[[1]]
  } else {
    merged_reports <- Reduce(
      function(merged_rep, rep_index) {
        if (isTRUE(update_progress))
          shiny::setProgress(value = rep_index, detail = paste(length(my_reports)-rep_index," left."))
        dplyr::full_join(merged_rep, my_reports[[rep_index]], by = id_cols)
      }, 
      seq(from=2, to=length(my_reports)),
      init=my_reports[[1]]
      )
      
  }
  
  tax_data <- merged_reports[, id_cols, drop = FALSE]
  ## remove s_, g_, etc
  tax_data[, 1] <- sub("^[a-z-]_", "", tax_data[, 1])
  
  idx_cladeReads <- seq(from = length(id_cols) + 1, to = ncol(merged_reports), by = 2)
  idx_taxonReads <- seq(from = length(id_cols) + 2, to = ncol(merged_reports), by = 2)
  
  cladeReads <- as.matrix(merged_reports[, idx_cladeReads, drop = FALSE])
  taxonReads <- as.matrix(merged_reports[, idx_taxonReads, drop = FALSE])
  
  if (!is.null(col_names)) {
    stopifnot(length(col_names) == ncol(cladeReads))
    colnames(cladeReads) <- col_names
    colnames(taxonReads) <- col_names
  }
  
  list(tax_data = tax_data, cladeReads = cladeReads, taxonReads = taxonReads)
}


#' Filter taxon
#'
#' @param tax_data tax_data
#' @param rm_clades clades to remove
#' @param rm_taxa taxa to remove
#' @param taxRank taxRank to keep
#'
#' @return selected rows
#' @export
filter_taxa <- function(tax_data, rm_clades = NULL, rm_taxa = NULL, taxRank = "-") {
  stopifnot(all(c("name","taxLineage","taxRank") %in% colnames(tax_data)))
  
  sel_rm_taxa <- tax_data[["name"]] %in% rm_clades | tax_data[["name"]] %in% rm_taxa
  sel_rm_clades <- tax_data[["name"]] %in% rm_clades
  
  taxLineages <- tax_data[["taxLineage"]]
  if (sum(sel_rm_clades) > 0) {
    for (taxLineage in taxLineages[sel_rm_clades]) {
      sel_rm_taxa <- sel_rm_taxa | startsWith(taxLineages, taxLineage)
    }
  }
  
  my_shown_rows <- !sel_rm_taxa
  if (!is.null(taxRank) && isTRUE(taxRank != "-")) {
    if (!taxRank %in% tax_data[["taxRank"]]) {
      dmessage("taxRank ", taxRank, " not in tax_data")
    }
    my_shown_rows <- my_shown_rows & tax_data[["taxRank"]] %in% taxRank
  }
  
  my_shown_rows
}

##
##
#' Remove specified taxa or whole clades this function removes the read numbers from the parents, too
#'
#' @param cladeReads clade "cladeReads"
#' @param tax_data tax data
#' @param rm_taxa taxa to remove
#'
#' @return filtered clade "cladeReads"
#' @export
filter_cladeReads <- function(cladeReads, tax_data, rm_taxa) {
  stopifnot(all(c("name","taxLineage","taxRank") %in% colnames(tax_data)))
  taxLineages <- tax_data[["taxLineage"]]
  sel_rm_taxa <- tax_data[["name"]] %in% rm_taxa
  
  if (sum(sel_rm_taxa) > 0) {
    ## Update all the parent numbers
    for (contaminant_i in which(sel_rm_taxa)) {
      reads_clade <- cladeReads[contaminant_i, ,drop=FALSE]
      tax_string <- taxLineages[contaminant_i]
      update_indices <- taxLineages == tax_string
      repeat {
        new_tax_string <- sub("(.*)\\|.*", "\\1",tax_string)
        if (length(tax_string) == 0 || isTRUE(tax_string == new_tax_string)) break;
        update_indices <- update_indices | taxLineages == new_tax_string
        tax_string <- new_tax_string
      }
      cladeReads[update_indices, ] <- cladeReads[update_indices, ,drop=FALSE] - rep(reads_clade, each=sum(update_indices))
    }
  }
  cladeReads
}

#' Helper function to normalize data
#'
#' @param m matrix
#' @param sums column sums
#'
#' @return normalized matrix
#' @export
normalize <- function(m, sums = colSums(m, na.rm=T)) {
  sweep(m, 2, sums, FUN="/")
}

combine_df <- function(tax_data, reads) {
  
  #for (stat in input$opt_statistic) {
  #  str(apply("cladeReads", 1, stat_name_to_f[[stat]]))
  #summarized_report[[stat]] <- apply(mydata, 1, stat_name_to_f[[stat]])
  #}
  
  data.frame(Name=get_col(tax_data,"name"),
             TaxID=get_col(tax_data,"taxID"),
             reads,
             TaxLineage = get_col(tax_data(),"taxLineage"),
             stringsAsFactors = FALSE)
}

get_dt_container <- function(numericColumns, taxColumns, sampleNames, groupSampleColumns = TRUE) {
  numericColumns <- sub("Reads"," reads",numericColumns)
  numericColumns <- sub(" identity", "", numericColumns)
  if (length(numericColumns) > 1) {
    if (isTRUE(groupSampleColumns)) {
      groupedColumns <- sampleNames
      dataColumns <- numericColumns
    } else {
      groupedColumns <- numericColumns 
      dataColumns <- sampleNames
    }
    
    dt_container = htmltools::withTags(table(
      class = 'display',
      thead(
        tr( # top row
          lapply(taxColumns, function(x) th(style='border-bottom: 0px;')),
          lapply(groupedColumns, th, colspan = length(dataColumns), 
                 class="numeric_header1"),
          th(rowspan = 2, 'Lineage')
        ),
        tr( # bottom row
          lapply(taxColumns, th),
          rep(lapply(seq_along(dataColumns),
                     function(i) 
                       if (i==1) th(dataColumns[i], class="numeric_header2 border_left") 
                     else th(dataColumns[i], class="numeric_header2")),
              length(groupedColumns))
        )
      )
    ))
    
  } else {
    dt_container = dt_container = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          lapply(taxColumns, th),
          lapply(sampleNames, th, colspan = length(numericColumns)),
          th('Lineage')
        )
      )
    ))
  }
  dt_container
}

one_df <- function(cladeReads, taxonReads, tax_data, sample_data, 
                   numericColumns, statsColumns, sum_reads = NULL,
                   groupSampleColumns = FALSE, specific_tax_rank = FALSE,
                   min_scale_reads = 1, min_scale_percent = 0.001) {
 
   
  taxColumns <- colnames(tax_data)
  taxColumns <- taxColumns[taxColumns != "taxLineage"]
  taxColumns1 <- COLNAMES[taxColumns]
  sampleNames <- c(statsColumns, get_col(sample_data, "Name"))
  
  dt_container <- get_dt_container(numericColumns, taxColumns1, sampleNames, groupSampleColumns)
  #add_names_to_columns <- length(numericColumns) > 1
  numericColumns2 <- strsplit(numericColumns, " ")
  
  numeric_data <- lapply(numericColumns2, function(column) {
    if (isTRUE(column[1] == "cladeReads")) {       mydata <- data.frame(cladeReads)
    } else if (isTRUE(column[1] == "taxonReads")) {  mydata <- data.frame(taxonReads)
    #} else if (isTRUE(column[1] == "cladeKmers")) {  mydata <- data.frame(cladeKmers)
    } else { stop("column ", column, "??") }
    is_percent <- FALSE
    for (col in column[-1]) {
      if (col == "%") {
        is_percent <- TRUE
        
        if (is.null(sum_reads)) {
          normalize_by_colSums <- column[1] == "taxonReads" || specific_tax_rank
          if (normalize_by_colSums) {
            sum_reads <- colSums(mydata, na.rm=TRUE)
          } else {
            ## For clade reads (when all are displayed), normalize by the sum of all taxon reads
            sum_reads <- colSums(taxonReads, na.rm=TRUE)
          }
        }
        
        mydata <- signif(normalize(mydata,sum_reads),4) * 100
      } else if (col == "rank") {
        mydata.na <- is.na(mydata)
        mydata <- data.frame(apply(-mydata, 2, rank))
        mydata[mydata.na] <- NA
      } else if (col == "z-score") {
        min_scale <- ifelse(is_percent, min_scale_percent, min_scale_reads)
        m1 <- mydata
        m1[is.na(m1)] <- 0
        med1 <- apply(m1,1,stats::median)
        mad1 <- pmax(apply(m1,1,stats::mad), min_scale)
        mydata <- data.frame(signif(t(scale(t(mydata),
                                 center = med1,
                                 scale  = mad1)),4))
        
      } else if (col == "identity") {
        # do nothing
      } else {
        stop("Unknown columnd definition ",column[2],"?!")
      }
    }
    colnames(mydata) <- paste(colnames(mydata),paste(setdiff(column,"identity"),collapse=" "),sep="\n")
    mydata[mydata == 0] <- NA
    
    if (!is.null(statsColumns)) {
      stats <- lapply(statsColumns,function(stat) {
        if (length(column) > 1 && column[2] == "rank" && stat == "Max") {
          stat <- "Min"
        }
        apply(mydata, 1, function(x) {
          if (all(is.na(x))) {
            return(NA)
          } else {
            return(signif(stat_name_to_f[[stat]](x), 5))
          }
        })
      })
      names(stats) <- statsColumns
      mydata <- cbind(stats,mydata)
    }
    mydata
  })
  
  numeric_data1 <- do.call(data.frame, numeric_data)
  
  if (groupSampleColumns && length(numericColumns) > 1) {
    idx <- order(unlist( lapply(lapply(numeric_data,ncol), seq_len)))
    numeric_data1 <- numeric_data1[,idx]
  }
  
  summarized_report <- cbind(tax_data[, taxColumns], 
                             numeric_data1, 
                             lineage =beautify_taxLineage(get_col(tax_data,"taxLineage")))
  
  # filter empty rows
  sel <- apply(numeric_data1, 1, function(x) all(is.na(x) | x==0))
  list(summarized_report[!sel,, drop=FALSE], dt_container)
}

