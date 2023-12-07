#####################################################################################################
##  Author: Eoin Ó Laighléis
##  
##  The function below performs primary and secondary suppression on a table with three columns: Rows,
##  Columns, and a third column containing frequency numbers. "Total" variables in Rows and Columns must be
##  removed before entering into this function. Primary suppression is performed on cells with fewer
##  than "min_unsuppressed" elements. Variables to avoid primary suppressing can be included in a
##  character vector as an input.
##  
##  Function options:
##  - unsuppressed_data: a data-frame where the first two columns are the columns to perform suppression
##      over. Optionally, the third column should contain numerical values if unsuppressed_data isn't microdata
##  - is_microdata: Boolean variable indicating whether unsuppressed_data is microdata or not.
##  - min_unsuppressed: The minimum number that will not get suppressed by primary suppression.
##  - variables_avoiding_prim_supp: A vector of strings for variables to avoid primary suppression. For example,
##      a selection of "Unknown" or "Prefer not to say" is typically considered non-sensitive, so these options
##      may be included here. If NULL or empty, all variables will undergo primary suppression.
##  - variables_avoiding_sec_supp: A vector of strings for variables to avoid secondary suppression. For example,
##      a selection of "Unknown" or "Prefer not to say" is typically considered non-sensitive, so these options
##      may be included here. If NULL or empty, all variables will be eligible for secondary suppression.
##  - variables_to_prefer_supp: A vector of strings containing any variables to prefer suppressing when
##      performing secondary suppression. These are typically chosen to reduce loss of information, e.g.
##      suppressing "Prefer not to say" or "Unknown".
##  - sec_supp: A string identifying which direction secondary suppression occurs over. Default is "both",
##      which performs secondary suppression across columns and rows. Other options are "column" or "row",
##      performing suppression over the variables in Rows or Columns respectively only (i.e. keeping
##      suppression to within a single column or row respectively).
##  - suppress_zeroes: A logical variable, defaults to FALSE. If TRUE, zeroes are suppressed in addition
##      to everything usually suppressed.
##  
##  Edited in Sept 2021 to alter suppression method used (to OPT from HYPERCUBE) after over-suppression
##  identified and to add extra suppression when total of suppressed cells is less than min_unsuppressed.
##  
##  Edited in Nov 2021 to add suppression when either Rows or Columns has a single variable in it and to 
##  add a summarise step when microdata is supplied.
##  
##  Edited in Jan 2022 to add option for suppression across rows or columns only
##  
######################################################################################################

suppress_table <- function(unsuppressed_data, is_microdata = FALSE, min_unsuppressed = 5,
                           variables_avoiding_prim_supp = NULL,
                           variables_avoiding_sec_supp = NULL,
                           variables_to_prefer_supp = NULL,
                           sec_supp = "both", suppress_zeroes = FALSE) {
  
## If variables_avoiding_sec_supp and variables_to_prefer_supp share values, remove those values from the latter 
  if (length(intersect(variables_avoiding_sec_supp, variables_to_prefer_supp))!=0) {
    warning(str_c("'variables_avoiding_sec_supp' and 'variables_to_prefer_supp' share one or more variables. ",
                  "These variables will avoid suppression in the following."))
    variables_to_prefer_supp <- variables_to_prefer_supp[!variables_to_prefer_supp %in% variables_avoiding_sec_supp]
  }
  
## Rename the column headings -----------------------------------------------
  col_names <- colnames(unsuppressed_data)
  
  unsuppressed_data <- unsuppressed_data %>%
    rename(Rows = 1, Columns = 2) %>%
    mutate(Rows = as.character(Rows), Columns = as.character(Columns)) %>%
    arrange(Rows,Columns)
  
## If microdata is supplied, summarise the data first ----------------------
  if (is_microdata) {
    unsuppressed_data <- unsuppressed_data %>%
      group_by(Rows,Columns) %>%
      summarise(N = n()) %>%
      ungroup() %>%
      complete(Rows, Columns, fill=list(N=0)) %>%
      mutate(N = as.double(N))
  } else {
    unsuppressed_data <- unsuppressed_data %>%
      rename(N = 3) %>%
      complete(Rows, Columns, fill=list(N=0)) %>%
      mutate(N = as.double(as.character(N)))
  }
  
## If incorrect mode of secondary suppression chosen, return early ----------------------------
  if (!sec_supp %in% c("both","column","row")) {
    print(str_c("Error: Invalid type of secondary suppression (",sec_supp,") detected. ",
                "Valid types are 'column', 'row' or 'both'."))
    return(NA)
    
## Secondary suppression across columns or rows only ----------------------------
  } else if (sec_supp %in% c("column","row")) {
    ## Complete the set of variables
    unsuppressed_data <- unsuppressed_data %>%
      complete(Rows, Columns, fill = list(N=0))
    
    ## Primary suppression
    suppressed_data <- unsuppressed_data %>%
      add_row(unsuppressed_data %>%
                group_by(Rows) %>%
                summarise(N = sum(N, na.rm = TRUE)) %>%
                mutate(Columns = "Total"), .before=0)
    
    suppressed_data <- suppressed_data %>%
      group_by(Columns) %>%
      summarise(N = sum(N, na.rm = TRUE)) %>%
      transmute(Rows="Total", Columns, N) %>%
      add_row(suppressed_data) %>%
      mutate(N = as.double(N),
             N = case_when(Rows %in% variables_avoiding_prim_supp ~ N,
                           Columns %in% variables_avoiding_prim_supp ~ N,
                           N == 0 ~ case_when(suppress_zeroes ~ NA_real_, TRUE ~ N),
                           N < min_unsuppressed ~ NA_real_,
                           TRUE ~ N))
    
    ## Perform secondary suppression if something is suppressed
    if (sum(is.na(suppressed_data$N))!=0) {
      sec_supp_var <- ifelse(sec_supp == "column", "Rows", "Columns")
      grouping_var <- ifelse(sec_supp == "column", "Columns", "Rows")
      
      for (Var in variables_to_prefer_supp) {
        suppressed_data <- suppressed_data %>%
          group_by(.dots = grouping_var) %>%
          mutate(N = case_when(sum(is.na(N)) == 0 | is.na(N) ~ N,
                               sum(case_when(!!as.symbol(sec_supp_var)=="Total"~N,TRUE~0), na.rm = TRUE) -
                                 sum(case_when(!!as.symbol(sec_supp_var)!="Total"~N,TRUE~0), na.rm = TRUE) >=
                                 min_unsuppressed ~ N,
                               !!as.symbol(grouping_var) %in% variables_avoiding_prim_supp ~ N,
                               N == 0 ~ case_when(suppress_zeroes ~ NA_real_, TRUE ~ N),
                               !!as.symbol(sec_supp_var) == Var ~ NA_real_,
                               TRUE ~ N)) %>%
          ungroup()
      }
      
      ## Check if further suppression needed
      suppressed_data <- suppressed_data %>%
        group_by(.dots = grouping_var) %>%
        mutate(Total = ifelse(!!as.symbol(sec_supp_var)=="Total", N, NA),
               Finished = (2*max(Total, na.rm = TRUE)==sum(N, na.rm=TRUE)) |
                 is.infinite(max(Total, na.rm = TRUE)) | 
                 (2*max(Total, na.rm = TRUE)-sum(N, na.rm=TRUE) >= min_unsuppressed)) %>%
        ungroup()
      
      ## While further suppression needed, continue secondary suppression
      iter <- 0
      while (sum(!suppressed_data$Finished, na.rm = TRUE)>0) {
        suppressed_data <- suppressed_data %>%
          group_by(.dots = grouping_var) %>%
          mutate(N = case_when(sum(is.na(N)) == 0 | is.na(N) ~ N,
                               sum(ifelse(!!as.symbol(sec_supp_var)=="Total",N,0), na.rm = TRUE) -
                                 sum(ifelse(!!as.symbol(sec_supp_var)!="Total",N,0), na.rm = TRUE) >=
                                 min_unsuppressed ~ N,
                               !!as.symbol(grouping_var) %in% variables_avoiding_prim_supp ~ N,
                               !!as.symbol(grouping_var) %in% variables_avoiding_sec_supp ~ N,
                               !!as.symbol(sec_supp_var) %in% variables_avoiding_sec_supp ~ N,
                               N==0 ~ case_when(suppress_zeroes~NA_real_,TRUE~N),
                               N == min(case_when(!!as.symbol(grouping_var) %in% variables_avoiding_prim_supp ~
                                                    NA_real_,
                                                  !!as.symbol(grouping_var) %in% variables_avoiding_sec_supp ~
                                                    NA_real_,
                                                  !!as.symbol(sec_supp_var) %in% variables_avoiding_sec_supp ~
                                                    NA_real_,
                                                  N>0 ~ N,
                                                  TRUE ~ NA_real_),
                                        na.rm=TRUE) ~ NA_real_,
                               TRUE ~ N),
                 Total = ifelse(!!as.symbol(sec_supp_var)=="Total", N, NA),
                 Finished = (2*max(Total, na.rm = TRUE)==sum(N, na.rm=TRUE)) |
                   is.infinite(max(Total, na.rm = TRUE)) | 
                   (2*max(Total, na.rm = TRUE)-sum(N, na.rm=TRUE) >= min_unsuppressed)) %>%
          ungroup()
        
        if (iter > (length(unique(unsuppressed_data$Rows))+1)*(length(unique(unsuppressed_data$Columns))+1)) {
          print(str_c("Error: tertiary suppression continued beyond the number of boxes to suppress. ",
                      "Suppression may not be complete. Consider finishing manually"))
          break
        } else {
          iter <- iter+1
        }
      }
      suppressed_data <- suppressed_data %>%
        select(-c(Total,Finished))
    }
  }
  
## If one of the groups has only one variable, we perform secondary suppression ---------
## in one dimension only
  if (sec_supp == "both" & min(length(unique(unsuppressed_data$Rows)),
                               length(unique(unsuppressed_data$Columns)))==1) {
    
    ## Primary suppression
    unsuppressed_data <- unsuppressed_data %>%
      add_row(unsuppressed_data %>%
                group_by(Rows) %>%
                summarise(N = sum(N, na.rm = TRUE)) %>%
                mutate(Columns = "Total"), .before=0)
    
    suppressed_data <- unsuppressed_data %>%
      add_row(unsuppressed_data %>%
                group_by(Columns) %>%
                summarise(N = sum(N, na.rm = TRUE)) %>%
                mutate(Rows = "Total"), .before=0) %>%
      mutate(N = as.double(N),
             N = case_when(Rows %in% variables_avoiding_prim_supp ~ N,
                           Columns %in% variables_avoiding_prim_supp ~ N,
                           N == 0 ~ case_when(suppress_zeroes ~ NA_real_, TRUE ~ N),
                           N < min_unsuppressed ~ NA_real_,
                           TRUE ~ N))
    
    ## Perform secondary suppression if at least one cell suppressed
    if (sum(is.na(suppressed_data$N))!=0) {
      ## Identify the group with only one variable
      sec_supp_var <- ifelse(length(unique(unsuppressed_data$Rows)) < length(unique(unsuppressed_data$Columns)),
                             "Columns", "Rows")
      grouping_var <- ifelse(length(unique(unsuppressed_data$Rows)) < length(unique(unsuppressed_data$Columns)),
                             "Rows", "Columns")
      
      ## Perform secondary suppression
      for (Var in variables_to_prefer_supp) {
        suppressed_data <- suppressed_data %>%
          mutate(N = case_when(sum(is.na(N)) == 0 |is.na(N) ~ N,
                               2*sum(case_when(!!as.symbol(sec_supp_var)=="Total"~N,
                                               TRUE~0)) - sum(N,na.rm=TRUE) >=
                                 min_unsuppressed ~ N,
                               N == 0 ~ case_when(suppress_zeroes ~ NA_real_, TRUE ~ N),
                               Rows == Var ~ NA_real_,
                               Columns == Var ~ NA_real_,
                               TRUE ~ N))
      }
      
      ## Check if further suppression needed
      suppressed_data <- suppressed_data %>%
        mutate(Total = ifelse(!!as.symbol(sec_supp_var)=="Total", N, NA),
               Finished = (2*max(Total, na.rm = TRUE)==sum(N, na.rm=TRUE)) |
                 is.infinite(max(Total, na.rm = TRUE)) | 
                 (2*max(Total, na.rm = TRUE)-sum(N, na.rm=TRUE) >= min_unsuppressed))
      
      ## While further suppression needed, continue secondary suppression
      iter <- 0
      while (sum(!suppressed_data$Finished, na.rm = TRUE)>0) {
        suppressed_data <- suppressed_data %>%
          group_by(.dots=grouping_var) %>%
          mutate(N = case_when(sum(is.na(N)) == 0 | is.na(N) ~ N,
                               sum(case_when(!!as.symbol(sec_supp_var)=="Total"~N,TRUE~0), na.rm = TRUE) -
                                 sum(case_when(!!as.symbol(sec_supp_var)!="Total"~N,TRUE~0), na.rm = TRUE) >=
                                 min_unsuppressed ~ N,
                               !!as.symbol(grouping_var) %in% variables_avoiding_prim_supp ~ N,
                               !!as.symbol(grouping_var) %in% variables_avoiding_sec_supp ~ N,
                               !!as.symbol(sec_supp_var) %in% variables_avoiding_sec_supp ~ N,
                               N==0 ~ case_when(suppress_zeroes ~ NA_real_, TRUE ~ N),
                               N == min(case_when(!!as.symbol(grouping_var) %in% variables_avoiding_prim_supp ~
                                                    NA_real_,
                                                  !!as.symbol(grouping_var) %in% variables_avoiding_sec_supp ~
                                                    NA_real_,
                                                  !!as.symbol(sec_supp_var) %in% variables_avoiding_sec_supp ~
                                                    NA_real_,
                                                  N>0 ~ N,
                                                  TRUE ~ NA_real_),
                                        na.rm=TRUE) ~ NA_real_,
                               TRUE ~ N),
                 Total = ifelse(!!as.symbol(sec_supp_var)=="Total", N, NA),
                 Finished = (2*max(Total, na.rm = TRUE)==sum(N, na.rm=TRUE)) |
                   is.infinite(max(Total, na.rm = TRUE)) | 
                   (2*max(Total, na.rm = TRUE)-sum(N, na.rm=TRUE) >= min_unsuppressed)) %>%
          ungroup()
        
        if (iter > (length(unique(unsuppressed_data$Rows))+1)*(length(unique(unsuppressed_data$Columns))+1)) {
          print(str_c("Error: tertiary suppression continued beyond the number of boxes to suppress. ",
                      "Suppression may not be complete. Consider finishing manually"))
          break
        } else {
          iter <- iter+1
        }
      }
      suppressed_data <- suppressed_data %>%
        select(-c(Total,Finished))
    }
  } else if (sec_supp == "both") {
## If secondary suppression over both rows and columns desired, run the code below ------------
    ## Create dimension lists
    dim1 <- hier_create(root = "Total", nodes = unsuppressed_data[[1]] %>% unique())
    dim2 <- hier_create(root = "Total", nodes = unsuppressed_data[[2]] %>% unique())
    
    ## Create an sdcProblem and perform primary suppression
    prim_supp <- unsuppressed_data %>%
      makeProblem(dimList = list(Rows = dim1, Columns = dim2), freqVarInd = 3) %>%
      primarySuppression(type = "freq", maxN = min_unsuppressed-1)
    
    ## Perform secondary suppression - first pass
    sec_supp <- prim_supp %>%
      protectTable("OPT", timeLimit=1) %>%
      getInfo(type = "finalData") %>%
      mutate(N = ifelse(sdcStatus %in% c("s","z"), Freq, NA))
    
    ## Modify the secondary suppression to re-insert zeroes and remove primary suppression on
    ## variables in "variables_avoiding_prim_supp". Then, protect variables in "variables_avoiding_sec_supp"
    ## from secondary suppression
    modified_prim_supp <- sec_supp %>%
      mutate(N = Freq,
             sdcStatus = case_when(sdcStatus=="x" ~ "s",
                                   Rows %in% variables_avoiding_prim_supp ~ "s",
                                   Columns %in% variables_avoiding_prim_supp ~ "s",
                                   N==0 ~ ifelse(suppress_zeroes,"u","z"),
                                   N > 0 ~ sdcStatus,
                                   TRUE ~ sdcStatus),
             sdcStatus = case_when(Rows %in% variables_avoiding_sec_supp & sdcStatus!="u" ~ "z",
                                   Columns %in% variables_avoiding_sec_supp & sdcStatus!="u" ~ "z",
                                   TRUE ~ sdcStatus))
    
    ## Apply the modified primary suppression status
    prim_supp <- prim_supp %>%
      setInfo(type = "sdcStatus", index = 1:nrow(modified_prim_supp),
              input = modified_prim_supp[["sdcStatus"]])
    
    ## Perform secondary suppression and output data in the same format as the input, including Totals
    sec_supp <- prim_supp %>%
      protectTable("OPT", timeLimit=1) %>%
      getInfo(type = "finalData") %>%
      mutate(N = ifelse(sdcStatus %in% c("s","z"), Freq, NA))
    
    ## Check if sum of suppressed cells in column or row add to less than min_unsuppressed
    ## (may need further suppression). Also add protection against further suppression
    check_supp <- sec_supp %>%
      mutate(sdcStatus = case_when(N==0 ~ ifelse(suppress_zeroes,"u","z"),
                                   Rows %in% variables_avoiding_sec_supp ~ "z",
                                   Columns %in% variables_avoiding_sec_supp ~ "z",
                                   TRUE ~ sdcStatus)) %>%
      group_by(Rows) %>%
      mutate(Total1 = ifelse(Columns=="Total", N, NA),
             Finished1 = (2*max(Total1, na.rm = TRUE)==sum(N, na.rm=TRUE)) |
               is.infinite(max(Total1, na.rm = TRUE)) | 
               (2*max(Total1, na.rm = TRUE)-sum(N, na.rm=TRUE) >= min_unsuppressed)) %>%
      group_by(Columns) %>%
      mutate(Total2 = ifelse(Rows=="Total", N, NA),
             Finished2 = (2*max(Total2, na.rm = TRUE)==sum(N, na.rm=TRUE)) |
               is.infinite(max(Total2, na.rm = TRUE)) |
               (2*max(Total2, na.rm = TRUE)-sum(N, na.rm=TRUE) >= min_unsuppressed)) %>%
      ungroup()
    
    ## If no further suppression needed, return result of secondary suppression
    if (!sum(!check_supp$Finished1, na.rm = TRUE) & !sum(!check_supp$Finished2, na.rm = TRUE)) {
      suppressed_data <- sec_supp %>%
        select(Rows,Columns,N)
    } else {
      
      ## Otherwise, perform further suppression
      for (var in variables_to_prefer_supp) {
        check_supp <- check_supp %>%
          mutate(sdcStatus = case_when(sdcStatus != "s" | (Finished1 & Finished2) ~ sdcStatus,
                                       !Finished1 & Columns==var ~ "u",
                                       !Finished2 & Rows==var ~ "u",
                                       TRUE ~ sdcStatus),
                 N = ifelse(sdcStatus =="u", NA, N),
                 Total1 = ifelse(Columns=="Total", N, NA),
                 Total2 = ifelse(Rows=="Total", N, NA)) %>%
          group_by(Rows) %>%
          mutate(Finished1 = (2*max(Total1, na.rm = TRUE)==sum(N, na.rm=TRUE)) |
                   is.infinite(max(Total1, na.rm = TRUE)) | 
                   (2*max(Total1, na.rm = TRUE)-sum(N, na.rm=TRUE) >= min_unsuppressed)) %>%
          group_by(Columns) %>%
          mutate(Finished2 = (2*max(Total2, na.rm = TRUE)==sum(N, na.rm=TRUE)) |
                   is.infinite(max(Total2, na.rm = TRUE)) |
                   (2*max(Total2, na.rm = TRUE)-sum(N, na.rm=TRUE) >= min_unsuppressed)) %>%
          ungroup()
      }
      
      ## While further suppression needed, continue secondary suppression
      iter <- 0
      while (sum(!check_supp$Finished1, na.rm = TRUE)>0 | sum(!check_supp$Finished2, na.rm = TRUE)>0) {
        check_supp <- check_supp %>%
          group_by(Rows) %>%
          mutate(sdcStatus = case_when(sdcStatus != "s" ~ sdcStatus,
                                       !Finished1 & N == min(case_when(Columns %in% variables_avoiding_sec_supp ~
                                                                         NA_real_,
                                                                       N>0 ~ N,
                                                                       TRUE ~ NA_real_),
                                                             na.rm = TRUE) ~ "u",
                                       TRUE ~ sdcStatus),
                 N = ifelse(sdcStatus=="u", NA, N),
                 Finished1 = (2*max(Total1, na.rm = TRUE)==sum(N, na.rm=TRUE)) |
                   is.infinite(max(Total1, na.rm = TRUE)) |
                   (2*max(Total1, na.rm = TRUE)-sum(N, na.rm=TRUE) >= min_unsuppressed)) %>%
          group_by(Columns) %>%
          mutate(sdcStatus = case_when(sdcStatus != "s" ~ sdcStatus,
                                       !Finished2 & N == min(case_when(Rows %in% variables_avoiding_sec_supp ~
                                                                         NA_real_,
                                                                       N>0 ~ N,
                                                                       TRUE ~ NA_real_),
                                                             na.rm = TRUE) ~ "u",
                                       TRUE ~ sdcStatus),
                 N = ifelse(sdcStatus=="u", NA, N),
                 Finished2 = (2*max(Total2, na.rm = TRUE)==sum(N, na.rm=TRUE)) |
                   is.infinite(max(Total2, na.rm = TRUE)) |
                   (2*max(Total2, na.rm = TRUE)-sum(N, na.rm=TRUE) >= min_unsuppressed)) %>%
          ungroup()
        
        if (iter > length(unique(check_supp$Rows))*length(unique(check_supp$Columns))) {
          print(str_c("Error: tertiary suppression continued beyond the number of boxes to suppress. ",
                      "Suppression may not be complete. Consider finishing manually"))
          break
        } else {
          iter <- iter+1
        }
      }
      
      ## Perform final secondary suppression and output data in the same format as the input, including Totals
      suppressed_data <- prim_supp %>%
        setInfo(type = "sdcStatus", index = 1:nrow(check_supp), input = check_supp[["sdcStatus"]]) %>%
        protectTable("OPT", timeLimit=1) %>%
        getInfo(type = "finalData") %>%
        mutate(N = ifelse(sdcStatus %in% c("s","z"), Freq, NA)) %>%
        select(Rows,Columns,N)
    }
  }
  
  ## Rename the output data column headings to match that of the input, then output it -------------------
  colnames(suppressed_data) <- c(col_names[1],col_names[2],"N")
  
  suppressed_data
}