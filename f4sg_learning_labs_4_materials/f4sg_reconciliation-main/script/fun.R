# Fancy list of matrix print function
print_list_matrix <- function(list){
  sapply(names(list), function(x) setNames(dim(list[[x]]), c("row", "col")))
}

# Aggregated TS (future release on FoReco)
agg_ts <- function(agg_order, x, align = "end", rm_na = FALSE){
  if(is.ts(x)){
    tspx <- tsp(x)
    x <- as.matrix(x)
    isvec <- NCOL(x)==1
  }else{
    if(is.vector(x)){
      x <- cbind(x)
    }
    tspx <- NULL
  }
  
  align <- match.arg(align, c("end","start"))
  n <- NROW(x)
  agg_order <- as.integer(agg_order)
  
  if(align=='end'){
    start <- n%%agg_order + 1L
  }else{
    start <- 1L
  }
  
  nk <- trunc(n/agg_order)
  out <- apply(x, 2, function(col){
    tmp <- matrix(col[start - 1L + seq_len(agg_order*nk)], ncol=nk)
    colSums(tmp)
  }, simplify = FALSE)
  out <- do.call(cbind, out)
  
  if(align=='end' & n%%agg_order != 0){
    out <- rbind(NA, out)
  }else if(align=='start' & n%%agg_order != 0){
    out <- rbind(out, NA)
  }
  
  if(NCOL(out)==1){
    out <- out[,]
  }
  
  
  if(!is.null(tspx)){
    out <- ts(out, frequency=tspx[3]/agg_order,
              start=tspx[1]-1+tspx[3]/agg_order)
  }
  
  if(rm_na){
    out <- na.omit(out)
  }
  
  return(out)
}

# Plot K = {24, 8, 4, 1} agg ts
plot_agg_time <- function(x, title = "none"){
  rbind(tibble(value = x, 
               time = seq(from = as.POSIXct("2006-05-03 00:00:00", tz="UTC"), 
                          by = "hour", length.out = 24*14),
               k = 1),
        tibble(value = agg_ts(4, x), 
               time = seq(from = as.POSIXct("2006-05-03 00:00:00", tz="UTC"), 
                          by = "4 hour", length.out = 14*6),
               k = 4),
        tibble(value = agg_ts(8, x), 
               time = seq(from = as.POSIXct("2006-05-03 00:00:00", tz="UTC"), 
                          by = "8 hour", length.out = 14*3),
               k = 8),
        tibble(value = agg_ts(24, x), 
               time = seq(from = as.POSIXct("2006-05-03 00:00:00", tz="UTC"), 
                          by = "24 hour", length.out = 14),
               k = 24)) |>
    ggplot(aes(x = time, y = value)) +
    geom_line() +
    facet_grid(k~., scales = "free", labeller= function(x) label_both(x, sep = " = "))+
    scale_x_datetime(labels = scales::label_date("%d/%m"),
                     breaks = seq(as.POSIXct("2006-05-03 00:00:00", tz="UTC"), 
                                  by = "day", length.out = 15)) +
    labs(x = NULL, y = NULL, title = title) +
    theme_minimal() +
    theme(legend.title = element_blank())
}
