calculate_opt_bin_val_along_axis <- function(x){
  h <- 2 * IQR(x) / length(x)^(1/3) # bin width
  return(h)
}

calculate_effective_number_of_bins <- function(data, var_tsne_1 = tSNE1, var_tsne_2 = tSNE2){

  bw1 <- calculate_opt_bin_val_along_axis(data %>%
                                            pull({{ var_tsne_1 }}))

  bw2 <- calculate_opt_bin_val_along_axis(data %>%
                                            pull({{ var_tsne_2 }}))

  diameter <- sqrt(bw1^2 + bw2^2)

  xbnds <- range(data %>%
                   pull({{ var_tsne_1 }}))

  num_bins <- round(diff(xbnds)/diameter, 0) ## This should be an integer
  num_bins

}

calculate_effective_shape_value <- function(data, var_tsne_1 = tSNE1, var_tsne_2 = tSNE2){
  xwidth <- diff(range(data %>%
                         pull({{ var_tsne_1 }})))
  yheight <- diff(range(data %>%
                          pull({{ var_tsne_2 }})))

  shape <- yheight/xwidth # Here, yheight is the range of y and xwidth is the renge of x
  shape

}
