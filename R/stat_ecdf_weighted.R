# This function is copied one-to-one from Nicolas Woloszko on github 
#     \url{https://github.com/NicolasWoloszko/stat_ecdf_weighted}.
# 
# The stat_ecdf function in the ggplot2 package does not allow to 
#     specify \code{weights}. However, we want to exploit the 
#     ggplot package to plot weighted ecdf's. Thus, we incorporate 
#     the workaround by Nicolas Woloszko, see 
#     \url{https://github.com/NicolasWoloszko/stat_ecdf_weighted}.

stat_ecdf <- function(mapping = NULL, data = NULL,
  geom = "step", position = "identity",
  weight =  NULL, 
  ...,
  n = NULL,
  pad = TRUE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatEcdf,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      n = n,
      pad = pad,
      na.rm = na.rm,
      weight = weight,
      ...
    )
  )
}


# #' @rdname ggplot2-ggproto
# #' @format NULL
# #' @usage NULL
# #' @export

StatEcdf <- ggplot2::ggproto("StatEcdf", ggplot2::Stat,
  compute_group = function(data, scales, weight, n = NULL, pad = TRUE) {
    # If n is NULL, use raw values; otherwise interpolate
    if (is.null(n)) {
      x <- unique(data$x)
    } else {
      x <- seq(min(data$x), max(data$x), length.out = n)
    }
    
    if (pad) {
      x <- c(-Inf, x, Inf)
    }
    y <- spatstat::ewcdf(data$x, weights=data$weight / sum(data$weight))(x)
    
    data.frame(x = x, y = y)
  },
  
  default_aes = ggplot2::aes(y = ggplot2::stat(y)),
  
  required_aes = c("x")
)