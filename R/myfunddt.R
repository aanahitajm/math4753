#' @title myfunddt
#'
#' @param df Data frame that is read into the function.
#' @param Species Specify the species of fish you want to plot data for. Options are LMBASS, CCATFISH, SMBUFFALO
#' @importFrom dplyr '%>%' filter
#' @importFrom ggplot2 ggplot geom_point ggtitle aes geom_smooth
#' @importFrom utils write.csv
#' @return A graph for the ddt data frame. Plots Length vs Weight and adds a quadratic model.
#'
#' @export
#'
#' @examples
#'   \dontrun{myfunddt(df = ddt, Species  = "CCATFISH") }
myfunddt <- function(df, Species){
  df1 <- df %>% filter(SPECIES==Species)
  g <- ggplot(df1, aes(x=WEIGHT, y=LENGTH)) +
    geom_point(aes(color = RIVER)) +
    ggtitle("Aanahita Ervin") +
    geom_smooth(method = "lm", formula = y ~ poly(x, 2))

  freq = table(df$RIVER)/length(df$RIVER)
  print(freq)

  write.csv(df1, file=sprintf("LvsWfor%s.csv", Species))
  my_list <- list(DDTFull = df,
                  DDTSubset = df1,
                  RiverFreq = freq)
  print(my_list)

  g
}

globalVariables(c("SPECIES", "WEIGHT", "LENGTH", "RIVER"))
