################################################################################
#############################CODES FOR EQUILIBI+RIA A COLORING###################
################################################################################


#### here is more generic with some facets (in case we want it)

## wew try the same bifurcation but with facets
plot_bif_sweep_facet <- function(full_sweep, facet_1, par_sw, xmax) {
  # Define variable ordering for faceting


  varOrder <- c("P" = 3, "Na" = 2, "Nl" = 1, "R" = 0)

  #wide to long format
  bifPlot <- full_sweep |>
    tidyr::pivot_longer(
      c(R, Nl, Na, P),
      names_to = "varName",
      values_to = "value"
    ) |>
    dplyr::mutate(
      orden = case_when(
        varName %in% names(varOrder) ~ varOrder[varName]
      )
    ) |>
    # ggplot(aes(x = !!sym(par_sw), y = value), na.rm=TRUE) + ## the !!sym is for make the parsw a symbol
    ggplot(aes(x = S, y = value)) + ## the !!sym is for make the parsw a symbol
    geom_line(
      aes(
        group = as.factor(interaction(type, direccion, minMax)),
        #shape = as.factor(type),
        #color = as.factor(direccion)
      ),
      size = 0.5
    ) +
    facet_grid(
      vars(fct_reorder(varName, orden, .desc = TRUE)),
      #vars(!!sym(facet_1)),
      vars(K),
      scales = "free"
    ) +
    #scale_y_discrete(breaks = full_sweep$value) +
    scale_shape_manual(values = c(19, 1)) +
    scale_color_viridis_d(
      option = "inferno",
      end = 0.8,
      direction = -1
    ) +
    theme_bw() +
    # ggtitle(label= "", subtitle = as.character(facet_1)) +
    #theme(plot.subtitle = element_text(hjust = 0.5))+
    theme(
      axis.text = element_text(size = 12), # Change tick labels for all axes
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      strip.text.y = element_blank()
    ) +
    labs(y = "", x = "IGP Symmetry (S)") # this depends on the sr parameter
  xlim(0, xmax)

  return(bifPlot)
}


add_eq_gray <- function(gg_plot, par_sw, reso = 1, only_coex = TRUE) {
   data <- gg_plot$data
  combPred <- unique(data$combPred)

 colRecList <-  list("LB.LB" = "darkgreen", "PB.LB" = "#ffff67ff","PB.PB" = "#00b61bff", "LB.PB"= "#18474dff", "LB.PA"= "gray")

colorRec <- colRecList[[combPred]]

  # I can change this if needed so, if I change the alpha, i can leave only the coexistence area..

  alpha_val <- c("0" = 0, "R" = 0, "RN" = 0, "RNP" = 0.4, "RP" = 0)

  if (only_coex == FALSE) {
    alpha_val <- c("0" = 1, "R" = 1, "RN" = 1, "RNP" = 1, "RP" = 1)
  }

  newPlot <- gg_plot +
    geom_rect(aes(
      xmin = !!sym(par_sw),
      xmax = !!sym(par_sw) + reso,
      ymin = 0,
      ymax = Inf,
      fill = EQ,
      alpha = EQ
    )) + ## que cada rectangulo use todo su valor

    scale_alpha_manual(values = alpha_val) +
    scale_fill_manual(
      values = colsEQ <- c(
        "0" = "white",
        "R" = "#e6e6e6",
        "RN" = "#b5b5b5",
        "RNP" = colorRec,
        "RP" = "gray"
      )
    ) +
   # geom_point(aes(shape = as.factor(type)), color = "black") +
    geom_line(aes(group = interaction(type, direccion, minMax)), color = "black") +

    theme(legend.position = "none")

  return(newPlot)
}
