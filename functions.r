#' Wrapper around the write.gexf function
#' @seealso rgexf package, by George Vega Yon and Jorge Fabrega Lacoa
save_gexf <- function(n, name, mode, colors) {

  require(dplyr)
  require(rgexf)
  require(network)
  require(sna)

  # nodes
  n_ids = data_frame(id = network.vertex.names(n) %>% factor %>% as.integer,
                     label = network.vertex.names(n))

  # node attributes
  n_att = data_frame(url = gsub("&", "&amp;", n %v% "url"),
                     party = n %v% "partyname",
                     constituency = n %v% "constituency",
                     n_bills = n %v% "n_bills",
                     photo = n %v% "photo")

  # node colors
  n_col = t(col2rgb(colors[ names(colors) %in% as.character(n %v% "party") ]))
  n_col = lapply(n %v% "party", function(x)
    data_frame(r = n_col[ x, 1 ], g = n_col[ x, 2 ], b = n_col[ x, 3 ], a = .5))
  n_col = n_col %>% bind_rows %>% as.matrix

  # node size
  q = sna::degree(n)
  q = as.integer(cut(q, quantile(q) %>% unique, include.lowest = TRUE))

  # node placement
  n_xyz = network::as.matrix.network.adjacency(n)
  n_xyz = do.call(paste0("gplot.layout.", mode), list(n_xyz, NULL))
  n_xyz = as.matrix(cbind(round(n_xyz, 1), 1))
  colnames(n_xyz) = c("x", "y", "z")

  # edges
  e = data_frame(
    source = as.numeric(factor(n %e% "source", levels = levels(factor(n_ids$label)))),
    target = as.numeric(factor(n %e% "target", levels = levels(factor(n_ids$label)))),
    weight = n %e% "raw"
  )

  # all weights are positive
  stopifnot(all(e$weight > 0))

  # export .gexf
  write.gexf(nodes = n_ids, nodesAtt = n_att,
             edges = e[, 1:2 ], edgesWeight = e$weight,
             nodesVizAtt = list(position = n_xyz, color = n_col, size = q),
             defaultedgetype = "directed",
             meta = list(creator = "rgexf",
                         description = paste("legislative cosponsorship network,",
                                             mode, "placement,",
                                             n %n% "n_cosponsored", "bills"),
                         keywords = paste("parliament,", tolower(n %n% "country"))),
             output = paste0(name, ".gexf"))

}

#' Lighter version of the ggnet function, co-authored with Moritz Marbach
#' @seealso GGally package, by Barret Schloerke, for the full version
save_plot <- function(n, name, i, j, mode, colors) {

  require(ggplot2)
  require(grid)
  require(network)
  require(sna)

  print(table(n %v% "party", exclude = NULL))
  stopifnot(all(unique(n %v% "party") %in% names(colors)))

  # color same-party ties
  party = as.vector(i)
  party[ i != j ] = "#AAAAAA"

  # node placement
  m = network::as.matrix.network.adjacency(n)
  m = do.call(paste0("gplot.layout.", mode), list(m, NULL))

  # edge placement
  ij = network::as.matrix.network.edgelist(n)
  xy = data.frame(m[ij[, 1], ], m[ij[, 2], ])
  colnames(xy) = c("X1", "Y1", "X2", "Y2")

  # node size
  q = sna::degree(n)
  q = as.integer(cut(q, unique(quantile(q)), include.lowest = TRUE))

  g = ggplot(cbind(data.frame(m), q), aes(X1, X2)) +
    geom_segment(data = xy, aes(x = X1, y = Y1, xend = X2, yend = Y2),
                 size = 0.25, colour = party, alpha = 0.5) +
    geom_point(alpha = 1/3, aes(size = q, color = n %v% "party")) +
    geom_point(alpha = 1/2, aes(size = q / 2, color = n %v% "party")) +
    scale_color_manual("", values = colors, breaks = names(colors)) +
    scale_size_continuous(range = c(4, 12)) +
    scale_x_continuous(breaks = NULL) +
    scale_y_continuous(breaks = NULL) +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid = element_blank(), axis.title = element_blank(),
          legend.key = element_blank(),
          legend.key.size = unit(1, "cm"),
          legend.position = "right",
          legend.text = element_text(size = 16)) +
    guides(color = guide_legend(override.aes = list(alpha = 1/2, size = 6)),
           size = FALSE)

  print(g)

  ggsave(paste0(name, ".pdf"), g, width = 10, height = 9)

  ggsave(paste0(name, ".jpg"), g + theme(legend.position = "none"),
         width = 9, height = 9, dpi = 150)

}

#' Clean spaces
#' @seealso qdap package, by Tyler Rinker
str_space <- function(x) {

  gsub("\\s+", " ", x)

}

#' Clean commas, quotes and spaces
#' @seealso qdap package, by Tyler Rinker
str_clean <- function(x) {

  y = gsub("\\\\r|\\\\n|\\n|\\\\t", " ", x)

  y = str_space(y)
  y = str_space(str_trim(y))

  y = gsub("\"", "", y)
  y = gsub(" ,", ",", y)

  n = nchar(y)
  y = paste0(str_trim(substring(y, 1, n - 1)), substring(y, n))

  y[ is.na(x) ] = NA

  return(y)

}
