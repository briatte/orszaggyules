b$legislature[ b$legislature == "2014-" ] = "2014-2018"
a$legislature[ a$legislature == "2014-" ] = "2014-2018"

for (ii in unique(na.omit(b$legislature))) {

  cat(ii)
  data = subset(b, legislature == ii & n_au > 1)

  # subset sponsors (no duplicates)
  s = filter(a, legislature == ii)

  cat(":", nrow(data), "cosponsored documents, ")

  # check for missing sponsors
  u = unlist(strsplit(data$authors, ";"))
  u = na.omit(u[ !u %in% s$uid ])
  if (length(u)) {
    cat("Missing", length(u), "sponsors:")
    print(table(u))
  }

  # ============================================================================
  # DIRECTED EDGE LIST
  # ============================================================================

  edges = lapply(data$authors, function(d) {

    w = unlist(strsplit(d, ";"))
    w = w[ w %in% s$uid ] # remove a few missing Fidesz sponsors in 2014-

    if (length(w) > 0) {

      d = expand.grid(i = s$uid[ s$uid %in% w ],
                      j = s$uid[ s$uid == w[1]], stringsAsFactors = FALSE)

      return(data.frame(d, w = length(w) - 1)) # number of cosponsors

    }

  }) %>% bind_rows

  # convert uids to urls
  rownames(s) = s$uid
  edges$i = s[ edges$i, "url" ]
  edges$j = s[ edges$j, "url" ]

  # remove duplicates (different uid but all details -including url- identical)
  s = select(s, -uid) %>%
    unique

  # fix duplicated names
  s = group_by(s, name) %>%
    mutate(n = n(), o = 1:n()) %>%
    group_by %>%
    mutate(name = ifelse(n < 2, name, paste0(name, "-", o))) %>%
    data.frame

  # switch to names (note: MPs with different constituencies are distinct nodes)
  rownames(s) = s$url

  edges$i = s[ edges$i, "name" ]
  edges$j = s[ edges$j, "name" ]

  stopifnot(!duplicated(s$name))
  rownames(s) = s$name

  # ============================================================================
  # EDGE WEIGHTS
  # ============================================================================

  # first author self-loops, with counts of cosponsors
  self = subset(edges, i == j)

  # count number of bills per first author
  n_au = table(self$j)

  # remove self-loops from directed edge list
  edges = subset(edges, i != j)

  # count number of bills cosponsored per sponsor
  n_co = table(edges$i)

  # identify directed ties
  edges$ij = apply(edges[, 1:2 ], 1, paste0, collapse = "///")

  # raw edge counts
  raw = table(edges$ij)

  # Newman-Fowler weights (weighted quantity of bills cosponsored)
  edges = aggregate(w ~ ij, function(x) sum(1 / x), data = edges)

  # expand to edge list
  edges = data.frame(i = gsub("(.*)///(.*)", "\\1", edges$ij),
                     j = gsub("(.*)///(.*)", "\\2", edges$ij),
                     raw = as.vector(raw[ edges$ij ]), # raw edge counts
                     nfw = edges$w, stringsAsFactors = FALSE)

  # Gross-Shalizi weights (weighted propensity to cosponsor)
  edges = merge(edges, aggregate(w ~ j, function(x) sum(1 / x), data = self))
  edges$gsw = edges$nfw / edges$w

  # sanity check
  stopifnot(edges$gsw <= 1)

  # final edge set: cosponsor, first author, weights
  edges = select(edges, i, j, raw, nfw, gsw)

  cat(nrow(edges), "edges, ")

  # ============================================================================
  # DIRECTED NETWORK
  # ============================================================================

  n = network(edges[, 1:2 ], directed = TRUE)

  n %n% "country" = meta[ "cty" ] %>% as.character
  n %n% "lang" = meta[ "lang" ] %>% as.character
  n %n% "years" = ii
  n %n% "legislature" = NA
  n %n% "chamber" = meta[ "ch" ] %>% as.character
  n %n% "type" = meta[ "type" ] %>% as.character
  n %n% "ipu" = meta[ "ipu" ] %>% as.integer
  n %n% "seats" = meta[ "seats" ] %>% as.integer

  n %n% "n_cosponsored" = nrow(data)
  n %n% "n_sponsors" = table(subset(b, legislature == ii)$n_au)

  # ============================================================================
  # VERTEX-LEVEL ATTRIBUTES
  # ============================================================================

  n_au = as.vector(n_au[ network.vertex.names(n) ])

  n %v% "n_au" = ifelse(is.na(n_au), 0, n_au)

  n_co = as.vector(n_co[ network.vertex.names(n) ])
  n %v% "n_co" = ifelse(is.na(n_co), 0, n_co)

  n %v% "n_bills" = n %v% "n_au" + n %v% "n_co"

  cat(network.size(n), "nodes\n")

  n %v% "url" = s[ network.vertex.names(n), "url" ]
  n %v% "sex" = s[ network.vertex.names(n), "sex" ]
  n %v% "born" = s[ network.vertex.names(n), "born" ]
  n %v% "constituency" = s[ network.vertex.names(n), "constituency" ]
  n %v% "party" = s[ network.vertex.names(n), "party" ]
  n %v% "partyname" = groups[ n %v% "party" ] %>% as.character
  n %v% "lr" = scores[ n %v% "party" ] %>% as.numeric
  n %v% "photo" = s[ network.vertex.names(n), "photo" ]
  n %v% "nyears" = s[ network.vertex.names(n), "nyears" ]

  set.edge.attribute(n, "source", as.character(edges[, 1])) # cosponsor
  set.edge.attribute(n, "target", as.character(edges[, 2])) # first author

  set.edge.attribute(n, "raw", edges$raw) # raw edge counts
  set.edge.attribute(n, "nfw", edges$nfw) # Newman-Fowler weights
  set.edge.attribute(n, "gsw", edges$gsw) # Gross-Shalizi weights

  # ============================================================================
  # SAVE PLOTS
  # ============================================================================

  if (plot) {

    save_plot(n, paste0("plots/net_hu", ii),
              i = colors[ s[ n %e% "source", "party" ] ],
              j = colors[ s[ n %e% "target", "party" ] ],
              mode, colors)

  }

  # ============================================================================
  # SAVE OBJECTS
  # ============================================================================

  assign(paste0("net_hu", substr(ii, 1, 4)), n)
  assign(paste0("edges_hu", substr(ii, 1, 4)), edges)
  assign(paste0("bills_hu", substr(ii, 1, 4)), data)

  # ============================================================================
  # SAVE GEXF
  # ============================================================================

  if (gexf)
    save_gexf(n, paste0("net_hu", ii), mode, colors)

}

if (gexf)
  zip("net_hu.zip", dir(pattern = "^net_hu\\d{4}-\\d{4}\\.gexf$"))
