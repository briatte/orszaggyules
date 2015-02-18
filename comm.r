# add committee co-memberships

load("data/net_hu.rda")
raw = data.frame()
sponsors = dir("raw", pattern = "^\\w{1}\\d{3}\\.html$", full.names = TRUE)

# find unique committees

cat("Parsing committees")
for(i in sponsors) {

  h = htmlParse(i)
  y = xpathSApply(h, "//div[@id='biz-tagsag']/table/tr/td[1]", xmlValue)
  n = xpathSApply(h, "//div[@id='biz-tagsag']/table/tr/td[2]//a", xmlValue)
  l = xpathSApply(h, "//div[@id='biz-tagsag']/table/tr/td[2]//a/@href")
  if(length(l)) {

    for(j in 1:length(y)) { # LOCF for legislatures
      if(y[ j ] == "")
        y[ j ] = k
      else
        k = y[ j ]
    }

    raw = rbind(raw, data.frame(i, y, n, l, stringsAsFactors = FALSE))

  }

}

raw = filter(raw, substr(y, 1, 4) >= 1998 & y != "2014-")

cat(":", nrow(unique(raw[, -1 ])), "unique categories\n")

# save flat list
write.csv(raw[, -1 ] %>%
            arrange(y, n, l) %>%
            group_by(y, n, l) %>%
            mutate(members = n()) %>%
            unique, "data/committees.csv", row.names = FALSE)

# unique legislature-committee pairings, using links
raw$u = paste(raw$y, raw$l)

comm = data.frame(u = unique(raw$u), stringsAsFactors = FALSE)

# add sponsor columns
for(i in sponsors)
  comm[, gsub("raw/|\\.html", "", i) ] = 0

raw$i = gsub("raw/|\\.html", "", raw$i)

for(i in colnames(comm)[ -1 ])
    comm[ , i ] = as.numeric(comm$u %in% raw$u[ raw$i == i ])

stopifnot(a$url %in% names(comm[, -1]))

# assign co-memberships to networks
for(i in ls(pattern = "^net_")) {

  n = get(i)
  cat(i, ":", network.size(n), "nodes")

  sp = network.vertex.names(n)
  names(sp) = n %v% "url"
  stopifnot(names(sp) %in% colnames(comm))

  m = comm[ substr(comm$u, 1, 4) == gsub("\\D", "", i), names(sp) ]

  # solve a few duplicated entries due to same-legislature party transitions
  if(any(grepl("\\.", colnames(m)))) {

    cat(" (solving duplicates)")
    z = data.frame(x = network.vertex.names(n), y = n %v% "url", stringsAsFactors = FALSE)
    z = group_by(z, y) %>% mutate(z = paste0(y, ".", 1:length(y) - 1))
    z$z = gsub("\\.0", "", z$z)
    sp = z$x
    names(sp) = z$z

  }

  cat(" :", nrow(m), "committees", ncol(m), "MPs")
  M = m

  m = t(as.matrix(m)) # sponsors in rows, committees in columns
  m = m %*% t(m) # adjacency matrix

  stopifnot(ncol(m) == network.size(n))
  colnames(m) = sp[ colnames(m) ]
  rownames(m) = sp[ rownames(m) ]

  e = data.frame(i = n %e% "source",
                 j = n %e% "target",
                 stringsAsFactors = FALSE)
  e$committee = NA

  for(j in 1:nrow(e))
    e$committee[ j ] = m[ e$i[ j ], e$j[ j ] ]

  cat(" co-memberships:",
      str_pad(paste0(range(e$committee), collapse = "-"), 6, "right"),
      sum(e$committee == 0), "null,",
      sum(e$committee == 1), "single,",
      sum(e$committee > 1), "> 1\n")

  nn = network(e[, 1:2], directed = FALSE)
  nn %e% "committee" = e$committee

  print(table(nn %e% "committee", exclude = NULL))
  stopifnot(!is.na(nn %e% "committee"))

  n %e% "committee" = e$committee
  assign(i, n)

  nn %n% "committees" = as.table(rowSums(M))
  assign(paste0("co", i), nn)

}

save(list = ls(pattern = "^((co)?net|edges|bills)_hu\\d{4}$"),
     file = "data/net_hu.rda")
