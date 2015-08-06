bills = "data/bills.csv"
sponsors = "data/sponsors.csv"

# scrape bills (semi-manual: the index files require to be manually downloaded)

if (!file.exists(bills) | !file.exists(sponsors)) {

  a = data_frame()
  b = data_frame()

  for (i in rev(list.files("raw/bill-lists", full.names = TRUE))) {

    legislature = gsub("raw/bill-lists/bills|\\.html", "", i)
    h = htmlParse(i, encoding = "UTF-8")

    url = xpathSApply(h, "//div[@class='irom-cim']//a[1]/@href")
    ref = xpathSApply(h, "//div[@class='irom-cim']//a[1]", xmlValue)
    title = xpathSApply(h, "//div[@class='irom-cim']/table/tbody/tr/td[2]", xmlValue)
    status = xpathSApply(h, "//div[@class='irom-adat']//table/tbody/tr[2]/td[2]", xmlValue)
    authors = xpathSApply(h, "//div[@class='irom-adat']//table/tbody/tr/td[text()='Benyújtó(k)']/following-sibling::td", xmlValue)

    b = rbind(b, data.frame(legislature, url, ref, title, status, authors, stringsAsFactors = FALSE))
    cat(i, ":", length(ref), "bills\n")

    url = xpathSApply(h, "//div[@class='irom-adat']//table/tbody/tr[3]/td[2]//a/@href")
    uid = xpathSApply(h, "//div[@class='irom-adat']//table/tbody/tr[3]/td[2]//a", xmlValue)
    a = rbind(a, data.frame(legislature, url, uid, stringsAsFactors = FALSE))

  }

  # remove dates in sponsor lists
  b$authors = gsub("\\d{4}\\.\\d{2}\\.\\d{2}\\.:", ", ", b$authors)
  b$authors[ grepl("^kormány", b$authors) ] = "GOV"
  write.csv(b, bills, row.names = FALSE)

}

b = read.csv(bills, stringsAsFactors = FALSE)
b$authors = gsub(",\\s?", ";", b$authors)
b$n_au = 1 + str_count(b$authors, ";")

# scrape sponsors

if (!file.exists(sponsors)) {

  # extract URLs
  a = unique(a)
  a$url = gsub("(.*)p_azon%3D", "", a$url)
  a = subset(a, !grepl("http", url))

  # rerun to solve network errors
  u = unique(a$url)
  s = data_frame()

  for (i in rev(u)) {

    # cat("Downloading MP", sprintf("%4.0f", which(u == i)))
    file = paste0("raw/mp-pages/mp-", i, ".html")

    if (!file.exists(file))
      download.file(paste0("http://www.parlament.hu/internet/cplsql/ogy_kpv.kepv_adat?p_azon=", i),
                    file, mode = "wb", quiet = TRUE)

    if (!file.info(file)$size) {

      # cat(": failed\n")
      file.remove(file)

    } else {

      h = htmlParse(file, encoding = "UTF-8")

      name = xpathSApply(h, "//h1", xmlValue)

      legislature = xpathSApply(h, "//div[@id='kepvcsop-tagsag']/table/tr/td[1]", xmlValue)
      # legislature = legislature[ legislature != "" ]

      # last legislature carried forward
      while(any(legislature == ""))
        legislature[ which(legislature == "") ] = legislature[ which(legislature == "") - 1 ]

      # start of mandate
      start = xpathSApply(h, "//div[@id='kepvcsop-tagsag']/table/tr/td[3]", xmlValue)

      # end of mandate
      end = xpathSApply(h, "//div[@id='kepvcsop-tagsag']/table/tr/td[4]", xmlValue)

      # seniority
      end[ grepl("\\s", end) ] = Sys.Date()
      nyears = apply(cbind(start, end), 1, function(x) {
        x = as.numeric(str_extract(x, "[0-9]{4}"))
        seq(x[1], x[2])
      })
      nyears = unique(as.vector(unlist(nyears)))

      photo = xpathSApply(h, "//img[contains(@alt, 'fényképe')]/@src")

      s = rbind(s, full_join(
        data_frame(
          url = i, name, legislature,
          party = xpathSApply(h, "//div[@id='kepvcsop-tagsag']/table/tr/td[2]", xmlValue),
          t = strptime(end, "%Y.%m.%d.") - strptime(start, "%Y.%m.%d."),
          photo = ifelse(is.null(photo), NA, photo),
          nyears = sapply(substr(legislature, 1, 4), function(x) sum(nyears < x))
        ),
        data_frame(
          legislature = xpathSApply(h, "//div[@id='valasztas']/table/tr/td[1]", xmlValue),
          constituency = xpathSApply(h, "//div[@id='valasztas']/table/tr/td[3]", xmlValue)
        ),
        by = "legislature"
      ))

    }

  }

  s$t[ is.na(s$t) ] = Sys.Date() - as.Date("2014-01-01")

  # select longest party affiliations
  s = group_by(s, legislature, url) %>%
    mutate(max = max(t %>% as.numeric)) %>%
    filter(t == max) %>%
    select(-t, -max) %>%
    unique

  # ==============================================================================
  # CHECK CONSTITUENCIES
  # ==============================================================================

  s$constituency = gsub("(.*)\\smegye(.*)", "\\1 megye", s$constituency)
  s$constituency[ grepl("Budapest", s$constituency) ] = "Budapest"
  s$constituency[ s$constituency == "Országos lista" ] = "Magyarország" # national
  s$constituency = gsub("\\s", "_", s$constituency)
  table(s$constituency, exclude = NULL)

  cat("Checking constituencies,", sum(is.na(s$constituency)), "missing...\n")
  for (i in na.omit(unique(s$constituency))) {

    g = GET(paste0("https://", meta[ "lang"], ".wikipedia.org/wiki/", i))

    if (status_code(g) != 200)
      cat("Missing Wikipedia entry:", i, "\n")

    g = xpathSApply(htmlParse(g), "//title", xmlValue)
    g = gsub("(.*) – Wikipédia(.*)", "\\1", g)

    if (gsub("\\s", "_", g) != i)
      cat("Discrepancy:", g, "(WP) !=", i ,"(data)\n")

  }

  stopifnot(paste(a$url, a$legislature) %in% paste(s$url, s$legislature))
  a = left_join(a, s, by = c("url", "legislature"))

  a$photo[ !grepl("jpg$", a$photo) ] = NA
  table(is.na(a$photo))

  u = na.omit(unique(a$photo))
  for (i in rev(u)) {

    # cat("Downloading photo", sprintf("%4.0f", which(u == i)))
    file = gsub("http://www.parlament.hu/kepv/kepek", "photos", i)

    if (!file.exists(file))
      download.file(i, file, mode = "wb", quiet = TRUE)

    if (!file.info(file)$size) {

      # cat(": failed\n")
      file.remove(file)

    } else {

      a$photo[ a$photo == i ] = file
      # cat("\n")

    }

  }

  # clean names
  a$name = gsub("(.*)(D|d)r\\.\\s?", "", a$name)

  # bugfix (dr. prefix at end of name)
  a$name[ a$name == "" & a$url == "t027" ] = "Tóth Tiborné"

  # reorder first/family names
  a$name = sapply(a$name, function(x) {
    x = unlist(strsplit(x, " "))
    paste0(rev(x), collapse = " ")
  })

  a$firstname = sapply(a$name, function(x) {
    unlist(strsplit(x, " "))[1]
  })

  # solve duplicate: two rows on same legislature, same MP details
  a = subset(a, !(legislature == "1998-2002" & url == "t385"))

  a$sex = NA
  a$sex[ a$firstname %in% c("Ádám", "Ákos", "Alajos", "Albert", "Alpár", "Andor", "András", "Antal", "Árpád", "Attila", "Balázs", "Bálint", "Barna", "Barnabás", "Béla", "Bence", "Benedek", "Bertalan", "Csaba", "Dániel", "Dávid", "Dénes", "Dezső", "Előd", "Endre", "Erik", "Ernő", "Etele", "Ervin", "Ferenc", "Flórián", "Gábor", "Gellért", "Gergely", "Gergő", "Géza", "Gordon", "György", "Győző", "Gyula", "Ildikó", "Imre", "István", "Iván", "János", "Jenő", "József", "Kálmán", "Károly", "Kornél", "Kristóf", "Krisztián", "Lajos", "László", "Lénárd", "Levente", "Lorántné", "Lukács", "Marcell", "Máriusz", "Márk", "Márton", "Máté", "Mátyás", "Mihály", "Miklós", "Mózes", "Nándor", "Norbert", "Oszkár", "Ottó", "Pál", "Péter", "Pierre", "Rezső", "Richard", "Richárd", "Róbert", "Roland", "Sándor", "Szabolcs", "Sebestyén", "Szilárd", "Tamás", "Tibor", "Tihamér", "Tivadar", "Viktor", "Vilmos", "Zoltán", "Zsolt", "(Veszprém)") ] = "M"
  a$sex[ a$firstname %in% c("Ágnes", "Anita", "Anna", "Annamária", "Bernadett", "Dóra", "Edit", "Endréné", "Erika", "Erzsébet", "Éva", "Ferencné", "Gáborné", "Gabriella", "Györgyi", "Ibolya", "Ilona", "Istvánné", "Józsefné", "Judit", "Katalin", "Klára", "Krisztina", "Lajosné", "Lászlóné", "Lorántné", "Magda", "Magdolna", "Margit", "Mária", "Márta", "Melinda", "Monika", "Mónika", "Rebeka", "Róbertné", "Rózsa", "Sándorné", "Szilvia", "Szófia", "Tiborné", "Timea", "Valéria", "Virág", "Zita", "Zsuzsa", "Zsuzsanna") ] = "F"

  stopifnot(!is.na(a$sex))
  unique(subset(a, is.na(a$sex)))

  write.csv(a, sponsors, row.names = FALSE)

}

a = read.csv(sponsors, stringsAsFactors = FALSE)

# standardize parties
a$party[ a$party == "független" ] = "IND"
a$party = toupper(a$party)

stopifnot(!is.na(groups[ a$party ]))

a$born = NA # sadly missing at 100%
a$url = paste0("http://www.parlament.hu/internet/cplsql/ogy_kpv.kepv_adat?p_azon=", a$url)

# ============================================================================
# QUALITY CONTROL
# ============================================================================

# - might be missing: born (int of length 4), constituency (chr),
#   photo (chr, folder/file.ext)
# - never missing: sex (chr, F/M), nyears (int), url (chr, URL),
#   party (chr, mapped to colors)

cat("Missing", sum(is.na(a$born)), "years of birth\n")
stopifnot(is.integer(a$born) & nchar(a$born) == 4 | is.na(a$born))

cat("Missing", sum(is.na(a$constituency)), "constituencies\n")
stopifnot(is.character(a$constituency))

cat("Missing", sum(is.na(a$photo)), "photos\n")
stopifnot(is.character(a$photo) & grepl("^photos(_\\w{2})?/(.*)\\.\\w{3}", a$photo) | is.na(a$photo))

stopifnot(!is.na(a$sex) & a$sex %in% c("F", "M"))
stopifnot(!is.na(a$nyears) & is.integer(a$nyears))
stopifnot(!is.na(a$url) & grepl("^http(s)?://(.*)", a$url))
stopifnot(a$party %in% names(colors))
