# hi Hungary

source("load.r")
source("functions.r")
source("parties.r")

# folders

dir.create("data"  , showWarnings = FALSE)
dir.create("plots" , showWarnings = FALSE)

if (file.exists("photos.zip"))
  unzip("photos.zip")

dir.create("photos", showWarnings = FALSE)

if (file.exists("raw.zip"))
  unzip("raw.zip")

dir.create("raw"            , showWarnings = FALSE)
dir.create("raw/bill-lists" , showWarnings = FALSE)
dir.create("raw/mp-pages"   , showWarnings = FALSE)

# parameters

plot = TRUE
gexf = TRUE
mode = "fruchtermanreingold"
meta = c(
  "cty" = "Hungary",
  "lang" = "hu", # Wikipedia language for chamber and constituencies
  "ch" = "Országgyűlés",
  "type" = "Unicameral",
  "ipu" = 2141,
  "seats" = 384
)

# build routine

source("data.r")  # scrape bills and sponsors -- see README first
source("build.r") # assemble the networks
source("comm.r")  # add committee co-membership

save(list = ls(pattern = "^((co)?net|edges|bills)_hu\\d{4}$"),
     file = "data/net_hu.rda")

# have a nice day
