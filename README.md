This repository contains code to build cosponsorship networks from bills passed in the [Hungarian Parliament](http://www.parlament.hu/).

- [interactive demo](http://f.briatte.org/parlviz/orszaggyules)
- [static plots](http://f.briatte.org/parlviz/orszaggyules/plots.html)
- [more countries](https://github.com/briatte/parlnet)

# HOWTO

Replicate by running `make.r` in R.

The `data.r` script downloads information on _sponsors only_: __you will need to download the raw bill indexes manually__, because I was too lazy to get a grip of the dynamic URL generation scheme for just four links. The files are copied in [this Gist](https://gist.github.com/briatte/2456835d188eabed0382) for convenience.

The procedure to get the bill indexes manually is:

- Go to [this index of all legislation](http://www.parlament.hu/iromanyok-elozo-ciklusbeli-adatai).
- Click the first legislative cycle.
- Set "Típus" to "törvényjavaslat".
- Click "Lekérdezés" to get the index.
- Save and repeat for each cycle.

The files go into `raw/bill-lists`. The filenames should be of the form `1998-2002` or of the form `2014-` for the ongoing legislature.

The `build.r` script then assembles the edge lists and plots the networks, with the help of a few routines coded into `functions.r`. Adjust the `plot`, `gexf` and `mode` parameters to skip the plots or to change the node placement algorithm.

# DATA

## Bills

- `legislature` -- legislature years
- `ref` -- bill id (of the form "T/123")
- `url` -- URL
- `title` -- short title
- `authors` -- sponsors list, as text, or "GOV" for government bills
- `status` -- bill status (always _lezárt_, closed)
- `n_au` -- total number of sponsors

## Sponsors

The sponsors data has one row per legislature in which the sponsor sat.

- `legislature` -- legislature years
- `url` -- profile URL, shortened
- `name` -- name (in Western order, cleaned up from "Dr." titles)
- `photo` -- photo dummy (0/1), to indicate if a sponsor photo exists
- `party` -- party affiliation, abbreviated
- `partyname` -- party affiliation, full name (in Hungarian)
- `sex` -- gender (F/M), imputed from first names
- `mandate` -- number of years in office when legislature started (int)
- `constituency` -- constituency, stored as the string to its Wikipedia English entry
