
require(tidyverse)
require(foreach)
#
# (collembola  OR  acari) AND (trophic  OR  trophic interactions OR  diet)  AND soil AND (gut content OR  stable isotope OR  food preferences OR fatty acid OR lipids ) AND (family OR genus OR species) AND (soil food web)
#  2015 - 
url_name <- 'https://scholar.google.com.ar/scholar?q=%28collembola++OR++acari%29+AND+%28trophic++OR++trophic+interactions+OR++diet%29++AND+soil+AND+%28gut+content+OR++stable+isotope+OR++food+preferences+OR+fatty+acid+OR+lipids+%29+AND+%28family+OR+genus+OR+species%29+AND+%28soil+food+web%29&hl=es&as_sdt=0%2C5&as_vis=1&as_ylo=2015&as_yhi='
siguiente <- 'https://scholar.google.com.ar/scholar?start=10&q=(collembola++OR++acari)+AND+(trophic++OR++trophic+interactions+OR++diet)++AND+soil+AND+(gut+content+OR++stable+isotope+OR++food+preferences+OR+fatty+acid+OR+lipids+)+AND+(family+OR+genus+OR+species)+AND+(soil+food+web)&hl=es&as_sdt=0,5&as_ylo=2015&as_vis=1'


wp <- xml2::read_html(url_name)
# Extract raw data
titles <- rvest::html_text(rvest::html_nodes(wp, '.gs_rt'))
authors_years <- rvest::html_text(rvest::html_nodes(wp, '.gs_a'))
# Process data
authors <- gsub('^(.*?)\\W+-\\W+.*', '\\1', authors_years, perl = TRUE)
years <- gsub('^.*(\\d{4}).*', '\\1', authors_years, perl = TRUE)
# Make data frame
df <- data.frame(titles = titles, authors = authors, years = years, stringsAsFactors = FALSE)

foreach(rr=1:50) %do% {
wp <- xml2::read_html(siguiente)
# Extract raw data
titles <- rvest::html_text(rvest::html_nodes(wp, '.gs_rt'))
authors_years <- rvest::html_text(rvest::html_nodes(wp, '.gs_a'))
# Process data
authors <- gsub('^(.*?)\\W+-\\W+.*', '\\1', authors_years, perl = TRUE)
years <- gsub('^.*(\\d{4}).*', '\\1', authors_years, perl = TRUE)
# Make data frame
df <- bind_rows(df, data.frame(titles = titles, authors = authors, years = years, stringsAsFactors = FALSE))
}
