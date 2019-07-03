# Kiernan Nicholls
# Wed Jul  3 14:58:21 2019 ------------------------------

pacman::p_load(
  tidyverse,
  janitor,
  vroom,
  rvest,
  here,
  fs
)

dir_create("data/fec/raw")

# candidate master ---------------------------------------------------------------------------

cn_url <- "https://www.fec.gov/files/bulk-downloads/2020/cn20.zip"
cn_head_url <- "https://www.fec.gov/files/bulk-downloads/data_dictionaries/cn_header_file.csv"
cn_header <- str_to_lower(unlist(str_split(read_lines(cn_head_url), ",")))

download.file(
  url = cn_url,
  destfile = "data/fec/raw/cn20.zip"
)

unzip(
  zipfile = "data/fec/raw/cn20.zip",
  exdir = "data/fec/raw/"
)

file_delete("data/fec/raw/cn20.zip")

cn <- vroom(
  file = "data/fec/raw/cn.txt",
  delim = "|",
  col_names = cn_header,
  col_types = cols(
    .default = col_character(),
    cand_election_yr = col_integer()
  )
)

dem_cn <- cn %>%
  filter(
    cand_pty_affiliation == "DEM",
    cand_election_yr == "2020",
    cand_office == "P",
    cand_status == "C"
  )


# candidate committee link -------------------------------------------------------------------

ccl_url <- "https://www.fec.gov/files/bulk-downloads/2020/ccl20.zip"
ccl_head_url <- "https://www.fec.gov/files/bulk-downloads/data_dictionaries/ccl_header_file.csv"
ccl_header <- str_to_lower(unlist(str_split(read_lines(ccl_head_url), ",")))
cmte_tp_url <- "https://www.fec.gov/campaign-finance-data/committee-type-code-descriptions/"

download.file(
  url = ccl_url,
  destfile = "data/fec/raw/ccl.zip"
)

unzip(
  zipfile = "data/fec/raw/ccl.zip",
  exdir = "data/fec/raw/"
)

file_delete("data/fec/raw/ccl.zip")

ccl <- vroom(
  file = "data/fec/raw/ccl.txt",
  delim = "|",
  col_names = ccl_header,
  col_types = cols(
    .default = col_character(),
    cand_election_yr = col_integer(),
    fec_election_yr = col_integer(),
  )
)

cmte_tp_codes <-
  read_html(cmte_tp_url) %>%
  html_node("table") %>%
  html_table(header = TRUE) %>%
  as_tibble() %>%
  clean_names()

# committee master ---------------------------------------------------------------------------

cm_url <- "https://www.fec.gov/files/bulk-downloads/2020/cm20.zip"
cm_head_url <- "https://www.fec.gov/files/bulk-downloads/data_dictionaries/cm_header_file.csv"
cm_header <- str_to_lower(unlist(str_split(read_lines(cm_head_url), ",")))

download.file(
  url = cm_url,
  destfile = "data/fec/raw/cm20.zip"
)

unzip(
  zipfile = "data/fec/raw/cm20.zip",
  exdir = "data/fec/raw/"
)

file_delete("data/fec/raw/cm20.zip")

cm <- vroom(
  file = "data/fec/raw/cm.txt",
  delim = "|",
  col_names = cm_header,
  col_types = cols(
    .default = col_character()
  )
)

dem_cn <- cn %>%
  filter(
    cand_pty_affiliation == "DEM",
    cand_election_yr == "2020",
    cand_office == "P",
    cand_status == "C"
  )

# ind conts ----------------------------------------------------------------------------------

indiv_url <- "https://www.fec.gov/files/bulk-downloads/2020/indiv20.zip"
indiv_head_url <- "https://www.fec.gov/files/bulk-downloads/data_dictionaries/indiv_header_file.csv"
indiv_header <- str_to_lower(unlist(str_split(read_lines(indiv_head_url), ",")))

rpt_tp_url <- "https://www.fec.gov/campaign-finance-data/report-type-code-descriptions/"
tran_tp_url <- "https://www.fec.gov/campaign-finance-data/transaction-type-code-descriptions/"

download.file(
  url = indiv_url,
  destfile = "data/fec/raw/indiv20.zip"
)

unzip(
  zipfile = "data/fec/raw/indiv20.zip",
  exdir = "data/fec/raw/"
)

file_delete("data/fec/raw/indiv20.zip")

indiv <- vroom(
    file = "data/fec/raw/itcont.txt",
    col_names = indiv_header,
    col_types = cols(
      .default = col_character(),
      transaction_amt = col_double(),
      transaction_dt = col_date("%m%d%Y")
    )
  )

rpt_tp_codes <-
  read_html(rpt_tp_url) %>%
  html_node(".simple-table") %>%
  html_table(header = TRUE) %>%
  as_tibble() %>%
  clean_names() %>%
  select(
    rpt_tp = report_type_code,
    rpt_desc = report_type
  )

tran_tp_codes <-
  read_html(tran_tp_url) %>%
  html_node(".simple-table") %>%
  html_table(fill = TRUE, header = TRUE) %>%
  as_tibble() %>%
  clean_names() %>%
  select(
    transaction_tp = transaction_type,
    transaction_desc = transaction_type_description
  )


# join ---------------------------------------------------------------------------------------

dem_ccl <- dem_cn %>%
  left_join(
    ccl,
    by = c("cand_id", "cand_election_yr")
  )

dem_indiv <- indiv %>%
  right_join(dem_ccl, by = "cmte_id")

dem_indiv %>%
  ggplot() +
  geom_boxplot(
    mapping = aes(x = cand_name, y = transaction_amt),
    varwidth = TRUE
  ) +
  scale_y_log10() +
  coord_flip()

# write --------------------------------------------------------------------------------------

write_csv(
  x = dem_indiv,
  path = "data/fec/dem_indiv.csv",
  na = ""
)
