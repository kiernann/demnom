temp_dir <- fs::path_temp()
fec_temp_file <- fs::file_temp()
fec_zip_url <- "https://www.fec.gov/files/bulk-downloads/2020/indiv20.zip"
fec_header_url <- "https://www.fec.gov/files/bulk-downloads/data_dictionaries/indiv_header_file.csv"
report_code_url <- "https://www.fec.gov/campaign-finance-data/report-type-code-descriptions/"
tran_code_url <- "https://www.fec.gov/campaign-finance-data/transaction-type-code-descriptions/"
cand_url <- "https://www.fec.gov/files/bulk-downloads/2020/cn20.zip"
cand_header_url <- "https://www.fec.gov/files/bulk-downloads/data_dictionaries/cn_header_file.csv"
cand_temp_file <- fs::file_temp()

download.file(
  url = zip_url,
  destfile = temp_file
)

unzip(
  zipfile = temp_file,
  exdir   = temp_dir
)


fec <-
  str_c(fs::path_dir(temp_file), "itcont.txt", sep = "/") %>%
  vroom(
    .name_repair = make_clean_names,
    col_names = unlist(str_split(read_lines(header_url), ",")),
    col_types = cols(
      .default = col_character(),
      TRANSACTION_AMT = col_double(),
      TRANSACTION_DT = col_date("%m%d%Y")
    )
  )

report_codes <-
  read_html(report_code_url) %>%
  html_node(".simple-table") %>%
  html_table(header = TRUE) %>%
  as_tibble() %>%
  clean_names()

tran_codes <-
  read_html(tran_code_url) %>%
  html_node(".simple-table") %>%
  html_table(header = TRUE, fill = TRUE) %>%
  as_tibble() %>%
  clean_names()

download.file(
  url = cand_url,
  destfile = cand_temp_file
)

unzip(
  zipfile = cand_temp_file,
  exdir = temp_dir
)

cand <-
  str_c(temp_dir, "cn.txt", sep = "/") %>%
  vroom(
    .name_repair = make_clean_names,
    col_names = unlist(str_split(read_lines(cand_header_url), ",")),
    col_types = cols(.default = col_character())
  )

dem_cand <- cand %>%
  filter(
    cand_pty_affiliation == "DEM",
    cand_election_yr == "2020",
    cand_office == "P",
    cand_status == "C"
  )

fec2 <- left_join(
  fec,
  dem_cand,
  by = c("cmte_id" = "cand_id")
)
