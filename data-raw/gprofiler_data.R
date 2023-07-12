## code to prepare `gprofiler_data` dataset goes here

library(gprofiler2)
gostres <- gost(query = c("ENSG00000105974","ENSG00000134240","ENSG00000184983","ENSG00000136869","ENSG00000126787","ENSG00000145715","ENSG00000198888","ENSG00000119013","ENSG00000212907","ENSG00000196632","ENSG00000119782","ENSG00000171497","ENSG00000109390","ENSG00000101680","ENSG00000168610","ENSG00000160439","ENSG00000256060","ENSG00000126562","ENSG00000054148"),
                organism = "hsapiens",evcodes = T,highlight = T)

res <- gostres$result |>
  dplyr::select(source,term_name,term_id,highlighted,p_value,evidence_codes,intersection) |>
  dplyr::filter(grepl("^GO",term_id)) |>
  dplyr::mutate(`−log10(pval)` = -log10(p_value))


library(tidyverse)
res <- res |>
  tibble::as_tibble() |>
  dplyr::mutate(evidence_codes = strsplit(evidence_codes,","),
                intersection = strsplit(intersection,",")) |>
  unnest(cols = c(evidence_codes, intersection))

gprofiler_data <- res |>
  pivot_wider(names_from = intersection,values_from = evidence_codes)



usethis::use_data(gprofiler_data, overwrite = TRUE)
