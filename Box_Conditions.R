library(bigrquery)
library(tidyverse)
library(knitr)
library(kableExtra)
library(gtsummary)
library(scales)

project <- "nih-nci-dceg-connect-prod-6d04"

box_cond_bq <- 
"
WITH counts AS (
  SELECT 'Package in Good Condition' AS condition_desc, COUNTIF(d_238268405_d_679749262='1') AS cnt FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.boxes`
  UNION ALL
  SELECT 'Manifest/Vial/Paperwork', COUNTIF(d_238268405_d_922995819='1') FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.boxes`
  UNION ALL
  SELECT 'Improper Packaging', COUNTIF(d_238268405_d_847410060='1') FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.boxes`
  UNION ALL
  SELECT 'Cold Packs - Warm', COUNTIF(d_238268405_d_595987358='1') FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.boxes`
  UNION ALL
  SELECT 'Damaged Vials', COUNTIF(d_238268405_d_387564837='1') FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.boxes`
  UNION ALL
  -- SELECT 'Returned Empty Vials', COUNTIF(d_238268405_d_631290535='1') FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.boxes`
  -- UNION ALL
  SELECT 'No Connect Label on Vials', COUNTIF(d_238268405_d_399948893='1') FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.boxes`
  UNION ALL
  SELECT 'Shipment Delay', COUNTIF(d_238268405_d_958000780='1') FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.boxes`
  UNION ALL
  SELECT 'Damaged Shipper (outer and/or inner)', COUNTIF(d_238268405_d_678483571='1') FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.boxes`
  UNION ALL
  SELECT 'Other', COUNTIF(d_238268405_d_933646000='1') FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.boxes`
),

total AS (
  SELECT SUM(cnt) AS total_cnt FROM counts
)

-- condition rows
SELECT
  condition_desc,
  cnt AS condition_count,
  ROUND(SAFE_DIVIDE(cnt, total_cnt)*100,2) AS percent_of_total
FROM counts, total

UNION ALL

-- total row
SELECT
  'Total Conditions' AS condition_desc,
  total_cnt AS condition_count,
  100 AS percent_of_total
FROM total

ORDER BY condition_desc = 'TOTAL', condition_count DESC;
"


box_cond_table <- bq_project_query(project, box_cond_bq)
box_conditions <- bq_table_download(box_cond_table, bigint="integer64",n_max = Inf, page_size = 10000)



box_conditions <- box_conditions %>% 
  dplyr::rename('Conditions'='condition_desc',
                "N" = "condition_count",
                "P"="percent_of_total") %>% 
  mutate(N = scales::comma(as.numeric(N)),
         'N (%)' = paste0(N, " (", P, "%)")) %>% 
  select('Conditions', 'N (%)')


knitr::kable(box_conditions, 
             caption=' Baseline- Package Conditions Reported for Site Shipments', 
             row.names=FALSE,align=c("l", "c"), booktabs = TRUE) %>%  
  add_indent(seq(1, nrow(box_conditions) - 1)) %>% 
  kable_styling(latex_options = "scale_down") %>% 
  footnote(general = "Note: The number of package conditions recorded may exceed the number of packages received from sites, as more than one condition may be selected.", 
           general_title = "",
           footnote_as_chunk = TRUE, 
           escape = FALSE, 
           threeparttable = TRUE)


