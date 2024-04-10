library(tidyverse)
library(ggthemes)
library(ggfortify)
library(GGally)
library(vdemdata)


party <- read.csv("~/elements/data/partyinst_data.csv") %>%
  rename(v2paid = party_id) %>%
  select(v2paid, psla, piendla, COWcode, year)
dat <- left_join(party, vdem, by = c("year", "COWcode"))
dat <- left_join(dat, vparty, by = c("year", "v2paid")) %>%
  select(COWcode.x, v2x_polyarchy, v2elvaptrn, v2eltrnout, v2eltype_0, v2eltype_6, v2elcomvot, v2elparlel, v2xpa_antiplural, v2pavote, v2paseatshare, v2paelcont, v2papariah, piendla, psla, v2pariglef)

write.csv(dat, file = "partyinst_data_330.csv", row.names = FALSE)
