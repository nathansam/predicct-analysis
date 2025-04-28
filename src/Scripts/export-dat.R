library(dplyr)
library(writexl)

flare.cd.df.sub <- flare.cd.df %>%
  select(ParticipantNo,
         SiteNo,
         Sex,
         Ethnicity,
         FC,
         cat,
         IMD,
         diagnosis,
         diagnosis2,
         Biologic,
         Smoke,
         ECigs,
         CReactiveProtein,
         Haemoglobin,
         WCC,
         Platelets,
         Albumin,
         Location,
         L4,
         Behaviour,
         Perianal,
         softflare,
         softflare_time,
         hardflare,
         hardflare_time)


flare.uc.df.sub <- flare.uc.df %>%
  select(ParticipantNo,
         SiteNo,
         Sex,
         Ethnicity,
         FC,
         cat,
         IMD,
         diagnosis,
         diagnosis2,
         Biologic,
         Smoke,
         ECigs,
         CReactiveProtein,
         Haemoglobin,
         WCC,
         Platelets,
         Albumin,
         Extent,
         Mayo,
         Mayo_cat,
         softflare,
         softflare_time,
         hardflare,
         hardflare_time)

temp <- plyr::rbind.fill(flare.cd.df.sub, flare.uc.df.sub)
temp[, c(22:25, 26, 27, 28)] <- temp[, c(26, 27, 28, 22:25)]
colnames(temp)[c(22:25, 26, 27, 28)] <- colnames(temp)[c(26, 27, 28, 22:25)]

write_xlsx(temp, "export-phenotype.xlsx")

