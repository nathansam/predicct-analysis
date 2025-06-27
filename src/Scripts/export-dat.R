library(dplyr)
library(writexl)


if (file.exists("/docker")) {
  # If running in docker
  outdir <- "data/processed/"
} else {
  # Run on OS directly
  outdir <- "/Volumes/igmm/cvallejo-predicct/predicct/processed/"
}

flare.cd.df <- readRDS(paste0(outdir, "flares-biochem-cd.RDS"))
flare.uc.df <- readRDS(paste0(outdir, "flares-biochem-uc.RDS"))

flare.cd.df.sub <- flare.cd.df %>%
  select(
    ParticipantNo,
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
    hardflare_time
  )

flare.uc.df.sub <- flare.uc.df %>%
  select(
    ParticipantNo,
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
    hardflare_time
  )

combined[, c(22:25, 26, 27, 28)] <- combined[, c(26, 27, 28, 22:25)]
combined <- plyr::rbind.fill(flare.cd.df.sub, flare.uc.df.sub)
colnames(combined)[c(22:25, 26, 27, 28)] <- colnames(combined)[c(
  26,
  27,
  28,
  22:25
)]

write_xlsx(temp, "export-phenotype.xlsx")
