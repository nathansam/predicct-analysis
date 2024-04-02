library(readxl)
library(datefixR)
library(splines)

if (file.exists("/docker")) { # If running in docker
  processed <- "data/processed/"
  EOF.dir <- "data/end-of-follow-up/"
} else { # Run on OS directly
  processed <- "/Volumes/igmm/cvallejo-predicct/predicct/processed/"
  EOF.dir <- "/Volumes/igmm/cvallejo-predicct/predicct/end-of-follow-up/"
}

demo.fcal <- readRDS(paste0(processed, "demo.RDS"))
fcal <- read_xlsx(paste0(EOF.dir, "EOF_fcal.xlsx"))
fcal <- subset(fcal, ParticipantNo %in% demo.fcal$ParticipantNo)
fcal <- subset(fcal, FCALDate != ".")

ids <- names(table(fcal$ParticipantNo))[table(fcal$ParticipantNo) >= 3]
fcal <- subset(fcal, ParticipantNo %in% ids)
demo.fcal <- subset(demo.fcal, ParticipantNo %in% ids)
fcal <- subset(fcal, IsBaseline == 0)


fcal <- fix_date_df(fcal, "FCALDate", excel = TRUE)
fcal$entry_date <- as.Date(fcal$entry_date)

fcal$time <- with(fcal, FCALDate - entry_date)
fcal <- subset(fcal, time >= 0)
fcal$time <- as.numeric(fcal$time) / 365.25
fcal <- subset(fcal, time < 10)

fcal.df <- data.frame(
  ParticipantNo = character(),
  FCALLevel = numeric(),
  time = numeric()
)

for (i in 1:nrow(demo.fcal)) {
  id <- demo.fcal[i, "ParticipantNo"]
  fcal.df <- rbind(fcal.df, data.frame(
    ParticipantNo = id,
    FCALLevel = demo.fcal[i, "FC"],
    time = 0
  ))
  fcal.sub <- subset(fcal, ParticipantNo == id)
  fcal.df <- rbind(fcal.df, data.frame(
    ParticipantNo = rep(id, nrow(fcal.sub)),
    FCALLevel = fcal.sub$FCALLevel,
    time = fcal.sub$time
  ))
}

fcal.df <- subset(
  fcal.df,
  !(ParticipantNo %in% names(table(fcal.df$ParticipantNo))[table(fcal.df$ParticipantNo) < 3])
)

ids.finished <- tools::file_path_sans_ext(list.files(paste0(
  "/Volumes/igmm/cvallejo-predicct/",
  "predicct/app/PIPIT-II/data/prediction/"
)))

ids <- unique(fcal.df$ParticipantNo)
ids <- ids[!(ids %in% ids.finished)]

for (id in ids) {
  print(id)
  temp <- subset(fcal.df, ParticipantNo == id)
  predict.time <- data.frame(time = seq(0, max(temp$time), by = 0.01))


  model <- lm(FCALLevel ~ ns(time, 2), data = temp)
  prediction <- predict(model, newdata = predict.time, interval = "confidence")
  subject.pred <- data.frame(
    pred = prediction[, "fit"],
    lower = prediction[, "lwr"],
    upper = prediction[, "upr"],
    time = predict.time
  )
  saveRDS(subject.pred,
    file = paste0(
      "/Volumes/igmm/cvallejo-predicct/",
      "predicct/app/PIPIT-II/data/prediction/",
      id,
      ".RDS"
    )
  )
}

saveRDS(fcal.df, file = paste0(
  "/Volumes/igmm/cvallejo-predicct/",
  "predicct/app/PIPIT-II/data/fcal.RDS"
))
