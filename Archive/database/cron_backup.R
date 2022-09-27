
library(taskscheduleR)

myscript <- c("C:/Projects/CPAT/cpatr/development/cron_dev.R")

taskscheduler_create(taskname = "1.task",
                     rscript = myscript,
                     schedule = "MINUTE",
                     startdate = format(Sys.Date(), "%m/%d/%Y"),
                     modifier = 3)

taskscheduler_delete(taskname = "1.task")
