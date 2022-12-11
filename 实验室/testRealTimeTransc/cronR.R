## https://github.com/bnosac/cronR
## http://www.bnosac.be/index.php/blog/64-scheduling-r-scripts-and-processes-on-windows-and-unix-linux
## 

#'@ suppressWarnings(require('cronR'))
#'@ Sys.setenv(TZ = 'GMT')

#'@ f <- system.file('global.R')
#'@ cmd <- cron_rscript(f)

## Scheduled calculation every weekdays from Monday to Friday at 12AM.
#'@ cron_add(cmd, frequency = 'daily', id = 'job1', at = '00:00', 
#'@          description = 'Daily forecast', days_of_week = 1:5)

## Get all the jobs
#'@ cron_ls()

## Remove all scheduled jobs
#'@ cron_clear(ask = FALSE)

