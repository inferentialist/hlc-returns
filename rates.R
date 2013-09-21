
library(survival)
library(RColorBrewer)
library(xtable)

source('altdur.R')
source('rate_functions.R')

df = read.table('lc.csv', header=TRUE, sep='|',
  colClasses=c(
    'character',   # id
    'numeric',     # funded_amnt
    'integer',     # term
    'numeric',     # int_rate
    'numeric',     # installment
    'factor',      # grade
    'Date',        # issue_d
    'numeric',     # total_pymnt
    'factor'       # loan_status
    )
  )
dur = loan.duration(df)
df$time   = dur$time
df$status = dur$status

## historical data, by grade
rates = matrix(NA, nr=7, nc=6)
for(gidx in 1:7)
  {
    g = LETTERS[gidx]
    bdf = df[df$grade == g,]

    quoted.rate        = quoted_rate_f(bdf)
    monthly.rate       = (quoted.rate -1) / 12
    compounded.rate    = (1+monthly.rate)^12
    ideal.static.rate  = ideal_static_rate_f(bdf)
    ideal.managed.rate = ideal_managed_rate_f(bdf)
    hist.static.rate   = hist_static_rate_f(bdf)
    hist.managed.rate  = hist_managed_rate_f(bdf)

    rates[gidx,] = c(
           quoted.rate,
           compounded.rate,
           ideal.static.rate,
           hist.static.rate,
           ideal.managed.rate,
           hist.managed.rate
           )
  }
dimnames(rates) = list(LETTERS[1:7], c('Quoted', 'Actual', 'Ideal Static', 'Hist. Static', 'Ideal Reinvest', 'Hist. Reinvest'))

## Print the LaTeX table
print(xtable(100*(rates - 1),
             label='table:lchp',
             caption='Lending Club Historical Performance:'
             ), digits=2)

## Make the KM survival plot
fit = survfit(Surv(df$time, df$status) ~ df$grade)
colors = brewer.pal(8,'Set1')[-6]
svg('surv.svg', h=8, w=8)
par(mar=c(4,4,1,1), mgp=c(3,1,0))
plot(fit, mark=NA, col=colors, lwd=2,
     ylab='Survival Probability', xlab='Months')
grid(col='gray')
legend('bottomleft', fill=colors, legend=paste0('Grade ', LETTERS[1:7]), bg='white')
dev.off()
