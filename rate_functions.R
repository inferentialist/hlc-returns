
quoted_rate_f = function(xdf)
  {
    rval = 1 + median(xdf$int_rate) / 100
    return(rval)
  }

ideal_static_rate_f = function(xdf)
  {
    quoted_rate = quoted_rate_f(xdf)
    monthly.rate      = (quoted_rate -1) / 12

    ## Assume funded_amnt is $1
    coup = monthly.rate * (1+monthly.rate)^36 / ( (1+monthly.rate)^36 - 1)
    rval = (coup * 36)^(1/3)

    return(rval)
  }

ideal_managed_rate_f = function(xdf)
  {
    quoted_rate  = quoted_rate_f(xdf)
    monthly.rate = (quoted_rate -1) / 12

    ## Assume funded_amnt is $1
    coup = monthly.rate * (1+monthly.rate)^36 / ( (1+monthly.rate)^36 - 1)

    ## Reinvesting rate, no defaults
    k = 36:0
    Tk = coup / monthly.rate + (1+monthly.rate)^k*(1-coup/monthly.rate)
    v  = c(1, coup*(coup+1)^(0:35))
    rval = (Tk %*% v)^(1/3)

    return(rval)
  }

hist_static_rate_f = function(xdf)
  {
    rval =mean(xdf$total_pymnt / xdf$funded_amnt)^(1/3)
    return(rval)
  }

my.lag = function(z, k = 1)
  {
    lz = length(z)
    z = c(rep(NA,k), z)
    z = z[1:lz]
    return(z)
  }

hist_managed_rate_f = function(xdf)
  {
    coup            = xdf$installment / xdf$funded_amnt
    monthly.rate    = xdf$int_rate / 100 / 12
    k = 0:36
    prncp.remaining = coup / monthly.rate + exp(log(1+monthly.rate) %o% k)*(1-coup/monthly.rate)
    sched.coupons   = matrix(rep(coup, times=37), nr=nrow(prncp.remaining), nc=37)

    couponpay.mask  = matrix(1, nr=nrow(prncp.remaining), nc=length(k))
    couponpay.mask[,1] = 0

    prepay.mask     = matrix(0, nr=nrow(prncp.remaining), nc=length(k))

    ## update masks to reflect the historical activity
    for(i in which(xdf$time<36))
      {
        time   = xdf$time[i]
        status = xdf$status[i]

        tidx = time + 1
        if(status == TRUE)
          {
            ## default at k[tidx], no payments from here on out
            couponpay.mask[i,tidx:37] = 0
          } else {
            ## prepay at k[tidx], i.e. lump sum, but no future coupon payments
            couponpay.mask[i,(tidx+1):37] = 0
            prepay.mask[i,tidx] = 1
          }
      }

    ## construct the payout matrix
    cashflow.matrix = sched.coupons * couponpay.mask + prncp.remaining * prepay.mask
    n.loans = nrow(xdf)
    weights = rep(1/n.loans, n.loans)

    ## portfolio & reinvestment payouts at time k
    payout.k = t(weights %*% cashflow.matrix)
    payout.k[1,1] = NA
    for(cc in 2:36)
      {
        cash.avail = apply(payout.k, 1, sum)
        next.col = my.lag(payout.k[,1] * cash.avail[cc], k=(cc-1))
        payout.k = cbind(payout.k, next.col)
      }

    ## Multiply the residual portfolio value at time k by the weights of the
    ## cash reinvested at time k to get the total portfolio value
    cash.reinvested.k  = apply(payout.k, 1, sum, na.rm=TRUE)
    cash.reinvested.k[1] = 1
#    hist.prncp.remaining = rev(weights %*% (prncp.remaining * couponpay.mask))
    hist.prncp.remaining = rev(apply(prncp.remaining * couponpay.mask,2, function(x) pmax(0,x) %*% weights))
    hist.prncp.remaining[37] = 1

    rval = (cash.reinvested.k %*% hist.prncp.remaining)^(1/3)

    return(rval)
  }


## super simple test case example
##  z = data.frame(
##    funded_amnt     = 1,
##    int_rate        = 7.88,
##    time            = 36,
##    status          = FALSE,
##    installment     = 0.5733334,
##    total_pymnt     = 36*0.5733334,
##    total_rec_prncp = 1
##    )
##  hist_managed_rate_f(z) == ideal_managed_rate_f(z) == 1.081709
