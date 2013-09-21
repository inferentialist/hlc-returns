
## This function, at it's core, vectorizes the closed form solution
## of the first order linear difference equation describing the remaining
## principal given the terms of a loan and the current time.
loan.duration = function(df)
  {
    loan_status     = df$loan_status
    total_pymnt     = df$total_pymnt
    installment     = df$installment
    funded_amnt     = df$funded_amnt
    period_int_rate = df$int_rate/100/12

    k = 0:36
    tk = matrix(rep(total_pymnt, length(k)), nc=length(k))

    ## Daily historical data suggest that 98% of non-terminal coupon payments are
    ## equal to the installment column, i.e. most people make the minimum payment.
    ## Hence, two possible cash flows:  pay fixed coupons until time k (Xk)                    [default case]
    ##                                  pay fixed coupons and then a lump sum at time k (Tk)   [prepay case]
    Xk = installment %o% k
    Tk = Xk +  
      installment / period_int_rate + 
        exp(log(1+period_int_rate) %o% k) * (funded_amnt - installment/period_int_rate)

    ## Find the time at which the total paid amount is closest to the cash flow at time k
    dXk = abs(Xk - tk)
    dTk = abs(Tk - tk)
    mXk = apply(dXk, 1, function(x) min(36, k[which.min(x)]+1))  ## Note, default will be after last pymnt
    mTk = apply(dTk, 1, function(x) k[which.min(x)])

    ## blend the two duration metrics & mark the death events
    fp_ind = (loan_status == 'Fully Paid')
    time   = ifelse(fp_ind, mTk, mXk)
    status = (loan_status %in% c('Default', 'Charged Off', 'Late (16-30 days)', 'Late (31-120 days)'))

    rval = list(time=time, status=status)
    return(rval)
  }
              
