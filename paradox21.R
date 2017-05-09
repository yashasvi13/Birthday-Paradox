dayprobs = c(.0026123,.0026785,.0026838,.0026426,.0026702,.0027424,.0028655,
             .0028954,.0029407,.0027705,.0026842,.0026864)
daysmo = c(31,28,31,30,31,30,31,31,30,31,30,31)
daysmo2 = c(31,28.25,31,30,31,30,31,31,30,31,30,31)
# need both: the former is how the probs are reported, 
# while the latter allows leap days

moprob = daysmo * dayprobs

mob = sample(1:12,10000 * 367,rep=TRUE,prob=moprob)
dob = sapply(mob,function(x) ceiling(sample((4*daysmo2[x]),1)/4) )
# The ceiling() function isn't vectorized, so we make the equivalent
# using sapply().

mobdob = paste(mob,dob)
# concatenate the month and day to make a single variable to compare
# between people.  The ISOdate() function would approximate the SAS mdy() 
# function but would be much longer, and we don't need it.

# convert the vector into a matrix with the maximum
# group size as the number of columns
# as noted above, this could safely be truncated, with great savings
mdmat = matrix(mobdob, ncol=367, nrow=10000)


matchn = function(x) {
  for (i in 1:367){
    if (length(unique(x[1:i])) != i) break
  }
  return(i)
}

groups = apply(mdmat, 1, matchn)

bdprobs = cumsum(table(groups)/10000)
# find the N with each group number, divide by number of groups
# and get the cumulative sum

rgroups = as.numeric(names(bdprobs))
# extract the group sizes from the table
probs = 1 - ((factorial(rgroups) * choose(365,rgroups)) / 365**rgroups)
# calculate the analytic answer, for any group size 
# for which there was an observed simulated value

diffs = bdprobs - probs
diffpcts = diffs/probs


addsecondy <- function(x, y, origy, yname="Y2") {
  prevlimits <- range(origy)
  axislimits <- range(y)
  axis(side=4, at=prevlimits[1] + diff(prevlimits)*c(0:5)/5,
       labels=round(axislimits[1] + diff(axislimits)*c(0:5)/5, 3))
  mtext(yname, side=4)
  newy <- (y-axislimits[1])/(diff(axislimits)/diff(prevlimits)) +
    prevlimits[1]
  points(x, newy, pch=2)
  abline(h=(-axislimits[1])/(diff(axislimits)/diff(prevlimits)) +
           prevlimits[1])
}

plottwoy <- function(x, y1, y2, xname="X", y1name="Y1", y2name="Y2")
{
  plot(x, y1, ylab=y1name, xlab=xname)
  abline(h=0)
  addsecondy(x, y2, y1, yname=y2name)
}

plottwoy(rgroups, diffs, diffpcts, xname="Number in group",
         y1name="Diff in prob", y2name="Diff in percent")
legend(80, .0013, pch=1:2, legend=c("Diffs", "Pcts"))

