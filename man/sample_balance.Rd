% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SampleBalance.R
\name{sample_balance}
\alias{sample_balance}
\title{Sample balancing processing with a k-factor.}
\usage{
sample_balance(data = data, ID = ID, targstr = targstr,
  dweights = rep(1, nrow(data)), cap = T, typeofcap = 1, capval = 6,
  floor = F, floorval = -Inf, eps = 0.01, rounding = F, klimit = F,
  klimitval = Inf, maxiter = 20000, random = FALSE, out = NULL)
}
\arguments{
\item{data}{Data to be sample balanced.}

\item{ID}{Unique ID of the dataset.}

\item{targstr}{Targ structure returned by the function \code{sample_balance_init}.}

\item{dweights}{Weight in the data. Default: rep(1,nrow(data))}

\item{cap}{Cap flag. If yes, a cap will be applied in sample balancing process. Default: True.}

\item{typeofcap}{Type of the cap. if 1, then cap by n*mean; if 2, then cap by mean +/-n*sd. n is assigned by capval. Default: 1 (mean cap).}

\item{capval}{The cap value. If mean cap is used, during each iteration, when new weights are larger than \code{capvalue*mean}, they will be adjusted back to \code{capvalue*mean}. If std cap is used, during each iteration, when new weights are larger than \code{mean+capvalue*std(weight)}, they will be adjusted back to \code{mean+capvalue*std(weight)}. when new weights are smaller than \code{mean-capvalue*std(weight)}, they will be adjusted up to \code{mean-capvalue*std(weight)}. Default: 6.}

\item{floor}{Floor flag. Indicate if a floor value will be used in sample balancing process. Default: False. When no cap used, this option is recommended to be TRUE.}

\item{floorval}{Floor value.  During each iteration, when new weights are smaller than the floorvalue, they will be adjusted up to floorvalue. Default: -Inf.}

\item{eps}{Episilon. The threshold used in iteration algorithm. Algorithm converges when the diff is less than eps. Default: 0.01}

\item{rounding}{Round flag. Indicate if the weights will be rounded in sample balancing process. Default: FALSE.}

\item{klimit}{Klimit flag. Indicate if a klimit value will be used in sample balancing process. Default: FALSE.}

\item{klimitval}{Klimit value. When Klimit value is applied, before iteration, two new vector will be created: \code{maxneww=dweights*klimitval} and \code{minneww=dweights/klimitval}. During each iteration, when new weights are larger than maxneww, they will be adjusted back to maxneww, when new weights are lower than minneww, they will be adjusted up to minneww. This option works similarly as the Cap option. Default: -Inf.}

\item{maxiter}{Max number of iteration in sample balancing process. Default: 20000.}

\item{random}{Randomization flag. If yes, data will be randimized first before sample balancing process. Default: FALSE.}

\item{out}{The filename of the spreadsheet which contains the output information of sample balancing. The file is saved in current working directory. If NULL, no output file is generated.}
}
\value{
Return a list with 2 elements. The first element contains sample balancing information such as convergence, iterations, DEFF, capval, stat efficiency et al. The second element is a dataframe with two columns, ID and new balanced (capped) weights.
}
\description{
Sample balancing processing with a k-factor.
}
\examples{
data <- read_sav("OnlineWgts.sav")
targ <- read.xlsx("targ.xlsx")

targstr <- sample_balance_init(data=data, targ=targ)

# Cap using 6*means
results.sb <- sample_blance(data = data,
                            ID = "BOOK_ID",
                            targstr = targstr,
                            dweights = wgt,
                            cap = T,
                            typeofcap = 1,
                            capval = 6,
                            floor = T,
                            floorval = 50,
                            eps = .01,
                            rounding = F,
                            klimit = F,
                            klimitval = Inf,
                            out = "out.xlsx")

}
