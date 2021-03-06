% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Models.R
\name{score_new_data}
\alias{score_new_data}
\title{Model Deployment: Score the new data based on the model.}
\usage{
score_new_data(model = model, newdata = newdata, ID = ID, file = file)
}
\arguments{
\item{model}{The model generated by lrm_model.}

\item{newdata}{A new data to be scored}

\item{ID}{The ID variable in the data.}

\item{file}{The file name to save the model probabilities and the segments.}
}
\value{
.
}
\description{
Based on the models built, score a new data. Probability will be calculated as the model scores.
}
\examples{

model.score <- score_new_data(model = model.final, newdata = newdata, ID = "BOOK_ID", file = "modelscores.xlsx") 

}
