### Functions from FPEMglobal. These are copies
### of functions in the FPEMglobal package
### (https://github.com/FPcounts/FPEMglobal) as at 2023-01-24.
###
### 'FPEMglobal' is released under the MIT License.
###
### Copyright (c) 2013 FPEMglobal authors.
###
### MIT LICENSE
### -----------
###
### Permission is hereby granted, free of charge, to any person obtaining a copy
### of this software and associated documentation files (the "Software"), to deal
### in the Software without restriction, including without limitation the rights
### to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
### copies of the Software, and to permit persons to whom the Software is
### furnished to do so, subject to the following conditions:
###
### The above copyright notice and this permission notice shall be included in all
### copies or substantial portions of the Software.
###
### THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
### IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
### FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
### AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
### LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
### OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
### SOFTWARE.


extractDenominators <- function(denominator_csv, in_union) {
  data <- read.csv(
    file = denominator_csv,
    header = TRUE,
    stringsAsFactors = FALSE,
    na.strings = c("", "NA")
  )

  verifyDenominators(data, in_union = in_union)

  if (!any(names(data) == "In.union")) {
    return(data)
  }

  data <-data %>% dplyr::filter(In.union == in_union) %>% dplyr::select(-In.union)

  # Remove empty columns
  data[, sapply(data, function(i) {
    !all(is.na(i))
  })]
}
