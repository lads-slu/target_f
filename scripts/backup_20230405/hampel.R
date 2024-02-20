low <- median(a) - 3 * mad(a, constant = 1) #hampel filter https://statsandr.com/blog/outliers-detection-in-r/#hampel-filter
high <- median(a) + 3 * mad(a, constant = 1)

outlier_ind <- which(a < lower_bound | a > upper_bound)
outlier_ind
