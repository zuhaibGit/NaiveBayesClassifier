library(h2o)
setwd("/home/zuhaib/Desktop/Machine Learning/Assignment 1")

#Stores training labels in a vector called vec
to.read.label = file("train-labels.idx1-ubyte", "rb")
readBin(to.read.label, integer(), n=2, size=4, endian="big")
vec <- readBin(to.read.label, integer(), n=60000, size=1, endian="big")

#Stores training images in a list called training_images
to.read.img = file("train-images.idx3-ubyte", "rb")
readBin(to.read.img, integer(), n=4, endian="big")
training_images <- list()
training_images_size <- 1
for(i in 1:60000) {
  m = matrix(readBin(to.read.img,integer(), size=1, n=28*28, endian="big", signed = F),28,28)
  m <- m[,28:1]
  training_images[[training_images_size]] <- m
  training_images_size <- training_images_size + 1
}
training_images <- rapply(training_images, function(x) ifelse(x > 128, 255, 0), how = "replace")

#Testing Thresholding on a small list
# img_lst <- list(training_images[[3]], training_images[[321]])
# img_lst <- rapply(img_lst, function(x) ifelse(x > 100, 255, 0), how = "replace")

#Stores training labels in a vector called vec2
to.read.label = file("t10k-labels.idx1-ubyte", "rb")
readBin(to.read.label, integer(), n=2, size=4, endian="big")
vec2 <- readBin(to.read.label, integer(), n=10000, size=1, endian="big")

#Stores training images in a list called testing_images
to.read.img = file("t10k-images.idx3-ubyte", "rb")
readBin(to.read.img, integer(), n=4, endian="big")
testing_images <- list()
testing_images_size <- 1
for(i in 1:10000) {
  m = matrix(readBin(to.read.img,integer(), size=1, n=28*28, endian="big", signed = F),28,28)
  m <- m[,28:1]
  testing_images[[testing_images_size]] <- m
  testing_images_size <- testing_images_size + 1
}
testing_images <- rapply(testing_images, function(x) ifelse(x > 128, 255, 0), how = "replace")

#Finds the average value for each pixel and returns the resulting
#matrix
find_average_image <- function(lst_of_images) {
  return(Reduce('+', lst_of_images)/length(lst_of_images))
}

#Does the same as above, finding the average value of each pixel, but it transfroms the original image so that pixels
#have values of 0 if they were 0 in the original image, and 1, otherwise.
bern_find_average_image <- function(lst_of_images) {
  lst <- lapply(lst_of_images, function(x) {x[which(x != 0)] <- 1; return(x)})
  return(Reduce('+', lst)/length(lst))
}

#Finds the standard deviation for each pixel and returns the resulting
#matrix
#Code obtained from: https://stackoverflow.com/questions/39351013/standard-deviation-over-a-list-of-matrices-in-r
find_sd_image <- function(lst) { 
  n <- length(lst)
  rc <- dim(lst[[1]])
  ar1 <- array(unlist(lst), c(rc, n))
  round(apply(ar1, c(1, 2), sd), 2) 
}

#Finds average training image for each digit
avg_imgs <- list(
  avg_img_0 <- find_average_image(training_images[which(vec == 0)]),
  avg_img_1 <- find_average_image(training_images[which(vec == 1)]),
  avg_img_2 <- find_average_image(training_images[which(vec == 2)]),
  avg_img_3 <- find_average_image(training_images[which(vec == 3)]),
  avg_img_4 <- find_average_image(training_images[which(vec == 4)]),
  avg_img_5 <- find_average_image(training_images[which(vec == 5)]),
  avg_img_6 <- find_average_image(training_images[which(vec == 6)]),
  avg_img_7 <- find_average_image(training_images[which(vec == 7)]),
  avg_img_8 <- find_average_image(training_images[which(vec == 8)]),
  avg_img_9 <- find_average_image(training_images[which(vec == 9)])
)
avg_imgs <- rapply(avg_imgs, function(x) ifelse(x > 110, 255, 0), how = "replace")

#Finds sd for each digit
sd_imgs <- list(
  sd_img_0 <- find_sd_image(training_images[which(vec == 0)]),
  sd_img_1 <- find_sd_image(training_images[which(vec == 1)]),
  sd_img_2 <- find_sd_image(training_images[which(vec == 2)]),
  sd_img_3 <- find_sd_image(training_images[which(vec == 3)]),
  sd_img_4 <- find_sd_image(training_images[which(vec == 4)]),
  sd_img_5 <- find_sd_image(training_images[which(vec == 5)]),
  sd_img_6 <- find_sd_image(training_images[which(vec == 6)]),
  sd_img_7 <- find_sd_image(training_images[which(vec == 7)]),
  sd_img_8 <- find_sd_image(training_images[which(vec == 8)]),
  sd_img_9 <- find_sd_image(training_images[which(vec == 9)])
)
#sd_imgs <- rapply(sd_imgs, function(x) ifelse(x > 200, 255, 0), how = "replace")


bern_avg_imgs <- list(
  bern_avg_img_0 <- bern_find_average_image(training_images[which(vec == 0)]),
  bern_avg_img_1 <- bern_find_average_image(training_images[which(vec == 1)]),
  bern_avg_img_2 <- bern_find_average_image(training_images[which(vec == 2)]),
  bern_avg_img_3 <- bern_find_average_image(training_images[which(vec == 3)]),
  bern_avg_img_4 <- bern_find_average_image(training_images[which(vec == 4)]),
  bern_avg_img_5 <- bern_find_average_image(training_images[which(vec == 5)]),
  bern_avg_img_6 <- bern_find_average_image(training_images[which(vec == 6)]),
  bern_avg_img_7 <- bern_find_average_image(training_images[which(vec == 7)]),
  bern_avg_img_8 <- bern_find_average_image(training_images[which(vec == 8)]),
  bern_avg_img_9 <- bern_find_average_image(training_images[which(vec == 9)])
)
#bern_avg_imgs <- rapply(bern_avg_imgs, function(x) ifelse(x > 0.5, 1, 0), how = "replace")


#Given a matrix, finds the posterior distributions for each digit,
#and returns the most likely value
determine_digit <- function(mat) {
  lst_of_posterior_probs <- c()
  for (k in 0:9) {
    avg_img <- avg_imgs[[k+1]]
    sd_img <- sd_imgs[[k+1]]

    accum <- 0.1 + sum(log(0.0000000001 + dnorm(mat, avg_img, sd_img + 10)))

    lst_of_posterior_probs <- c(lst_of_posterior_probs, accum)
  }
  return(which(lst_of_posterior_probs == max(lst_of_posterior_probs))[1] - 1)
}

#Given a matrix, finds the posterior distributions for each digit,
#and returns the most likely value
bern_determine_digit <- function(mat) {
  mat[which(mat != 0)] <- 1
  lst_of_posterior_probs <- c()
  for (k in 0:9) {
    avg_img <- bern_avg_imgs[[k+1]]
    
    accum <- 0.1 + sum(log(0.0000000001 + dbinom(mat, avg_img, size = 1)))    # for (i in 1:nrow(mat)) {

    lst_of_posterior_probs <- c(lst_of_posterior_probs, accum)
  }
  return(which(lst_of_posterior_probs == max(lst_of_posterior_probs))[1] - 1)
}

gaussian_untouched_guesses <- lapply(testing_images, determine_digit)
gaussian_untouched_guesses_training <- lapply(training_images, determine_digit)

bern_untouched_guesses <- lapply(testing_images, bern_determine_digit)
bern_untouched_guesses_training <- lapply(training_images, bern_determine_digit)

# Accuracies on the training and test sets predictions, for both methods (using binomial distribution and using normal distribution)
length(which(gaussian_untouched_guesses == vec2)) / length(vec2)
length(which(bern_untouched_guesses == vec2)) / length(vec2)
length(which(gaussian_untouched_guesses_training == vec)) / length(vec)
length(which(bern_untouched_guesses_training == vec)) / length(vec)


#######################
# Now we'll use the same Naive Bayes' algorithm as above but will crop each image to remove the margins of dead pixels.
# This is done by finding the horizontal and vertical ranges in which coloured pixels occue in each image, and cropping
# by that much. Then each image is resized so that they all have the same size afterwards.
#######################
find_vertical_ink_range <- function(img) {
  top <- min(apply(img, 2, function(x) return(min(which(x != 0)))))
  bottom <- max(apply(img, 2, function(x) return(max(which(x != 0)))))
  return(c(top, bottom))
}

find_horizontal_ink_range <- function(img) {
  left <- min(apply(img, 1, function(x) return(min(which(x != 0)))))
  right <- max(apply(img, 1, function(x) return(max(which(x != 0)))))
  return(c(left, right))
}

#Resizes an image to a given width and height. Returns the matrix
#Code obtained from https://stackoverflow.com/questions/35786744/resizing-image-in-r
resize_image <- function(img, new_width, new_height) {
  new_img = apply(img, 2, function(y){return (spline(y, n = new_height)$y)})
  new_img = t(apply(new_img, 1, function(y){return (spline(y, n = new_width)$y)}))
  
  new_img[new_img < 0] = 0
  new_img = round(new_img)
  
  return (new_img)
}

#A vector with the numbers in the list images, cropped by 4 pixels on 
#each side.
bounded_box_images_training <- lapply(training_images, function(x) {
  vert <- find_vertical_ink_range(x);
  hor <- find_horizontal_ink_range(x);
  return(x[vert[1]:vert[2], hor[1]:hor[2]])
})
#bounded_box_images_training <- rapply(bounded_box_images_training, function(x) ifelse(x > 220, 255, 0), how = "replace")


#A vector with the numbers in the list images, cropped by 4 pixels on 
#each side.
bounded_box_images_testing <- lapply(testing_images, function(x) {
  vert <- find_vertical_ink_range(x);
  hor <- find_horizontal_ink_range(x);
  return(x[vert[1]:vert[2], hor[1]:hor[2]])
})
#bounded_box_images_testing <- rapply(bounded_box_images_testing, function(x) ifelse(x > 220, 255, 0), how = "replace")

resized_bounded_box_images_training <- lapply(bounded_box_images_training, resize_image, 20, 20)
resized_bounded_box_images_testing <- lapply(bounded_box_images_testing, resize_image, 20, 20)

touched_avg_imgs <- list(
  touched_avg_img_0 <- find_average_image(resized_bounded_box_images_training[which(vec == 0)]),
  touched_avg_img_1 <- find_average_image(resized_bounded_box_images_training[which(vec == 1)]),
  touched_avg_img_2 <- find_average_image(resized_bounded_box_images_training[which(vec == 2)]),
  touched_avg_img_3 <- find_average_image(resized_bounded_box_images_training[which(vec == 3)]),
  touched_avg_img_4 <- find_average_image(resized_bounded_box_images_training[which(vec == 4)]),
  touched_avg_img_5 <- find_average_image(resized_bounded_box_images_training[which(vec == 5)]),
  touched_avg_img_6 <- find_average_image(resized_bounded_box_images_training[which(vec == 6)]),
  touched_avg_img_7 <- find_average_image(resized_bounded_box_images_training[which(vec == 7)]),
  touched_avg_img_8 <- find_average_image(resized_bounded_box_images_training[which(vec == 8)]),
  touched_avg_img_9 <- find_average_image(resized_bounded_box_images_training[which(vec == 9)])
)
touched_avg_imgs <- rapply(touched_avg_imgs, function(x) ifelse(x > 110, 255, 0), how = "replace")

touched_sd_imgs <- list(
  touched_sd_img_0 <- find_sd_image(resized_bounded_box_images_training[which(vec == 0)]),
  touched_sd_img_1 <- find_sd_image(resized_bounded_box_images_training[which(vec == 1)]),
  touched_sd_img_2 <- find_sd_image(resized_bounded_box_images_training[which(vec == 2)]),
  touched_sd_img_3 <- find_sd_image(resized_bounded_box_images_training[which(vec == 3)]),
  touched_sd_img_4 <- find_sd_image(resized_bounded_box_images_training[which(vec == 4)]),
  touched_sd_img_5 <- find_sd_image(resized_bounded_box_images_training[which(vec == 5)]),
  touched_sd_img_6 <- find_sd_image(resized_bounded_box_images_training[which(vec == 6)]),
  touched_sd_img_7 <- find_sd_image(resized_bounded_box_images_training[which(vec == 7)]),
  touched_sd_img_8 <- find_sd_image(resized_bounded_box_images_training[which(vec == 8)]),
  touched_sd_img_9 <- find_sd_image(resized_bounded_box_images_training[which(vec == 9)])
)

touched_bern_avg_imgs <- list(
  touched_bern_avg_img_0 <- bern_find_average_image(resized_bounded_box_images_training[which(vec == 0)]),
  touched_bern_avg_img_1 <- bern_find_average_image(resized_bounded_box_images_training[which(vec == 1)]),
  touched_bern_avg_img_2 <- bern_find_average_image(resized_bounded_box_images_training[which(vec == 2)]),
  touched_bern_avg_img_3 <- bern_find_average_image(resized_bounded_box_images_training[which(vec == 3)]),
  touched_bern_avg_img_4 <- bern_find_average_image(resized_bounded_box_images_training[which(vec == 4)]),
  touched_bern_avg_img_5 <- bern_find_average_image(resized_bounded_box_images_training[which(vec == 5)]),
  touched_bern_avg_img_6 <- bern_find_average_image(resized_bounded_box_images_training[which(vec == 6)]),
  touched_bern_avg_img_7 <- bern_find_average_image(resized_bounded_box_images_training[which(vec == 7)]),
  touched_bern_avg_img_8 <- bern_find_average_image(resized_bounded_box_images_training[which(vec == 8)]),
  touched_bern_avg_img_9 <- bern_find_average_image(resized_bounded_box_images_training[which(vec == 9)])
)

gaussian_touched_guesses <- lapply(resized_bounded_box_images_testing, determine_digit_touched)
gaussian_touched_guesses_training <- lapply(resized_bounded_box_images_training, determine_digit_touched)

bern_touched_guesses <- lapply(resized_bounded_box_images_testing, bern_determine_digit_touched)
bern_touched_guesses_training <- lapply(resized_bounded_box_images_training, bern_determine_digit_touched)

length(which(gaussian_touched_guesses == vec2))
length(which(bern_touched_guesses == vec2))
length(which(gaussian_touched_guesses_training == vec))
length(which(bern_touched_guesses_training == vec))

#Given a matrix, finds the posterior distributions for each digit,
#and returns the most likely value
determine_digit_touched <- function(mat) {
  lst_of_posterior_probs <- c()
  for (k in 0:9) {
    avg_img <- touched_avg_imgs[[k+1]]
    sd_img <- touched_sd_imgs[[k+1]]
    
    accum <- 0.1 + sum(log(0.0000000001 + dnorm(mat, avg_img, sd_img + 10)))
    
    lst_of_posterior_probs <- c(lst_of_posterior_probs, accum)
  }
  #print(lst_of_posterior_probs)
  return(which(lst_of_posterior_probs == max(lst_of_posterior_probs))[1] - 1)
}

#Given a matrix, finds the posterior distributions for each digit,
#and returns the most likely value
bern_determine_digit_touched <- function(mat) {
  mat[which(mat != 0)] <- 1
  lst_of_posterior_probs <- c()
  for (k in 0:9) {
    avg_img <- touched_bern_avg_imgs[[k+1]]
    
    accum <- 0.1 + sum(log(0.0000000001 + dbinom(mat, avg_img, size = 1)))    # for (i in 1:nrow(mat)) {

    lst_of_posterior_probs <- c(lst_of_posterior_probs, accum)
  }
  #print(lst_of_posterior_probs)
  return(which(lst_of_posterior_probs == max(lst_of_posterior_probs))[1] - 1)
}

####################
#This part of the code deals with the random forest genreation
####################
h2o.init()

#Takes a matrix, and converts it into a row vector
convert_to_row <- function(mat) {
  return(c(mat))
}

#Converts training data into a data frame, where each row is
#a lable for the digi, followed by 784 pixel values
make_df_for_h2o <- function(col_labels, img_list) {
  lst <- lapply(img_list, convert_to_row)
  names(lst) <- 1:length(lst)
  df <- data.frame(as.character(col_labels), t(as.data.frame(lst)))
  return(df)
}

#Creates the data frames for the four sets.
h2o_training_untouched <- make_df_for_h2o(vec, training_images)
h2o_training_touched <- make_df_for_h2o(vec, resized_bounded_box_images_training)
h2o_testing_untouched <- make_df_for_h2o(vec2, testing_images)
h2o_testing_touched <- make_df_for_h2o(vec2, resized_bounded_box_images_testing)

#Turns the above data frames into h2o frames
h2o_df_training_untouched <- as.h2o(x = h2o_training_untouched)
h2o_df_training_touched <- as.h2o(x = h2o_training_touched)
h2o_df_testing_untouched <- as.h2o(x = h2o_testing_untouched[,-1])
h2o_df_testing_touched <- as.h2o(x = h2o_testing_touched[,-1])

#Creates the random forest models
model_training_untouched_10_4 <- h2o.randomForest(y = 1,
                                             training_frame = h2o_df_training_untouched,
                                             ntrees = 10, max_depth = 4)
model_training_touched_10_4 <- h2o.randomForest(y = 1,
                                             training_frame = h2o_df_training_touched,
                                             ntrees = 10, max_depth = 4)
model_training_untouched_10_16 <- h2o.randomForest(y = 1,
                                             training_frame = h2o_df_training_untouched,
                                             ntrees = 10, max_depth = 16)
model_training_touched_10_16 <- h2o.randomForest(y = 1,
                                           training_frame = h2o_df_training_touched,
                                           ntrees = 10, max_depth = 16)
model_training_untouched_30_4 <- h2o.randomForest(y = 1,
                                             training_frame = h2o_df_training_untouched,
                                             ntrees = 30, max_depth = 4)
model_training_touched_30_4 <- h2o.randomForest(y = 1,
                                           training_frame = h2o_df_training_touched,
                                           ntrees = 30, max_depth = 4)
model_training_untouched_30_16 <- h2o.randomForest(y = 1,
                                             training_frame = h2o_df_training_untouched,
                                             ntrees = 30, max_depth = 16)
model_training_touched_30_16 <- h2o.randomForest(y = 1,
                                           training_frame = h2o_df_training_touched,
                                           ntrees = 30, max_depth = 16)

#Predictions
testing_untouched_10_4 <- predict(model_training_untouched_10_4,  h2o_df_testing_untouched)
length(which(as.vector(testing_untouched_10_4[,1]) == vec2))

testing_untouched_10_16 <- predict(model_training_untouched_10_16,  h2o_df_testing_untouched)
length(which(as.vector(testing_untouched_10_16[,1]) == vec2))

testing_untouched_30_4 <- predict(model_training_untouched_30_4,  h2o_df_testing_untouched)
length(which(as.vector(testing_untouched_30_4[,1]) == vec2))

testing_untouched_30_16 <- predict(model_training_untouched_30_16,  h2o_df_testing_untouched)
length(which(as.vector(testing_untouched_30_16[,1]) == vec2))

testing_touched_10_4 <- predict(model_training_touched_10_4,  h2o_df_testing_touched)
length(which(as.vector(testing_touched_10_4[,1]) == vec2))

testing_touched_10_16 <- predict(model_training_touched_10_16,  h2o_df_testing_touched)
length(which(as.vector(testing_touched_10_16[,1]) == vec2))

testing_touched_30_4 <- predict(model_training_touched_30_4,  h2o_df_testing_touched)
length(which(as.vector(testing_touched_30_4[,1]) == vec2))

testing_touched_30_16 <- predict(model_training_touched_30_16,  h2o_df_testing_touched)
length(which(as.vector(testing_touched_30_16[,1]) == vec2))



training_untouched_10_4 <- predict(model_training_untouched_10_4,  h2o_df_training_untouched)
length(which(as.vector(training_untouched_10_4[,1]) == vec))

training_untouched_10_16 <- predict(model_training_untouched_10_16,  h2o_df_training_untouched)
length(which(as.vector(training_untouched_10_16[,1]) == vec))

training_untouched_30_4 <- predict(model_training_untouched_30_4,  h2o_df_training_untouched)
length(which(as.vector(training_untouched_30_4[,1]) == vec))

training_untouched_30_16 <- predict(model_training_untouched_30_16,  h2o_df_training_untouched)
length(which(as.vector(training_untouched_30_16[,1]) == vec))

training_touched_10_4 <- predict(model_training_touched_10_4,  h2o_df_training_touched)
length(which(as.vector(training_touched_10_4[,1]) == vec))

training_touched_10_16 <- predict(model_training_touched_10_16,  h2o_df_training_touched)
length(which(as.vector(training_touched_10_16[,1]) == vec))

training_touched_30_4 <- predict(model_training_touched_30_4,  h2o_df_training_touched)
length(which(as.vector(training_touched_30_4[,1]) == vec))

training_touched_30_16 <- predict(model_training_touched_30_16,  h2o_df_training_touched)
length(which(as.vector(training_touched_30_16[,1]) == vec))
