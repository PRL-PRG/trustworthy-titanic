using DataFrames

# Load in the data
train = readtable("../input/train.csv")

println("The columns in the train set are:\n")
println(names(train))
println(@sprintf("\nThere are %d rows in the training set", nrow(train)))

# It's yours to take from here!