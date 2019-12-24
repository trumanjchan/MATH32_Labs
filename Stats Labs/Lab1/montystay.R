montystay = function(i)
{
  # set up vector of doors
  doors = c(1,2,3)
  # randomly pick where the car is
  cardoor = sample(doors, 1)
  # randomly pick player’s door
  playerdoor = sample(doors, 1)
  if (playerdoor == cardoor) return (TRUE)
  else return (FALSE)
}

# validate function:
# run experiment 10000 times
results = replicate(10000, montystay(0))
# print computed probability
print(sum(results)/length(results))