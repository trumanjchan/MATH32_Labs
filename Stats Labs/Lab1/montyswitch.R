montyswitch = function(i)
{
  # set up vector of doors
  doors = c(1,2,3)
  # randomly pick where the car is
  cardoor = sample(doors, 1)
  # randomly pick player’s door
  playerdoor = sample(doors, 1)
  # pick host’s door: not player's door, not car door
  hostdoor = doors[-c(playerdoor, cardoor)]
  if (length(hostdoor)>1)
    hostdoor = sample(hostdoor,1)
  finalchoice = doors[-c(playerdoor, hostdoor)]
  if (finalchoice == cardoor) return(TRUE)
  else return(FALSE)
}
# validate function:
# run experiment 10000 times
results = replicate(10000, montyswitch(0))
# print computed probability
print(sum(results)/length(results))
