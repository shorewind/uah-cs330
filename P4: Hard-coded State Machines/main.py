# Names: Gianna Foti, Esther Shore
# Professor: Jay Sebastian
# Course: CS330 AI and Game Dev
# Assignment: Program 4 Hard-coded Sate Machines
# Due: 26 April 2024

# Definining the States
FOLLOW = 1
PULL_OUT = 2
ACCELERATE = 3
PULL_IN_AHEAD = 4
PULL_IN_BEHIND = 5
DEACELERATE = 6
DONE = 7 

#selecting scenario 
scenario = 1 
trace = [True, False] [scenario - 1]
iterations = [100, 1000000][scenario - 1]
transition_probability = [ 
    [0.8, 0.4, 0.3, 0.4, 0.3, 0.3, 0.8, 0.8, 0.8],
    [0.9, 0.6, 0.3, 0.2, 0.2, 0.4, 0.7, 0.9, 0.7]
][scenario - 1]


#state counters and transition counters 
state_counter = [0] *7 
transition_counter = [0] * 9 

# Define actions of the states functions 
def follow_action():
    state_counter[FOLLOW - 1] += 1

def pull_out_action():
    state_counter[PULL_OUT - 1] += 1

def accelerate_action():
    state_counter[ACCELERATE - 1] += 1

def pull_in_ahead_action():
    state_counter[PULL_IN_AHEAD - 1] += 1

def pull_in_behind_action():
    state_counter[PULL_IN_BEHIND - 1] += 1

def decelerate_action():
    state_counter[DEACELERATE - 1] += 1

def done_action():
    state_counter[DONE - 1] += 1

    