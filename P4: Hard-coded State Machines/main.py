# Names: Gianna Foti, Esther Shore
# Professor: Jay Sebastian
# Course: CS330 AI and Game Dev
# Assignment: Program 4 Hard-coded State Machines
# Due: 26 April 2024

import random
from datetime import datetime

# defining the states
FOLLOW = 1
PULL_OUT = 2
ACCELERATE = 3
PULL_IN_AHEAD = 4
PULL_IN_BEHIND = 5
DECELERATE = 6
DONE = 7 

# selecting scenario 
scenario = 2  # can be 1 or 2
trace = [True, False] [scenario - 1]
iterations = [100, 1000000][scenario - 1]
transition_probability = [ 
    [0.8, 0.4, 0.3, 0.4, 0.3, 0.3, 0.8, 0.8, 0.8],
    [0.9, 0.6, 0.3, 0.2, 0.2, 0.4, 0.7, 0.9, 0.7]
][scenario - 1]

# define state and transition sequences
state_sequence = [list(range(1, 8)), [7] + list(range(1, 7))][scenario-1]
transition_sequence = list(range(1, 10)) 

# initalize output file
output_file = f"CS 330 Program 4 Scenario {scenario} Output.txt"

# initialize state and transition counters 
state_counter = [0] * 7
transition_counter = [0] * 9

# define actions of the states functions 
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
    state_counter[DECELERATE - 1] += 1

def done_action():
    state_counter[DONE - 1] += 1

# execute iterations and transitions
for i in range(iterations):
    state = FOLLOW  # initial state
    follow_action()
    while state != DONE:
        # random number generator between 0 and 1
        r = random.uniform(0.0, 1.0)
        # conditionally checking transitions
        if (state == FOLLOW):
            if (r < transition_probability[0]):
                transition_counter[0] += 1
                state = PULL_OUT
                pull_out_action()
            else:
                state = FOLLOW
                follow_action()
        elif (state == PULL_OUT):
            if (r < transition_probability[1]):
                transition_counter[1] += 1
                state = ACCELERATE
                accelerate_action()
            elif (r < sum(transition_probability[i] for i in [1, 3])):
                transition_counter[3] += 1
                state = PULL_IN_BEHIND
                pull_in_behind_action()
            else:
                state = PULL_OUT
                pull_out_action()
        elif (state == ACCELERATE):
            if (r < transition_probability[2]):
                transition_counter[2] += 1
                state = PULL_IN_AHEAD
                pull_in_ahead_action()
            elif (r < sum(transition_probability[i] for i in [2, 4])):
                transition_counter[4] += 1
                state = PULL_IN_BEHIND
                pull_in_behind_action()
            elif (r < sum(transition_probability[i] for i in [2, 4, 5])):
                transition_counter[5] += 1
                state = DECELERATE
                decelerate_action()
            else:
                state = ACCELERATE
                accelerate_action()
        elif (state == PULL_IN_AHEAD):
            if (r < transition_probability[8]):
                transition_counter[8] += 1
                state = DONE
                done_action()
            else:
                state = PULL_IN_AHEAD
                pull_in_ahead_action()
        elif (state == PULL_IN_BEHIND):
            if (r < transition_probability[6]):
                transition_counter[6] += 1
                state = FOLLOW
                follow_action()
            else:
                state = PULL_IN_BEHIND
                pull_in_behind_action()
        elif (state == DECELERATE):
            if (r < transition_probability[7]):
                transition_counter[7] += 1
                state = PULL_IN_BEHIND
                pull_in_behind_action()
            else:
                state = DECELERATE
                decelerate_action()
        elif(state == DONE):
           break

# frequencies
state_freq = [round(count / sum(state_counter), 3) for count in state_counter]
transition_freq = [round(count / sum(transition_counter), 3) for count in transition_counter]

current_date_time = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

# output results to text file
with open(f"CS 330 Program 4 Scenario {scenario}.txt", 'w') as output_file:
    output_file.write(f"CS 330, State Machines, Begin {current_date_time}\n\n")
    output_file.write(f"scenario                = {scenario}\n")
    output_file.write(f"trace                   = {trace}\n")
    output_file.write(f"iterations              = {iterations}\n")
    output_file.write(f"transition probabilities = {transition_probability}\n")
    output_file.write(f"state counts            = {state_counter}\n")
    output_file.write(f"state frequencies       = {state_freq}\n")
    output_file.write(f"transition counts       = {transition_counter}\n")
    output_file.write(f"transition frequencies  = {transition_freq}\n\n")
    output_file.write(f"CS 330, State Machines, End {current_date_time}\n")
