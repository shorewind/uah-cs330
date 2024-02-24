# Names:Gianna Foti, Esther Shore 
# Professor: Jay Sebastian
# Course: CS330 
# Due: 23 February 2024

import math
import numpy as np

# Helper Functions

# calculate length of a 2d vector 
def length_vec(v):
	length = math.sqrt(v[0]**2 + v[1]**2)
	return length

# normalize a 2d vector to unit length 1
def normalize_vec(v):
    length = length_vec(v)
    if length != 0:
        return [v[0]/length, v[1]/length]
    else:
        return [0.0, 0.0]

# multiply vector v by scalar m
def multiply_vec(m, v):
	return [m*v[0], m*v[1]]


# Dynamic Movement Behaviors
CONTINUE = 1
SEEK = 5
FLEE = 7
ARRIVE = 8  # global constants

class Character:
	# parameterized constructor with default values
	def __init__(self,
				char_id=0,
				steering_behavior=CONTINUE,
				position=[0.0, 0.0],
				velocity=[0.0, 0.0],
				linear_accel=[0.0, 0.0],
				orientation=0.0,
				max_vel=0.0,
				max_accel=0.0,
				target=None,
				arrival_radius=0.0,
				slowing_radius=0.0,
				time_to_target=0.0,
				collision_status=False):
		self.char_id = char_id
		self.steering_behavior = steering_behavior
		self.position = position
		self.velocity = velocity
		self.linear_accel = linear_accel
		self.orientation = orientation
		self.max_vel = max_vel
		self.max_accel = max_accel
		self.target = target
		self.arrival_radius = arrival_radius
		self.slowing_radius = slowing_radius
		self.time_to_target = time_to_target
		self.collision_status = collision_status

	# output data in comma-separated values format
	def print_csv_data(self, timestamp):
		return (
			f"{timestamp}, {self.char_id}, {self.position[0]}, {self.position[1]}, "
			f"{self.velocity[0]}, {self.velocity[1]}, {self.linear_accel[0]}, "
			f"{self.linear_accel[1]}, {self.orientation}, {self.steering_behavior}, "
			f"{self.collision_status}\n"
		)

	def get_continue_steering(self):
		# steering unchanged
		return {"linear": self.linear_accel}

	def get_seek_steering(self):
		# get direction to target
		dir_x = self.target.position[0] - self.position[0]
		dir_y = self.target.position[1] - self.position[1]
		dir_vec = [dir_x, dir_y]

		# use max accel along direction
		linear_accel = normalize_vec(dir_vec)
		linear_accel = multiply_vec(self.max_accel, linear_accel)

		return {"linear": linear_accel}

	def get_flee_steering(self):
		# get direction directly away from target
		dir_x = self.position[0] - self.target.position[0]
		dir_y = self.position[1] - self.target.position[1]
		dir_vec = [dir_x, dir_y]

		# use max accel along direction
		linear_accel = normalize_vec(dir_vec)
		linear_accel = multiply_vec(self.max_accel, linear_accel)

		return {"linear": linear_accel}
	
	def get_arrive_steering(self):
		# get direction to target
		dir_x = self.target.position[0] - self.position[0]
		dir_y = self.target.position[1] - self.position[1]
		dir_vec = [dir_x, dir_y]

		# if within arrival radius, no steering
		distance = length_vec(dir_vec)
		if distance < self.arrival_radius:
			return {"linear": [0.0, 0.0]}

		# if within slowing radius, set scaled speed, otherwise max speed
		if distance > self.slowing_radius:
			target_speed = self.max_vel
		else:
			target_speed = self.max_vel * distance / self.slowing_radius
		
		# get velocity by combining direction and speed
		target_vel_dir = normalize_vec(dir_vec)
		target_vel = multiply_vec(target_speed, target_vel_dir)

		# set accel to get to target vel
		linear_accel_x = target_vel[0] - self.velocity[0]
		linear_accel_y = target_vel[1] - self.velocity[1]
		linear_accel_x /= self.time_to_target
		linear_accel_y /= self.time_to_target
		linear_accel = [linear_accel_x, linear_accel_y]

		# clip accel if it exceeds max accel
		if length_vec(linear_accel) > self.max_accel:
			linear_accel = normalize_vec(linear_accel)
			linear_accel = multiply_vec(self.max_accel, linear_accel)

		return {"linear": linear_accel}

	def update_position(self, steering_output):
		# set new position, velocity, and linear accel
		self.position[0] += self.velocity[0]*time_step
		self.position[1] += self.velocity[1]*time_step

		self.velocity[0] += steering_output["linear"][0]*time_step
		self.velocity[1] += steering_output["linear"][1]*time_step

		self.linear_accel = [steering_output["linear"][0], steering_output["linear"][1]]
		
		# clip velocity if it exceeds max velocity
		if length_vec(self.velocity) > self.max_vel:
			self.velocity = normalize_vec(self.velocity)
			self.velocity = multiply_vec(self.max_vel, self.velocity)


# Simulation
# intialize four character objects with different steering behaviors
char1 = Character(
    char_id=2601
)

char2 = Character(
    char_id=2602,
    steering_behavior=FLEE,
    position=[-30.0, -50.0],
    velocity=[2.0, 7.0],
    linear_accel=[0.0, 0.0],
    orientation=math.pi/4,
    max_vel=8.0,
    max_accel=1.5,
    target=char1,
)

char3 = Character(
    char_id=2603,
    steering_behavior=SEEK,
    position=[-50.0, 40.0],
    velocity=[0.0, 8.0],
    orientation=3*math.pi/2,
    max_vel=8.0,
    max_accel=2.0,
    target=char1,
)

char4 = Character(
    char_id=2604,
    steering_behavior=ARRIVE,
    position=[50.0, 75.0],
    velocity=[-9.0, 4.0],
    orientation=math.pi,
    max_vel=10.0,
    max_accel=2.0,
    target=char1,
    arrival_radius=4.0,
    slowing_radius=32.0,
    time_to_target=1.0,
)

characters = [char1, char2, char3, char4]

# simulation time parameters in seconds
total_run_time = 50
time_step = 0.5
num_time_steps = int(total_run_time / time_step)
sim_time = 0

# run simulation and write to output file
output_file = open("data.txt", 'w')

with open("data.txt", 'a') as output_file:
	for char in characters:
		# print(char.print_csv_data(sim_time))
		output_file.write(char.print_csv_data(sim_time))

