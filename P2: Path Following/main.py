# Names: Gianna Foti, Esther Shore
# Professor: Jay Sebastian
# Course: CS330 AI and Game Dev
# Assignment: Program 2 Path Following
# Due: 22 March 2024

import math

# Helper Functions
# add 2d vectors
def add_vec(v1, v2):
    v_x = v1[0] + v2[0]
    v_y = v1[1] + v2[1]
    return [v_x, v_y]

# subtract 2d vectors (v1 - v2)
def subtract_vec(v1, v2):
    v_x = v1[0] - v2[0]
    v_y = v1[1] - v2[1]
    return [v_x, v_y]

# multiply vector v by scalar m
def multiply_vec(m, v):
	return [m*v[0], m*v[1]]

# divide vector v by scalar m
def divide_vec(v, m):
	return [v[0]/m, v[1]/m]

# calculate length (magnitude) of a 2d vector
def length_vec(v):
	return math.sqrt(v[0]**2 + v[1]**2)

# normalize a 2d vector to unit length 1
def normalize_vec(v):
	length = length_vec(v)
	if length != 0:
		return [v[0]/length, v[1]/length]
	else:
		return [0.0, 0.0]

# calculate scalar dot product of two 2D vectors
def dot(v1, v2):
    return v1[0]*v2[0] + v1[1]*v2[1]

# find the point on the segment AB closest to the query point Q in 2D
def closest_point_segment(Q, A, B):
    T = dot(subtract_vec(Q, A), subtract_vec(B, A)) / dot(subtract_vec(B, A), subtract_vec(B, A))
    if (T <= 0):
        return A
    elif (T >= 1):
        return B
    else:
    	return add_vec(A, multiply_vec(T, subtract_vec(B, A)))
	
# calculate the distance between two points in 2D
def distance_point_point(x, z):
    distance = math.sqrt((z[0]-x[0])**2 + (z[1]-x[1])**2)
    return distance


# Dynamic Movement Behaviors
CONTINUE = 1
SEEK = 6
FLEE = 7
ARRIVE = 8 
FOLLOW_PATH = 11 # global constants


class Path:
	def __init__(self, id=1, x=[], y=[], num_segments=0, param_list=[], distance_list=[]):
		self.id = id
		self.x = x
		self.y = y
		self.num_segments = num_segments
		self.param_list = param_list
		self.distance_list = distance_list

	# formulate path from coordinates of vertices
	def assemble(self):
		# number of segments is one less the number of vertices
		self.num_segments = len(self.x) - 1
		
		# initialize array of distances
		self.distance_list = [0.0] * (self.num_segments + 1)
		# for each segment, record cumulative total path length
		for i in range(1, self.num_segments+1):
			self.distance_list[i] = self.distance_list[i-1] + distance_point_point([self.x[i-1], self.y[i-1]], [self.x[i], self.y[i]])
		
		# initialize array of path parameters
		self.param_list = [0.0] * (self.num_segments + 1)
		# for each segment, record proportion of path length
		for i in range(1, self.num_segments+1):
			self.param_list[i] = self.distance_list[i] / max(self.distance_list)

	def get_position(self, param):
		# get index of param in path param list prior to specified param
		i = max([i for i in range(len(self.param_list)) if param > self.param_list[i]])
		# establish coordinates of segment where parameter is located
		A = [self.x[i], self.y[i]]
		B = [self.x[i+1], self.y[i+1]]
		
		# calculate relative position along segment
		T = (param - self.param_list[i]) / (self.param_list[i+1] - self.param_list[i])
		# calulate position along path
		P = add_vec(A, multiply_vec(T, subtract_vec(B, A)))
		return P

	def get_param(self, position):
		closest_distance = float("inf")  # initialized to positive infinity
		
		# for each segment: update closest point, distance, and segment
		for i in range(0, self.num_segments):
			A = [self.x[i], self.y[i]]
			B = [self.x[i+1], self.y[i+1]]
			point = closest_point_segment(position, A, B)
			distance = distance_point_point(position, point)
			if (distance < closest_distance):
				closest_point = point
				closest_distance = distance
				closest_segment = i
		
		# establish endpoints and parameters for the endpoints of closest segment
		A = [self.x[closest_segment], self.y[closest_segment]]
		A_param = self.param_list[closest_segment]
		B = [self.x[closest_segment+1], self.y[closest_segment+1]]
		B_param = self.param_list[closest_segment+1]
		
		C = closest_point
		# calculate relative position of closest point on closest segment
		T = length_vec(subtract_vec(C, A)) / length_vec(subtract_vec(B, A))
		# calculate parameter along path of closest point
		C_param = A_param + (T * (B_param - A_param))
		return C_param


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
				path_to_follow=None,
				path_offset=0.0,
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
		self.path_to_follow = path_to_follow
		self.path_offset = path_offset
		self.collision_status = collision_status

	# output data in comma-separated values format
	def print_csv_data(self, timestamp):
		return (
			f"{timestamp}, {self.char_id}, "
			f"{self.position[0]}, {self.position[1]}, "
			f"{self.velocity[0]}, {self.velocity[1]}, "
			f"{self.linear_accel[0]}, {self.linear_accel[1]}, "
			f"{self.orientation}, {self.steering_behavior}, {self.collision_status}\n"
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
		dist_to_target = length_vec(dir_vec)
		if dist_to_target < self.arrival_radius:
			speed = 0
		# if within slowing radius, set scaled speed, otherwise max speed
		elif dist_to_target > self.slowing_radius:
			speed = self.max_vel
		else:
			speed = self.max_vel * dist_to_target / self.slowing_radius

		# get desired velocity by combining direction and speed
		vel_dir = normalize_vec(dir_vec)
		vel = multiply_vec(speed, vel_dir)

		# set accel to get to desired velocity
		linear_accel_x = vel[0] - self.velocity[0]
		linear_accel_y = vel[1] - self.velocity[1]
		linear_accel_x /= self.time_to_target
		linear_accel_y /= self.time_to_target
		linear_accel = [linear_accel_x, linear_accel_y]

		# clip accel if it exceeds max accel
		if length_vec(linear_accel) > self.max_accel:
			linear_accel = normalize_vec(linear_accel)
			linear_accel = multiply_vec(self.max_accel, linear_accel)

		return {"linear": linear_accel}
	
	def get_follow_path_steering(self):
		# determine current path parameter from character position
		path = self.path_to_follow
		current_param = path.get_param(self.position)

		# calculate target path parameter with offset
		target_param = current_param + self.path_offset
		# trim target parameter to max 1
		if target_param >= 1:
			target_param = 1
		
		# set target position to pass to Seek
		self.target = Character()
		self.target.position = path.get_position(target_param)
		return self.get_seek_steering()

	def update_position(self, steering_output):
		# set new position, velocity, and linear accel
		self.position[0] += self.velocity[0]*time_step
		self.position[1] += self.velocity[1]*time_step

		self.velocity[0] += steering_output["linear"][0]*time_step
		self.velocity[1] += steering_output["linear"][1]*time_step

		self.linear_accel = steering_output["linear"]

		# clip velocity if it exceeds max velocity
		if length_vec(self.velocity) > self.max_vel:
			self.velocity = normalize_vec(self.velocity)
			self.velocity = multiply_vec(self.max_vel, self.velocity)


# Simulation
# initialize path object with coordinates of vertices
path1 = Path(
	id=1,
	x=[0, -20, 20, -40, 40, -60, 60, 0],
	y=[90, 65, 40, 15, -10, -35, -60, -85],
)

path1.assemble()

# intialize character object
char1 = Character(
	char_id=2701,
	steering_behavior=FOLLOW_PATH,
	position=[20.0, 95.0],
	max_vel=4.0,
	max_accel=2.0,
	path_to_follow=path1,
	path_offset=0.04
)

# list of character objects
characters = [char1]

# simulation time parameters in seconds
total_run_time = 125
time_step = 0.5
num_time_steps = int(total_run_time / time_step)
sim_time = 0

# run simulation and write to output file
output_file = open("data.txt", 'w')

while sim_time <= total_run_time :
	for char in characters:
		# append data for each character to output file
		with open("data.txt", 'a') as output_file:
			output_file.write(char.print_csv_data(sim_time))

		# get new steering output based on steering behavior
		if char.steering_behavior == CONTINUE:
			steering_output = char.get_continue_steering()
		elif char.steering_behavior == SEEK:
			steering_output = char.get_seek_steering()
		elif char.steering_behavior == FLEE:
			steering_output = char.get_flee_steering()
		elif char.steering_behavior == ARRIVE:
			steering_output = char.get_arrive_steering()
		elif char.steering_behavior == FOLLOW_PATH:
			steering_output = char.get_follow_path_steering()

		# update character data
		char.update_position(steering_output)

	# increment simulation time
	sim_time += time_step

