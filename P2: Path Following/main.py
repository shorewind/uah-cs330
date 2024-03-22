# Names: Gianna Foti, Esther Shore
# Professor: Jay Sebastian
# Course: CS330 AI and Game Dev
# Assignment: Program 2 Path Following
# Due: 22 March 2024

import math

# Helper Functions
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

# multiply vector v by scalar m
def multiply_vec(m, v):
	return [m*v[0], m*v[1]]

# Calculate scalar dot product of two 2D vectors.
def dot(v1,v2):
    sum = ([v1[0]*v2[0] + v1[1]*v2[1]])
    return sum

# Find the point on the line closest to the query point in 2D.
def closestPointLine(x,z,q):
    point = dot((q - x),(z - x))
    point /= dot((z - x), (z - x))
    point = (x + (point * (z - x)))
    return point

# Find the point on the segment closest to the query point in 2D.
#q is the query point and x and z are distinct points on the line
def closestPointSegment(x,z,q):
    point = dot((q - x), (z - x))
    point /= dot((z - x), (z - x))
    if (point <= 0):
        return x
    elif (point >= 1):
        return z
    else:
        point = (x + (point * (z - x)))
        return point


# Dynamic Movement Behaviors
CONTINUE = 1
SEEK = 6
FLEE = 7
ARRIVE = 8 
FOLLOW_PATH = 11 # global constants


class Path:
	def __init__(self, id=1, x=0.0, y=0.0, segments=[0.0], param=0.0, distance=0.0):
		self.id = id
		self.x = x
		self.y = y
		self.segments = segments
		self.param = param
		self.distance = distance

	def assemble(self):
		pass

	def get_position(self):
		pass

	def get_param(self):
		pass


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
		pass

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
# intialize character object
char1 = Character(
	char_id=2701,
	steering_behavior=FOLLOW_PATH,
	position=[20.0, 95.0],
	max_vel=4.0,
	max_accel=2.0,
	path_to_follow=1,
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

		# update character data
		char.update_position(steering_output)

	# increment simulation time
	sim_time += time_step

