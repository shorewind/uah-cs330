# Names:Gianna Foti, Esther Shore 
# Professor: Jay Sebastian
# Course: CS330 
# Due: 23 February 2024

import math
import numpy as np

#vector math 

#calculate length of a 2d vector 
def vectorLength(v):
	length = math.sqrt(v[0]**2 + v[1]**2)
	return length

#normalize a 2D vector
def vectorNormalize(v):
    length = vectorLength(v)
    if length != 0:
        return [v[0]/length, v[1]/length]
    else:
        return [0, 0]

# Calculate scalar dot product of two 2D vectors.
def dot(v1,v2):
	sum = ([v1[0]*v2[0] + v1[1]*v2[1]])
	return sum

#converting an orientation in radians to a unit vector using the sine and cosine trigonometric functions.
def vectorOrientation(orientation):
    result = [math.sin(orientation), math.cos(orientation)]
    return result

#Geometry Functions
#calculate the distance between two points in 2D
def distancePointPoint(x,z):
	distance = math.sqrt((z[1]-x[1])**2 + (z[2]-x[2])**2)
	return distance

# Calculate the distance from a point to a line in 2D.
def distancePointLine(x, z, q):
    x0, y0 = x
    x1, y1 = z
    x2, y2 = q
    numerator = abs(((x2 - x1) * (y1 - y0)) - ((x1 - x0) * (y2 - y1)))
    denominator = np.sqrt((x2 - x1)**2 + (y2 - y1)**2)
    return numerator / denominator

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

# Convert an angle (in radians) to the interval [-pi, pi].
def convertAngle(theta):
    theta = theta % (2 * math.pi)
    if abs(theta) > math.pi:
        theta = theta - (2 * math.pi * (theta / abs(theta)))
    return theta



# dynamic movement behaviors
CONTINUE = 1
SEEK = 5
FLEE = 7
ARRIVE = 8

total_run_time = 50
time_step = 0.5
num_time_steps = int(total_run_time / time_step)
sim_time = 0

class Character:
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

	def print_csv_data(self, timestamp):
		return (
				f"{timestamp}, {self.char_id}, {self.position[0]}, {self.position[1]}, "
				f"{self.velocity[0]}, {self.velocity[1]}, {self.linear_accel[0]}, "
				f"{self.linear_accel[1]}, {self.orientation}, {self.steering_behavior}, "
				f"{self.collision_status}"
		)

	def get_continue_steering(self):
		pass

	def get_seek_steering(self):
		pass

	def get_flee_steering(self):
		pass
	
	def get_arrive_steering(self):
		pass

	def update_position(self):
		pass


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

output_file = open("data.txt", 'w')

with open("data.txt", 'a') as output_file:
	for char in characters:
		print(char.print_csv_data(sim_time))
