# Names:Gianna Foti, Esther Shore 
# Professor: Jay Sebastian
# Course: CS330 
# Due: 23 February 2024

# notes; a lot of this is used from the r code. I translated the r code into python

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




