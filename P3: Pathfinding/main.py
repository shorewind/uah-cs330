# Names: Gianna Foti, Esther Shore
# Professor: Jay Sebastian
# Course: CS330 AI and Game Dev
# Assignment: Program 3 Pathfinding
# Due: 12 April 2024

import math
from datetime import datetime

# constants
UNDEFINED = 0
UNVISITED = 1
OPEN = 2
CLOSED = 3


class Node:
    def __init__(self, number, status, cost_so_far, heuristic, total, previous, loc_x, loc_z):
        self.number = number
        self.status = status
        self.cost_so_far = cost_so_far
        self.heuristic = heuristic
        self.total = total
        self.previous = previous
        self.loc_x = loc_x
        self.loc_z = loc_z


class Connection:
    def __init__(self, number, from_node, to_node, cost):
        self.number = number
        self.from_node = from_node
        self.to_node = to_node
        self.cost = cost


class Graph:
    def __init__(self, nodes={}, connections={}):
        self.nodes = nodes
        self.connections = connections

    def add_node(self, node):
        self.nodes[node.number] = node

    def add_connection(self, connection):
        self.connections[connection.number] = connection


# A* helper functions
# find open node with lowest total cost
def find_lowest(graph, open_nodes):
    min_total = float('inf')
    min_node = None

    for node in open_nodes:
        current_total = graph.nodes[node].total
        if current_total < min_total:
            min_total = current_total
            min_node = node
    return min_node

# calculate heuristic value using standard Euclidean 2D distance
def heuristic(graph, node1, node2):
    distance = math.sqrt((graph.nodes[node2].loc_x - graph.nodes[node1].loc_x)**2 + (graph.nodes[node2].loc_z - graph.nodes[node1].loc_z)**2)
    return distance 

# get all outgoing connections for current node
def get_connections(graph, current_node):
    connections = [num for num, connection in graph.connections.items() if connection.from_node == current_node]
    return connections

# initialize list of open nodes
open_nodes = []

# find path from start node to goal node using A*
def find_path(graph, start_node, goal_node):
    # initialize node array
    for node_num in range(1, len(graph.nodes) + 1):
        graph.nodes[node_num].status = UNVISITED
        graph.nodes[node_num].cost_so_far = float("inf")
        graph.nodes[node_num].previous = UNDEFINED
    
    # initialize start node
    graph.nodes[start_node].status = OPEN
    graph.nodes[start_node].cost_so_far = 0
    iteration = 0
    open_nodes.append(start_node)

    # execute once for each node or until path found
    while len(open_nodes) > 0:
        iteration = iteration + 1

        current_node = find_lowest(graph, open_nodes)
        if current_node == goal_node:
            break

        current_connections = get_connections(graph, current_node)

        for connection in current_connections:
            to_node = graph.connections[connection].to_node
            to_cost = graph.nodes[current_node].cost_so_far + graph.connections[connection].cost

            if to_cost < graph.nodes[to_node].cost_so_far:
                graph.nodes[to_node].status = OPEN
                graph.nodes[to_node].cost_so_far = to_cost
                graph.nodes[to_node].heuristic = heuristic(graph, to_node, goal_node)
                graph.nodes[to_node].total = graph.nodes[to_node].cost_so_far + graph.nodes[to_node].heuristic
                graph.nodes[to_node].previous = current_node
                open_nodes.append(to_node)

        graph.nodes[current_node].status = CLOSED
        open_nodes.remove(current_node)

# retrieve path nodes from start node to goal node
def retrieve_path(graph, start_node, goal_node):
    path = []
    current_node = goal_node
    while current_node != start_node and current_node != UNDEFINED:
        path.append(current_node)
        current_node = graph.nodes[current_node].previous

    if current_node == start_node:
        path.append(start_node)
        path.reverse()
    else:
        path = []
    return path

# initialize graph object of nodes and connections
graph = Graph()

# load nodes to graph
with open("CS 330, Pathfinding, Graph AB Nodes v3.txt", 'r') as nodes_file:
    for line in nodes_file:
        fields = line.strip().split(',')
        fields = [field.strip() for field in fields]
        if fields[0] == '"N"':
            current_node = Node(int(fields[1]), int(fields[2]), float(fields[3]), float(fields[4]), float(fields[5]), int(fields[6]), float(fields[7]), float(fields[8]))
            graph.add_node(current_node)

# load connections to graph
with open("CS 330, Pathfinding, Graph AB Connections v3.txt", 'r') as connections_file:
    for line in connections_file:
        fields = line.strip().split(',')
        fields = [field.strip() for field in fields]
        if fields[0] == '"C"':
            current_connection = Connection(int(fields[1]), int(fields[2]), int(fields[3]), float(fields[4]))
            graph.add_connection(current_connection)

# run A* and output results to file
test_cases = [(1, 29), (1, 38), (11, 1), (33, 66), (58, 43)]

with open(f"CS 330, Pathfinding, Trace Adventure Bay AB {datetime.today().strftime('%Y-%m-%d')}.txt", 'w') as output_file:
    output_file.write(f"CS 330, Pathfinding, Begin {datetime.today().strftime('%Y-%m-%d %H:%M:%S')}\n\n")
    output_file.write("Loaded scenario Adventure Bay AB\n\n")

    output_file.write("Nodes\n")
    for node in graph.nodes.values():
        output_file.write(
                f"N {node.number} "
                f"{node.status} {node.cost_so_far} {node.heuristic} {node.total} {node.previous} "
                f"{node.loc_x} {node.loc_z}\n"
            )

    output_file.write("\nConnections\n")
    for connection in graph.connections.values():
        output_file.write(f"C {connection.number} {connection.from_node} {connection.to_node} {connection.cost}\n")

    for start_node, goal_node in test_cases:
        find_path(graph, start_node, goal_node)
        path = retrieve_path(graph, start_node, goal_node)
        if path:
            output_file.write(f"\nPath from {start_node} to {goal_node} path= {' '.join(str(node) for node in path)} cost= {graph.nodes[goal_node].cost_so_far}")
        else:
            output_file.write(f"\nPath from {start_node} to {goal_node} not found")

    output_file.write(f"\n\nCS 330, Pathfinding, End {datetime.today().strftime('%Y-%m-%d %H:%M:%S')}\n")
