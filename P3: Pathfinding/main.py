# Names: Gianna Foti, Esther Shore
# Professor: Jay Sebastian
# Course: CS330 AI and Game Dev
# Assignment: Program 3 Pathfinding
# Due: April 12 2024

# output structure of loaded graph (nodes, connections, connection weights)
# output shortest path for each of five pairs of nodes

# paths
# 1 to 29

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

    def print_nodes(self):
        for node in self.nodes.values():
            print(
                    f"N {node.number} "
                    f"{node.status} {node.cost_so_far} {node.heuristic} {node.total} {node.previous} "
                    f"{node.loc_x} {node.loc_z}\n"
                )

    def print_connections(self):
        for connection in self.connections.values():
            print(f"C {connection.number} {connection.from_node} {connection.to_node} {connection.cost}\n")

    # A*
    def find_lowest(open_nodes):
        pass

    def heuristic(node1, node2):
        pass

    def get_connections(current_node):
        pass

    def find_path(start_node, goal_node):
        for i in range(1, len(graph.nodes) + 1):
            pass
        pass

    def retrieve_path(start_node, goal_node):
        pass


graph = Graph()

# load nodes to graph
with open("CS 330, Pathfinding, Graph AB Nodes v3.txt", 'r') as nodes_file:
    for line in nodes_file:
        fields = line.strip().split(',')
        fields = [field.strip() for field in fields]
        if fields[0] == '"N"':
            current_node = Node(fields[1], fields[2], fields[3], fields[4], fields[5], fields[6], fields[7], fields[8])
            graph.add_node(current_node)

# load connections to graph
with open("CS 330, Pathfinding, Graph AB Connections v3.txt", 'r') as connections_file:
    for line in connections_file:
        fields = line.strip().split(',')
        fields = [field.strip() for field in fields]
        if fields[0] == '"C"':
            current_connection = Connection(fields[1], fields[2], fields[3], fields[4])
            graph.add_connection(current_connection)

