#!/usr/bin/env python

import sys
import graphviz as gv

colors = {
    'scheme': 'spectral11',
    'lit': '4',
    'app': '8',
    'lam': '2'
}

def newgraph(title=None):
    graph_attr = {'rankdir': 'TD'}
    if not title is None:
        graph_attr['labelloc'] = 't'
        graph_attr['label']    = title
    return gv.Digraph(
        graph_attr=graph_attr,
        node_attr={ 'style': 'filled', 'shape': 'square', 'colorscheme': 'spectral11'}, # lightblue2
        edge_attr={'arrowhead': 'none'},
        format='png', # svg
        engine='dot'
    )

def Abs(name,body):
    return ('Abs',name,body)
def App(l,r):
    return ('App',l,r)
def Lit(name):
    return ('Lit',name)
Lam = Abs

def lambdagraph(g,expr):
    nodes = []
    edges = []
    to_connect = []
    next_node_id = 0
    nodes_to_visit = [expr]
    while len(nodes_to_visit) > 0:
        curr = nodes_to_visit.pop(0)
        typ  = curr[0]
        if not len(to_connect) < 1:
            edge = (to_connect.pop(0), str(next_node_id))
            edges.append(edge)
        if typ == 'App':
            node = g.node(str(next_node_id), label='App', shape='square', color='8')
            nodes.append(node)
            nodes_to_visit.append(curr[1]) # left
            nodes_to_visit.append(curr[2]) # right
            to_connect.append(str(next_node_id))
            to_connect.append(str(next_node_id))
        elif typ == 'Lit':
            node = g.node(str(next_node_id), label=curr[1], shape='circle', color='4')
        elif typ == 'Abs':
            node = g.node(str(next_node_id), label='λ'+curr[1], shape='doublecircle', color='2')
            nodes.append(node)
            nodes_to_visit.append(curr[2]) # body
            to_connect.append(str(next_node_id))
        else:
            break
        next_node_id += 1
    for e in edges:
        g.edge(e[0],e[1])
    return g


import os

def path(p):
    return os.path.join(os.getcwd(),p)

# def save(g,name='graph'):
#     g.render(filename='./graphs/g.gv')
#     try:
#         os.remove(path(f'./graphs/{name}.png'))
#         os.rename(path('./graphs/g.gv.png'), path(f'./graphs/{name}.png'))
#         os.remove(path('./graphs/g.gv'))
#     except err:
#         print(err)
#         print('Saved to: ', path(f'./graphs/{name}.png'))
#         exit()
def save(g,name='graph'):
    g.render(filename='./graphs/g.gv',view=False)
    try:
        try:
            os.remove(path(f'./graphs/{name}.png'))
        except BaseException:
            t = None
        os.rename(path('./graphs/g.gv.png'), path(f'./graphs/{name}.png'))
        os.remove(path('./graphs/g.gv'))
    except BaseException:
        print('Saved to: ', path(f'./graphs/{name}.png'))
        exit()
import random

max_depth = 20
def randexpr(n=1, choices=[1,1, 2,2,2,2, 3,3,3,3]):
    rchar = random.choice('abcdefghijklmnopqrstuvwxyz')
    c = random.choice(choices)
    if n > max_depth:
        choices = choices.copy() + [1]
    if c == 1:
        return Lit(rchar)
    elif c == 2:
        return Lam(rchar,randexpr(n+1,choices))
    elif c == 3:
        return App(randexpr(n+1,choices),randexpr(n+1,choices))
    print(n,c)
    return Lit(rchar)


def codegen_expr(expr,lambda_symbol='lambda ',abstraction_symbol=' -> ',redundant_parens=True):
    def codegen_expr_(e):
        return codegen_expr(e,lambda_symbol=lambda_symbol,abstraction_symbol=abstraction_symbol,redundant_parens=redundant_parens)
    def lit(e):
        return e[1]
    def app(e):
        return f'({codegen_expr_(e[1])} {codegen_expr_(e[2])})'
    def lam(e):
        return f'({lambda_symbol}{e[1]}{abstraction_symbol}({codegen_expr_(e[2])}))'
    match_ = {
        'Lit': lit,
        'App': app,
        'Abs': lam
    }
    node_type = expr[0]
    return match_[node_type](expr)


def traditional_notation(e):
    return codegen_expr(e, lambda_symbol='λ', abstraction_symbol='.',redundant_parens=False)



import json


def expr_from_dict(lambda_dict):
    def lit(d):
        return Lit(d['name'])
    def app(d):
        return App(expr_from_dict(d['lhs']),expr_from_dict(d['rhs']))
    def lam(d):
        return Lam(d['var'],expr_from_dict(d['body']))
    match_ = {
        'Lit': lit,
        'App': app,
        'Lam': lam,
        'Abs': lam,
    }
    rule = lambda_dict['rule']
    return match_[rule](lambda_dict)

def lambda_expr_from_json(json_string):
    parsed_json = json.loads(json_string)
    return expr_from_dict(parsed_json)


def graph_from_expr(lambda_expr):
    lambda_string = traditional_notation(lambda_expr)
    lambda_graph = lambdagraph(newgraph(title=lambda_string),lambda_expr)
    return lambda_graph

def graph_from_json(json_string):
    graph = graph_from_expr(lambda_expr_from_json(json_string))
    return graph




json_string = sys.argv[1] # r'{"var":"x","body":{"rule":"Lit","name":"x"},"rule":"Lam"}'
# lambda_expr = lambda_expr_from_json(json_string)
# lambda_expr
g = graph_from_json(json_string)

save(g)




