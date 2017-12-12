# fa13

def lookup(d, k):
    return [val for key, val in d if key == k]

def cond(b, t, f):
    if b: 
        return t
    else: 
        return f

def update(d, k, v):
    return [(key, cond(key == k, v, val)) for key, val in d]

def delete(d, k):
    return [(key, val) for key, val in d if key != k]

def add(d, k, v):
    return d + [(k, v)]

def update(d, k, v):
    res = []
    for key, val in d:
        if key == k:
            res.append((k, v))
        else:
            res.append((key, val))
    return res

def in_range(i, range):
    def decorator(f):
        def decorated(*args):
            res = f(*args)
            if i == -1:
                if res < range[0]:
                    raise Exception('Return value ' + str(res) + ' too small')
                elif res > range[1]:
                    raise Exception('Return value ' + str(res) + ' too big')
            elif 0 <= i < len(args):
                if args[i] < range[0]:
                    raise Exception (str(i) + "th arg " + str(args[i]) + " too small")
                elif args[i] > range[1]:
                    raise Exception (str(i) + "th arg " + str(args[i]) + " too big")
            return res
        return decorated
    return decorator


# sp13

def rev(l):
    return l[::-1]

def rev(l):
    def fold_fn(acc, ele):
        return [ele] + acc
    return reduce(fold_fn, l, [])

def print_some(l):
    def decorator(f):
        def decorated(*args):
            for i in l:
                if i in range(len(args)):
                    print "Arg " + str(i) + ": " + str(args[i])
            res = f(*args)
            if -1 in l:
                print "Return: " + str(res)
            return res
        return decorated
    return decorator

class Tree:
    def __init__(self, name, children):
        self.name = name
        self.children = children
    # Returns True if the Tree represents a prolog variable (e.g. X), and False otherwise
    def is_var(self): pass
    # Returns the string representation of the Tree as a Prolog term.
    def __repr__(self): pass

def var(n): return Tree(n, [])

def node(n, c): return Tree(n, c)

def apply_to_tree(s, t):
    if not t.is_var():
        return node(t.name, [apply_to_tree(s, c) for c in t.children])
    elif t.name in s:
        return apply_to_tree(s, s[t.name])
    else:
        return t

def unify(a, b, s={}):
    a = apply_to_tree(s, a)
    b = apply_to_tree(s, b)

    result = s.copy()

    if a.is_var() and b.is_var():
        result[a.name] = b
    elif a.is_var() and not b.is_var():
        if a.name in result:
            result = unify(result[a.name], b, result)
        else:
            result[a.name] = b
    elif not a.is_var() and b.is_var():
        return unify(b, a, s)
    elif not a.is_var() and not b.is_var():
        if a.name != b.name:
            return False
        if len(a.children) != len(b.children):
            return False
        for i in range(a.children):
            result = unify(a.children[i], b.children[i], result)
    
    return result


# wi13

def transpose(m):
    height = len(m)
    width = len(m[0])
    return [[m[i][j] for i in range(height)] for j in range(width)]

def access(g, x, y):
    try:
        return g[y][x]
    except: 
        return 0

def count_live_neighbours(g, x, y):
    live = 0
    for x_delta in [-1, 0, 1]:
        for y_delta in [-1, 0, 1]:
            if (x_delta, y_delta) != (0, 0):
                live += access(g, x+x_delta, y+y_delta)

    return live

def new_val(g, x, y):
    live = count_live_neighbours(g, x, y)

    if access(g, x, y):
        if live < 2 or live > 3:
            return 0
        if live == 2 or live == 3:
            return 1
    else:
        if live == 3:
            return 1
        return 0

def step(g):
    height = len(g)
    width = len(g[0])
    return [[new_val(g, i, j) for i in range(width)] for j in range(height)]


def lift_1(f):
    def decorated(x):
        return [f(i) for i in x]
    return decorated

def lift_2(f):
    def decorated(x,y):
        return [f(i, j) for (i, j) in zip(x, y)]
    return decorated

def lift(f):
    def decorated(*args):
        return [f(*l) for l in transpose(args)]
    return decorated