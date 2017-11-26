from misc import Failure

class profiled(object):
    def __init__(self,f):
        self.__count=0
        self.__f=f
        self.__name__=f.__name__
    def __call__(self,*args,**dargs):
        self.__count+=1
        return self.__f(*args,**dargs)
    def count(self):
        return self.__count
    def reset(self):
        self.__count=0

class traced(object):
    """A decorator that prints out an ASCII art tree 
    of the recursive calls and their return values"""
    __level = 0
    
    def __init__(self,f):
        """Initialize the decorator with the decorated function"""
        self.__f = f
        self.__name__ = f.__name__
    
    def __call__(self, *args, **kargs):
        """Print out an ASCII art tree of the recursive calls on the decorated 
        function and their return values. If an exception occurs in the decorated 
        funciton, throw the same exception"""
        line = '| ' * traced.__level + ',- '
        line += self.__name__ + '('
        line += ', '.join([repr(arg) for arg in args])
        line += ', '.join([repr(k) + '=' + repr(v) for k, v in kargs.items()])
        line += ')'
        print line
        traced.__level += 1

        try:
            result = self.__f(*args, **kargs)
            traced.__level -= 1
            line = '| ' * traced.__level + '`- ' + repr(result)
            print line
            return result
        except Exception, e:
            traced.__level -= 1
            raise e

class memoized(object):
    """A decorator that memorizes functions that have already been called with
    the given arguments. Return the same value if the function is called again"""

    def __init__(self,f):
        """Initialize the decorator with the decorated function"""
        self.__f = f
        self.__name__ = f.__name__
        self.__mem = {}

    def __call__(self, *args, **kargs):
        """Use a dictionary to store the functions that have been called with
        the given positional arguments and keyword arguments. Return the same
        value or throw the same exception is the function is called again"""
        key = (self.__name__, str(args), str(kargs))

        if key in self.__mem:
            result = self.__mem[key]

            if isinstance(result, Exception):
                raise result
            else:
                return result
        
        try:
            result = self.__f(*args, **kargs)
            self.__mem[key] = result
            return result
        except Exception, e:
            self.__mem[key] = e
            raise e

# run some examples.  The output from this is in decorators.out
def run_examples():
    for f,a in [(fib_t,(7,)),
                (fib_mt,(7,)),
                (fib_tm,(7,)),
                (fib_mp,(7,)),
                (fib_mp.count,()),
                (fib_mp,(7,)),
                (fib_mp.count,()),
                (fib_mp.reset,()),
                (fib_mp,(7,)),
                (fib_mp.count,()),
                (even_t,(6,)),
                (quicksort_t,([5,8,100,45,3,89,22,78,121,2,78],)),
                (quicksort_mt,([5,8,100,45,3,89,22,78,121,2,78],)),
                (quicksort_mt,([5,8,100,45,3,89,22,78,121,2,78],)),
                (change_t,([9,7,5],44)),
                (change_mt,([9,7,5],44)),
                (change_mt,([9,7,5],44)),
                ]:
        print "RUNNING %s(%s):" % (f.__name__,", ".join([repr(x) for x in a]))
        rv=f(*a)
        print "RETURNED %s" % repr(rv)

@traced
def fib_t(x):
    if x<=1:
        return 1
    else:
        return fib_t(x-1)+fib_t(x-2)

@traced
@memoized
def fib_mt(x):
    if x<=1:
        return 1
    else:
        return fib_mt(x-1)+fib_mt(x-2)

@memoized
@traced
def fib_tm(x):
    if x<=1:
        return 1
    else:
        return fib_tm(x-1)+fib_tm(x-2)

@profiled
@memoized
def fib_mp(x):
    if x<=1:
        return 1
    else:
        return fib_mp(x-1)+fib_mp(x-2)

@traced
def even_t(x):
    if x==0:
        return True
    else:
        return odd_t(x-1)

@traced
def odd_t(x):
    if x==0:
        return False
    else:
        return even_t(x-1)

@traced
def quicksort_t(l):
    if len(l)<=1:
        return l
    pivot=l[0]
    left=quicksort_t([x for x in l[1:] if x<pivot])
    right=quicksort_t([x for x in l[1:] if x>=pivot])
    return left+l[0:1]+right

@traced
@memoized
def quicksort_mt(l):
    if len(l)<=1:
        return l
    pivot=l[0]
    left=quicksort_mt([x for x in l[1:] if x<pivot])
    right=quicksort_mt([x for x in l[1:] if x>=pivot])
    return left+l[0:1]+right

class ChangeException(Exception):
    pass

@traced
def change_t(l,a):
    if a==0:
        return []
    elif len(l)==0:
        raise ChangeException()
    elif l[0]>a:
        return change_t(l[1:],a)
    else:
        try:
            return [l[0]]+change_t(l,a-l[0])
        except ChangeException:
            return change_t(l[1:],a)

@traced
@memoized
def change_mt(l,a):
    if a==0:
        return []
    elif len(l)==0:
        raise ChangeException()
    elif l[0]>a:
        return change_mt(l[1:],a)
    else:
        try:
            return [l[0]]+change_mt(l,a-l[0])
        except ChangeException:
            return change_mt(l[1:],a)


