#PA 4

import re

"Miscellaneous functions to practice Python"

class Failure(Exception):
    """Failure exception"""
    def __init__(self,value):
        self.value=value
    def __str__(self):
        return repr(self.value)

# Problem 1

# data type functions

def closest_to(l,v):
    """Return the element of the list l closest in value to v.  In the case of
       a tie, the first such element is returned.  If l is empty, None is returned."""
    c = m = None

    for n in l:
        if (m is None or abs(n - v) < m):
            c = n
            m = abs(n - v)
    
    return c

def make_dict(keys,values):
    """Return a dictionary pairing corresponding keys to values."""
    return {k : v for (k, v) in zip(keys, values)}
   
# file IO functions
def word_count(fn):
    """Open the file fn and return a dictionary mapping words to the number
       of times they occur in the file.  A word is defined as a sequence of
       alphanumeric characters and _.  All spaces and punctuation are ignored.
       Words are returned in lower case"""
    dic = {}
    s = open(fn, 'r').read().lower()
    l = re.findall(r'\w+', s)

    for w in l:
        dic[w] = dic.get(w, 0) + 1

    return dic








