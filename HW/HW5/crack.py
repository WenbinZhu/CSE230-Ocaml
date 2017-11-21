
from misc import *
import crypt
import re

def load_words(filename,regexp):
    """Load the words from the file filename that match the regular
       expression regexp.  Returns a list of matching words in the order
       they are in the file."""
    ll = [l[:-1] for l in open(filename, 'r').readlines()]
    return filter(lambda s: re.match(regexp, s), ll)

def transform_reverse(str):
    """Return a list with the original string and the reversal of the original string."""
    return [str, str[::-1]]

def transform_capitalize(str):
    """return a list of all the possible ways to capitalize the input string."""
    if not str:
        return [""]

    rest = transform_capitalize(str[1:])
    c = str[0]

    if not c.isalpha():
        return [c + s for s in rest]

    lower = [c.lower() + s for s in rest]
    upper = [c.upper() + s for s in rest]

    return lower + upper

def transform_digits(str):
    """Return a list of all possible ways to replace letters with similar 
    looking digits according to the mappings"""
    def helper(str, mapping):
        if not str:
            return [""]
        
        rest = helper(str[1:], mapping)
        ll = [str[0] + s for s in rest]
        digits = mapping.get(str[0].lower(), [])
        
        for d in digits:
            ll += [d + s for s in rest]
        
        return ll

    mapping = {'o': '0', 'i': '1', 'l': '1', 'z': '2', 'e': '3', 'a': '4',
               's': '5', 'b': '68', 't': '7', 'g': '9', 'q': '9'}

    return helper(str, mapping)

def check_pass(plain,enc):
    """Check to see if the plaintext plain encrypts to the encrypted
       text enc"""
    return enc == crypt.crypt(plain, enc[:2])

def load_passwd(filename):
    """Load the password file filename and returns a list of
       dictionaries with fields "account", "password", "UID", "GID",
       "GECOS", "directory", and "shell", each mapping to the
       corresponding field of the file."""
    ll = []
    
    with open(filename, 'r') as file:
        for line in file:
            account, password, UID, GID, GECOS, directory, shell = line[:-1].split(':')
            ll.append({'account': account, 'password': password, 'UID': int(UID), 'GID': int(GID),
                       'GECOS': GECOS, 'directory': directory, 'shell': shell})

    return ll

def crack_pass_file(pass_filename,words_filename,out_filename):
    """Crack as many passwords in file fn_pass as possible using words
       in the file words"""
    passwds = load_passwd(pass_filename)

    with open(words_filename, 'r') as fin:
        lines = [w[:-1] for w in fin.readlines() if 7 <= len(w) <= 9]
        lines = set(lines)
    
    with open(out_filename, 'w') as fout:
        rev = set()
        for w in lines:
            rev |= set(transform_reverse(w))
        find(rev, passwds, fout)

        cap = set()
        for w in rev:
            cap |= set(transform_capitalize(w))
        cap -= rev
        find(cap, passwds, fout)

        for word in lines:
            print word
            words = set(transform_reverse(word))
            for s in [transform_capitalize(w) for w in words]:
                words = words.union(s)

            dig = set()
            for s in [transform_digits(w) for w in words]:
                dig = dig.union(s)
            dig -= words

            find(dig, passwds, fout)

            if len(passwds) == 0:
                return

def find(words, passwds, fout):
    """Helper function for crack_pass_file."""
    for word in words:
        cracked = []
        for i in range(len(passwds)):
            p = passwds[i]
            if check_pass(word, p['password']):
                fout.write(p['account'] + "=" + word + '\n')
                fout.flush()
                cracked.append(i)
        
        for i in cracked:
            del passwds[i]
                            