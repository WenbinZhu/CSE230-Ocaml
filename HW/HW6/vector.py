from misc import Failure

class Vector(object):
    """A fixed length vector"""
    
    def __init__(self, n):
        """Initialize vector with the argument either the length of vector or a sequence"""
        if isinstance(n, int) or isinstance(n, long):
            if n < 0:
                raise ValueError("vector length cannot be negative")
            self.data = [0.0] * n
        elif hasattr(n, '__len__') and hasattr(n, '__getitem__'):
            self.data = list(n)
        else:
            raise TypeError("vector constructor argument should be int or long or sequence")

    def __repr__(self):
        """String representation of the vector"""
        return "Vector(%s)" % str(self.data)

    def __len__(self):
        """Return the length of the vector"""
        return len(self.data)

    def __iter__(self):
        """Return an iterator for the vector"""
        for v in self.data:
            yield v
    
    def __add__(self, other):
        """Adds the vector with another vector or sequence element-wise"""
        if isinstance(other, Vector) or hasattr(other, '__len__') and hasattr(other, '__getitem__'):
            if len(self) != len(other):
                raise ValueError("operands of vector add operation must have same length")

            return Vector([x + y for x, y in zip(self, other)])

        raise TypeError("operand types of vector add operation must be either Vector or Sequence")
    
    def __radd__(self, other):
        """Adds the vector with another vector or sequence element-wise with swapped operands"""
        return self.__add__(other)

    def dot(self, other):
        """Return the sum of the element-wise products"""
        if isinstance(other, Vector) or hasattr(other, '__len__') and hasattr(other, '__getitem__'):
            if len(self) != len(other):
                raise ValueError("operands of vector dot operation must have same length")
            
            return sum([x * y for x, y in zip(self, other)])

        raise TypeError("operand types of vector dot operation must be either Vector or Sequence")

    def __getitem__(self, i):
        """Return the ith element of the vector, also supports slice level access"""
        try:
            if type(i) == slice:
                return Vector(self.data[i])
            return self.data[i]
        except:
            raise IndexError("vector index out of range")

    def __setitem__(self, i, v):
        """Set the ith element of the vector to the specified value, also supports slice level access"""
        if type(i) == slice:
            copy = self.data[:]
            copy[i] = v
            if len(copy) != len(self.data):
                raise ValueError("slice operation should not chage vector length")
        try:
            self.data[i] = v
        except TypeError, e:
            raise TypeError("can only assign an iterable")
        except:
            raise IndexError("vector index out of range")

    def __eq__(self, other):
        """Return true if all vector element is equal to the respective element in the second Vector"""
        if not isinstance(other, Vector):
            return False

        return all([x == y for x, y in zip(self, other)])

    def __ne__(self, other):
        """Return false if any vector element is not equal to the respective element in the second Vector"""
        return not self.__eq__(other)

    def __gt__(self, other):
        """Compare elements in two vectors from largest to smallest, return true/false as
        soon as the emelent of the first vector is larger/smaller than the one in second
        vector. Return false after all elements are compared and not yet returned"""
        for x, y in zip(sorted(self, reverse=True), sorted(other, reverse=True)):
            if x > y:
                return True
            if x < y:
                return False

        return False

    def __ge__(self, other):
        """Compare elements in two vectors from largest to smallest, return true/false as
        soon as the emelent of the first vector is larger/smaller than the one in second
        vector. Return true after all elements are compared and not yet returned"""
        for x, y in zip(sorted(self, reverse=True), sorted(other, reverse=True)):
            if x > y:
                return True
            if x < y:
                return False

        return True

    def __lt__(self, other):
        """Compare elements in two vectors from largest to smallest, return false/true as
        soon as the emelent of the first vector is smaller/larger than the one in second
        vector. Return false after all elements are compared and not yet returned"""
        return other.__gt__(self)

    def __le__(self, other):
        """Compare elements in two vectors from largest to smallest, return false/true as
        soon as the emelent of the first vector is smaller/larger than the one in second
        vector. Return true after all elements are compared and not yet returned"""
        return other.__ge__(self)