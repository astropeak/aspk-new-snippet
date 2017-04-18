class Structure:
    _fields = []
    def __init__(self, *args):
        if len(self._fields) != len(args):
            raise TypeError('Expected {} arguments'.format(len(self._fields)))
        for k, v in zip(self._fields, args):
            setattr(self, k, v)

    def __str__(self):
        return '({})'.format(', '.join('{}: {}'.format(f, getattr(self, f)) for f in self._fields))

