def with_doc(f):
    return property(f, None, None, f.__doc__)
def cached(f):
    def get(self):
        try:
            return self._property_cache[f]
        except AttributeError:
            self._property_cache = {}
        except KeyError:
            pass
        x = self._property_cache[f] = f(self)
        return x
    def set(self, val):
        propcache = self.__dict__.setdefault('_property_cache', {})
        propcache[f] = val
    def fdel(self):
        propcache = self.__dict__.setdefault('_property_cache', {})
        del propcache[f]
    return property(get, set, fdel)