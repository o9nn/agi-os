from operator import methodcaller
def add_eqcost_linkage_order(original_class):
    class eqcost_sorted_parse(original_class.sentence_parse):
        def __init__(self, linkages):
            self.linkages = linkages
            self.sent = linkages.sent
            self.rc = linkages.rc
            self.linkage_list = []
            self.num = 0
            self.cost = None
            self.saved_next = None
        def __iter__(self):
            return self
        def next(self):
            if self.num >= len(self.linkage_list) - 1:
                if self.linkage_list and (not self.saved_next):
                    raise StopIteration()
                self.linkage_list = []
                self.cost = None
                while True:
                    if self.saved_next:
                        linkage = self.saved_next
                        self.saved_next = None
                    else:
                        try:
                            linkage = self.linkages.next()
                        except StopIteration:
                            break
                    if not self.sent.parse_options.use_sat:
                        cost = [linkage.unused_word_cost(), linkage.disjunct_cost(), linkage.link_cost()]
                        if not self.cost:
                            self.cost = cost
                        elif self.cost != cost:
                            self.saved_next = linkage
                            break
                    self.linkage_list.append(linkage)
                if not self.linkage_list:
                    raise StopIteration()
                self.num = -1
                self.linkage_list.sort(key=methodcaller('diagram', screen_width=9999))
            self.num += 1
            return self.linkage_list[self.num]
        __next__ = next
    original_class.original_parse = original_class.parse
    def parse(self, parse_options=None):
        linkages = self.original_parse() if parse_options is None else self.original_parse(parse_options)
        return eqcost_sorted_parse(linkages)
    original_class.parse = parse
def add_test_option(original_class, test='', debug='', verbosity=0):
    class ParseOptions_testing(original_class):
        def __init__(self, *args, **kwargs):
            super(ParseOptions_testing, self).__init__(*args, **kwargs)
            if test:
                self.test = test
            if debug:
                self.debug = debug
            if verbosity:
                self.verbosity = verbosity
    return ParseOptions_testing