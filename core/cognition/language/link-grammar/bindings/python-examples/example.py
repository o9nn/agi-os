from linkgrammar import Sentence, ParseOptions, Dictionary, Clinkgrammar as clg
print('Version:', clg.linkgrammar_get_version())
po = ParseOptions(verbosity=1)
def desc(lkg):
    print(lkg.diagram())
    print('Postscript:')
    print(lkg.postscript())
    print('---')
def s(q):
    return '' if q == 1 else 's'
def linkage_stat(psent, lang, lkgs, sent_po):
    random = ' of {} random linkages'.format(clg.sentence_num_linkages_post_processed(psent._obj)) if clg.sentence_num_linkages_found(psent._obj) > sent_po.linkage_limit else ''
    print('{}: Found {} linkage{} ({}{} had no P.P. violations)'.format(lang, clg.sentence_num_linkages_found(psent._obj), s(clg.sentence_num_linkages_found(psent._obj)), len(lkgs), random))
en_lines = ['This is a test.', 'I feel is the exciter than other things']
po = ParseOptions(min_null_count=0, max_null_count=999)
en_dir = Dictionary()
for text in en_lines:
    sent = Sentence(text, en_dir, po)
    linkages = sent.parse()
    linkage_stat(sent, 'English', linkages, po)
    for linkage in linkages:
        desc(linkage)
sent = Sentence('Целью курса является обучение магистрантов основам построения и функционирования программного обеспечения сетей ЭВМ.', Dictionary('ru'), po)
linkages = sent.parse()
linkage_stat(sent, 'Russian', linkages, po)
for linkage in linkages:
    desc(linkage)
po = ParseOptions(islands_ok=True, max_null_count=1, display_morphology=True, verbosity=1)
sent = Sentence('Senin ne istediğini bilmiyorum', Dictionary('tr'), po)
linkages = sent.parse()
linkage_stat(sent, 'Turkish', linkages, po)
for linkage in linkages:
    desc(linkage)
po = ParseOptions(verbosity=0)