import sys
import re
import argparse
from linkgrammar import Sentence, ParseOptions, Dictionary, LG_Error, LG_TimerExhausted, Clinkgrammar as clg
def nsuffix(q):
    return '' if q == 1 else 's'
class Formatter(argparse.HelpFormatter):
    def _format_usage(self, usage, actions, groups, prefix):
        usage_message = super(Formatter, self)._format_usage(usage, actions, groups, prefix)
        return re.sub('(usage: \\S+) (.*) \\[lang]', '\\1 [lang] \\2', str(usage_message))
is_stdin_atty = sys.stdin.isatty()
PROMPT = 'sentence-check: ' if is_stdin_atty else ''
DISPLAY_GUESSES = True
BATCH_LABELS = '*: '
print('Version:', clg.linkgrammar_get_version())
args = argparse.ArgumentParser(formatter_class=Formatter)
args.add_argument('lang', nargs='?', default='en', help='language or dictionary location')
args.add_argument('-v', '--verbosity', type=int, default=0, choices=range(0, 199), metavar='[0-199]', help='1: Basic verbosity; 2-4: Trace; >5: Debug')
args.add_argument('-p', '--position', action='store_true', help='show word sentence position')
args.add_argument('-nm', '--no-morphology', dest='morphology', action='store_false', help='do not display morphology')
args.add_argument('-i', '--interactive', action='store_true', help='interactive mode after each result')
arg = args.parse_args()
try:
    lgdict = Dictionary(arg.lang)
except LG_Error:
    args.print_usage()
    sys.exit(2)
po = ParseOptions(verbosity=arg.verbosity)
po.max_null_count = 999
po.linkage_limit = 10000
po.max_parse_time = 10
po.spell_guess = True if DISPLAY_GUESSES else False
po.display_morphology = arg.morphology
while True:
    try:
        sentence_text = input(PROMPT)
    except EOFError:
        print('EOF')
        exit(0)
    if not is_stdin_atty and sentence_text:
        if sentence_text[0] == '%':
            continue
        if sentence_text[0] == '!':
            continue
        if sentence_text[0] in BATCH_LABELS:
            sentence_text = sentence_text[1:]
    if sentence_text.strip() == '':
        continue
    if not is_stdin_atty:
        print('\n' + sentence_text)
    sent = Sentence(str(sentence_text), lgdict, po)
    try:
        linkages = sent.parse()
    except LG_TimerExhausted:
        print('Sentence too complex for parsing in ~{} second{}.'.format(po.max_parse_time, nsuffix(po.max_parse_time)))
        continue
    if not linkages:
        print('Error occurred - sentence ignored.')
        continue
    if len(linkages) <= 0:
        print('Cannot parse the input sentence')
        continue
    null_count = sent.null_count()
    if arg.position:
        print(' ' * len(PROMPT), end='')
        for p in range(0, len(sentence_text)):
            print(p % 10, end='')
        print()
    if null_count == 0:
        print('Sentence parsed OK', end='')
    linkages = list(linkages)
    correction_found = False
    for l in linkages:
        for word in l.words():
            if word.find('.#') > 0:
                correction_found = True
                break
        if correction_found:
            break
    if correction_found:
        print(' - with correction', end='')
    if null_count == 0:
        print('.')
    guess_found = False
    if DISPLAY_GUESSES:
        for word in linkages[0].words():
            if re.search('\\S+\\[[^]]+]', word):
                guess_found = True
                break
    if arg.position or guess_found or correction_found or (null_count != 0):
        print('Sentence has {} unlinked word{}:'.format(null_count, nsuffix(null_count)))
        result_no = 0
        uniqe_parse = {}
        for linkage in linkages:
            words = list(linkage.words())
            if str(words) in uniqe_parse:
                continue
            result_no += 1
            uniqe_parse[str(words)] = True
            if arg.position:
                words_char = []
                words_byte = []
                for wi, w in enumerate(words):
                    words_char.append(w + str((linkage.word_char_start(wi), linkage.word_char_end(wi))))
                    words_byte.append(w + str((linkage.word_byte_start(wi), linkage.word_byte_end(wi))))
                print(u'{}: {}'.format(result_no, ' '.join(words_char)))
                print(u'{}: {}'.format(result_no, ' '.join(words_byte)))
            else:
                print('{}: {}'.format(result_no, ' '.join(words)))
    if arg.interactive:
        print('Interactive session (^D to end):')
        import code
        code.interact(local=locals())