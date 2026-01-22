import os
import sys
import fileinput
import re
import collections
sifFile = './PathwayCommons9.Detailed.hgnc.sif'
ruleFile = './RulesOfTranslation.txt'
resultFile = './PathwayCommons9.scm'
tempFile = open(sifFile, 'r+')
translationFile = open(ruleFile, 'r+')
finalFile = open(resultFile, 'a')
def findInstruction(text, atom1, atom2):
    copy = False
    instruction = ''
    translationFile.seek(0)
    for line in translationFile:
        x = line.strip()
        if x == text:
            copy = True
            continue
        if copy:
            if not line.isupper():
                if '"$P1"' in line:
                    replace1 = '"' + atom1 + '"'
                    instruction += line.replace('"$P1"', replace1)
                elif '"$P2"' in line:
                    replace2 = '"' + atom2 + '"'
                    instruction += line.replace('"$P2"', replace2)
                else:
                    instruction += line
            else:
                break
    return instruction
for lne in tempFile:
    words = lne.split()
    print('rule is ' + words[1].strip().upper())
    write = findInstruction(words[1].strip().upper(), words[0], words[2])
    finalFile.write(write + '\n')