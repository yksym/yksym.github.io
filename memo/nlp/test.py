#!/usr/bin/python
# -*- coding: utf-8 -*-

import MeCab
import zenhan
import sys
import string
import re, pprint

def pp(obj):
  pp = pprint.PrettyPrinter(indent=2, width=160)
  str = pp.pformat(obj)
  return re.sub(r"\\u([0-9a-f]{4})", lambda x: unichr(int("0x"+x.group(1), 16)), str)

sentence = """タイヤがスリップしないようにしてほしい。
雨でも雪でもスリップしないようにしてほしい。ただし油の場合はいい感じに滑ってほしい!
"""

def readStr (filepath):
    if filepath == '-':
        return "".join([l for l in sys.stdin])
    else:
        return "".join([l for l in open(filepath, 'r')])

#名詞の頻度
def showHist (s):
    t = MeCab.Tagger ("-ochasen")
    m = t.parseToNode(s)
    result = {}
    while m:
        if m.surface:
            #help (m)
            key = m.surface
            info = m.feature.split(",")
            label = info[0]
            label2 = info[1]
            bform = info[-3]
            prono = info[-2]
            #print key,m.feature
            if label == '名詞' and label2 not in ['接尾', '非自立', '特殊', '代名詞']:
               result[key]  = result.get(key, 0) + 1
            #print m.surface, "\t", label, bform, prono, m.feature
        m = m.next
    for k, v in sorted(result.items(), key=lambda x:x[1], reverse=True):
        print k, v

def pyon (s):
    end = ["。","！","？","．","・","…","\n","\r"]
    print re.sub("((?:%s)+)" % "|".join(end), 'ぴょん\\1', s)

if __name__ == "__main__":
    s = sentence
    if len(sys.argv) > 1:
        s = readStr(sys.argv[1])
    s = zenhan.h2z(s.decode('utf-8')).encode('utf-8')
    #showHist (s)
    pyon (s)



