import os
import json
import nltk
from nltk.corpus import wordnet as wn

class TagException(Exception):
    pass

def get_tags(string):
    return nltk.pos_tag(nltk.word_tokenize(string))

def wn_pos(pos):
    first = pos[0].lower()
    if first in ['n', 'v', 'a', 'r']:
        return first
    return None

tag_descs = nltk.data.load("help/tagsets/upenn_tagset.pickle")

def get_tag_indices(string, tok_tags):
    cur_index = 0
    out_tags = [None] * len(tok_tags)
    for i, tag in enumerate(tok_tags):
        # tag[0] is the string, tag[1] is the part of speech
        # find_char = tag[0].replace('``', '"')
        # cur_index = string.find(find_char, cur_index)
        # if cur_index == -1:
        #     raise TagException("tags for " + tag[0] +
        #                        " could not be matched to indices")
        out_tags[i] = {
            # "start_index": cur_index,
            # "end_index": cur_index + len(tag[0]),
            "text": tag[0],
            "pos": tag[1]
        }
    return out_tags

def tag(string):
    return get_tag_indices(string, get_tags(string))
