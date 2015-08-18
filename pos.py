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

def wn_defn(string, pos):
    wnpos = wn_pos(pos)
    if wnpos is None:
        return None
    sets = wn.synsets(string, wnpos)
    # if none found for specific pos, use generic
    if len(sets) == 0:
        sets = wn.synsets(string)
        if len(sets) == 0:
            return None
    return sets[0].definition()

def get_word_dict_path():
    return os.path.dirname(os.path.realpath(__file__)) + '/dictionary.json'

word_dict = None

def get_word_dict():
    global word_dict
    if word_dict is None:
        with open(get_word_dict_path(), 'r') as d:
            word_dict = json.load(d)
    return word_dict

tag_descs = nltk.data.load("help/tagsets/upenn_tagset.pickle")

def get_tag_indices(string, tok_tags):
    cur_index = 0
    out_tags = [None] * len(tok_tags)
    for i, tag in enumerate(tok_tags):
        # tag[0] is the string, tag[1] is the part of speech
        cur_index = string.find(tag[0], cur_index)
        if cur_index == -1:
            raise TagException("tags for " + tag[0] +
                               " could not be matched to indices")
        defn = wn_defn(tag[0], tag[1])
        if defn is None:
            try:
                defn = get_word_dict()[tag[0].upper()]
            except KeyError:
                pass
        out_tags[i] = {
            "start_index": cur_index,
            "end_index": cur_index + len(tag[0]),
            "text": tag[0],
            "definition": defn,
            "pos": tag[1]
        }
    return out_tags

def tag(string):
    return get_tag_indices(string, get_tags(string))
