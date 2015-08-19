import sys
import json
import pos

def main():
    run(sys.argv[1:])

def run(args):
    if len(args) == 0:
        print(tag_stdin())
    else:
        print(get_descs(args))

def tag_stdin():
    tags = pos.tag(sys.stdin.read())
    return json.dumps(tags)

def get_descs(tags):
    descs = [pos.tag_descs[tag] for tag in tags]
    return json.dumps(descs)

if __name__ == '__main__':
    main()
