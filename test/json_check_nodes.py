#!/usr/bin/env python3
import re
import sys
import json


EXPR_RE = re.compile('(?P<label>[a-zA-Z0-9_-]+):(?P<address>[^=!]+)(?:(?P<type>[=!])(?P<val>.*))?', re.DOTALL|re.MULTILINE)


if len(sys.argv) < 2:
    sys.exit('usage: '+ sys.argv[0] + """ EXPR [EXPR]

Takes json data on stdin and evaluates test expressions specified in
arguments.  Each test is evaluated, and output is printed only if the
test fails.  If any test fails the return value of execution will be
non-zero.

EXPR can be one of following types:

Value test: test that object in json data found at address is equal to
specified value:

  label:address=value

Existence test: test that dict or list in json data found at address
does *not* contain the specified key:

  label:address!key

Extract: extract object from json data found at address and print

  label:address

Results are printed to stdout prefixed by expression label.  In all
cases the test will fail if object does not exist in data.

Example:

0 $ echo '["a", "b", {"c": 1}]' | python3 json_check_nodes.py 'second_d:[1]="d"' 'no_c:[2]!"c"'
second_d: value not equal: data[1] = 'b' != 'd'
no_c: dict contains key: data[2]["c"] = 1
1 $

""")


# parse expressions from arguments
exprs = []
for expr in sys.argv[1:]:
    m = re.match(EXPR_RE, expr)
    if not m:
        sys.exit("Invalid expression: {}".format(expr))
    exprs.append(m)

data = json.load(sys.stdin)

fail = False

for expr in exprs:
    # print(expr.groups(),fail)

    e = 'data{}'.format(expr.group('address'))
    try:
        val = eval(e)
    except SyntaxError:
        fail = True
        print("{}: syntax error on evaluation of object: {}".format(
            expr.group('label'), e))
        continue
    except:
        fail = True
        print("{}: object not found: data{}".format(
            expr.group('label'), expr.group('address')))
        continue

    if expr.group('type') == '=':
        try:
            obj_val = json.loads(expr.group('val'))
        except:
            fail = True
            print("{}: error evaluating value: {}".format(
                expr.group('label'), expr.group('address')))
            continue
        if val != obj_val:
            fail = True
            print("{}: value not equal: data{} = {} != {}".format(
                expr.group('label'), expr.group('address'), repr(val), repr(obj_val)))

    elif expr.group('type') == '!':
        if not isinstance(val, (dict, list)):
            fail = True
            print("{}: not a dict or a list: data{}".format(
                expr.group('label'), expr.group('address')))
            continue
        try:
            idx = json.loads(expr.group('val'))
            if idx in val:
                fail = True
                print("{}: {} contains key: {}[{}] = {}".format(
                    expr.group('label'), type(val).__name__, e, expr.group('val'), val[idx]))
        except SyntaxError:
            fail = True
            print("{}: syntax error on evaluation of value: {}".format(
                expr.group('label'), expr.group('val')))
            continue


    elif expr.group('type') is None:
        print("{}: {}".format(expr.group('label'), val))


if fail:
    sys.exit(1)
sys.exit(0)
