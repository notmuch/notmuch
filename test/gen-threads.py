# Generate all possible single-root message thread structures of size
# argv[1].  Each output line is a thread structure, where the n'th
# field is either a number giving the parent of message n or "None"
# for the root.
import sys
from itertools import chain, combinations

def subsets(s):
    return chain.from_iterable(combinations(s, r) for r in range(len(s)+1))

nodes = set(range(int(sys.argv[1])))

# Queue of (tree, free, to_expand) where tree is a {node: parent}
# dictionary, free is a set of unattached nodes, and to_expand is
# itself a queue of nodes in the tree that need to be expanded.
# The queue starts with all single-node trees.
queue = [({root: None}, nodes - {root}, (root,)) for root in nodes]

# Process queue
while queue:
    tree, free, to_expand = queue.pop()

    if len(to_expand) == 0:
        # Only print full-sized trees
        if len(free) == 0:
            print(" ".join(map(str, [msg[1] for msg in sorted(tree.items())])))
    else:
        # Expand node to_expand[0] with each possible set of children
        for children in subsets(free):
            ntree = {child: to_expand[0] for child in children}
            ntree.update(tree)
            nfree = free.difference(children)
            queue.append((ntree, nfree, to_expand[1:] + tuple(children)))
